require(raster)
require(terra)
require(dplyr)

puc1 = raster('pucClass-0000000000-0000000000.tif')
m <- cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
           to = c(1.8, 2.6, 3.4, 4.2, Inf), 
           becomes = c(1, 2, 3, 4, 5))
puc1class <- reclassify(puc1, m)
rm(puc1)

puc2 = raster('pucClass-0000000000-0000023296.tif')
m <- cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
           to = c(1.8, 2.6, 3.4, 4.2, Inf), 
           becomes = c(1, 2, 3, 4, 5))
puc2class <- reclassify(puc2, m)
rm(puc2)

puc3 = raster('pucClass-0000000000-0000023296.tif')
m <- cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
           to = c(1.8, 2.6, 3.4, 4.2, Inf), 
           becomes = c(1, 2, 3, 4, 5))
puc3class <- reclassify(puc3, m)
rm(puc3)

puc4 = raster('pucClass-0000000000-0000023296.tif')
m = cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
          to = c(1.8, 2.6, 3.4, 4.2, Inf), 
          becomes = c(1, 2, 3, 4, 5))
puc4class = reclassify(puc4, m)
rm(puc4)

pucClass = merge(puc1class, puc2class)
pucClass = merge(pucClass, puc3class)
pucClass = merge(pucClass, puc4class)
writeRaster(pucClass, 'pucClass.tif')
rm(puc1class, puc2class, puc3class, puc4class)

pucClass = raster('pucClass.tif')

mapbiomas20 = raster('mapbiomas-brazil-collection-60-minasgerais-2020.tif')
mapbiomas85 = raster('mapbiomas-brazil-collection-60-minasgerais-1985.tif')

extent(mapbiomas20)
extent(mapbiomas85)
extent(pucClass)
e1 = c(extent(mapbiomas20)[1], extent(mapbiomas85)[1], extent(pucClass)[1])
e2 = c(extent(mapbiomas20)[2], extent(mapbiomas85)[2], extent(pucClass)[2])
e3 = c(extent(mapbiomas20)[3], extent(mapbiomas85)[3], extent(pucClass)[3])
e4 = c(extent(mapbiomas20)[4], extent(mapbiomas85)[4], extent(pucClass)[4])

mapbiomas20 = crop(mapbiomas20, 
                   extent(max(e1), min(e2), max(e3), min(e4)))
proj4string(mapbiomas20) <- CRS("+init=epsg:4326")

mapbiomas85 = crop(mapbiomas85, 
                   extent(max(e1), min(e2), max(e3), min(e4)))
proj4string(mapbiomas85) <- CRS("+init=epsg:4326")
mapbiomas85 = raster(vals=values(mapbiomas85),
                  ext=extent(mapbiomas20),
                  crs=crs(mapbiomas20),
                  nrows=dim(mapbiomas20)[1],
                  ncols=dim(mapbiomas20)[2])

pucClass = crop(pucClass, 
                extent(max(e1), min(e2), max(e3), min(e4)))
pucClass = raster(vals=values(pucClass),
                  ext=extent(mapbiomas20),
                  crs=crs(mapbiomas20),
                  nrows=dim(mapbiomas20)[1],
                  ncols=dim(mapbiomas20)[2])
proj4string(pucClass) <- CRS("+init=epsg:4326")

mystack = stack(mapbiomas85, mapbiomas20,pucClass)
names(mystack) = c("mapbiomas85", "mapbiomas20", "pucClass")

meso = shapefile('ide_1102_mg_mesorregioes_ibge_pol.shp')
proj4string(meso) <- CRS("+init=epsg:4326")
unique(meso$nm_meso)
# 'CAMPO DAS VERTENTES''CENTRAL MINEIRA''JEQUITINHONHA''METROPOLITANA DE BELO HORIZONTE''NOROESTE DE MINAS''NORTE DE MINAS''OESTE DE MINAS''SUL/SUDOESTE DE MINAS''TRI\xc2NGULO MINEIRO/ALTO PARANA\xcdBA''VALE DO MUCURI''VALE DO RIO DOCE''ZONA DA MATA'

central = subset(meso, nm_meso == 'CENTRAL MINEIRA')
stack.meso = crop(mystack, central)   
stack.meso = mask(stack.meso, central)

centro_85.20 = crosstab(stack.meso$mapbiomas85, stack.meso$mapbiomas20)

centro_85.20agg = data.frame(floresta = rowSums(centro_85.20[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(centro_85.20[ , c("15", "21")]), 
                             agricultura = rowSums(centro_85.20[ , c("20", "41", "46", "48")]),
                             silvicultura = centro_85.20[ , c("9")], 
                             agua = centro_85.20[ , c("33")], 
                             outros = rowSums(centro_85.20[ , c("24", "25", "30")]))

centro_85.20agg2 = rbind(centro_85.20agg[c("3") , ] + centro_85.20agg[c("4") , ] + centro_85.20agg[c("11") , ] + centro_85.20agg[c("12") , ] + centro_85.20agg[c("29") , ],
                         centro_85.20agg[c("15") , ] + centro_85.20agg[c("21") , ],
                         centro_85.20agg[c("20") , ] + centro_85.20agg[c("41") , ] + centro_85.20agg[c("46") , ] + centro_85.20agg[c("48") , ],
                         centro_85.20agg[c("9") , ],
                         centro_85.20agg[c("33") , ],
                         centro_85.20agg[c("24") , ] + centro_85.20agg[c("25") , ] + centro_85.20agg[c("30") , ])
rownames(centro_85.20agg2) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")

write.csv(centro_85.20agg2, 'freqMap20-85_centro.csv')
rm(centro_85.20, centro_85.20agg)

centro_85puc = crosstab(stack.meso$mapbiomas85, stack.meso$pucClass)
write.csv(centro_85puc, 'freqMap85PUC_centro.csv')

centro_20puc = crosstab(stack.meso$mapbiomas20, stack.meso$pucClass)
write.csv(centro_20puc, 'freqMap20PUC_centro.csv')