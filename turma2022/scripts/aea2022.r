require(raster)
require(terra)
require(dplyr)
install.packages('corrplot')
require(corrplot)

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

puc3 = raster('pucClass-0000023296-0000000000.tif')
m <- cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
           to = c(1.8, 2.6, 3.4, 4.2, Inf), 
           becomes = c(1, 2, 3, 4, 5))
puc3class <- reclassify(puc3, m)
rm(puc3)

puc4 = raster('pucClass-0000023296-0000023296.tif')
m = cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
          to = c(1.8, 2.6, 3.4, 4.2, Inf), 
          becomes = c(1, 2, 3, 4, 5))
puc4class = reclassify(puc4, m)
rm(puc4)

pucClass = merge(puc1class, puc2class)
pucClass = merge(pucClass, puc3class)
pucClass = merge(pucClass, puc4class)
rm(puc1class, puc2class, puc3class, puc4class)

pucClass = raster('pucClass.tif')
plot(pucClass)

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
writeRaster(mapbiomas20, 'mapbiomas20.tif')

mapbiomas85 = crop(mapbiomas85, 
                   extent(max(e1), min(e2), max(e3), min(e4)))
proj4string(mapbiomas85) <- CRS("+init=epsg:4326")
mapbiomas85 = raster(vals=values(mapbiomas85),
                  ext=extent(mapbiomas20),
                  crs=crs(mapbiomas20),
                  nrows=dim(mapbiomas20)[1],
                  ncols=dim(mapbiomas20)[2])
writeRaster(mapbiomas85, 'mapbiomas85.tif')

pucClass = crop(pucClass, 
                extent(max(e1), min(e2), max(e3), min(e4)))
pucClass = raster(vals=values(pucClass),
                  ext=extent(mapbiomas20),
                  crs=crs(mapbiomas20),
                  nrows=dim(mapbiomas20)[1],
                  ncols=dim(mapbiomas20)[2])
proj4string(pucClass) <- CRS("+init=epsg:4326")
writeRaster(pucClass, 'pucClass.tif')

expSilvi = overlay(mapbiomas20, mapbiomas85, fun=function(x,y){return(ifelse(x == 9 & y != 9, 1, 0))})
proj4string(expSilvi) <- CRS("+init=epsg:4326")
writeRaster(expSilvi, 'expSilvi.tif')

mapbiomas85 = raster('mapbiomas85.tif')
mapbiomas20 = raster('mapbiomas20.tif')
pucClass = raster('pucClass.tif')
expSilvi = raster('expSilvi.tif')

mystack = stack(mapbiomas85, mapbiomas20, pucClass, expSilvi)
names(mystack) = c("mapbiomas85", "mapbiomas20", "pucClass", "expSilvi")

meso = shapefile('ide_1102_mg_mesorregioes_ibge_pol.shp')
proj4string(meso) <- CRS("+init=epsg:4326")
unique(meso$nm_meso)
# 'CAMPO DAS VERTENTES''CENTRAL MINEIRA''JEQUITINHONHA''METROPOLITANA DE BELO HORIZONTE''NOROESTE DE MINAS''NORTE DE MINAS''OESTE DE MINAS''SUL/SUDOESTE DE MINAS''TRI\xc2NGULO MINEIRO/ALTO PARANA\xcdBA''VALE DO MUCURI''VALE DO RIO DOCE''ZONA DA MATA'

central = subset(meso, nm_meso == 'CENTRAL MINEIRA')
stack.meso = crop(mystack, central)   
stack.meso = mask(stack.meso, central)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
centro_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
centro_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("20", "46")]), 
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

vertentes = subset(meso, nm_meso == 'CAMPO DAS VERTENTES')
stack.meso = crop(mystack, vertentes)   
stack.meso = mask(stack.meso, vertentes)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
vertentes_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
vertentes_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "12", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("20", "41", "46")]),
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

jequitinhonha = subset(meso, nm_meso == 'JEQUITINHONHA')
stack.meso = crop(mystack, jequitinhonha)   
stack.meso = mask(stack.meso, jequitinhonha)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
jequitinhonha_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
jequitinhonha_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12", "13", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = temp[ , c("46")], 
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

noroeste = subset(meso, nm_meso == 'NOROESTE DE MINAS')
stack.meso = crop(mystack, noroeste)   
stack.meso = mask(stack.meso, noroeste)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
noroeste_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
noroeste_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("39", "41", "48", "46")]),
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

norte = subset(meso, nm_meso == 'NORTE DE MINAS')
stack.meso = crop(mystack, norte)   
stack.meso = mask(stack.meso, norte)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
norte_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
norte_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("20", "39", "41", "46", "48")]),
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

oeste = subset(meso, nm_meso == 'OESTE DE MINAS')
stack.meso = crop(mystack, oeste)   
stack.meso = mask(stack.meso, oeste)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
oeste_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
oeste_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("41", "46")]),
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

sul = subset(meso, nm_meso == 'SUL/SUDOESTE DE MINAS')
stack.meso = crop(mystack, sul)   
stack.meso = mask(stack.meso, sul)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
sul_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
sul_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("20", "41", "46")]), 
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

triangulo = subset(meso, nm_meso == 'TRI\xc2NGULO MINEIRO/ALTO PARANA\xcdBA')
stack.meso = crop(mystack, triangulo)   
stack.meso = mask(stack.meso, triangulo)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
triangulo_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
triangulo_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("20", "41", "46")]), 
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))


mucuri = subset(meso, nm_meso == 'VALE DO MUCURI')
stack.meso = crop(mystack, mucuri)   
stack.meso = mask(stack.meso, mucuri)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
mucuri_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
mucuri_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12", "13", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = temp[ , c("46")],
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25")]))

doce = subset(meso, nm_meso == 'VALE DO RIO DOCE')
stack.meso = crop(mystack, doce)   
stack.meso = mask(stack.meso, doce)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
doce_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
doce_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("41", "46")]),
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

mata = subset(meso, nm_meso == 'ZONA DA MATA')
stack.meso = crop(mystack, mata)   
stack.meso = mask(stack.meso, mata)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
mata_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])

temp = crosstab(stack.meso$expSilvi, stack.meso$mapbiomas85)
mata_expMap85 = data.frame(floresta = rowSums(temp[ , c("3", "4", "12", "29")]), 
                             pastagem = rowSums(temp[ , c("15", "21")]), 
                             agricultura = rowSums(temp[ , c("41", "46")]),
                             agua = temp[ , c("33")], 
                             outros = rowSums(temp[ , c("24", "25", "30")]))

centro_expPuc = centro_expPuc[2,]
rownames(centro_expPuc) = "centro"

vertentes_expPuc = vertentes_expPuc[2,]
rownames(vertentes_expPuc) = "vertentes"

jequitinhonha_expPuc = jequitinhonha_expPuc[2,]
rownames(jequitinhonha_expPuc) = "jequitinhonha"

noroeste_expPuc = noroeste_expPuc[2,]
rownames(noroeste_expPuc) = "noroeste"

norte_expPuc = norte_expPuc[2,]
rownames(norte_expPuc) = "norte"

oeste_expPuc = oeste_expPuc[2,]
rownames(oeste_expPuc) = "oeste"

sul_expPuc = sul_expPuc[2,]
rownames(sul_expPuc) = "sul"

triangulo_expPuc = triangulo_expPuc[2,]
rownames(triangulo_expPuc) = "triangulo"

mucuri_expPuc = mucuri_expPuc[2,]
rownames(mucuri_expPuc) = "mucuri"

doce_expPuc = doce_expPuc[2,]
rownames(doce_expPuc) = "doce"

mata_expPuc = mata_expPuc[2,]
rownames(mata_expPuc) = "mata"

pucExpansao = rbind(mata_expPuc, doce_expPuc, mucuri_expPuc, triangulo_expPuc, sul_expPuc, oeste_expPuc,
     norte_expPuc, noroeste_expPuc, jequitinhonha_expPuc, vertentes_expPuc, centro_expPuc)

write.csv(pucExpansao, 'pucExpansao.csv')

centro_expMap85 = centro_expMap85[2,]
rownames(centro_expMap85) = "centro"

vertentes_expMap85 = vertentes_expMap85[2,]
rownames(vertentes_expMap85) = "vertentes"

jequitinhonha_expMap85 = jequitinhonha_expMap85[2,]
rownames(jequitinhonha_expMap85) = "jequitinhonha"

noroeste_expMap85 = noroeste_expMap85[2,]
rownames(noroeste_expMap85) = "noroeste"

norte_expMap85 = norte_expMap85[2,]
rownames(norte_expMap85) = "norte"

oeste_expMap85 = oeste_expMap85[2,]
rownames(oeste_expMap85) = "oeste"

sul_expMap85 = sul_expMap85[2,]
rownames(sul_expMap85) = "sul"

triangulo_expMap85 = triangulo_expMap85[2,]
rownames(triangulo_expMap85) = "triangulo"

mucuri_expMap85 = mucuri_expMap85[2,]
rownames(mucuri_expMap85) = "mucuri"

doce_expMap85 = doce_expMap85[2,]
rownames(doce_expMap85) = "doce"

mata_expMap85 = mata_expMap85[2,]
rownames(mata_expMap85) = "mata"

usoExpansao = rbind(mata_expMap85, doce_expMap85, mucuri_expMap85, triangulo_expMap85, sul_expMap85, oeste_expMap85,
     norte_expMap85, noroeste_expMap85, jequitinhonha_expMap85, vertentes_expMap85, centro_expMap85)

write.csv(usoExpansao, 'usoExpansao.csv')

# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

chisq_pucExpansao = chisq.test(pucExpansao)
corrplot(chisq_pucExpansao$residuals, is.cor = FALSE)

chisq_usoExpansao = chisq.test(usoExpansao)
corrplot(chisq_usoExpansao$residuals, is.cor = FALSE)


transicao = crosstab(mystack$mapbiomas85, mystack$mapbiomas20)
uso85_20 = data.frame(floresta = rowSums(transicao[ , c("3", "4", "11", "12", "13", "29")]), 
                             pastagem = rowSums(transicao[ , c("15", "21")]), 
                             agricultura = rowSums(transicao[ , c("20", "41", "46", "39", "47", "48")]),
                             silvicultura = transicao[ , c("9")], 
                             agua = transicao[ , c("33")], 
                             outros = rowSums(transicao[ , c("23", "24", "25", "30")]))

uso85_20 = rbind(uso85_20[c("3") , ] + uso85_20[c("4") , ] + uso85_20[c("11") , ] + uso85_20[c("12") , ] + uso85_20[c("13") , ] + uso85_20[c("29") , ],
                         uso85_20[c("15") , ] + uso85_20[c("21") , ],
                         uso85_20[c("20") , ] + uso85_20[c("46") , ] + uso85_20[c("41") , ] + uso85_20[c("39") , ] + uso85_20[c("47") , ] + uso85_20[c("48") , ],
                         uso85_20[c("9") , ],
                         uso85_20[c("33") , ],
                         uso85_20[c("24") , ] + uso85_20[c("25") , ] + uso85_20[c("30") , ] + uso85_20[c("23") , ])
rownames(uso85_20) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
write.csv(uso85_20, 'uso85_20.csv')

chisq_transicao = chisq.test(uso85_20)
corrplot(chisq_transicao$residuals, is.cor = FALSE)
