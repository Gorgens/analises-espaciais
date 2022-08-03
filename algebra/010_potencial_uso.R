# Algebra de mapas

## Preparando ambiente

require(raster)
require(rgdal)
require(ggplot2)
require(dplyr)
require(gridExtra)

## Open data

meso = shapefile('./algebra/mg_mesorregioes.shp')
lito = raster('./algebra/litoPesos.tif')

solo = raster('./algebra/soloPesos.tif') %>% crop(lito)
solo = raster(vals=values(solo),
                ext=extent(lito), 
                nrows=dim(lito)[1],
                ncols=dim(lito)[2])


slope = raster('./algebra/slopePesos.tif') %>% crop(lito)
slope = raster(vals=values(slope),
              ext=extent(lito), 
              nrows=dim(lito)[1],
              ncols=dim(lito)[2])

## Algebra de mapas
puc = 0.5 * slope + 0.39 * solo + 0.11 * lito
writeRaster(puc, './classificacao/puc.tif')
