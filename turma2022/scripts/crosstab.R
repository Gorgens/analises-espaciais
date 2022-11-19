## Open required packages
require(raster)
require(dplyr)
require(rgdal)

r1 = raster('norte_mapbiomas85.tif')
dim(r1)

r2 = raster('norte_mapbiomas20.tif')
dim(r2)

e1 = c(extent(r1)[1], extent(r2)[1])
e2 = c(extent(r1)[2], extent(r2)[2])
e3 = c(extent(r1)[3], extent(r2)[3])
e4 = c(extent(r1)[4], extent(r2)[4])


r1 = crop(r1, extent(max(e1), min(e2), max(e3), min(e4)))
extent(r1)
r2 = crop(r2, extent(max(e1), min(e2), max(e3), min(e4)))
extent(r2) = extent(r1)
extent(r2)

tabelaFreq = crosstab(r1, r2)
write.csv(tabelaFreq, 'norte_transicao1985-2020.csv')
