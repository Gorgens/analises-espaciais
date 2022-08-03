# Classificação Não Supervisionada

## Preparando ambiente
require(raster)
require(rgdal)
require(ggplot2)
require(dplyr)
require(gridExtra)

# Importando os dados

## Imagem de referência
aoi = stack('./classificacao/cipef_50cm_v2.tif')
plotRGB(aoi, r = 1, g = 2, b = 3, stretch = 'lin')

blue = getValues(aoi[[1]])
green = getValues(aoi[[2]])
red = getValues(aoi[[3]])
nr = cbind(blue, green, red)

## Classificação

kmncluster = kmeans(na.omit(nr), centers = 2, iter.max = 500, nstart = 4, algorithm="Lloyd")
str(kmncluster)

mycolor = c("gray","green")

classified = setValues(aoi[[1]], kmncluster$cluster)
plot(classified, main = 'Unsupervised classification - kmeans', col=mycolor)
