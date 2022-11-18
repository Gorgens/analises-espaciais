# Classificação Supervisionada

## Preparando ambiente

require(raster)
require(rgdal)
require(ggplot2)
require(dplyr)
require(gridExtra)

# Importando os dados

## Imagem de referência
aoi = brick('./classificacao/cipef_50cm_v2.tif')
names(aoi) = c("red", "green", 'blue', 'alpha')
aoi = aoi[[-4]]

plotRGB(aoi, r = 1, g = 2, b = 3, stretch = 'lin')


## Pontos de referência para treinamento
pontosTreinamento = shapefile('./classificacao/cipef_poi.shp')
table(pontosTreinamento@data$class)

sampvals = extract(aoi, pontosTreinamento, df = TRUE)
sampvals = sampvals[, -1]
sampdata = data.frame(classvalue = pontosTreinamento@data$class, sampvals)
head(sampdata)

## Centroides de cada grupo
mediaGrupo = sampdata %>%
  group_by(classvalue) %>%
  summarise(red = mean(red),
            green = mean(green),
            blue = mean(blue))

mediaGrupo

c1 = ggplot(sampdata, aes(x = red, y = green)) + 
  geom_point(aes(colour = factor(classvalue))) + 
  geom_point(data = mediaGrupo, aes(red, green), shape = 4, size = 3) +
  scale_color_manual(values = c("1" = "green", "2" = "gray"))

c2 = ggplot(sampdata, aes(x = green, y = blue)) + 
  geom_point(aes(colour = factor(classvalue))) + 
  geom_point(data = mediaGrupo, aes(green, blue), shape = 4, size = 3) +
  scale_color_manual(values = c("1" = "green", "2" = "gray"))

c3 = ggplot(sampdata, aes(x = red, y = blue)) + 
  geom_point(aes(colour = factor(classvalue))) + 
  geom_point(data = mediaGrupo, aes(red, blue), shape = 4, size = 3) +
  scale_color_manual(values = c("1" = "green", "2" = "gray"))

grid.arrange(c1, c2, c3,ncol = 1)


# Classificação

blue = getValues(aoi[[1]])
green = getValues(aoi[[2]])
red = getValues(aoi[[3]])
nr = cbind(red, green, blue)

euclidean = function(a, b){sqrt(sum((a - b)^2))}

## Distância euclidiana de cada centroide para o primeiro pixel

euclidean(nr[1,], mediaGrupo[1,2:4])
euclidean(nr[1,], mediaGrupo[2,2:4])

## Classificação da área de interesse

distClass1 = data.frame(distClass1 = apply(nr, 1, function(x) sqrt(sum((x - mediaGrupo[1,2:4])^2))))
distClass2 = data.frame(distClass2 = apply(nr, 1, function(x) sqrt(sum((x - mediaGrupo[2,2:4])^2))))
nr = cbind(nr, distClass1, distClass2)
nr$class = ifelse(nr$distClass1 < nr$distClass2, 1, 2)

mycolor = c("green","gray")
classified = setValues(aoi[[1]], nr$class)
plot(classified, main = 'Supervised classification - Euclidian', col=mycolor)

writeRaster(classified, './classificacao/classifedEuclidian.tif')