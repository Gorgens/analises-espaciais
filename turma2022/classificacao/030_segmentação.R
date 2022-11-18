# Segmentação de imagem

## Preparando ambiente
require(OpenImageR)
require(raster)
require(rgdal)
require(dplyr)

## Área de interesse

areaImage = brick('./classificacao/cipef_50cm_v2.tif')
plotRGB(areaImage, r = 1, g = 2, b = 3, stretch = 'lin')

nrows = areaImage@nrows
ncols = areaImage@ncols
jpeg('colorImage.jpg', width = ncols, height = nrows)
  plotRGB(areaImage, r = 1, g = 2, b = 3, stretch = 'lin')
dev.off()

## Criar superpixels
colorImage = readImage('./classificacao/colorImage.jpg')
Region.slic = superpixels(input_image = colorImage, 
  method = 'slic', superpixel = 60,
  compactness = 30, return_slic_data = TRUE,
  return_labels = TRUE, write_slic = '',
  verbose = FALSE)

imageShow(Region.slic$slic_data)
