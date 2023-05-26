#Teste do método de Otsu em uma imagem
#Apagar memória
#remove(list = ls())
#Ativar pacote
library(EBImage)
library(imager)
#Indicar pasta de trabalho
setwd("C:/Users/Heverton/OneDrive/Mestrado UFCA/Disciplinas/00_Projeto_de_pesquisa/Imagens_de_satelite/Image_LANDSAT_KIRWIN_CUT")
image <- EBImage::readImage("LC08_CU_015009_20190627_20210504_02_DIAG.png")
image1 <- as.cimg(image)
grayimage <- grayscale(image1)
otsuimage <- grayimage > EBImage::otsu(grayimage)
plot(otsuimage)
display(otsuimage)
#Cálculo da área
aux <- sum(otsuimage)
pixel <- 900 #Resolução da imagem de 30m
area <- pixel*aux
area

#1. Montar a série de dados de área alagada
#2. adequar o modelo de série temporal
#3. vídeo mostrando a dinâmica da área alagada

