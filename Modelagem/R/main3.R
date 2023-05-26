# Title: Análise de série temporal de área alagada,
# obtida através de imagens de satélite.
# Date: 14/12/2022.
# Author: Heverton Barbosa do Nascimento.

#Apagar memória
rm(list=ls()); graphics.off()
#Ativar pacotes
if(!require("EBImage")) install.packages("EBImage")
if(!require("imager")) install.packages("imager")
if(!require("forecast")) install.packages("forecast")

library(EBImage)
library(imager)
library(forecast)
#Indicar pasta de trabalho
#setwd("C:/Users/Heverton/OneDrive/Mestrado UFCA/Disciplinas/00_Projeto_de_pesquisa/Imagens_de_satelite/Image_LANDSAT_KIRWIN_CUT")
#Função para transformar em escala de cinza,
#realizar a limiarização das imagens e cálculo da área alagada
para.image <- function(date,image) {
  image1 <- as.cimg(image)
  grayimage <- grayscale(image1)
  otsuimage <- grayimage > EBImage::otsu(grayimage)
  #Cálculo da área
  aux <- sum(otsuimage)
  pixel <- 900  # Resolução 30m
  dim.area <- pixel*aux
  saida <- data.frame(data=date,area=dim.area)
  return(saida)
}
#Criação de lista de imagens
files_Path <- list.files(path = "Data/Image_LANDSAT_KIRWIN_CUT/")
files_Path
nfiles <- length(files_Path)
data_result = data.frame(matrix(ncol = 2, nrow = nfiles))
colnames(data_result) = c("data", "area")
#Loop para acessar cada imagem, separarando data e área alagada.
for (i in 1:nfiles) { #i=1
  name_i <- files_Path[i]
  namei_split <- strsplit(name_i, split = "_")[[1]]
  date = namei_split[4]
  image <- EBImage::readImage(paste0("Data/Image_LANDSAT_KIRWIN_CUT/", name_i))
  data_result[i,] <- para.image(date,image)# image_transformed
}
View(data_result)

###2ª Etapa: Modelagem dos dados coletados.

#Transformando o data frame em series temporais
#dados.ts<-ts(data_result$area)
dados.ts<-ts(data_result$area, frequency = 12, start = 2013)
dados.ts
plot(dados.ts, ylab="Área alagada em m²", main="Série temporal")
png(filename = "Results/area_alagada.png", width = 1000, height = 500, res = 100)
plot(dados.ts, ylab="Área alagada em m²", main="Série temporal")
dev.off()
#bp=boxplot(data_result$area)

#Modelagem
#Aplicação do log devido dados maiores que zero.
y = ts(log(data_result$area),frequency=12)
lts = y
plot(lts, ylab="log de área alagada", main= "Série temporal")
##seperar dados de treinamento
se<-0.899*length(lts)
tr<-length(lts)-se
trainning<-subset(lts,end = length(lts)-tr)
lines(trainning,col="red")
#pegar outra parte da série para validação
test<-subset(lts,start = length(lts)-(tr-1))
lines(test,col="green")
trainning
test
# #Suavização
# HoltWinters(x = trainning)
# fit<-HoltWinters(x = trainning)
# #Analisando os resíduos
# #par(mfrow=c(2,2))
# plot(resid(fit))
# qqnorm(resid(fit)) #Normalidade dos dados
# qqline(resid(fit))
# acf(resid(fit)) #Correlação
# pacf(resid(fit)) #Correlação parcial
# par(mfrow=c(1,1))
# plot(lts)
# plot(fit)
# predict(fit,n.ahead = tr)
# forecast<-predict(fit,n.ahead = tr)
# plot(lts,ylim=c(16,17))
# lines(forecast,lwd=2,col="blue")
#
model_arima = auto.arima(y = y, D=1)
#model_arima = auto.arima(y = y)
model_arima
summary(model_arima)
qqnorm(model_arima$residuals)
qqline(model_arima$residuals)
shapiro.test(model_arima$residuals)
acf(model_arima$residuals)
pacf(model_arima$residuals)
hist(model_arima$residuals)
auto.arima(model_arima$residuals)
previsao<-exp(forecast(model_arima, h=12)$mean)
previsao
plot(c(exp(model_arima$fitted), previsao), type = "l", col="red")
lines(data_result$area)
plot(x= as.numeric(exp(model_arima$fitted)),
     y = data_result$area, type = "p")
modelo_linear<-lm(data_result$area~exp(model_arima$fitted))
abline(modelo_linear)
summary(modelo_linear)


bp=boxplot(model_arima$residuals)
bp$out

#abline(h=)
#Fim