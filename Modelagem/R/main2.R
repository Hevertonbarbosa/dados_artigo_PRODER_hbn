# Title: Análise de série temporal de área alagada,
# obtida através de imagens de satélite.
# Date: 14/12/2022.
# Author: Heverton Barbosa do Nascimento.

### Limpar ambiente ####
rm(list=ls()); graphics.off()
### Instalar e importar pacotes ####
if(!require("EBImage")) install.packages("EBImage")
if(!require("imager")) install.packages("imager")
if(!require("forecast")) install.packages("forecast")
if(!require("rvest")) install.packages("rvest")
if(!require("nortest")) install.packages("nortest")
if(!require("readr")) install.packages("readr")
if(!require("ggplot2")) install.packages("ggplot2")

### 1ª Etapa: Processamento de imagens. ####
# Coleta de dados de área alagada das imagens.

source("R/auxiliar.R") # Função de Limiarização e cálculo da área alagada.
source("R/PerformanceMetrics.R") #Função para métricas.
source("R/boxplotgroup.R") #Função para boxplot em grupo.

# Criação de lista de imagens
url_images = "Data/Image_LANDSAT_KIRWIN_CUT/"
files_Path <- list.files(path = url_images)
#files_Path
nfiles <- length(files_Path)
data_result = data.frame(matrix(ncol = 2, nrow = nfiles))
colnames(data_result) = c("data", "area")

# Loop para acessar cada imagem, separarando data e área alagada.
for (i in 1:nfiles) { #i=1
  name_i <- files_Path[i]
  namei_split <- strsplit(name_i, split = "_")[[1]]
  date = namei_split[4]
  image <- EBImage::readImage(paste0(url_images, name_i))
  data_result[i,] <- para.image(date,image,resolucao = 900)# image_transformed
}
#View(data_result)
write.csv(data_result, file = "Results/1.areas.csv",row.names = F)
# Análise dados área
summary(data_result$area)
boxplot(data_result$area)
#png(filename = "Results/1.boxplot_area_alagada.png", width = 1000, height = 500, res = 100)
#dev.off()

# BoxPlot Anual
boxplotgrouparea
png(filename = "Results/1.boxplot_area_alagada_anual.png", width = 1000, height = 500, res = 100)
boxplotgrouparea
dev.off()

### 2ª Etapa: Modelagem dos dados coletados. ####
## Transformando o data frame em series temporais. 
dados.ts<-ts(data_result$area, frequency = 12, start = 2013)
dados.ts
plot(dados.ts, ylab="Área alagada em m²")
png(filename = "Results/2.Área alagada.png", width = 1000, height = 500, res = 100)
plot(dados.ts, ylab="Área alagada em m²")
dev.off()

## Transformar dados a partir da aplicação do log
transformada_ts = ts(log(data_result$area), frequency = 12, start = 2013)
plot(transformada_ts, ylab="Log da área alagada", xlab="Tempo")#, main= "Série temporal")
png(filename = "Results/3.Log da área alagada.png", width = 1000, height = 500, res = 100)
plot(transformada_ts, ylab="Log da área alagada")
dev.off()

## Decompor a série temporal
decomparea=decompose(transformada_ts)
plot(decompose(transformada_ts))
#decomparea$seasonal
#decomparea$trend
#decomparea$random
#decomparea$figure
png(filename = "Results/4.Decomposição da ST.png", width = 1000, height = 500, res = 100)
plot(decompose(transformada_ts))
dev.off()

## Separar série temporal em conjuntos de treinamento teste
perc_train = 0.9
indice_train = round(perc_train*length(transformada_ts))
train_ts = transformada_ts[1:indice_train]
test_ts = transformada_ts[(indice_train+1):length(transformada_ts)]
# Plotar gráfico do split train/test
plot.ts(c(train_ts, test_ts), lwd=1, ylab="Log da área alagada"
        , xlab="Índice")+
  abline(v=indice_train, lwd=2, col="red")
png(filename = "Results/5.Dados_train_test.png", width = 1000, height = 500, res = 100)
plot.ts(c(train_ts, test_ts), lwd=1, ylab="Log da área alagada"
        , xlab="Índice")+
  abline(v=indice_train, lwd=2, col="red")
dev.off()

## MODELO ARIMA #### 
model_arima = auto.arima(train_ts, trace = T, seasonal = T #, D=1
                         , stepwise = F, max.D = 2, start.P = 1) 

summary(model_arima)
# Avaliar resíduos ARIMA
par(mfrow=c(2,2))
qqnorm(model_arima$residuals,main = "")
qqline(model_arima$residuals,main = "")
hist(model_arima$residuals, main = "")
acf(model_arima$residuals, main = "")
pacf(model_arima$residuals, main = "")
shapiro.test(model_arima$residuals) #p > 0,05?
lillie.test(model_arima$residuals)
auto.arima(model_arima$residuals)
par(mfrow=c(1,1)) #Retornar para o gráfico 1 para 1

#OBS.: O MODELO PODE SER MELHORADO.
#Trabalhar com o melhor modelo ARIMA de sazonalidade 12.

## MODELO ARIMA - SAZONALIDADE 12.####
# O MELHOR MODELO TESTADO FOI O ARIMA(0,1,2)(1,0,1)12
model_arima <- arima(train_ts, order = c(0,1,2),
                     seasonal = list(order = c(1,0,1), period = 12))

summary(model_arima)
# Avaliar resíduos ARIMA
par(mfrow=c(2,2))
qqnorm(model_arima$residuals,main = "")
qqline(model_arima$residuals,main = "")
hist(model_arima$residuals, main = "")
acf(model_arima$residuals, main = "")
pacf(model_arima$residuals, main = "")
shapiro.test(model_arima$residuals) #p > 0,05?
lillie.test(model_arima$residuals)
auto.arima(model_arima$residuals)
par(mfrow=c(1,1)) #Retornar para o gráfico 1 para 1

## Previsão com horizonte de tempo igual a 12
previsao = forecast(model_arima, h=12)
write.csv(previsao,file = "Results/8.previsao.csv",row.names = F)
# Gráfico 1
plot(previsao)+
  lines(c(rep(NA, indice_train), test_ts), col="red", lwd=1)
png(filename = "Results/9.Previsão_SARIMA.png", width = 1000, height = 500, res = 100)
plot(previsao)+
  lines(c(rep(NA, indice_train), test_ts), col="red", lwd=1)
dev.off()
# Gráfico 2
previsao_real_12 = exp(previsao$mean)
write.csv(previsao_real_12,file = "Results/10.previsao_12_arima.csv",row.names = F)
teste_real =  exp(test_ts)
write.csv(teste_real,file = "Results/11.teste_real.csv",row.names = F)
plot(teste_real, type = "l", lwd=2, xlab="Conjunto de teste"
     , ylab="Área alagada", ylim=c(min(teste_real, previsao_real_12)
                                   , 1.1*max(teste_real, previsao_real_12)))
lines(as.numeric(previsao_real_12), col="red", lwd=2)
legend("bottomright", c("Observações", "ARIMA"), col=c(1,2)
       , lwd=c(2,2), box.col = "white", inset = 0.01)

## Previsão com horizonte de tempo igual a 1 (ONE STEP AHEAD)
fit = Arima(teste_real, model = model_arima)
previsao_1sta = fitted(fit)
write.csv(previsao_1sta,file = "Results/13.previsao_STA_arima.csv",row.names = F)
# Gráfico 3 
plot(teste_real, type="l",lwd=2, xlab="Conjunto de teste", ylab="Área alagada"
     , ylim=c(min(teste_real, previsao_1sta), 1.1*max(teste_real, previsao_1sta)))
lines(previsao_1sta, col="red", lwd=2)
legend("bottomright", c("Observações", "ARIMA"), col=c(1,2)
       , lwd=c(2,2), box.col = "white", inset = 0.01)

## MODELO ETS ####
model_ets = ets(train_ts)
summary(model_ets)
# Avaliar resíduos ETS
par(mfrow=c(2,2))#Quadro com 4 gráficos
qqnorm(model_ets$residuals)
qqline(model_ets$residuals)
hist(model_ets$residuals)
acf(model_ets$residuals)
pacf(model_ets$residuals)
shapiro.test(model_ets$residuals)
auto.arima(model_ets$residuals)
par(mfrow=c(1,1))#Retornar para o gráfico 1 para 1

## Previsão com horizonte de tempo igual a 1 (ONE STEP AHEAD)
fit_ets = ets(teste_real, model = model_ets)
previsao_1sta_ets = fitted(fit_ets)
write.csv(previsao_1sta_ets,file = "Results/16.previsao_STA_ets.csv",row.names = F)

# Gráfico 3 - Observações x ETS
plot(teste_real, type="l",lwd=2, xlab="Conjunto de teste", ylab="Área alagada"
     , ylim=c(min(teste_real, previsao_1sta), 1.1*max(teste_real, previsao_1sta)))
lines(previsao_1sta_ets, col="blue", lwd=2)
legend("bottomright", c("Observações", "ETS"), col=c(1,"blue")
       , lwd=c(2,2), box.col = "white", inset = 0.01, cex = 0.9)
# Gráfico 4 - Observações x ARIMA X ETS
lines(previsao_1sta, col="red", lwd=2)
lines(previsao_1sta_ets, col="blue", lwd=2)
legend("bottomright", c("Observações", "ARIMA", "ETS"), col=c(1,"red","blue")
       , lwd=c(2,2), box.col = "white", inset = 0.01, cex = 0.9)

## COMPARAR RESULTADO DO MODELO COM DADOS DE ONI. ####
# GERANDO SÉRIE TEMPORAL ONI.
oni = web_scraping()
oni_ts = ts(oni$oni_ts, start = c(1950,3), frequency = 12)
oni_ts = window(oni_ts, start = c(2013,1), end = c(2022,11))
plot.ts(oni_ts)

# PLATAR SÉRIES EM UMA MESMA PÁGINA
par(mfrow=c(2,1))
plot.ts(dados.ts)
plot.ts(oni_ts)
par(mfrow=c(1,1))

# AVALIAR CORRELAÇÃO
cor.test(dados.ts, oni_ts)
plot(as.numeric(dados.ts) ~ as.numeric(oni_ts))+
  abline(lm(dados.ts ~ oni_ts), col = "red", lwd = 3)+
  text(paste("Correlation:", round(cor(dados.ts, oni_ts)
                                   , 2)), x = 2, y = 2.4e7)

# Existe correlação negativa.
# p-valor = 0,0014.

# Avaliar fenômenos El niño e la niña.
# Quando índice diminui (la niña), aumenta a área.

# SOBREPOR OS GRÁFICOS
plot(dados.ts)
par(new = TRUE)
plot(oni_ts, type = "l", axes = FALSE
     , bty = "n", xlab = "", ylab = "", col=2)
axis(side=4, at = pretty(range(oni_ts)))
legend("topleft", c("Observações", "ONI"), col=c(1,"red")
       , lwd=c(2,2), box.col = "white", inset = 0.01, cex = 0.75)

## HOLTWINTERS ####
# Viu-se a possibilidade de melhoria do modelo ETS. 
# Para isto, utilizamos a função de Holtwinters.
train_ts_2 = ts(train_ts, start = c(2013,2),frequency = 12)
model_hw = HoltWinters(train_ts_2)
model_hw
pred_hw = predict(model_hw, n.ahead = length(test_ts))
plot.ts(test_ts)
lines(as.numeric(pred_hw), col = 2)

# Análise de Risíduos
error = model_hw$x-model_hw$fitted[,1]
plot(error)
par(mfrow=c(2,2))#Quadro com 4 gráficos
qqnorm(error)
qqline(error)
hist(error)
acf(error)
pacf(error)
shapiro.test(error)
auto.arima(error)
par(mfrow=c(1,1))#Retornar para o gráfico 1 para 1
#PREVISÃO 
previsao_hw_12 = exp(pred_hw)
write.csv(previsao_hw_12,file = "Results/26.previsao_hw_12.csv",row.names = F)

## BOX PLOT DE PREVISÃO DOS 4 MODELOS ####

boxplot(previsao_real_12,previsao_1sta,previsao_1sta_ets,previsao_hw_12)#,= "previsao_real_12","previsao_1sta","previsao_1sta_ets","previsao_hw_12")

## Métricas para avaliar modelos (< melhor) ####

rmse_arima = getRMSE(teste_real, previsao_1sta);rmse_arima
rmse_ets = getRMSE(teste_real, previsao_1sta_ets);rmse_ets
rmse_hw = getRMSE(teste_real, previsao_hw_12);rmse_hw

mape_arima = getMAPE(teste_real, previsao_1sta);mape_arima
mape_ets = getMAPE(teste_real, previsao_1sta_ets);mape_ets
mape_hw = getMAPE(teste_real, previsao_hw_12);mape_hw

theil_arima = getTheil(teste_real, previsao_1sta);theil_arima
theil_ets = getTheil(teste_real, previsao_1sta_ets);theil_ets
theil_hw = getTheil(teste_real, previsao_hw_12);theil_hw


#error_1 = teste_real-previsao_real_12
#error_2 = teste_real-previsao_hw_12
#dm.test(error_1,error_2,alternative = "greater")

## ANOTAÇÕES ####
## Trabalhar com o melhor modelo arima de sazonalidade 12.

# verificar se permite variáveis exógenas indicar ideia de 
# Sazonalidade. Tentar ARIMA SAZONAL.

## modelos sobre os resíduos.

## Sugerir regressão linear.

## Segerir Redes neuráis simples.

