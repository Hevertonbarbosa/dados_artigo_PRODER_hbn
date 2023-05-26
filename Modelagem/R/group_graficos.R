#Executar main completo.

## GERAÇÃO DE GRÁFICOS RESUMO.###
#AVALIAÇÃO DE RESÍDUOS
#QQPLOT NORMALY TEST. ####
par(mfrow=c(2,2))#Quadro com 4 gráficos
qqnorm(model_arima$residuals,main = "ARIMA")
qqline(model_arima$residuals,main = "")
qqnorm(model_sarima$residuals,main = "SARIMA")
qqline(model_sarima$residuals,main = "")
qqnorm(model_ets$residuals,main = "ETS")
qqline(model_ets$residuals,main = "")
qqnorm(error,main = "HOLTWINTERS")
qqline(error,main = "")

#HISTOGRAMA. ####
par(mfrow=c(2,2))#Quadro com 4 gráficos
hist(model_arima$residuals, main = "ARIMA")
hist(model_sarima$residuals, main = "SARIMA")
hist(model_ets$residuals, main = "ETS")
hist(error, main = "HOLTWINTERS")

#AUTOCORRELAÇÃO. ####
par(mfrow=c(2,2))#Quadro com 4 gráficos
acf(model_arima$residuals, main = "ARIMA")
acf(model_sarima$residuals, main = "SARIMA")
acf(model_ets$residuals, main = "ETS")
acf(error, main = "HOLTWINTERS")

#AUTOCORRELAÇÃO PARCIAL. ####
#png(filename = "Results/34.CORRELOGRAMAS.png", width = 1000, height = 500, res = 100)
par(mfrow=c(2,2))#Quadro com 4 gráficos
pacf(model_arima$residuals, main = "ARIMA")
pacf(model_sarima$residuals, main = "SARIMA")
pacf(model_ets$residuals, main = "ETS")
pacf(error, main = "HOLTWINTERS")
#dev.off()

#PREVISÃO - 12 PASSOS A FRENTE. #####
par(mfrow=c(2,2))#Quadro com 4 gráficos
#ARIMA
plot(teste_real, type = "l", lwd=2, xlab="Conjunto de teste"
     , ylab="Área alagada", main = "PREVISÃO ARIMA"
     , ylim=c(min(teste_real, previsao_real_12_arima)
              , 1.1*max(teste_real, previsao_real_12_arima)))
lines(as.numeric(previsao_real_12_arima), col="red", lwd=2)
#legend("topright", c("Observações", "ARIMA"), col=c(2,1)
#       , lwd=c(2,2), box.col = "white", inset = 0.001)

#SARIMA
plot(teste_real, type = "l", lwd=2, xlab="Conjunto de teste"
     , ylab="Área alagada" , main = "PREVISÃO SARIMA"
     , ylim=c(min(teste_real, previsao_real_12_sarima)
              , 1.1*max(teste_real, previsao_real_12_sarima)))
lines(as.numeric(previsao_real_12_sarima), col="red", lwd=2)
#legend("bottomright", c("Observações", "SARIMA"), col=c(1,2)
#       , lwd=c(2,2), box.col = "white", inset = 0.01)

#ETS
plot(teste_real, type = "l", lwd=2, xlab="Conjunto de teste"
     , ylab="Área alagada" , main = "PREVISÃO ETS"
     , ylim=c(min(teste_real, previsao_real_12_ets)
              , 1.1*max(teste_real, previsao_real_12_ets)))
lines(as.numeric(previsao_real_12_ets), col="red", lwd=2)
#legend("bottomright", c("Observações", "ETS"), col=c(1,2)
#       , lwd=c(2,2), box.col = "white", inset = 0.01)

#Holt
plot.ts(test_ts, type = "l", lwd=2, xlab="Conjunto de teste"
        , ylab="Área alagada", main = "PREVISÃO HOLTWINTERS")
lines(as.numeric(pred_hw), col="red", lwd=2)
#legend("bottomright", c("Observações", "HOLT"), col=c(1,2)
#       , lwd=c(2,2), box.col = "white", inset = 0.01)

