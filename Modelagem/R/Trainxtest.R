
perc_train = 0.9
indice_train = round(perc_train*length(transformada_ts))
train_ts = transformada_ts[1:indice_train]
train_ts = ts(train_ts,start = c(2013,01), frequency = 12)
test_ts = transformada_ts[(indice_train+1):length(transformada_ts)]
test_ts = ts(test_ts,start = c(2022,12), frequency = 12)
transformada_ts = ts(transformada_ts,start = c(2013,01), frequency = 12)

# Plotar gráfico do split train/test
png(filename = "Results/30.Trainxtes.png", width = 1000, height = 500, res = 100)
plot.ts(transformada_ts, lwd=1, ylab="Log da área alagada"
        , xlab="Índice")
abline(v=c(2022,11), lwd=2, col="red")
dev.off()
