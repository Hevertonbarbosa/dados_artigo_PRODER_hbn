#png(filename = "Results/33.Prev_Model.png", width = 1000, height = 500, res = 100)
dados_prev_model = read.csv2('Data/dados_prev_model.csv')
dados_prev_model$modelo <- as.factor(dados_prev_model$modelo)
boxplotgroupmodel = ggplot(data = dados_prev_model,aes(x = modelo,y = area)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(width = 0.6, fill = "grey90",
               outlier.shape = 1, outlier.size = 2) +
  labs(y = "Área alagada em m²", x = "Modelo") +
  theme_classic()
boxplotgroupmodel
#dev.off()
