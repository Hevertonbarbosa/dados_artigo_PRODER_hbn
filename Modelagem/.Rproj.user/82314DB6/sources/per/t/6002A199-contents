#if(!require("readr")) install.packages("readr")
#if(!require("ggplot2")) install.packages("ggplot2")
#setwd("C:/Users/Heverton/OneDrive/Mestrado UFCA/Disciplinas/00_Projeto_de_pesquisa/Modelagem")
#png(filename = "Results/3.boxplot_area.png", width = 1000, height = 500, res = 100)
dados_ano = read.csv2('Data/dados_ano.csv')
dados_ano$ano <- as.factor(dados_ano$ano)
boxplotgrouparea = ggplot(data = dados_ano,aes(x = ano,y = area)) +
                  geom_errorbar(stat = "boxplot", width = 0.2) +
                  geom_boxplot(width = 0.6, fill = "grey90",
                               outlier.shape = 1, outlier.size = 2) +
                  labs(y = "Área alagada em m²", x = "Ano") +
                  theme_classic()
boxplotgrouparea
#dev.off()
