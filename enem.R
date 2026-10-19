#install.packages(c("tidyverse"))
library(tidyverse)
library(smplot2)
dados <- read_delim("microdados_enem_2025/DADOS/RESULTADOS_2025.csv",
                    delim = ';',
                    col_select = c(NU_NOTA_MT, NU_NOTA_REDACAO), 
                    locale = locale(decimal_mark = '.', grouping_mark = ','))

dados <- dados |> drop_na()        


head(dados)

options(scipen = 999)


###### MATEMÁTICA #################################

dados |> ggplot(aes(x=NU_NOTA_MT)) +
        geom_histogram(fill = "#69b3a2", color="#e9ecef", alpha = 0.8) +
        ggtitle("Histograma das notas de Matemática ENEM 2025")+
        theme_classic() +
        labs(x = "Notas", y = "Frequência")

dados |> drop_na() |> 
  ggplot(aes(x = NU_NOTA_MT)) +
  geom_boxplot(colour = "black", fill="pink") +
  ggtitle("Boxplot das notas de Matemática 2025") +
  theme_minimal()+
  labs( x = "Notas")



######################################## REDAÇÃO ###################################
dados |> ggplot(aes(x=NU_NOTA_REDACAO)) +
        geom_histogram(fill = "#69b3a2", color="#e9ecef", alpha = 0.8) +
        ggtitle("Histograma das notas de Redação ENEM 2025")+
        theme_classic() +
        labs(x = "Notas", y = "Frequência")

dados |> drop_na() |> 
  ggplot(aes(x = NU_NOTA_REDACAO)) +
  geom_boxplot(colour = "black", fill="pink") +
  ggtitle("Boxplot das notas de Redação 2025") +
  theme_minimal()+
  labs( x = "Notas")

############################################

dados |> drop_na() |> ggplot(aes(x = NU_NOTA_MT, y = NU_NOTA_REDACAO)) +
  geom_point( colour = "pink") +
  ggtitle("Nota de Redação em função da nota de Matemática") +
  theme_classic() +
  labs(x = "Notas de Matemática", y = "Nota de Redação") +
  sm_statCorr(
    color = "black", corr_method = "pearson",
    linetype = "dashed"
  )

################# TABELAS ###############################################

tabelaM <- as.data.frame(table(cut(dados$NU_NOTA_MT, seq(-0.9, 1000, l = nclass.Sturges(dados$NU_NOTA_MT)+6)))) |> 
  rename(Notas = Var1, "Frequencia_Absoluta" = Freq) |> 
  mutate("Frequência Relativa" = Frequencia_Absoluta/sum(Frequencia_Absoluta),
         "Frequência Acumulada" = cumsum(Frequencia_Absoluta),
         "Porcentagem" = 100*Frequencia_Absoluta/sum(Frequencia_Absoluta),
         "Frequência Relativa Acumulada" = 100 * cumsum(Frequencia_Absoluta/sum(Frequencia_Absoluta))) |> 
  filter(Frequencia_Absoluta != 0)
write.csv(tabelaM, file = "tabelaM.csv")

tabelaR <- as.data.frame(table(cut(dados$NU_NOTA_REDACAO, seq(-0.9, 1000, l = nclass.Sturges(dados$NU_NOTA_MT))))) |> 
  rename(Notas = Var1, "Frequencia_Absoluta" = Freq) |> 
  mutate("Frequência Relativa" = Frequencia_Absoluta/sum(Frequencia_Absoluta),
         "Frequência Acumulada" = cumsum(Frequencia_Absoluta),
         "Porcentagem" = 100*Frequencia_Absoluta/sum(Frequencia_Absoluta),
         "Frequência Relativa Acumulada" = 100 * cumsum(Frequencia_Absoluta/sum(Frequencia_Absoluta)))

write.csv(tabelaR, file = "tabelaR.csv")


############### Cálculo de resumo ######

t(as.data.frame(dados |> drop_na() |> summarise(Média = mean(NU_NOTA_MT),
                                Mediana = median(NU_NOTA_MT),
                                Desvio = sd(NU_NOTA_MT),
                                min = min(NU_NOTA_MT),
                                q1 = quantile(NU_NOTA_MT,0.25),
                                q2 = quantile(NU_NOTA_MT,0.5),
                                q3 = quantile(NU_NOTA_MT,0.75),
                                max = max(NU_NOTA_MT),
                                dq = q3 - q1
                                ))
)
t(as.data.frame(dados |> drop_na() |> summarise(Média = mean(NU_NOTA_REDACAO),
                                Mediana = median(NU_NOTA_REDACAO),
                                Desvio = sd(NU_NOTA_REDACAO),
                                min = min(NU_NOTA_REDACAO),
                                q1 = quantile(NU_NOTA_REDACAO,0.25),
                                q2 = quantile(NU_NOTA_REDACAO,0.5),
                                q3 = quantile(NU_NOTA_REDACAO,0.75),
                                max = max(NU_NOTA_REDACAO),
                                dq = q3 - q1
)))


#### analise bivariada

dados_filtrados <- dados |> drop_na() |> 
  filter(NU_NOTA_MT >= 750)

dados_filtrados |> ggplot(aes(x=NU_NOTA_MT, y=NU_NOTA_REDACAO)) +
  geom_point(colour = "pink") +
  theme_dark() +
  ggtitle("Notas de Redação em função de Matemática (>= 750)") +
  xlab("Notas de Matemática") +
  ylab("Notas de Redação")


dados_filtrados |> filter(NU_NOTA_REDACAO > 0) |> 
  ggplot(aes(x=NU_NOTA_MT, y=NU_NOTA_REDACAO)) +
  geom_point(colour = "pink") +
  theme_dark() +
  ggtitle("Notas de Redação em função de Matemática (>= 750)") +
  xlab("Notas de Matemática") +
  ylab("Notas de Redação")

matematica.cut <- with(dados_filtrados, cut(NU_NOTA_MATEMATICA))