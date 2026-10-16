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


nclass.Sturges(dados$NU_NOTA_MT)
table(cut(dados$NU_NOTA_MT, seq(0, 1000, l = nclass.Sturges(dados$NU_NOTA_MT)))) 

rstudiothemes::install_rstudiothemes()


