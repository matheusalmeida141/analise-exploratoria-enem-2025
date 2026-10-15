library(tidyverse)

dados <- read_delim("microdados_enem_2025/DADOS/RESULTADOS_2025.csv",
                    delim = ';',
                    col_select = c(NU_NOTA_MT, NU_NOTA_REDACAO), 
                    locale = locale(decimal_mark = '.', grouping_mark = ','))
head(dados)

options(scipen = 999)

dados |> ggplot(aes(x=NU_NOTA_MT)) +
        geom_histogram(fill = "#69b3a2", color="#e9ecef", alpha = 0.8) +
        ggtitle("Histograma das notas de Matemática ENEM 2025")+
        theme_classic() +
        labs(x = "Notas", y = "Frequência")

dados |> ggplot(aes(x=NU_NOTA_REDACAO)) +
        geom_histogram(fill = "#69b3a2", color="#e9ecef", alpha = 0.8) +
        ggtitle("Histograma das notas de Redação ENEM 2025")+
        theme_classic() +
        labs(x = "Notas", y = "Frequência")



dados |> filter(is.na(NU_NOTA_MT) & !is.na()) 

dados |> ggplot( aes(x = NU_NOTA_MT)) +
  geom_boxplot(na.rm = TRUE, )

#install.packages("fdth")
library(fdth)

tabela <- fdt(dados$NU_NOTA_MT, na.rm=TRUE)
tabela
