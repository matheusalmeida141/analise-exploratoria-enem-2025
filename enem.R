library(tidyverse)

dados <- read_delim("microdados_enem_2025/DADOS/RESULTADOS_2025.csv",
                    delim = ';',
                    col_select = c(NU_NOTA_MT, NU_NOTA_REDACAO), 
                    locale = locale(decimal_mark = '.', grouping_mark = ','))
head(dados)

dados$NU_NOTA_REDACAO|> hist(col="darkblue")

dados |> ggplot(mapping = aes(x=NU_NOTA_MT, y=NU_NOTA_REDACAO)) +
         geom_point(na.rm = TRUE)


dados |> filter(is.na(NU_NOTA_MT) & !is.na()) 

dados |> ggplot( aes(x = NU_NOTA_MT)) +
  geom_boxplot(na.rm = TRUE)


#install.packages("fdth")
library(fdth)

tabela <- fdt(dados$NU_NOTA_MT, na.rm=TRUE)
tabela
