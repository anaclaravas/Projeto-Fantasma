#Projeto Fantasma: Análise 1
#Ana Clara Leal

# Objetivo - Número de lançamentos a cada década por formato de lançamento ----

#Pacotes
install.packages("readr")
library(readr)
install.packages("tidyr")
library(tidyr)
install.packages("lubridate")
library(lubridate)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)


#Transformando o formato das variáveis

banco_final$Datas <- as.Date.character(dmy(banco_final$date_aired))
banco_final$Formato <- as.factor(banco_final$format)

#Criando um data frame com as variáveis de interesse seperando-as em décadas

df <- as.data.frame(banco_final$Datas)
df <- df%>%
  dplyr::mutate(banco_final$Formato)


df <- df%>%
  dplyr::mutate(Decades = cut.Date(`banco_final$Datas`
                                   , breaks = "10 years"
                                   ,labels = FALSE))

df.summary <- df%>%
  dplyr::group_by(Decades)%>%
  dplyr::reframe(`banco_final$Formato`, groups = levels(`banco_final$Formato`))

#Resultado ----


tabela <- table(df.summary)
tabela_geral <- as.data.frame(tabela)



#Gráfico ----

barchart<-ggplot()
barchart<-barchart + geom_col(data=tabela_geral, 
                              aes(x=tabela_geral$Décadas, 
                                  y=tabela_geral$Frequência, 
                                  fill=tabela_geral$Formato), 
                              position = "dodge")
barchart<-barchart + labs(title=
                 "Número de lançamentos a cada década por formato de lançamento",
                 x="Décadas", y="Número de lançamentos", 
                 fill="Formato de Lançamento", 
                 caption="Dados fornecidos pela Warner Bros Entertainment")
barchart
