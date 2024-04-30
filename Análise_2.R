#Projeto Fantasma
#Análise 2
#Objetivo:Variação da nota IMDB por temporada dos episódios
 
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


#Análise 2 ----

# Objetivo: Variação da nota IMDB por temporada dos episódios

#Separando as notas no IMDb e as temporadas em um data frame

df <- as.data.frame(banco_final$imdb)
df <- df%>%
  dplyr::mutate(banco_final$season)

#Data frame com as variações das notas dos episódios divididas em temporadas

media <- df%>%
  group_by(banco_final$season) %>%
  summarise(variação = var(`banco_final$imdb`)) %>%
  arrange(`banco_final$season`, .locale = "en")

df1 <- filter(media, media$`banco_final$season`!="Crossover") 
df2 <- filter(df1, df1$`banco_final$season`!="Movie") 
variações <- filter(df2, df2$`banco_final$season`!="Special") 


#Gráfico----

barchart<-ggplot()
barchart<-barchart + geom_col(data=variações, 
                              aes(x=`banco_final$season`, 
                                  y=`variação`), 
                              position = "dodge")
barchart<-barchart + labs(title=
                            "Variação da nota IMDB por temporada dos episódios",
                          x="Temporada", y="Variação da nota IMDb", 
                          caption="Dados fornecidos pela Warner Bros Entertainment")
barchart




