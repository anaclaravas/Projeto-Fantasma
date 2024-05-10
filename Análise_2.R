#Projeto Fantasma
#Análise 2
 
install.packages("tidyverse")
library(tidyverse)

#Padronização 

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600",
                 "#CC9966", "#999966", "#006606", "#008091", "#041835",
                 "#666666")



theme_estat <- function(...){theme <- ggplot2::theme_bw() + ggplot2::theme(axis.title.y = ggplot2::element_text(colour="black",
                                                                                                                size = 12), axis.title.x = ggplot2::element_text(colour="black", size = 12),
                                                                           axis.text = ggplot2::element_text(colour="black", size=9.5), 
                                                                           panel.border = ggplot2::element_blank(),
                                                                           axis.line = ggplot2::element_line(colour="black"), legend.position = "top",...)
return(list(theme, scale_fill_manual(values=cores_estat), 
            scale_colour_manual(values=cores_estat)))}

#Análise 2 ----

# Objetivo: Variação da nota IMDB por temporada dos episódios

#Separando as notas no IMDb e as temporadas em um data frame

df <- as.data.frame(banco_final$imdb)
df <- df%>%
  dplyr::mutate(banco_final$season)

#Data frame com as variações das notas dos episódios divididas em temporadas

media <- df%>%
  group_by(banco_final$season) %>%
  summarise(variação = amp(`banco_final$imdb`)) %>%
  arrange(`banco_final$season`, .locale = "en")

df1 <- filter(media, media$`banco_final$season`!="Crossover") 
df2 <- filter(df1, df1$`banco_final$season`!="Movie") 
amplitude <- filter(df2, df2$`banco_final$season`!="Special") 


#Gráfico----

colnames(variações)[1]<-"Temporada"
colnames(variações)[2]<-"Variação da nota IMDb"


barchart<-ggplot(data=variações) + geom_col(data=variações, 
                                            aes(x=`Temporada`, 
                                                y=`Variação da nota IMDb`, 
                                                fill=FALSE), 
                                            show.legend=FALSE, 
                                            position = "dodge") + theme_estat()
  
ggsave("colunas -uni -freq.pdf", width = 158, height = 93, units = "mm")

barchart





