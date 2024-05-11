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

df<-as.data.frame(banco_final$imdb)
df<-df%>%
  dplyr::mutate(banco_final$season)

#Data frame com as variações das notas dos episódios divididas em temporadas

colnames(df)[2]<-"temp"
  
data<-df[!(df$temp %in% c("Crossover", "Movie", "Special")),]

colnames(data)[1]<-"nota"

#Gráfico----

boxplot<-ggplot(data) +
  aes(x = reorder(temp, nota, FUN = median), y = nota) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Temporada", y = "Variação da nota IMDb") + theme_estat()
ggsave("plot_analise2.pdf", width = 158, height = 93, units = "mm")








