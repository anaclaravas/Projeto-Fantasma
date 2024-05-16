#Análise 5
#Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro

#Pacotes
install.packages("tidyverse")
library(tidyverse)

#Padronização ESTAT

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

#Fred

df_fred<-as.data.frame(banco_final$caught_fred)

df_fred<-df_fred%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_fred)[1]<-"fred"
colnames(df_fred)[2]<-"eng"

df_fred<-df_fred[df_fred$fred == "TRUE",]

df_fred<-na.omit(df_fred)

#Daphne

df_daphne<-as.data.frame(banco_final$caught_daphnie)

df_daphne<-df_daphne%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_daphne)[1]<-"daphne"
colnames(df_daphne)[2]<-"eng"

df_daphne<-df_daphne[df_daphne$daphne == "TRUE",]

df_daphne<-na.omit(df_daphne)

#Velma

df_velma<-as.data.frame(banco_final$caught_velma)

df_velma<-df_velma%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_velma)[1]<-"velma"
colnames(df_velma)[2]<-"eng"

df_velma<-df_velma[df_velma$velma == "TRUE",]

df_velma<-na.omit(df_velma)


#Shaggy


df_shaggy<-as.data.frame(banco_final$caught_shaggy)

df_shaggy<-df_shaggy%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_shaggy)[1]<-"shaggy"
colnames(df_shaggy)[2]<-"eng"

df_shaggy<-df_shaggy[df_shaggy$shaggy == "TRUE",]

df_shaggy<-na.omit(df_shaggy)



#Scooby

df_scooby<-as.data.frame(banco_final$caught_scooby)

df_scooby<-df_scooby%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_scooby)[1]<-"scooby"
colnames(df_scooby)[2]<-"eng"

df_scooby<-df_scooby[df_scooby$scooby == "TRUE",]

df_scooby<-na.omit(df_scooby)


#Não foi pego

df_not<-as.data.frame(banco_final$caught_not)

df_not<-df_not%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_not)[1]<-"not"
colnames(df_not)[2]<-"eng"

df_not<-df_not[df_not$not == "TRUE",]

df_not<-na.omit(df_not)

#Pego por outro personagem

df_other<-as.data.frame(banco_final$caught_other)

df_other<-df_other%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_other)[1]<-"other"
colnames(df_other)[2]<-"eng"

df_other<-df_other[df_other$other == "TRUE",]

df_other<-na.omit(df_other)

#Gráfico
