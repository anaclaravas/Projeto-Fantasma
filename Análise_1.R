#Projeto Fantasma

#Ana Clara Leal

#Análise 1 ----

# Objetivo - Número de lançamentos a cada década por formato de lançamento 

#Pacotes
install.packages("lubridate")
library(lubridate)
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

#Transformando o formato das variáveis

banco_final$Datas<-as.Date.character(dmy(banco_final$date_aired))
banco_final$Formato<-as.factor(banco_final$format)

#Criando um data frame com as variáveis de interesse seperando-as em décadas

df<-as.data.frame(banco_final$Datas)
df<-df%>%
  dplyr::mutate(banco_final$Formato)


df<-df%>%
  dplyr::mutate(Decades = cut.Date(`banco_final$Datas`
                                   , breaks = "10 years"
                                   ,labels = FALSE))

colnames(df)[2]<-"Formato"

df$`banco_final$Datas`<-NULL


#Resultado ----

tabela<-table(df)
data<-as.data.frame(tabela)


#Gráfico ----

ggplot(data) +
  aes(x=Decades, y=Freq, group=Formato, colour=Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Formato", labels = c("Movie", "Serie", "Crossover")) +
  labs(x = "Década", y = "Número de lançamentos") +
  theme_estat()
ggsave("lineplot_1.pdf", width = 158, height = 93, units = "mm")
