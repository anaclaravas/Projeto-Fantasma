#Análise 5
#Variação do engajamento pelo personagem que conseguiu capturar o monstro

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

colnames(df_fred)[1]<-"cap"
colnames(df_fred)[2]<-"eng"

df_fred<-df_fred[df_fred$cap == "TRUE",]

df_fred<-na.omit(df_fred)

df_fred$cap<-"Fred"


#Daphne

df_daphne<-as.data.frame(banco_final$caught_daphnie)

df_daphne<-df_daphne%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_daphne)[1]<-"cap"
colnames(df_daphne)[2]<-"eng"

df_daphne<-df_daphne[df_daphne$cap == "TRUE",]

df_daphne<-na.omit(df_daphne)

df_daphne$cap<-"Daphne"

#Velma

df_velma<-as.data.frame(banco_final$caught_velma)

df_velma<-df_velma%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_velma)[1]<-"cap"
colnames(df_velma)[2]<-"eng"

df_velma<-df_velma[df_velma$cap == "TRUE",]

df_velma<-na.omit(df_velma)

df_velma$cap<-"Velma"



#Shaggy


df_shaggy<-as.data.frame(banco_final$caught_shaggy)

df_shaggy<-df_shaggy%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_shaggy)[1]<-"cap"
colnames(df_shaggy)[2]<-"eng"

df_shaggy<-df_shaggy[df_shaggy$cap == "TRUE",]

df_shaggy<-na.omit(df_shaggy)

df_shaggy$cap<-"Salsicha"


#Scooby

df_scooby<-as.data.frame(banco_final$caught_scooby)

df_scooby<-df_scooby%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_scooby)[1]<-"cap"
colnames(df_scooby)[2]<-"eng"

df_scooby<-df_scooby[df_scooby$cap == "TRUE",]

df_scooby<-na.omit(df_scooby)

df_scooby$cap<-"Scooby"


#Não foi pego

df_not<-as.data.frame(banco_final$caught_not)

df_not<-df_not%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_not)[1]<-"cap"
colnames(df_not)[2]<-"eng"

df_not<-df_not[df_not$cap == "TRUE",]

df_not<-na.omit(df_not)

df_not$cap<-"Não foi pego"

#Pego por outro personagem

df_other<-as.data.frame(banco_final$caught_other)

df_other<-df_other%>%
  dplyr::mutate(banco_final$engagement)

colnames(df_other)[1]<-"cap"
colnames(df_other)[2]<-"eng"

df_other<-df_other[df_other$cap == "TRUE",]

df_other<-na.omit(df_other)

df_other$cap<-"Outro"

#Juntando os dados

data<-bind_rows(df_fred, df_daphne, df_velma, 
                df_shaggy, df_scooby, df_not, df_other)

#Gráfico----

ggplot(data) +
  aes(x = reorder(cap, eng, FUN = median), y = eng) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Personagem que capturou o monstro", y = "Engajamento") +
  theme_estat()
ggsave("plot_analise5.pdf", width = 158, height = 93, units = "mm")


#Quadro de Medidas Resumo

quadro_resumo<-data%>%
  group_by(cap)%>% # caso mais de uma categoria
  summarize(Média = round(mean(eng),2),
              `Desvio Padrão` = round(sd(eng),2),
              `Variância` = round(var(eng),2),
              `Mínimo` = round(min(eng),2),
              `1º Quartil` = round(quantile(eng, probs = .25),2),
              `Mediana` = round(quantile(eng, probs = .5),2),
              `3º Quartil` = round(quantile(eng, probs = .75),2),
              `Máximo` = round(max(eng),2)) %>% t() %>% as.data.frame()
