#Análise 4


#Pacotes

install.packages("tidyverse")
library(tidyverse)

#Padronização 

cores_estat<-c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600",
               "#CC9966", "#999966", "#006606", "#008091", "#041835",
               "#666666")



theme_estat<-function(...){theme<-ggplot2::theme_bw() + ggplot2::theme(axis.title.y = ggplot2::element_text(colour="black",
                                                                                                                size = 12), axis.title.x = ggplot2::element_text(colour="black", size = 12),
                                                                           axis.text = ggplot2::element_text(colour="black", size=9.5), 
                                                                           panel.border = ggplot2::element_blank(),
                                                                           axis.line = ggplot2::element_line(colour="black"), legend.position = "top",...)
return(list(theme, scale_fill_manual(values=cores_estat), 
            scale_colour_manual(values=cores_estat)))}

#Análise 4----

df<-as.data.frame(banco_final$imdb)
df<-df%>%
  dplyr::mutate(banco_final$engagement)
colnames(df)[1]<-"imdb"
colnames(df)[2]<-"eng"

#Gráfico

ggplot(df) + aes(x = imdb, y = eng) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Nota IMDb",
    y = "Engajamento") + theme_estat()
ggsave("plot_analise4.pdf", width = 158, height = 93, units = "mm")

#Medidas resumo das variáveis

resumo_imdb<-df%>%
  summarize(Média = round(mean(imdb), 2),
              `Desvio Padrão` = round(sd(imdb),2),
              `Variância` = round(var(imdb),2),
              `Mínimo` = round(min(imdb),2),
              `1º Quartil` = round(quantile(imdb, probs = .25),2),
              Mediana = round(quantile(imdb, probs = .5),2),
              `3º Quartil` = round(quantile(imdb, probs = .75),2),
              `Máximo` = round(max(imdb),2)) %>% t() %>%as.data.frame()
resumo_eng<-df%>%
  summarize(Média = round(mean(eng), 2),
            `Desvio Padrão` = round(sd(eng),2),
            `Variância` = round(var(eng),2),
            `Mínimo` = round(min(eng),2),
            `1º Quartil` = round(quantile(eng, probs = .25),2),
            Mediana = round(quantile(eng, probs = .5),2),
            `3º Quartil` = round(quantile(eng, probs = .75),2),
            `Máximo` = round(max(eng),2)) %>% t() %>%as.data.frame()

#Boxplot das variáveis

boxplot_imdb<-ggplot(df) +
  aes(x= factor(""), y=imdb) + geom_boxplot(fill=c("#A11D21"), width=0.5) +
  guides(fill= FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
  labs(x="", y="Nota IMDb") + theme_estat()

boxplot_eng<-ggplot(df) +
  aes(x=factor(""), y=eng) + geom_boxplot(fill=c("#A11D21"), width=0.5) +
  guides(fill= FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
  labs(x="", y="Engajamento") + theme_estat()




