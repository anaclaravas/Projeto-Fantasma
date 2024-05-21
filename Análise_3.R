#Projeto Fantasma
#Análise 3

#Top 3 terrenos mais frequentes pela ativação da armadilha
#Quais são os 3 tipos de terrenos mais frequentes
#Quais armadilhas funcionam de primeira neles


#Pacotes
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

#Terrenos mais frequentes

top3<-as.data.frame(banco_final$setting_terrain)

colnames(top3)[1]<-"terreno"

tabela3<-table(top3)

top3<-as.data.frame(tabela3)

top3<-top3[-c(1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14), ]


#Separando as variáveis de interesse em data frames

df<-as.data.frame(banco_final$setting_terrain)
df<-df%>%
  dplyr::mutate(banco_final$trap_work_first)

colnames(df)[1]<-"terreno"
colnames(df)[2]<-"armadilha"

final<-df%>%
  filter(df$terreno=="Urban")

df2<-df%>%
  filter(df$terreno=="Rural")

df3<-df%>%
  filter(df$terreno=="Forest")

final<-rbind(final, df2, df3)

tabela<-table(final)

analise3<-as.data.frame(tabela)

analise3$freq_relativa<-round(analise3$Freq/sum(analise3$Freq) * 100, 1)


#Construção do gráfico

grafico<-analise3%>%
  mutate(terreno = case_when(terreno %>% str_detect("Forest") ~ "Floresta",
    terreno %>% str_detect("Rural") ~ "Rural",
    terreno %>% str_detect("Urban") ~ "Urbano")) %>%
  mutate(armadilha = case_when(armadilha %>% str_detect("TRUE") ~ "Sim",
                             armadilha %>% str_detect("FALSE") ~ "Não")) %>%
  group_by(terreno, armadilha) %>%
  summarise(Freq)%>%
  mutate(freq_relativa = round(Freq/sum(Freq) * 100,1)) 
porcentagens<-str_c(grafico$freq_relativa, "%") %>% str_replace("\\.", ",")
  legendas<-str_squish(str_c(grafico$Freq, "(", porcentagens,")"))
  
grafico<-ggplot(grafico) +
  aes(x = fct_reorder(terreno, Freq, .desc = T), y = Freq,
    fill = armadilha, label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, 
            hjust = 0.5, size = 3) + labs(x = "Terreno", y = "Frequência", 
                                          fill = "Armadilha funcionou de primeira") + theme_estat()

ggsave("plot_analise3.pdf", width = 158, height = 93, units = "mm")
