
library(openxlsx)
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)
DatosDCA <- data.frame(Especie=c("Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa","Bertholletia excelsa",
                                       "Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum","Calycophyllum spruceanum",
                                       "Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis", "Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis","Cedrelinga cateniformis"),
                       Trata =c("Trat 1", "Trat 1", "Trat 1", "Trat 1","Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 2", "Trat 2", "Trat 2", "Trat 2",  "Trat 2", "Trat 2", "Trat 2", "Trat 2","Trat 2", "Trat 2", "Trat 3", "Trat 3", "Trat 3", "Trat 3","Trat 3", "Trat 3", "Trat 3", "Trat 3","Trat 3", "Trat 3",
                                "Trat 1", "Trat 1", "Trat 1", "Trat 1","Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 2", "Trat 2", "Trat 2", "Trat 2",  "Trat 2", "Trat 2", "Trat 2", "Trat 2","Trat 2", "Trat 2", "Trat 3", "Trat 3", "Trat 3", "Trat 3","Trat 3", "Trat 3", "Trat 3", "Trat 3","Trat 3", "Trat 3",
                                "Trat 1", "Trat 1", "Trat 1", "Trat 1","Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 1", "Trat 2", "Trat 2", "Trat 2", "Trat 2",  "Trat 2", "Trat 2", "Trat 2", "Trat 2","Trat 2", "Trat 2", "Trat 3", "Trat 3", "Trat 3", "Trat 3","Trat 3", "Trat 3", "Trat 3", "Trat 3","Trat 3", "Trat 3"),
                       Adsorcion = c(52,55,58,61,64,67,70,73,76,79,51,54,57,60,63,66,69,72,75,78,53,56,59,62,65,68,71,74,77,80,
                                     81, 65, 64, 53, 92 ,93, 87, 64, 75, 76, 77, 55, 77, 78, 59, 79, 85, 58, 76, 57, 92, 59,69, 86, 64, 67, 93, 89, 62, 65,
                                     95, 87, 85, 88, 71, 77, 75, 64, 81, 59, 65, 76, 69, 93, 73,58, 64, 63, 55, 80, 97, 75, 99, 88, 90, 87, 91, 84, 77, 54))

DatosDCA

summary(DatosDCA$Adsorcion[1:10])  #Físico
summary(DatosDCA$Adsorcion[10:20])       #Alcalino
summary(DatosDCA$Adsorcion[20:30])     #Acido

summary(DatosDCA$Adsorcion[30:40])     #Físico
summary(DatosDCA$Adsorcion[40:50])   #Alcalino
summary(DatosDCA$Adsorcion[50:60])    #Acido

summary(DatosDCA$Adsorcion[60:70])      #Físico
summary(DatosDCA$Adsorcion[70:80])     #Alcalino
summary(DatosDCA$Adsorcion[80:90])     #Acido
DatosDCA$Trata = iconv(DatosDCA$Trata, from = "UTF-8", to= "latin1")


fit<-aov(Adsorcion~Trata, data=DatosDCA[1:90, 1:3])
summary(fit)

#Normalidad de los residuos
#Para verificar la normalidad de los residuos utilizaremos la prueba de Shapiro-Wilks cuyo script es el siguiente:
shapiro.test(residuals(fit))
#NO SON NORMALES

#Homocedasticidad de varianzas
#Para verificar el supuesto de homocedasticidad de las varianzas utilizaremos la prueba de Bartlett script es el siguiente:
bartlett.test(Adsorcion~Trata, data=DatosDCA)



anova <- aov( Adsorcion ~ Especie*Trata, data = DatosDCA)
summary(anova)

data_summary <- group_by(DatosDCA, Especie, Trata) %>%
  summarise(mean=mean( Adsorcion), sd=sd( Adsorcion)) %>%
  arrange(desc(mean))

tukey <- TukeyHSD(anova)

tukey.cld <- multcompLetters4(anova, tukey)


cld <- as.data.frame.list(tukey.cld$`Especie:Trata`)
data_summary$Tukey <- cld$Letters
data_summary


Creci_castaña_bar = ggplot(data_summary, aes(x = factor(Trata ), y = mean, fill = Especie , colour = Especie )) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.3)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x="Tratamientos", y="Crecimiento Longitudinal (cm)") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black",face="bold"),
        axis.title.y = element_text(color="black", face="bold"))+
  geom_text(label=paste(round(data_summary$mean),data_summary$Tukey), position = position_dodge(0.90), size = 3,
            vjust=-1, hjust=0.5)+
  
  ylim(0, 120) +
  geom_text(aes(label=c("A","AB","AB","AB","AB","AB", "B","B","B"), y = 10), position = position_dodge(0.90), show.legend = FALSE, size = 2)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "red"))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "red"))+
  scale_x_discrete(limit = c("Trat 1", "Trat 2","Trat 3"),
                   labels = c("Tratamiento 1","Tratamiento 2","Tratamiento 3"))
Creci_castaña_bar



library(tidyverse)
library(ggpubr)
library(rstatix)

DatosDCA
# Build the linear model
model  <- lm(Adsorcion ~ Trata*Especie,
             data = DatosDCA)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
         
# Calcular la prueba de normalidad de Shapiro-Wilk
shapiro_test(residuals(model))

DatosDCA %>%
  group_by(Trata , Especie) %>%
  shapiro_test(Adsorcion)

ggqqplot(DatosDCA, "Adsorcion", ggtheme = theme_bw()) +
  facet_grid(Trata ~ Especie)

#Suposición de homogeneidad de la varianza
DatosDCA %>% levene_test(Adsorcion ~ Trata*Especie)

res.aov <- DatosDCA%>% anova_test(Adsorcion ~ Trata*Especie)
res.aov

# Agrupar los datos por tratamiento y ajustar el anova
model <- lm(Adsorcion ~ Trata * Especie, data = DatosDCA)
DatosDCA %>%
  group_by(Trata) %>%
  anova_test(Adsorcion ~ Especie, error = model)

# comparaciones por pares
library(emmeans)
pwc <- DatosDCA %>% 
  group_by(Trata) %>%
  emmeans_test(Adsorcion ~ Especie, p.adjust.method = "bonferroni") 
pwc



DatosDCA

stat.test <- DatosDCA %>%
  group_by(Trata) %>%
  t_test(Adsorcion ~ Especie) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

# Añadir valores p a los gráficos de caja
stat.test <- stat.test %>% add_xy_position(x = "Trata", dodge = 0.8)


# Create a box plot
Creci_castaña_box= ggboxplot(
  DatosDCA, x = "Trata", y = "Adsorcion", add = c("jitter"),fill = "Especie",alpha=0.3,
  color = "Especie", palette = c("#00AFBB", "#E7B800", "red"),
  )+ 
  stat_pvalue_manual(
    stat.test,  label = "p = {p}", tip.length = 0,size = 2.5)+
  theme_bw()+
  scale_shape_discrete (guide =
                         guide_legend(label.theme = element_text(angle = 0, face = "italic")))+
  theme(legend.position = "top",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="Tratamientos", y="Crecimiento Longitudinal (cm)") +
  scale_x_discrete(limit = c("Trat 1", "Trat 2","Trat 3"),
                   labels = c("Tratamiento 1","Tratamiento 2","Tratamiento 3"))



Creci_castaña_box

library(ggpubr)
legend <- get_legend(Creci_castaña_box)

Creci_castaña_box_final = Creci_castaña_box +  theme(legend.position = "none")

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 16.5), expand = FALSE) +
  
  draw_plot(Creci_castaña_bar , width = 15, height = 15,x = 0, y = 0)+
  draw_plot(Creci_castaña_box_final, width = 15, height = 15,x = 15, y = 0)+
  
  draw_plot(legend , width = 5, height = 5,x = 12, y = 13)+
  
  
  theme(panel.background = element_rect(fill = "white"))


Expo 

ggsave(plot=Expo ,"Grafico de anova de dos factores.png",units = "cm",width = 30, #ancho
       height = 16.5, #alargo
       dpi=1200)
























































