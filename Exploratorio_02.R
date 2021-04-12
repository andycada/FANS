
#------Exploratorio Datos Toxinas TPM ------------------------------------------------#

## Analizo los datos incluyendo las 4 zonas menos muestreadas, N muy diferente para las 4 estaciones  ----------------------------------------------------------------
# BBB: Bahia Brown Bajo
# BBE: Bahia Brown Entrada
# BBF: Bahia Brown Fondo
# PP:  Punta Parana

library(readxl)
data <- read_excel("Data/totalR.xlsx")


# data$STX <- data$STX + 0.001

# Filtrar una sola especie (paquete tidyverse)
# data <- data %>% filter(Organism == "M. edulis" )


library(tidyverse)
theme_set(theme_bw())

library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))

d_sorted <- data %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))


# (Fig 2)  STX vs Area, datos totales------------------------------------#
theme_set(theme_light(base_size = 10, base_family = "Poppins"))
ggplot(data, aes(x = Date, y = STX,color=Organism)) + labs (y = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})),x = "Year") + geom_line(size = 0.7) +  facet_wrap(~ Area, nrow = 4, ncol = NULL,scale="free_y") +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 


# Fig 3 Box plot (Manuscrito,  season STX, escala log, asterisco p resumir tukey)---------#
# Escala log (mas medias)

ggplot(d_sorted, aes(x=season,y=log(data$STX + 0.001),color=season)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + labs(x = NULL , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1}))) + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank()) 

ggplot(d_sorted, aes(x=season,y=log(STX),color=season)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank()) 

# Sin escala log (medias)
ggplot(d_sorted, aes(x=season,y=STX,color=season)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none")


# los puntos mas grandes son las medianas o las medias
# agregar linea sobre los 80 ?g: geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6)
# Agregar mediana: stat_summary(fun = mean, geom = "point", size = 5)


# CORROBORAR MEDIAS Y MEDIANAS

library(dplyr)
group_by(data, season) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )



# ANALISIS NO PARAMETRICOS ---------------------------------------------------------------------------##
# ANOVAS no se cumple supuesto de normalidad asique se aplican test no parametricos 
# Prueba de Kruskal-Wallis para dos o m?s muestras independientes
# no-parametrica, no asume normalidad pero si homocedasticidad

# STX vs Area -------------------------------------------------------------#

STX<- data$STX
Area<-data$Area
data$season<-as.factor(data$season)

#Normalidad
shapiro.test(STX) #p-value < 2.2e-16
shapiro.test(log(STX + 1)) #p-value < 2.2e-16

# homogeneidad de varianzas 
bartlett.test(STX ~ Area, data = data, na.action=na.fail) #p-value < 2.2e-16

# homogeneidad de varianzas (No parametrico)
fligner.test(STX ~ Area, data = data, na.action=na.fail)

k1<-kruskal.test(STX ~ Area, data = data, na.action=na.fail)
k1 #p-value < 2.2e-16 # STX es sig diferente en cada area de muestreo (BBE>PP>BBF>BBB)
 
# Comparaciones multiples no parametrico
install.packages("PMCMRplus") 

pairwise.wilcox.test(data$STX,data$Area,p.adj="bonferroni") # todas areas sig difernetes excepto BBF y BBB, BBF y PP

# El test the dunn NO LO USAS EN EL PAPER eliminarlo del script
# 
#
# install.packages("dunn.test")
# library(dunn.test)
# install.packages("FSA")
# library(FSA)
# dunnTest(data$STX, data$Area, method="bonferroni") # todas las areas son sifg diferentes entre ellas 
# # estos dos test me dan resultados diferents


STX <- data$STX + 0.001
library(dplyr)
 group_by(data, Area) %>%
  summarise(
      count = n(),
           mean = mean(STX, na.rm = TRUE),
        sd = sd(STX, na.rm = TRUE),
         median = median(STX, na.rm = TRUE),
         IQR = IQR(STX, na.rm = TRUE)
       )
 
# STX vs Season-------------------------------------------------------------------#

# homogeneidad de varianzas 
bartlett.test(STX ~ season, data = data, na.action=na.fail) #p-value < 2.2e-16

# homogeneidad de varianzas (No parametrico)
fligner.test(STX ~ season, data = data, na.action=na.fail)


k2<-kruskal.test(STX ~ season, data = data, na.action=na.fail)
k2 #p-value < 2.2e-16 # STX es sig diferente en cada estacion del anio (summer> autumn> spring> winter)

# wilcox.test (anova NP dos muestras) Performs one- and two-sample Wilcoxon tests on vectors of data; the latter is also known as 'Mann-Whitney' test

# Comparaciones multiples no parametrico

pairwise.wilcox.test(data$STX,data$season,p.adj="bonferroni") # primero y luego x
# Calculate pairwise comparisons between group levels with corrections for multiple testing

dunnTest(data$STX, data$season, method="bonferroni") # todas las season son sifg diferentes entre ellas 

STX <- data$STX + 0.001
library(dplyr)
group_by(data, season) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )


library(sciplot)

#Fig 4a
#bargraph.CI(season, STX, Area, data = data,  xlab = "Season", ylab = "?g STX eq/100 g tissue", col = c("steelblue1", "steelblue4"), legend = TRUE)

#layout(matrix(c(1:2), 2, 1))# graficar primero b y luego a

a<-bargraph.CI(Area, STX, season, data = d_sorted, ylim =c(0,900), xlab = NA, ylab = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})), col = c("violetred4", "steelblue3","seagreen4","yellow"),legend = T, cex.leg= 0.9, ncol=1, x.leg=0.6, y.leg=850, cex.lab = 1,cex.names = 1)

#Fig 4b (frecuencia de vedas)

frecuencia_de_vedas <- read_excel("Data/frecuencia de vedas.xlsx")
d_sorted1 <- frecuencia_de_vedas %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

b<-bargraph.CI(Area, outbrakes, season, data = d_sorted1, ylim =c(0,70), xlab = "Area", ylab = "Outbrakes (%)", col = c("violetred4", "steelblue3","seagreen4","yellow"),legend = F, cex.leg= 0.9, ncol=1, x.leg=0.3, y.leg=100,  cex.lab = 1,cex.names = 1)

#Fig 4c (Duration)
data2 <- read_excel("Data/eventos toxicos y duracion.xlsx")

d_sorted1 <- data2 %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

c<-bargraph.CI(Area,Duration,season, data = d_sorted1, ylab = "Duration (days)", xlab = "Area", col = c("violetred4", "steelblue3","seagreen4","yellow"),legend = T, cex.leg= 0.9, ncol=1, x.leg=0.3, y.leg=150,  cex.lab = 1,cex.names = 1)

k3<-kruskal.test(Duration ~ season, data = data2, na.action=na.fail)#p-value = 0.001408

pairwise.wilcox.test(data2$Duration,data2$season,p.adj="bonferroni")# los unicos diferentes son autun de spring,spring summer da mal el test .
dunnTest(data2$Duration,data2$season,method="bonferroni") # los unicos diferentes summer, spring y summer winter



#layout(1)

# PARA COMPARAR CON INFOSTAT KRUSKAL WALLIS STX VS SEASON (PARTICIONADO, FILTRADO SEGUN AREA)

BBE <- data %>% filter(Area == "BBE" )
warnings()

kruskal.test(STX ~ season, data = BBE, na.action=na.fail)# p-value = 7.916e-14
pairwise.wilcox.test(BBE$STX,BBE$season,p.adj="bonferroni") #TODAS SIG  DIFERENTES menos spring winter
dunnTest(BBE$STX, BBE$season, method="bonferroni")# todas sig (sim infostat)

BBF <- data %>% filter(Area == "BBF" )

kruskal.test(STX ~ season, data = BBF, na.action=na.fail)#p-value = 3.991e-06
pairwise.wilcox.test(BBF$STX,BBF$season,p.adj="bonferroni")# todas sig menos spring winter, autumn summer y spring autun (similar Infostat)
dunnTest(BBF$STX, BBF$season, method="bonferroni")# todas sig menos autumn summer

BBB <- data %>% filter(Area == "BBB" )

kruskal.test(STX ~ season, data = BBB, na.action=na.fail)#p-value = 1.327e-05
pairwise.wilcox.test(BBB$STX,BBB$season,p.adj="bonferroni") ## todas sig menos autumn summer spring winter spring summer(similar a  Infostat)
dunnTest(BBB$STX, BBB$season, method="bonferroni")# todas sig menos autumn summer y spring winter 

PP <- data %>% filter(Area == "PP" )

kruskal.test(STX ~ season, data = PP, na.action=na.fail)#p-value = 1.778e-07
pairwise.wilcox.test(PP$STX,PP$season,p.adj="bonferroni")# todas sig menos spring autumn, autumn summer
dunnTest(PP$STX, PP$season, method="bonferroni")# todas sig menos spring autumn (similar a Infostat)



# STX vs organism ------------------------------------------------------------------#

# (Fig 6) grafico de barras organismo 
bargraph.CI(Organism,STX, data = data, ylab = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})), xlab = NA, space=0.5,cex.lab = 1.1, x.leg = 1.3,cex.names = 1.3, col = c("red1", "green3"))
#cex.lab tamanio de la letra

# Prueba de Wilcoxon Man Whitney U test (No P alternativo a prueba t para dos muestras independ)
organism <- read_excel("Data/organism.xlsx")
str(organism)

wilcox.test(STX ~ Organism, data = data, exact = FALSE)#p-value = 5.375e-06. 
#We can conclude that a ater's median weight is significantly different from m edulis's median weight with a p-value<0.05.

wilcox.test(STX ~ Organism, data = data, exact = FALSE, alternative = "greater")#p-value = 2.687e-06
# A ater> Medulis (hip alternativa comprobada)

# (Fig 6a) grafico de barras organismo para cd area 
bargraph.CI(Area,STX,Organism, data = data, ylab = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})), xlab = NA, cex.lab = 1.1,cex.names = 1.25,col = c("red1", "green3"), legend=T,  x.leg=9, y.leg=200, ncol=1)

# (Fig 6b) alternativa grafico de barras organismo por season  
bargraph.CI(season, STX, Organism, data = data,  xlab = NA, ylab = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})), cex.lab = 1.1,cex.names = 1.25,col = c("red1", "green3"),legend = TRUE, x.leg=10, y.leg=500, ncol=1)


## solo datos de Mejillones 

M <- data %>% filter(Organism == "M. edulis" )

k6<-kruskal.test(STX ~ Area, data = M, na.action=na.fail)
k6 # p-value = 2.721e-08 
# niveles de STX son sifnificativamente diferentes enlas 3 zonas q tienen mejillon 


pairwise.wilcox.test(M$STX,M$Area,p.adj="bonferroni") # BBF no es sig diferente de PP
dunnTest(M$STX,M$Area, method="bonferroni") # todas sig diferente


#solo datos de cholga 

C <- data %>% filter(Organism == "A. ater" )


k7<-kruskal.test(STX ~ Area, data = C, na.action=na.fail)
k7 # p-value = 1.761e-12  #niveles de STX son sig diferentes en las 2 zonas q tienen cholga (BBE> BBB)


# solo datos de BBE para comparar cholga vs mejillon 

BBE <- data %>% filter(Area == "BBE" )


k8<-kruskal.test(STX ~ Organism, data = BBE, na.action=na.fail)

wilcox.test(STX ~ Organism, data = BBE, exact = FALSE)#p-value = 0.001952
#We can conclude that a ater's median weight is significantly different from m edulis's median weight with a p-value<0.05.

wilcox.test(STX ~ Organism, data = BBE, exact = FALSE, alternative = "greater")#p-value = 0.0009761
# A ater> Medulis (hip alternativa comprobada)



## analisis de STX vs organimsmo por season (x separado)---------------------------------------------#

# winter
W <- data %>% filter(season == "winter" )

k9<-kruskal.test(STX ~ Organism, data = W, na.action=na.fail)#p-value = 0.008421


# autumn
A <- data %>% filter(season == "autumn" )

k10<-kruskal.test(STX ~ Organism, data = A, na.action=na.fail)#p-value = p-value = 0.000414
#los niveles de STX en las dos especies son significativamente diferentes durante el otonio
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups


# summer

S <- data %>% filter(season == "summer" )

k10<-kruskal.test(STX ~ Organism, data = S, na.action=na.fail)#p-value = 0.01953
#los niveles de STX  son significativamente diferentes durante el verano


# spring

SP <- data %>% filter(season == "spring" )

k11<-kruskal.test(STX ~ Organism, data = SP, na.action=na.fail)#p-value = 0.1874
#los niveles de STX no son significativamente diferentes durante el verano


# STX vs Year (medias/medianas anuales) -------------------------------------------------------------# 

# Fig 5 Box plot STX vs Year

d_sorted <- data %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))


ggplot(d_sorted, aes(x=Year,y=STX,color=Year)) + xlab("") + labs(x = "Year" , y = expression(PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1}))) + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none",panel.grid = element_blank()) 
ggplot(d_sorted, aes(x=Year,y=STX,color=Year)) + xlab("") + geom_jitter() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6)+ stat_summary(fun = median, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank())  


#escala log
ggplot(d_sorted, aes(x=Year,y=log(STX),color=Year)) + xlab("") + labs(x = "Year" , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1})))+ geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none", panel.grid = element_blank())  
ggplot(d_sorted, aes(x=Year,y=log(data$STX + 0.001),color=Year)) + xlab("") + labs(x = "Year" , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1})))+ geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none", panel.grid = element_blank())  

k8<-kruskal.test(STX ~ Year, data = data, na.action=na.fail)
k8 #p-value < 2.2e-16 ,los niveles de STX  son significativamente diferentes segun el anio

pairwise.wilcox.test(data$STX,data$Year,p.adj="bonferroni") # 
dunnTest(data$STX,data$Year, method="bonferroni") # 


# PARA TABLA GENERAL DE MANUSCRITO por anos toxicos y menos toxicos 
# Compute summary statistics by groups:

STX <- data$STX + 0.001

  library(dplyr)
group_by(data, Year) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )

group_by(data, season) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )


























