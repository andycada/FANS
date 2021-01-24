
#------Exploratorio Datos Toxinas TPM ------------------------------------------------#

## Analizo los datos incluyendo las 4 zonas menos muestreadas, N muy diferente para las 4 estaciones  ----------------------------------------------------------------
# BBB: Bahia Brown Bajo
# BBE: Bahia Brown Entrada
# BBF: Bahia Brown Fondo
# PP:  Punta Parana

library(readxl)

data <- totalR
data <- read_excel("Data/totalR.xlsx")
names(data)
str(data)
data$Area

# data$STX <- data$STX + 0.001

# Filtrar una sola especie (paquete tidyverse)
# data <- data %>% filter(Organism == "M. edulis" )


#Graficos exploratorios

library(tidyverse)
theme_set(theme_bw())

library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))

d_sorted <- data %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ Organism,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(d_sorted %>% filter(Organism=="M. edulis"), aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ season,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(d_sorted, aes(x = Area, y = STX,color=Area)) +
  coord_flip() + xlab("") + geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2)  + theme_bw()

# (Fig 1) Para manuscrito ( STX vs Area, datos totales)----------------------------------#
theme_set(theme_light(base_size = 10, base_family = "Poppins"))
ggplot(data, aes(x = Date, y = STX,color=Organism)) + labs (y = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})),x = "Year") + geom_line(size = 0.7) +  facet_wrap(~ Area, nrow = 4, ncol = NULL,scale="free_y") +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 


# Fig 2 Box plot (Manuscrito,  season STX, escala log, asterisco p resumir tukey)---------#
# Escala log (mas medias)
ggplot(d_sorted, aes(x=season,y=log(data$STX + 0.001),color=season)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + labs(x = NULL , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1}))) + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank()) 
ggsave("Figures/Fig2.png",width=6,height=4,units="in",dpi=600)

ggplot(d_sorted, aes(x=season,y=log(STX),color=season)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank()) 

# puntos (media y mediana)
ggplot(d_sorted, aes(x=season,y=log(data$STX + 0.001),color=season)) + xlab("") + geom_jitter() + scale_color_viridis_d() + geom_hline(aes(yintercept = log(80)), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none")
ggplot(d_sorted, aes(x=season,y=log(data$STX + 0.001),color=season)) + xlab("") + geom_jitter() + scale_color_viridis_d() + geom_hline(aes(yintercept = log(80)), color = "red", size = 0.6)+ stat_summary(fun = median, geom = "point", size = 5) + theme(legend.position = "none")

# Sin escala log (medias)
ggplot(d_sorted, aes(x=season,y=STX,color=season)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none")

# puntos (media y mediana)
ggplot(d_sorted, aes(x=season,y=STX,color=season)) + xlab("") + geom_jitter() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6) + stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none")
ggplot(d_sorted, aes(x=season,y=STX,color=season)) + xlab("") + geom_jitter() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6) + stat_summary(fun = median, geom = "point", size = 5)+ theme(legend.position = "none")


# los puntos mas grandes son las medianas o las medias
# agregar linea sobre los 80 ?g: geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6)
# Agregar mediana: stat_summary(fun = mean, geom = "point", size = 5)
# geom_jitter(size = 2, alpha = 0.25, width = 0.2) para puntos en vez de cajas 


#corroborar medias y medianas

library(dplyr)
group_by(data, season) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )


ggplot(d_sorted, aes(x=Area,y=STX,color=Area)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana
ggplot(d_sorted, aes(x=Organism,y=STX,color=Organism)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana









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

install.packages("dunn.test")
library(dunn.test)
install.packages("FSA")
library(FSA)
dunnTest(data$STX, data$Area, method="bonferroni") # todas las areas son sifg diferentes entre ellas 
# estos dos test me dan resultados diferents


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

# Comparaciones multiples non parametrico

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


# anova factorial a dos factores cruzados (area y season) ------------------------#

library(sciplot)


#Fig 3
#bargraph.CI(season, STX, Area, data = data,  xlab = "Season", ylab = "?g STX eq/100 g tissue", col = c("steelblue1", "steelblue4"), legend = TRUE)

layout(matrix(c(1:2), 2, 1))# graficar primero b y luego a

a<-bargraph.CI(Area, STX, season, data = d_sorted, ylim =c(0,900), xlab = NA, ylab = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})), col = c("violetred4", "steelblue3","seagreen4","yellow"),legend = T, cex.leg= 0.9, ncol=1, x.leg=0.6, y.leg=850, cex.lab = 1,cex.names = 1)

#Fig 3b (frecuencia de vedas)

frecuencia_de_vedas <- read_excel("Data/frecuencia de vedas.xlsx")
d_sorted1 <- frecuencia_de_vedas %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

b<-bargraph.CI(Area, outbrakes, season, data = d_sorted1, ylim =c(0,70), xlab = "Area", ylab = "Outbrakes (%)", col = c("violetred4", "steelblue3","seagreen4","yellow"),legend = F, cex.leg= 0.9, ncol=1, x.leg=0.3, y.leg=100,  cex.lab = 1,cex.names = 1)

layout(1)

# PARA COMPARAR CON INFOSTAT KRUSKAL WALLIS STX VS SEASON (PARTICIONADO, FILTRADO SEGUN AREA)

BBE <- data %>% filter(Area == "BBE" )

kruskal.test(STX ~ season, data = BBE, na.action=na.fail)# p-value = 7.916e-14
pairwise.wilcox.test(BBE$STX,BBE$season,p.adj="bonferroni") #TODAS SIG  DIFERENTES menos spring winter
dunnTest(BBE$STX, BBE$season, method="bonferroni")# todas sig (sim infostat)

BBF <- data %>% filter(Area == "BBF" )

kruskal.test(STX ~ season, data = BBF, na.action=na.fail)#p-value = 3.991e-06
pairwise.wilcox.test(BBF$STX,BBF$season,p.adj="bonferroni")# todas sig menos spring winter, autumn summer y spring autun (sim Infostat)
dunnTest(BBF$STX, BBF$season, method="bonferroni")# todas sig menos autumn summer

BBB <- data %>% filter(Area == "BBB" )

kruskal.test(STX ~ season, data = BBB, na.action=na.fail)#p-value = 1.327e-05
pairwise.wilcox.test(BBB$STX,BBB$season,p.adj="bonferroni") ## todas sig menos autumn summer spring winter spring summer(sim Infostat)
dunnTest(BBB$STX, BBB$season, method="bonferroni")# todas sig menos autumn summer y spring winter 

PP <- data %>% filter(Area == "PP" )

kruskal.test(STX ~ season, data = PP, na.action=na.fail)#p-value = 1.778e-07
pairwise.wilcox.test(PP$STX,PP$season,p.adj="bonferroni")# todas sig menos spring autumn, autumn summer
dunnTest(PP$STX, PP$season, method="bonferroni")# todas sig menos spring autumn (sim a Infostat)



# STX vs organism ------------------------------------------------------------------#

# (Fig 3) grafico de barras organismo 
bargraph.CI(Organism,STX, data = data, ylab = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})), xlab = NA, space=0.5,cex.lab = 1.1, x.leg = 1.3,cex.names = 1.3, col = c("red1", "green3"))

#cex.lab tamanio de la letra

# Prueba de Wilcoxon Man Whitney U test (No P alternativo a prueba t para dos muestras independ)
organism <- read_excel("Data/organism.xlsx")
str(organism)

wilcox.test(STX ~ Organism, data = data, exact = FALSE)#p-value = 5.375e-06. 
#We can conclude that a ater's median weight is significantly different from m edulis's median weight with a p-value<0.05.

wilcox.test(STX ~ Organism, data = data, exact = FALSE, alternative = "greater")#p-value = 2.687e-06
# A ater> Medulis (hip alternativa comprobada)

# (Fig 6) grafico de barras organismo para cd area 
bargraph.CI(Area,STX,Organism, data = data, ylab = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})), xlab = NA, cex.lab = 1.1,cex.names = 1.25,col = c("red1", "green3"), legend=T,  x.leg=9, y.leg=200, ncol=1)

# (Fig x) alternativa grafico de barras organismo por season  
bargraph.CI(season, STX, Organism, data = data,  xlab = NA, ylab = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})), cex.lab = 1.1,cex.names = 1.25,col = c("red1", "green3"),legend = TRUE, x.leg=10, y.leg=500, ncol=1)




## solo datos de Mejillones 

M <- data %>% filter(Organism == "M. edulis" )

d_sorted <- M %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x=season,y=STX,color=season)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana

ggplot(d_sorted, aes(x=Area,y=STX,color=Area)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana

#Histograma
ggplot(d_sorted, aes(x=STX,color=Area)) + xlab("") + geom_histogram(fill="white", alpha=0.5, position="dodge") + scale_color_viridis_d()

# Barras (mean)
bargraph.CI(Area, STX, data = M, xlab = "Area", ylab = "?g STX eq/100 g tissue", legend = TRUE)


k6<-kruskal.test(STX ~ Area, data = M, na.action=na.fail)
k6 # p-value = 2.721e-08 
# niveles de STX son sifnificativamente diferentes enlas 3 zonas q tienen mejillon 


pairwise.wilcox.test(M$STX,M$Area,p.adj="bonferroni") # BBF no es sig diferente de PP
dunnTest(M$STX,M$Area, method="bonferroni") # todas sig diferente



#solo datos de cholga 

C <- data %>% filter(Organism == "A. ater" )

d_sorted <- C %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x=season,y=STX,color=season)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana

ggplot(d_sorted, aes(x=Area,y=STX,color=Area)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana

#Histograma
ggplot(d_sorted, aes(x=STX,color=Area)) + xlab("") + geom_histogram(fill="white", alpha=0.5, position="dodge") + scale_color_viridis_d()

# Barras
bargraph.CI(Area, STX, data = C,  xlab = "Area", ylab = "?g STX eq/100 g tissue", legend = TRUE)


k7<-kruskal.test(STX ~ Area, data = C, na.action=na.fail)
k7 # p-value = 1.761e-12  #niveles de STX son sig diferentes en las 2 zonas q tienen cholga (BBE> BBB)



# solo datos de BBE para comparar cholga vs mejillon 

BBE <- data %>% filter(Area == "BBE" )

d_sorted <- BBE %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x=Organism,y=STX,color=Organism)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana

#Histograma
ggplot(d_sorted, aes(x=STX,color=Area)) + xlab("") + geom_histogram(fill="white", alpha=0.5, position="dodge") + scale_color_viridis_d()


# Barras
bargraph.CI(Organism, STX, data = BBE,  xlab = "Organism", ylab = "?g STX eq/100 g tissue", legend = TRUE)

wilcox.test(STX ~ Organism, data = BBE, exact = FALSE)#p-value = 0.001952
#We can conclude that a ater's median weight is significantly different from m edulis's median weight with a p-value<0.05.

wilcox.test(STX ~ Organism, data = BBE, exact = FALSE, alternative = "greater")#p-value = 0.0009761
# A ater> Medulis (hip alternativa comprobada)


library(readxl)
BBE <- read_excel("Data/BBE-organism.xlsx")

wilcox.test(BBE$A.ater,BBE$M.edulis)#p-value = 5.375e-06

## analisis de STX vs organimsmo por season (x separado)---------------------------------------------#

# winter
W <- data %>% filter(season == "winter" )
str(W)

d_sorted <- W %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x=Organism,y=STX,color=Organism)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana
ggplot(d_sorted, aes(x=Organism,y=log(STX),color=Organism)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = log(80)), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none") 


bargraph.CI(Organism, STX, data = W, xlab = "Winter", ylab = "?g STX eq/100 g tissue", legend = TRUE)


kruskal.test(STX ~ Organism, data = W, na.action=na.fail)#p-value = 0.008421



# autumn
A <- data %>% filter(season == "autumn" )
str(A)

d_sorted <- A %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x=Organism,y=STX,color=Organism)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana
ggplot(d_sorted, aes(x=Organism,y=log(STX),color=Organism)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = log(80)), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none", title(main="Autumn")) 


bargraph.CI(Organism, STX, data = A, xlab = "Autumn", ylab = "?g STX eq/100 g tissue", legend = TRUE)

kruskal.test(STX ~ Organism, data = A, na.action=na.fail)#p-value = p-value = 0.000414
#los niveles de STX en las dos especies son significativamente diferentes durante el otonio
#As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups


# summer

S <- data %>% filter(season == "summer" )
str(S)

d_sorted <- S %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x=Organism,y=STX,color=Organism)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana
ggplot(d_sorted, aes(x=Organism,y=log(STX),color=Organism)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = log(80)), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none")


bargraph.CI(Organism, STX, data = S, xlab = "Summer", ylab = "?g STX eq/100 g tissue", legend = TRUE)

kruskal.test(STX ~ Organism, data = S, na.action=na.fail)#p-value = 0.01953
#los niveles de STX  son significativamente diferentes durante el verano


# spring

SP <- data %>% filter(season == "spring" )
str(SP)

d_sorted <- SP %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x=Organism,y=STX,color=Organism))+ xlab("") + geom_jitter() + scale_color_viridis_d() + theme(legend.position = "none")
ggplot(d_sorted, aes(x=Organism,y=log(STX),color=Organism)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none")


bargraph.CI(Organism, STX, data = SP, xlab = "Spring", ylab = "?g STX eq/100 g tissue", legend = TRUE)

kruskal.test(STX ~ Organism, data = SP, na.action=na.fail)#p-value = 0.1874
#los niveles de STX no son significativamente diferentes durante el verano


ggplot(d_sorted, aes(x=season,y=STX,color=season)) + xlab("") + geom_jitter() + scale_color_viridis_d() ### Agregar mediana



#ubicar los 4 boxplot de season juntos 
#layout(matrix(c(1:6), 2, 3)) 
#layout(1)
#layout(matrix(c(1:2), 1, 2)) 
#layout(matrix(c(1:6), 2, 3)) 
#plot(a3, 1:6) 
#layout(1)

# Dif entre medias/medianas anuales -------------------------------------------------------------# 

k8<-kruskal.test(STX ~ Year, data = data, na.action=na.fail)
k8 #p-value < 2.2e-16 ,los niveles de STX  son significativamente diferentes segun el anio

pairwise.wilcox.test(data$STX,data$Year,p.adj="bonferroni") # 
dunnTest(data$STX,data$Year, method="bonferroni") # 

# Box plot (Manuscrito)

d_sorted <- data %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))


ggplot(d_sorted, aes(x=Year,y=STX,color=Year)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none",panel.grid = element_blank()) 
ggplot(d_sorted, aes(x=Year,y=STX,color=Year)) + xlab("") + geom_jitter() + scale_color_viridis_d() + geom_hline(aes(yintercept = 80), color = "red", size = 0.6)+ stat_summary(fun = median, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank())  


#escala log
ggplot(d_sorted, aes(x=Year,y=log(STX),color=Year)) + xlab("Year") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none", panel.grid = element_blank())  
ggplot(d_sorted, aes(x=Year,y=log(data$STX + 0.001),color=Year)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none", panel.grid = element_blank())  


# Ver de hacer test de freeman (no parametrico a dos vias, parece q es solo para medidas repetidas) con year y area a ver si da diferente 




# PARA TABLA GENERAL DE MANUSCRITO por anos toxicos y menso toxicos 
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




















### Eventos toxicos y duracion de veda ----------------------------------------------------------#
# Analisis de picos: 
# STXmax: concentracion maxima alcanzada duante el evento toxico
# Duration: duracion de la veda, T desde q el mejillon supera los 80 ?g STX/100 g tejido, hasta que tiene valores por debajo de ese umbral

library(readxl)

eventos_toxicos_y_duracion <- read_excel("Data/eventos toxicos y duracion.xlsx")
data2 <- eventos_toxicos_y_duracion
data2 <- read_excel("Data/eventos toxicos y duracion.xlsx")
names(data2)

bargraph.CI(Duration,STXmax,Organism, data = data2, ylab = "?g STX eq/100 g tissue", xlab = "Days", legend = TRUE, x.leg=0, y.leg=500)

a8 <- aov(log(Duration) ~ Organism, data = data2, na.action=na.fail) 
summary(a8) # la duracion del evento toxico depende del organismo? no dif sig en la duracion del evento toxico segun el organismo

layout(matrix(c(1:6), 2, 3)) # residuales parecen mejorar cn el log
plot(a8, 1:6) 
layout(1)

bargraph.CI(Duration,STXmax,Season, data = data2, ylab = "?g STX eq/100 g tissue", xlab = "Days", legend=T, x.leg=0, y.leg=500)


a9 <- aov(log(Duration) ~ Season, data = data2, na.action=na.fail) 
summary(a9) # dif sig en la duracion del evento toxico segun la season 

TukeyHSD(a9,"Season") # summer - autumn mismo grupo y diferente de spring - winter

layout(matrix(c(1:6), 2, 3)) 
plot(a9, 1:6) # residuales parecen mejorar cn el log
layout(1)

k9<-kruskal.test(Duration ~ Season, data = data2, na.action=na.fail)#p-value = 0.001408

pairwise.wilcox.test(data2$Duration,data2$Season,p.adj="bonferroni")# los unicos diferentes son autun de spring,spring summer.

dunnTest(data2$Duration,data2$Season,method="bonferroni") # los unios diferentes summer, spring y summer winter 

bargraph.CI(Duration,STXmax,Area, data = data2, ylab = "?g STX eq/100 g tissue", xlab = "Days", legend=T, x.leg=0, y.leg=600)

a10 <- aov(log(Duration) ~ Area, data = data2, na.action=na.fail) 
summary(a10) # no dif sig en la duracion del evento toxico segun el area 

k10<-kruskal.test(Duration ~ Area, data = data2, na.action=na.fail) # p-value = 0.6095 no dif sig en la duracion del evento toxico segun el area

layout(matrix(c(1:6), 2, 3)) 
plot(a10, 1:6) # residuales parecen mejorar cn el log
layout(1)

a11 <- aov(Duration ~ year, data = data2, na.action=na.fail) 
summary(a11) # no dif sig en la duracion del evento toxico segun el anio 

layout(matrix(c(1:6), 2, 3)) 
plot(a11, 1:6) # horribles los residuales 
layout(1)

k12<-kruskal.test(Duration ~ Organism, data = data2, na.action=na.fail) # p-value = 0.2295 no dif sig en la duracion del evento toxico segun el organism


# solo BBE (para ver si hay diferencias entre cholga y mejillon en esa area de cultivo q presenta las 2 especies juntas)

BBE <- data2 %>% filter(Area == "BBE" )
str(BBE)


a11 <- aov(log(Duration) ~ Organism, data = BBE, na.action=na.fail) 
summary(a11) # No dif sig en el tiempo de detoxificacion para cholgas y mejillones en BBE 

layout(matrix(c(1:6), 2, 3)) # residuales feos
plot(a11, 1:6) 
layout(1)

k11<-kruskal.test(Duration ~ Organism, data = BBE, na.action=na.fail) #p-value = 0.412 No dif sig


##HASTA ACA OK#############

library(readxl)

data <- totalR
data <- read_excel("Data/totalR.xlsx")
ENSO <- read_excel("Data/ENSO_MEI1990-2020.xlsx")
SAM  <- read_excel("Data/SAm.xlsx")



























# MODELOS LINEALES GENERALIZADOS 

GLM1<-glm(log(Duration) ~ STXmax,family="Gamma",data= data2,na.action=na.fail)
summary(GLM1) # Null deviance: 10.402  on 50  degrees of freedom, Residual deviance:  6.709  on 49  degrees of freedom
plot(data2$Duration, fitted (GLM1), pch=22, col="red", bg="red", xlab= "Duration of toxic event (Days)") #AIC: 146.4

GLM2<-glm(log(Duration) ~ STXmax + Season,data= data2, family="Gamma",na.action=na.fail)
summary(GLM2) # Null deviance: 10.4019  on 50  degrees of freedom, Residual deviance:  4.9871  on 46  degrees of freedom
plot(data2$Duration, fitted (GLM2), pch=22, col="red", bg="red", xlab= "Duration of toxic event (Days)") # AIC: 136.98


GLM3<-glm(log(Duration) ~ STXmax+Season+Organism,data= data2, family="Gamma",na.action=na.fail)
summary(GLM3) #AIC: 137.11
plot(data2$Duration, fitted (GLM3), pch=22, col="red", bg="red", xlab= "Duration of toxic event (Days)") # Null deviance: 10.4019  on 50  degrees of freedom, Residual deviance:  4.8095  on 45  degrees of freedom

GLM1<-glm(Duration ~ STXmax,family=Gamma(link="log"),data= data2,na.action=na.fail)
summary(GLM1) # Null deviance: 10.402  on 50  degrees of freedom, Residual deviance:  6.709  on 49  degrees of freedom
plot(data2$Duration, fitted (GLM1), pch=22, col="red", bg="red", xlab= "Duration of toxic event (Days)") #AIC: 146.4

##Datos totales (Stx vs Area, Season, Organism)

data <- read_excel("Data/totalR-STXpositiva.xlsx")
names(data)

library(mgcv)

GLM5<-glm (STX  ~ Area+season+Organism+Year,family="Gamma"(link="log"),data= data,na.action=na.fail)
summary(GLM5) 
drop1(GLM5, test="F")

GLM6<-glm (STX  ~ Area+season+Organism,family="Gamma"(link="log"),data= data,na.action=na.fail)
summary(GLM6) 
drop1(GLM6, test="F")
anova(GLM6)

















##-- GAMS exploratorios---------------------------------------------------################################

# 1) sitio BBE, BBF, PP para mejillon (archivo Medulis)
# 2) sitio BBB y BBE para cholga (archivo Aater)
# 3) BBE para comparar mejillon y cholga 

library(readxl)
library(nlme)

#1)
Medulis <- read_excel("Data/Medulis.xlsx")
names(Medulis)
str(Medulis)

Medulis$STX <- Medulis$STX + 0.001
Medulis$STX
Medulis$Area<-as.factor(Medulis$Area)

Medulis$Date<-as.Date(Medulis$Date)


# generar variable Time 

# Time variable: created which we'll use for the trend or between-year variable; I scale by 1000 as discussed above
Medulis <- transform(Medulis, Time = as.numeric(Date) / 1000)

#Graficos 
require(ggplot2)
ggplot(Medulis, aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ Organism,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(Medulis , aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ season,scale="free_y") + geom_smooth(se=FALSE) 




library(mgcv)
require(gratia)
 
# uso STx transformada en r directamente (STX + 0.0001)
# uso variable time o tengo que convertir date en variable numerica para que funcione el modelo sino me tira error
# Error in names(dat) <- object$term : 'names' attribute [1] must be the same length as the vector [0]

M1 <- gam(Medulis$STX ~ s(Time) + Area, data=Medulis,family=Gamma (link="log"), method = "REML") 
plot.gam(M1,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(M1)  
draw(M1, residuals = TRUE)
appraise(M1)


M2 <- gam(Medulis$STX ~ s(Time, by=Area) + Area, data=Medulis,family=Gamma (link="log"), method = "REML") 
plot.gam(M2,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(M2)  
draw(M2, residuals = TRUE)
appraise(M2)


# En realidad el modelo no está funcionando para nada por eso te da ese resultado de autocorrelacion
# No te funciona porque la fecha sola no alcanza para predecir la concentracion ademas de que es algo estacional
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/


# Grafico de Autocorrelacion para probar si hay independencia

E <- residuals(M1)
if( any(is.na(E)) ) 
  print("No hay NA!")

I1 <- !is.na(Medulis$STX)
Efull <- vector(length = length(Medulis$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")


# Incluyo autocorrelacion AR-1 

M1A<- gam(Medulis$STX ~ s(Time) + Area, na.action = na.omit, data = Medulis,family=Gamma (link="log"), correlation = corAR1(form =~ Time))
plot.gam(M1A,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1A, residuals = TRUE)
appraise(M1A)
summary(M1A)


M1B<- gam(Medulis$STX ~ s(Time, by= Area), na.action = na.omit, data = Medulis,family=Gamma (link="log"), correlation = corAR1(form =~ Time))
plot.gam(M1B,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1B, residuals = TRUE)
appraise(M1B)
summary(M1B)

M2 <- gam(Medulis$STX ~ s(Time, by=Area) + Area, data=Medulis,family=Gamma (link="log"), method = "REML", correlation = corAR1(form =~ Time)) 
plot.gam(M2,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(M2)  
draw(M2, residuals = TRUE)
appraise(M2)

# Cor ARMA
M1C<- gam(Medulis$STX ~ s(Time) + Area, na.action = na.omit, data = Medulis,family=Gamma (link="log"), correlation = corARMA(value=c(0.2,-0.2),form =~ Date | Area, p=2, q=0))
summary(M1C)
plot.gam(M1C,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1C, residuals = TRUE)
appraise(M1C)
summary(M1C)


# corARMA form argument is used to tell R that the order of the data is determined by the variable Date/ time/Year

AIC(M1,M1A,M1B,M1C) # M1A, M1C los mejorcitos pero feo el ajuste      


#### MODELOS JERARQUICOS  ######

# smooth.terms {mgcv}
# Factor smooth interactions
# bs="fs" Smooth factor interactions are often produced using by variables (see gam.models), but a special smoother class (see factor.smooth.interaction) is available for the case in which a smooth is required at each of a large number of factor levels (for example a smooth for each patient in a study), and each smooth should have the same smoothing parameter. 

## GS: A single common smoother plus group-level smoothers that have the same wiggliness

#1)

names(Medulis)
str(Medulis)

#CO2_modGS <- gam(log(uptake) ~  s(log(conc), k=5, m=2) + s(log(conc), Plant_uo, k=5, bs="fs", m=2), data=CO2, method="REML")


M1AGS <- gam(Medulis$STX ~ s(Time, k=5, m=2) + s(Time, Area,k=5, bs="fs", m=2), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")


M1AGS <- gam(Medulis$STX ~ s(Time, m=2) + s(Time, Area, bs="fs", m=2), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")


# luego agregar correlacion = corAR1(form =~ Date))

plot.gam(M1AGS,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1AGS, residuals = TRUE)
appraise(M1AGS)
summary(M1AGS) ##Warning message: In gam.side(sm, X, tol = .Machine$double.eps^0.5) :
               #le modle a des lissages 1-d rpts des mmes variables

#zoo_daph_modGS <- gam(density_adj ~  s(day, bs="cc", k=10) +
                      #  s(day, lake, k=10, bs="fs", xt=list(bs="cc")) +
                       # s(lake, year_f, bs="re"),
                     # data=daphnia_train, knots=list(day=c(0, 365)),
                     # family=Gamma(link="log"), method="REML",
                     # drop.unused.levels=FALSE)

M2AGS <- gam(Medulis$STX ~ s(Day, bs="cc", k=10) + s(Day, Area,k=10, bs="fs", xt=list(bs="cc"))+ s(Area, Year, bs="re"),na.action = na.omit,data = Medulis, knots=list(Day=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(M2AGS, residuals = TRUE)
appraise(M2AGS)
summary(M2AGS)

names(Medulis)
str(Medulis)
# year-F tiene q ser factor
Medulis$Year<-as.factor(Medulis$Year)


## GI A single common smoother plus group-level smoothers with differing wiggliness (Model GI)

#CO2_modGI <- gam(log(uptake) ~ s(log(conc), k=5, m=2, bs="tp") + s(log(conc), by=Plant_uo, k=5, m=1, bs="tp") + s(Plant_uo, bs="re", k=12), data=CO2, method="REML")


M1AGI <- gam(Medulis$STX ~ s(Time, m=2, bs="tp") + s(Time, by=Area, m=1, bs="tp") + s(Area, bs="re"), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")
draw(M1AGI, residuals = TRUE) ## Error in check_is_mgcv_smooth(smooth) : Object passed to 'smooth' is not a 'mgcv.smooth'.
appraise(M1AGI)   
summary(M1AGI)
gam.check(M1AGI) 


# Me tira errores, habra que Cambiar los parametros m y bs??


# MODELAR VARIABILIDAD ESTACIONAL 
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/

#1)any trend or long term change in the level of the time series: (x2) between year times
# to use the date of observation converted to a numeric variable for my between year data, x2 (time)
#2)any seasonal or within-year variation: (x1) within-year 
#you could use the month of observation as a decimal value, which is particularly useful if you only have monthly or less frequent data
#For more frequent observations I use the day of the year as my time variable. This information is also easily derived from a Date variable using the "%j" date format

# (x1) convierto las variables meses y dias en numericas para modelar seasonal or within-year variation: (x1) 

Medulis$Month<-as.numeric(Medulis$Month)
Medulis$Month 
Medulis$Day<-as.numeric(Medulis$Day)
Medulis$Year<-as.numeric(Medulis$Year)


str(Medulis)

# generar variable Time 

## (x2)Add in a Time variable: created which we'll use for the trend or between-year variable; I scale by 1000 as discussed above
Medulis <- transform(Medulis, Time = as.numeric(Date) / 1000)

# Uso month como (x1) variable seasonal or within-year variation
Ms <- gam(Medulis$STX ~ s(Month, bs = "cc", k = 12) + s(Time),data = Medulis, family=Gamma (link="log"), method="REML")
draw(Ms, residuals = TRUE) #bs basis type for the smooth term; "cc" cyclic cubic spline, which we want for the seasonal term as there should be no discontinuity between January and December
appraise(Ms)
summary(Ms)
gam.check(Ms) 

# uso Day como x1 variable seasonal or within-year variation
Ms <- gam(Medulis$STX ~ s(Day, bs = "cc", k = 12) + s(Time),data = Medulis, family=Gamma (link="log"), method="REML")
draw(Ms, residuals = TRUE) #bs basis type for the smooth term; "cc" cyclic cubic spline, which we want for the seasonal term as there should be no discontinuity between January and December
appraise(Ms) 
summary(Ms) # Day es no significativo 
gam.check(Ms) 


#grafico Autocorrelacion
E <- residuals(Ms)
I1 <- !is.na(Medulis$STX)
Efull <- vector(length = length(Medulis$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

# agrego autocorrelacion
# corARMA(form = ~ 1|Year, p = x) fit an ARMA process to the residual
# where p indicates the order for the AR part of the ARMA model
# form = ~ 1|Year means that the ARMA is nested within each year

## AR(1)
 
Ms1 <- gam(Medulis$STX ~ s(Month, bs = "cc", k = 12) + s(Time),data = Medulis, family=Gamma (link="log"), method="REML",correlation = corARMA(form = ~ 1|Year, p = 1))
draw(Ms1, residuals = TRUE)
plot.gam(Ms1,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(Ms1)
summary(Ms1)
gam.check(Ms1) 

## AR(2)
Ms2 <- gam(Medulis$STX ~ s(Month, bs = "cc", k = 12) + s(Time),data = Medulis, family=Gamma (link="log"), method="REML",correlation = corARMA(form = ~ 1|Year, p = 2))
draw(Ms2, residuals = TRUE)
plot.gam(Ms2,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(Ms2)
summary(Ms2)
gam.check(Ms2)

## AR(3)
Ms3 <- gam(Medulis$STX ~ s(Month, bs = "cc", k = 12) + s(Time),data = Medulis, family=Gamma (link="log"), method="REML",correlation = corARMA(form = ~ 1|Year, p = 3))
draw(Ms3, residuals = TRUE)
plot.gam(Ms3,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(Ms3)
summary(Ms3)
gam.check(Ms3)




## SEASONAL + JERARQUICO

# GS (Time y Month anidados en Area)

MsGS1 <- gam(Medulis$STX ~ s(Month, bs = "cc", k = 12) + s(Time) + s(Month, Area,bs="fs", m=2) + s(Time,Area,bs="fs", m=2),data = Medulis, family=Gamma (link="log"), method="REML",correlation = corARMA(form = ~ 1|Year, p = 1))
draw(MsGS1, residuals = TRUE)
appraise(MsGS1)
summary(MsGS1)
gam.check(MsGS1) 


# GI
MsGI1 <- gam(Medulis$STX ~ s(Month, bs = "cc", k = 12) + s(Time) + s(Month, by= Area,m=1, bs="tp") + s(Time, by= Area, m=1, bs="tp") + s(Area, bs="re"),data = Medulis, family=Gamma (link="log"), method="REML",correlation = corARMA(form = ~ 1|Year, p = 1))
draw(MsGI1, residuals = TRUE)
appraise(MsGI1)
summary(MsGI1)
gam.check(MsGI1) # Error in check_is_mgcv_smooth(smooth) :Object passed to 'smooth' is not a 'mgcv.smooth'



# probar esta correlation correlation = corCompSymm(form =??? Year)) pag 148 Zhur
# correlation argument corAR1: form argument is now essential as R needs to know position of the observations over time.
# ver pag 151 Zhur
# Note that the auto-correlation function in Fig. 6.3 becomes positive again for larger time lags. 
# This suggests that an error structure that allows for a sinusoidal pattern may be more appropriate.

# correlation = corAR1(form =??? Time | ID ) PROBAR ESTO CON ID correlation is applied at the deepest level
# The correlation between residuals of different time series is assumed to be 0.
# correlation is applied at the deepest level: Observations of the same time series. This means that all time series have the same ??.
# The form option specifies that the temporal order of the data is specified by the variable Time, and the time series are nested.
# The auto-correlation is therefore applied at the deepest level (on each individual time series), and we get one ?? for all four time series.






#---------------------------------------------------------------------------#

# 2) cholga   
Aater <- read_excel("Data/Aater.xlsx")
names(Aater)
str(Aater)

require(ggplot2)
ggplot(Aater, aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ Organism,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(Aater , aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ season,scale="free_y") + geom_smooth(se=FALSE) 

Aater$Area<-as.factor(Aater$Area)

Aater$STX <- Aater$STX + 0.001
Aater$Date <- as.numeric(Aater$Date)


M2 <- gam(Aater$STX  ~ s(Date) + Area, data = Aater,family=Gamma (link="log"), method = "REML") 
plot.gam(M2,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M2, residuals = TRUE)
appraise(M2)
summary(M2) #Deviance explained = 28%

M2A <- gam(Aater$STX  ~ s(Date, by= Area), data = Aater,family=Gamma (link="log"), method = "REML") 
plot.gam(M2A,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M2A, residuals = TRUE)
appraise(M2A)
summary(M2A) # Deviance explained = 42.4% mejora el modelo y los residuales 


# grafico Autocorrelacion
E <- residuals(M2A)
I1 <- !is.na(Aater$STX)
Efull <- vector(length = length(Aater$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals") # distribucion senoidal 

# Incluyo autocorrelacion AR-1 

M2B<- gam(Aater$STX  ~ s(Date, by= Area), na.action = na.omit, data = Aater, family=Gamma (link="log"), correlation = corARMA(form =~ Date))
plot.gam(M2B,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M2B, residuals = TRUE)
appraise(M2B)
summary(M2B) # Deviance explained =   44% pero no me toma la autocorrelacion 


AIC(M2,M2A,M2B) # M2B el mejorcito 



# 3) BBE
BBE <- read_excel("Data/BBE-R.xlsx")
names(BBE)
str(BBE)

ggplot(BBE, aes(x = Date, y = STX,color=organism)) + xlab("") + geom_line() + facet_grid( Area ~ Organism,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(BBE , aes(x = Date, y = STX,color=organism)) + xlab("") + geom_line() + facet_grid( Area ~ season,scale="free_y") + geom_smooth(se=FALSE) 

Aater$Area<-as.factor(Aater$Area)


BBE$STX <- BBE$STX + 0.001
Aater$organism<-as.factor(Aater$organism)

M3 <- gam(BBE$STX ~ s(Date) + organism, data = BBE,family=Gamma (link="log"), method = "REML") 
plot.gam(M3,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M3, residuals = TRUE)
appraise(M3)
summary(M3)


M3A <- gam(BBE$STX ~ s(Date, by= organism), data = BBE,family=Gamma (link="log"), method = "REML") 
plot.gam(M3A,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M3A, residuals = TRUE)
appraise(M3A)
summary(M3A)


#grafico Autocorrelacion
E <- residuals(M3A)
I1 <- !is.na(BBE$STX)
Efull <- vector(length = length(BBE$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

#Incluyo autocorrelacion AR-1 

M3B <- gam(BBE$STX ~ s(Date, by= organism), data = BBE,family=Gamma (link="log"), method = "REML", correlation = corARMA(form =~ Date)) 
plot.gam(M3B,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M3B, residuals = TRUE)
appraise(M3B)
summary(M3B)

AIC(M3,M3A,M3B)



# luego agregar correlacion # correlation = corAR1(form =~ Date))

#zoo_daph_modGI <- gam(density_adj???s(day, bs="cc", k=10) +s(lake, bs="re") +
                       # s(day, by=lake, k=10, bs="cc") +
                       # s(lake, year_f, bs="re"),
                     # data=daphnia_train, knots=list(day=c(0, 365)),
                     # family=Gamma(link ="log"), method="REML",
                     # drop.unused.levels=FALSE)

# k.check(test) EDF< k

# ciclic smoothers seasonal data
# knots: need to specify start and end points for our cycles
#  specify this smoother type as a factor-smooth interaction term using the xt

## #. Note that we also include a random
# smoother for both taxon and taxon:year_f, where year_f is year transformed
#into a factor variable. This deals with the fact that average zooplankton densities can
#show large year-to-year variation.

# In GAMs, the bias-variance trade-off is managed by the terms of the penalty matrix, and
# equivalently random effect variances in HGLMs.