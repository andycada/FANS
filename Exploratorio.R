
#------Exploratorio Datos Toxinas TPM ------------------------------------------------#


library(readxl)

data <- read_excel("Data/totalRsinbajo.xlsx")
names(data)
str(data)

data$STX <- data$STX + 0.001

  
# Area: BBE (Bahia Brown Entrada), BBF(B Broww Fondo), PP (Punta Parana)


# graficos sin incluir  la zona menos muestreada (BBB) -------------------------#

library(tidyverse)
theme_set(theme_bw())

d_sorted <- data %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ggplot(d_sorted, aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ Organism,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(d_sorted %>% filter(Organism=="M. edulis"), aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ season,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(d_sorted, aes(x = Area, y = STX,color=Area)) +
  coord_flip() + xlab("") + geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2)  + theme_bw()


require(fitdistrplus)
descdist(data$STX, boot=1000)


# STX vs Area, Season 

library(sciplot) 
bargraph.CI(Area,STX, data = data, ylim= c(0, 200), ylab = "?g STX eq/100 g tissue", xlab = "Season", col = "steelblue4")


bargraph.CI(season,STX, data = data, ylim= c(0, 700), ylab = "?g STX eq/100 g tissue", xlab = "Season", col = "steelblue4")



# ANOVAS
a1 <- aov(STX ~ Area, data = data, na.action=na.fail) 
summary(a1) 
TukeyHSD(a1,"Area") # dif sig en el area (BBE> PP> BBF)

# analisis de residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a1, 1:6) # no parece mejorar cn transformacion log 
layout(1)

# Prueba de Kruskal-Wallis para dos o m?s muestras independientes
#no-parametrica, no asume normalidad pero si homocedasticidad

k1<-kruskal.test(STX ~ Area, data = data, na.action=na.fail)
k1 #p-value < 2.2e-16

a2 <- aov(STX ~ season, data = data, na.action=na.fail) 
summary(a2)
TukeyHSD(a2,"season") # dif sig en la estacion  (summer> autumn> spring> winter)

# analisis de residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a2, 1:6) # no parece mejorar cn transformacion log
layout(1)

k2<-kruskal.test(STX ~ season, data = data, na.action=na.fail)
k2 #p-value < 2.2e-16


library(sciplot)
bargraph.CI(season, STX, Area, data = data,  xlab = "Season", ylab = "?g STX eq/100 g tissue", legend = TRUE)

bargraph.CI(Area, STX, season, data = data,  xlab = "Area", ylab = "?g STX eq/100 g tissue", legend = TRUE)


a3 <- aov(log(STX + 1) ~ Area*season, data = data) 
summary(a3)

TukeyHSD(a3,"Area:season")

# analisis de residuales

layout(matrix(c(1:6), 2, 3)) 
plot(a3, 1:6) 
layout(1) CAL ~ SEXE + HORM + SEXE:HORM

k3<-kruskal.test(STX ~ Area : season, data = data, na.action=na.fail)
k1 #p-value < 2.2e-16

# STX vs organism 

bargraph.CI(Organism, STX, data = data,  xlab = "Season", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a4 <- aov(log(STX +1)  ~ Organism, data = data) 
summary(a4) 

# residuales 
layout(matrix(c(1:6), 2, 3)) 
plot(a4, 1:6) # residuales horribles
layout(1)

t.test(STX ~ Organism, data = data, na.action=na.fail)# Welch Two Sample t-test, p-value = 0.01058 


#-----------------------------------------------#

## Analizo los datos incluyendo la zona menos muestreada (BBB), N muy diferente para las 4 estaciones  ----------------------------------------------------------------
# BBB: Bahia Brown Bajo

data <- read_excel("Data/totalR.xlsx")
names(data)
str(data)

ggplot(data, aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ Organism,scale="free_y") 

# STX vs Area 

library(sciplot) 

bargraph.CI(Area,STX, data = data, ylim= c(0, 200), ylab = "?g STX eq/100 g tissue", xlab = "Area")

a1 <- aov(STX ~ Area, data = data, na.action=na.fail) 
summary(a1)
TukeyHSD(a1,"Area") # STX es sig diferente en cada area de muestreo (BBE>PP>BBF>BBB)

# residuales
layout(matrix(c(1:6), 2, 3))  
plot(a1, 1:6) 
layout(1)

STX<- data$STX
Area<-data$Area

#Normalidad
shapiro.test(STX) #p-value < 2.2e-16
shapiro.test(log(STX + 1)) #p-value < 2.2e-16

# homogeneidad de varianzas 
bartlett.test(STX ~ Area, data = data, na.action=na.fail) #p-value < 2.2e-16

# homogeneidad de varianzas (No parametrico)
fligner.test(STX ~ Area, data = data, na.action=na.fail)

k1<-kruskal.test(STX ~ Area, data = data, na.action=na.fail)
k1 #p-value < 2.2e-16




# STX vs Season

bargraph.CI(season,STX, data = data, ylim= c(0, 400), ylab = "?g STX eq/100 g tissue", xlab = "Season")

a2 <- aov(log(STX + 1) ~ season, data = data, na.action=na.fail) 
summary(a2)
TukeyHSD(a2,"season") # STX es sig diferente en cada estacion del anio (summer> autumn> spring> winter)


#residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a2, 1:6) 
layout(1)


# homogeneidad de varianzas 
bartlett.test(STX ~ season, data = data, na.action=na.fail) #p-value < 2.2e-16

# homogeneidad de varianzas (No parametrico)
fligner.test(STX ~ season, data = data, na.action=na.fail)


k2<-kruskal.test(STX ~ season, data = data, na.action=na.fail)
k2 #p-value < 2.2e-16

layout(matrix(c(1:6), 2, 3)) 
plot(a2, 1:6) 
layout(1)




# anova factorial a dos factores cruzados (area y season)

library(sciplot)
bargraph.CI(season, STX, Area, data = data,  xlab = "Season", ylab = "?g STX eq/100 g tissue", col = c("steelblue1", "steelblue4"), legend = TRUE)

bargraph.CI(Area, STX, season, data = data, ylim =c(0,800), xlab = "Area", ylab = "?g STX eq/100 g tissue", legend = TRUE, x.leg=0, y.leg=850, ncol=1)


a3 <- aov(log(STX + 1) ~ Area*season, data = data) 
summary(a3)

TukeyHSD(a3,"Area:season")

# residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a3, 1:6) 
layout(1)


k3<-kruskal.test(STX ~ Area*season, data = data, na.action=na.fail)
k3 #p-value < 2.2e-16


# STX vs organism ---------------------

bargraph.CI(Organism,STX, data = data, ylab = "?g STX eq/100 g tissue", xlab = "Organism")

a4 <- aov(STX ~ Organism, data = data) 
summary(a4) #  no dif sig en la toxicidad en relacion al organismo, dats muy heterogneos

# residuales 
layout(matrix(c(1:6), 2, 3)) #residuales horribles incluso cn transformacion
plot(a4, 1:6) 
layout(1)

t.test(STX ~ Organism, data = data, na.action=na.fail)# p-value = 0.1607

bargraph.CI(season, STX, Organism, data = data,  xlab = "Season", ylab = "?g STX eq/100 g tissue", legend = TRUE, x.leg=0.5, y.leg=500, ncol=1)

bargraph.CI(Area, STX, Organismo, data = data,  xlab = "Area", ylab = "?g STX eq/100 g tissue", legend = TRUE)


## solo datos de Mejillones 

M <- read_excel("Data/mejillonR.xlsx")
str(M)

bargraph.CI(Area, STX, data = M, xlab = "Area", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a6 <- aov(STX ~ Area, data = M) # niveles de STX son sifnificativamente diferentes enlas 3 zonas q tienen mejillon 
summary(a6)

TukeyHSD(a6,"Area") #dif sig en los mejillones (BBE> PP> BBF)

layout(matrix(c(1:6), 2, 3)) 
plot(a6, 1:6) 
layout(1)

k6<-kruskal.test(STX ~ Area, data = M, na.action=na.fail)
k6 # p-value = 2.721e-08


#solo datos de cholga 

C <- read_excel("Data/cholgaR.xlsx")
str(C)

bargraph.CI(Area, STX, data = C,  xlab = "Area", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a7 <- aov(STX ~ Area, data = C) # niveles de STX son sig diferentes en las 2 zonas q tienen cholga (BBE> BBB)
summary(a7)

layout(matrix(c(1:6), 2, 3)) 
plot(a7, 1:6) 
layout(1)

k7<-kruskal.test(STX ~ Area, data = C, na.action=na.fail)
k7 # p-value = 1.761e-12


# solo datos de BBE para comparar cholga vs mejillon 

BBE <- read_excel("Data/BBE-R.xlsx")
str(BBE)

bargraph.CI(organism, STX, data = BBE,  xlab = "Organism", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a8 <- aov(STX ~ organism, data = BBE) 
summary(a8) # no hay dif en los valores de STX en el producto en la BBE

layout(matrix(c(1:6), 2, 3)) 
plot(a8, 1:6) 
layout(1)

t.test(STX ~ organism, data = BBE, na.action=na.fail)# p-value = 0.9054

## analisis de STX vs organimsmo por season (x separado)---------------#

# winter
W <- read_excel("Data/winterR.xlsx")
str(W)

bargraph.CI(organism, STX, data = W, xlab = "Winter", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a0 <- aov(STX ~ organism, data = W) #  significativos
summary(a0)

a0 <- aov(log (STX + 1) ~ organism, data = W) #  significativos
summary(a0)


layout(matrix(c(1:6), 2, 3)) # residuales horrible, no normalidad
plot(a0, 1:6) 
layout(1)

t.test(STX ~ organism, data = W, na.action=na.fail)# p-value = 0.01044


# autumn
A <- read_excel("Data/autumnR.xlsx")
str(A)

bargraph.CI(organism, STX, data = A, xlab = "Autumn", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a1 <- aov(log(STX + 1) ~ organism, data = A) #  significativos
summary(a1)

a1 <- aov(STX ~ organism, data = A) #  significativos
summary(a1)

layout(matrix(c(1:6), 2, 3)) # residuales horribles
plot(a1, 1:6) 
layout(1)
  
t.test(STX ~ organism, data = A, na.action=na.fail)# p-value = 0.01064


# summer

S <- read_excel("Data/summerR.xlsx")
str(S)

bargraph.CI(organism, STX, data = S, xlab = "Summer", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a2 <- aov(STX  ~ organism, data = S) # no significativo
summary(a2)

a2 <- aov(log(STX + 1)  ~ organism, data = S) # no significativo
summary(a2)

layout(matrix(c(1:6), 2, 3)) 
plot(a2, 1:6) 
layout(1)

t.test(STX ~ organism, data = S, na.action=na.fail)# p-value = 0.25

# spring

SP <- read_excel("Data/springR.xlsx")
str(SP)

bargraph.CI(organism, STX, data = SP, xlab = "Spring", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a3 <- aov(STX ~ organism, data = SP) # no significativo
summary(a3)

a3 <- aov(log(STX +1) ~ organism, data = SP) # no significativo
summary(a3)

layout(matrix(c(1:6), 2, 3)) 
plot(a3, 1:6) 
layout(1)


t.test(STX ~ organism, data = SP, na.action=na.fail)# p-value = 0.058

# dif entre medias anuales ---------------------------------#
bargraph.CI(Year, STX, data = data,  xlab = "Year", ylab = "?g STX eq/100 g tissue", legend = TRUE)

a6 <- aov(STX  ~ Year, data = data) #  significativos
summary(a6)

TukeyHSD(a6,"Year")

layout(matrix(c(1:6), 2, 3)) 
plot(a6, 1:6) 
layout(1)


# dif entre medias anuales por area
bargraph.CI(Year, STX, Area, data = data,  xlab = "Year", ylab = "?g STX eq/100 g tissue", legend = TRUE, x.leg=0, y.leg=700)

a7 <- aov(STX ~ Year*Area, data = data) # los 3 significativos
summary(a7)

TukeyHSD(a7,"Year:Area")

layout(matrix(c(1:6), 2, 3)) 
plot(a7, 1:6) 
layout(1)



### Eventos toxicos y duracion de veda ----------------------------------------------------------#
# Analisis de picos: 
# STXmax: concentracion maxima alcanzada duante el evento toxico
# Duration: duracion de la veda, T desde q el mejillon supera los 80 ?g STX/100 g tejido, hasta que tiene valores por debajo de ese umbral

library(readxl)
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

bargraph.CI(Duration,STXmax,Area, data = data2, ylab = "?g STX eq/100 g tissue", xlab = "Days", legend=T, x.leg=0, y.leg=600)

a10 <- aov(log(Duration) ~ Area, data = data2, na.action=na.fail) 
summary(a10) # no dif sig en la duracion del evento toxico segun el area 

layout(matrix(c(1:6), 2, 3)) 
plot(a10, 1:6) # residuales parecen mejorar cn el log
layout(1)

a11 <- aov(Duration ~ year, data = data2, na.action=na.fail) 
summary(a11) # no dif sig en la duracion del evento toxico segun el anio 

layout(matrix(c(1:6), 2, 3)) 
plot(a11, 1:6) # horribles los residuales 
layout(1)


# solo BBE (para ver si hay diferencias entre cholga y mejillon en esa area de cultivo q presenta las 2 especies juntas)

library(readxl)
BBE2 <- read_excel("Data/eventos toxicos y duracion solo BBE.xlsx")

a11 <- aov(log(Duration) ~ Organism, data = BBE2, na.action=na.fail) 
summary(a11) # No dif sig en el tiempo de detoxificacion para cholgas y mejillones en BBE 

layout(matrix(c(1:6), 2, 3)) # residuales feos
plot(a11, 1:6) 
layout(1)

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


##-- GAMS exploratorios---------------------------------------------------#####

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

#
# FIJATE LO QUE DA EL GRAFICO!!!!!!!!!!!!!!!! 
# Las fechas las toma mal
#
require(ggplot2)
ggplot(Medulis, aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ Organism,scale="free_y") + geom_smooth(se=FALSE) 

ggplot(Medulis , aes(x = Date, y = STX,color=Area)) + xlab("") + geom_line() + facet_grid( Area ~ season,scale="free_y") + geom_smooth(se=FALSE) 


library(mgcv)
require(gratia)

Medulis$Date<-as.numeric(Medulis$Date)
Medulis$Date

# con STx transformada en r directamente 

M1 <- gam(Medulis$STX ~ s(Date) + Area, data=Medulis,family=Gamma (link="log"), method = "REML") 
plot.gam(M1,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(M1)  
draw(M1, residuals = TRUE)
appraise(M1)


#
# En realidad el modelo no está funcionando para nada por eso te da ese resultado de autocorrelacion
# No te funciona porque la fecha sola no alcanza para predecir la concentracion ademas de que es algo estacional
# ademas no está leyendo bien la fecha del excel.
#
# Mirar lo de modelos jerarquicos
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6542350/
#
# Quizas mirar 
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/




# Grafico de Autocorrelacion para probar si hay independencia

E <- residuals(M1)
if( any(is.na(E)) ) 
  print("No hay NA!")

I1 <- !is.na(Medulis$STX0001)
Efull <- vector(length = length(Medulis$STX0001))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")



# Incluyo autocorrelacion AR-1 

M1A<- gam(Medulis$STX ~ s(Date) + Area, na.action = na.omit, data = Medulis,family=Gamma (link="log"), correlation = corAR1(form =~ Date))
plot.gam(M1A,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1A, residuals = TRUE)
appraise(M1A)
summary(M1A)

M1B<- gam(Medulis$STX ~ s(Date, by= Area), na.action = na.omit, data = Medulis,family=Gamma (link="log"), correlation = corAR1(form =~ Date))
plot.gam(M1B,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1B, residuals = TRUE)
appraise(M1B)
summary(M1B)


# Cor ARMA
M1C<- gam(Medulis$STX ~ s(Date) + Area, na.action = na.omit, data = Medulis,family=Gamma (link="log"), correlation = corARMA(value=c(0.2,-0.2),form =~ Date | Area, p=2, q=0))
summary(M1C)
plot.gam(M1C,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1C, residuals = TRUE)
appraise(M1C)
summary(M1C)


# form argument within this argument is used to tell R that the order of the data is determined by the variable Date

AIC(M1,M1A,M1B,M1C) # M1A, M1C los mejorcitos pero feo el ajuste      

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

## Modelos jerarquico
## smooth.terms {mgcv}
# Factor smooth interactions
# bs="fs" Smooth factor interactions are often produced using by variables (see gam.models), but a special smoother class (see factor.smooth.interaction) is available for the case in which a smooth is required at each of a large number of factor levels (for example a smooth for each patient in a study), and each smooth should have the same smoothing parameter. 

## GS: A single common smoother plus group-level smoothers that have the same wiggliness

#1)
Medulis <- read_excel("Data/Medulis.xlsx")
names(Medulis)
str(Medulis)

Medulis$STX <- Medulis$STX + 0.001
Medulis$STX
Medulis$Area<-as.factor(Medulis$Area)
Medulis$Date<-as.numeric(Medulis$Date)

#CO2_modGS <- gam(log(uptake) ??? s(log(conc), k=5, m=2) + s(log(conc), Plant_uo, k=5, bs="fs", m=2), data=CO2, method="REML")


M1AGS <- gam(Medulis$STX ~ s(Date, k=5, m=2) + s(Date, Area,k=5, bs="fs", m=2), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")

# luego agregar correlacion 
# correlation = corAR1(form =~ Date))

plot.gam(M1AGS,xlab= "Date",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1AGS, residuals = TRUE)
appraise(M1AGS)
summary(M1AGS) ##Warning message: In gam.side(sm, X, tol = .Machine$double.eps^0.5) :
               #le modle a des lissages 1-d rpts des mmes variables

## Model GS
#zoo_daph_modGS <- gam(density_adj ??? s(day, bs="cc", k=10) +
                      #  s(day, lake, k=10, bs="fs", xt=list(bs="cc")) +
                       # s(lake, year_f, bs="re"),
                     # data=daphnia_train, knots=list(day=c(0, 365)),
                     # family=Gamma(link="log"), method="REML",
                     # drop.unused.levels=FALSE)

M1BGS <- gam(Medulis$STX ~ s(Date, k=5, m=2) + s(Date, Area,k=5, bs="fs", m=2), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")



## GI A single common smoother plus group-level smoothers with differing wiggliness (Model GI)

#CO2_modGI <- gam(log(uptake) ??? s(log(conc), k=5, m=2, bs="tp") + s(log(conc), by=Plant_uo, k=5, m=1, bs="tp") + s(Plant_uo, bs="re", k=12), data=CO2, method="REML")


M1AGI <- gam(Medulis$STX ~ s(Date, m=2, bs="tp") + s(Date, by=Area, m=1, bs="tp") + s(Area, bs="re"), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")
draw(M1AGI, residuals = TRUE)
appraise(M1AGI)
summary(M1AGI)
gam.check(M1AGI) 

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