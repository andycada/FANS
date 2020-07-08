
#------Exploratorio Datos Toxinas TPM ------------------------------------------------#
#
library(readxl)

data <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/totalRsinbajo.xlsx")
View(data)
names(data)
str(data)

# Area: BBE (Bahia Brown Entrada), BBF(B Broww Fondo), PP (Punta Parana)


# graficos sin incluir  la zona menos muestreada (BBB) -------------------------#


# STX vs Area, Season 

library(sciplot) 
bargraph.CI(Area,STX, data = data, ylim= c(0, 200), ylab = "µg STX eq/100 g tissue", xlab = "Season", col = "steelblue4")


bargraph.CI(season,STX, data = data, ylim= c(0, 700), ylab = "µg STX eq/100 g tissue", xlab = "Season", col = "steelblue4")

# ANOVAS
a1 <- aov(STX ~ Area, data = data, na.action=na.fail) 
summary(a1) 
TukeyHSD(a1,"Area") # dif sig en el area (BBE> PP> BBF)

# analisis de residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a1, 1:6) # no parece mejorar cn transformacion log 
layout(1)


a2 <- aov(STX ~ season, data = data, na.action=na.fail) 
summary(a2)
TukeyHSD(a2,"season") # dif sig en la estacion  (summer> autumn> spring> winter)

# analisis de residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a2, 1:6) # no parece mejorar cn transformacion log
layout(1)


library(sciplot)
bargraph.CI(season, STX, Area, data = data,  xlab = "Season", ylab = "µg STX eq/100 g tissue", legend = TRUE)

bargraph.CI(Area, STX, season, data = data,  xlab = "Area", ylab = "µg STX eq/100 g tissue", legend = TRUE)


a3 <- aov(log(STX +1) ~ Area*season, data = data) 
summary(a3)

TukeyHSD(a3,"Area:season")

# analisis de residuales

layout(matrix(c(1:6), 2, 3)) 
plot(a3, 1:6) 
layout(1)

# STX vs organism 

bargraph.CI(Organism, STX, data = data,  xlab = "Season", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a4 <- aov(STX  ~ Organism, data = data) 
summary(a4) # no hay dif en los niveles de STX segun el organismo PERO EL N ES MUY DIFERENTE

# residuales 
layout(matrix(c(1:6), 2, 3)) 
plot(a4, 1:6) # residuales horribles
layout(1)

#-----------------------------------------------#

## Analizo los datos incluyendo la zona menos muestreada (BBB), N muy diferente para las 4 estaciones  ----------------------------------------------------------------
# BBB: Bahia Brown Bajo

data <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/totalR.xlsx")
names(data)
str(data)


# STX vs Area 

library(sciplot) 
layout(matrix(c(1:2), 1, 2)) 
bargraph.CI(Area,STX, data = data, ylim= c(0, 200), ylab = "µg STX eq/100 g tissue", xlab = "Area")

a1 <- aov(STX ~ Area, data = data, na.action=na.fail) 
summary(a1)
TukeyHSD(a1,"Area") # STX es sig diferente en cada area de muestreo (BBE>PP>BBF>BBB)

# residuales
layout(matrix(c(1:6), 2, 3))  
plot(a1, 1:6) 
layout(1)

# STX vs Season

bargraph.CI(season,STX, data = data, ylim= c(0, 400), ylab = "µg STX eq/100 g tissue", xlab = "Season")

a2 <- aov(log(STX + 1) ~ season, data = data, na.action=na.fail) 
summary(a2)
TukeyHSD(a2,"season") # STX es sig diferente en cada estacion del anio (summer> autumn> spring> winter)

#residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a2, 1:6) 
layout(1)

# anova factorial a dos factores cruzados (area y season)

library(sciplot)
bargraph.CI(season, STX, Area, data = data,  xlab = "Season", ylab = "µg STX eq/100 g tissue", col = c("steelblue1", "steelblue4"), legend = TRUE)

bargraph.CI(Area, STX, season, data = data, ylim =c(0,800), xlab = "Area", ylab = "µg STX eq/100 g tissue", legend = TRUE, x.leg=0, y.leg=850, ncol=1)


a3 <- aov(STX ~ Area*season, data = data) 
summary(a3)

TukeyHSD(a3,"Area:season")

# residuales
layout(matrix(c(1:6), 2, 3)) 
plot(a3, 1:6) 
layout(1)

# STX vs organism ---------------------

bargraph.CI(Organism,STX, data = data, ylab = "µg STX eq/100 g tissue", xlab = "Season")

a4 <- aov(STX ~ Organism, data = data) 
summary(a4) # no hay df en la toxicidad en relacion al organismo 

# residuales 
layout(matrix(c(1:6), 2, 3)) 
plot(a4, 1:6) 
layout(1)


bargraph.CI(season, STX, Organism, data = data,  xlab = "Season", ylab = "µg STX eq/100 g tissue", legend = TRUE, x.leg=0.5, y.leg=500, ncol=1)

bargraph.CI(Area, STX, Organismo, data = data,  xlab = "Area", ylab = "µg STX eq/100 g tissue", legend = TRUE)




## solo datos de Mejillones 

M <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/MejillonR.xlsx")
str(M)

bargraph.CI(Area, STX, data = M, xlab = "Area", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a6 <- aov(STX ~ Area, data = M) # niveles de STX son sifnificativamente diferentes enlas 3 zonas q tienen mejillon 
summary(a6)

TukeyHSD(a6,"Area") #dif sig en los mejillones (BBE> PP> BBF)

layout(matrix(c(1:6), 2, 3)) 
plot(a6, 1:6) 
layout(1)

#solo datos de cholga 

C <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/cholgaR.xlsx")
str(C)

bargraph.CI(Area, STX, data = C,  xlab = "Area", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a7 <- aov(STX ~ Area, data = C) # niveles de STX son sig diferentes en las 2 zonas q tienen cholga (BBE> BBB)
summary(a7)

layout(matrix(c(1:6), 2, 3)) 
plot(a7, 1:6) 
layout(1)

# solo datos de BBE para comparar cholga vs mejillon 

BBE <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/BBE-R.xlsx")
str(BBE)

bargraph.CI(organism, STX, data = BBE,  xlab = "Organism", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a8 <- aov(STX ~ organism, data = BBE) 
summary(a8) # no hay dif en los valores de STX en el producto en la BBE

layout(matrix(c(1:6), 2, 3)) 
plot(a8, 1:6) 
layout(1)


## analisis de STX vs organimsmo por season (x separado)---------------#

# winter
W <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/winterR.xlsx")
str(W)

bargraph.CI(organism, STX, data = W, xlab = "Winter", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a0 <- aov(STX ~ organism, data = W) #  significativos
summary(a0)

a0 <- aov(log (STX + 1) ~ organism, data = W) #  significativos
summary(a0)


layout(matrix(c(1:6), 2, 3)) # residuales horribles
plot(a0, 1:6) 
layout(1)

# autumn
A <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/autumnR.xlsx")
str(A)

bargraph.CI(organism, STX, data = A, xlab = "Autumn", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a1 <- aov(STX ~ organism, data = A) #  significativos
summary(a1)

layout(matrix(c(1:6), 2, 3)) # residuales horribles
plot(a1, 1:6) 
layout(1)
  
# summer

S <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/summerR.xlsx")
str(S)

bargraph.CI(organism, STX, data = S, xlab = "Summer", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a2 <- aov(STX  ~ organism, data = S) # no significativo
summary(a2)


layout(matrix(c(1:6), 2, 3)) 
plot(a2, 1:6) 
layout(1)

# spring

SP <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/springR.xlsx")
str(SP)

bargraph.CI(organism, STX, data = SP, xlab = "Spring", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a3 <- aov(STX ~ organism, data = SP) # no significativo
summary(a3)


layout(matrix(c(1:6), 2, 3)) 
plot(a3, 1:6) 
layout(1)


# dif entre medias anuales ---------------------------------#
bargraph.CI(Year, STX, data = data,  xlab = "Year", ylab = "µg STX eq/100 g tissue", legend = TRUE)

a6 <- aov(STX  ~ Year, data = data) #  significativos
summary(a6)

TukeyHSD(a6,"Year")

layout(matrix(c(1:6), 2, 3)) 
plot(a6, 1:6) 
layout(1)


# dif entre medias anuales por area
bargraph.CI(Year, STX, Area, data = data,  xlab = "Year", ylab = "µg STX eq/100 g tissue", legend = TRUE, x.leg=0, y.leg=700)

a7 <- aov(STX ~ Year*Area, data = data) # los 3 significativos
summary(a7)

TukeyHSD(a7,"Year:Area")

layout(matrix(c(1:6), 2, 3)) 
plot(a7, 1:6) 
layout(1)



### Eventos toxicos y duracion de veda ----------------------------------------------------------#
# Analisis de picos: 
# STXmax: concentracion maxima alcanzada duante el evento toxico
# Duration: duracion de la veda, T desde q el mejillon supera los 80 µg STX/100 g tejido, hasta que tiene valores por debajo de ese umbral

library(readxl)
data2 <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/eventos toxicos y duracion.xlsx")
names(data2)

bargraph.CI(Duration,STXmax,Organism, data = data2,  ylab = "µg STX eq/100 g tissue", xlab = "Days", col = "steelblue4")

a8 <- aov(log(Duration) ~ Organism, data = data2, na.action=na.fail) 
summary(a8) # la duracion del evento toxico depende del organismo? no dif sig en la duracion del evento toxico segun el organismo

layout(matrix(c(1:6), 2, 3)) # residuales parecen mejorar cn el log
plot(a8, 1:6) 
layout(1)


a9 <- aov(log(Duration) ~ Season, data = data2, na.action=na.fail) 
summary(a9) # dif sig en la duracion del evento toxico segun la season 

TukeyHSD(a9,"Season") # summer - autumn mismo grupo y diferente de spring - winter


layout(matrix(c(1:6), 2, 3)) 
plot(a9, 1:6) # residuales parecen mejorar cn el log
layout(1)

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
BBE2 <- read_excel("Doctorado 2018/MODELOS/ARCHIVOS ANDY/eventos toxicos y duracion solo BBE.xlsx")

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

