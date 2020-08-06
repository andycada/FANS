
#------------------------------------------------------#
# Detoxificacion 

 
library(readxl)

#BE cholga y mejillon
BEC1 <- read_excel("Data/BEC1.xlsx")
BEC2 <- read_excel("Data/BEC2.xlsx")
BEC3 <- read_excel("Data/BEC3.xlsx")
BEC4 <- read_excel("Data/BEC4.xlsx")
BEM1 <- read_excel("Data/BEM1.xlsx")
BEM2 <- read_excel("Data/BEM2.xlsx")
BEM3 <- read_excel("Data/BEM3.xlsx")
BEM4 <- read_excel("Data/BEM4.xlsx")
BEM5 <- read_excel("Data/BEM5.xlsx")

#B fondo mejillon
BFM1 <- read_excel("Data/BFM1.xlsx")
BFM2 <- read_excel("Data/BFM2.xlsx")
BFM3 <- read_excel("Data/BFM3.xlsx")

#PP mejillon

PPM1 <- read_excel("Data/PPM1.xlsx")
PPM2 <- read_excel("Data/PPM2.xlsx")


# modelos GAMs (curvas de detoxificacion separadas por evento toxico, organismo y area)
  
  library(mgcv)

layout(matrix(c(1:6), 2, 3)) 
layout(1)

model0 <- gam(STX ~ s(Days_T), data = BEC1, method = "REML") 
plot.gam(model0,xlab= "Detoxification days", residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBE magellan mussel")
summary(model0) #R-sq.(adj) =  0.699   Deviance explained = 76% # no lo inclui en el grafico general 
gam.check(model0) 
concurvity(model0, full = TRUE)

model1 <- gam(STX ~ s(Days_T), data = BEC2, method = "REML") 
plot.gam(model1,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T)
summary(model1)  #R-sq.(adj) =  0.222   Deviance explained = 30.8% # no lo inclui en el grafico general
gam.check(model1) 

model2 <- gam(STX ~ s(Days_T), data = BEC3, method = "REML") 
plot.gam(model2, xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBE magellan mussel")
summary(model2)  # R-sq.(adj) =  0.786   Deviance explained = 84.7%
gam.check(model2) # esta curva va queriendo, mejor ajuste y se ven mejor los residuales

model3 <- gam(STX ~ s(Days_T), data = BEM1, method = "REML") 
plot.gam(model3,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBE blue mussel")
summary(model3)  #R-sq.(adj) =  0.933   Deviance explained =   95%
gam.check(model3)  #residuales ok


model4 <- gam(STX ~ s(Days_T), data = BEM2, method = "REML") 
plot.gam(model4,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBE blue mussel")
summary(model4)  #R-sq.(adj) =   0.29   Deviance explained = 39.6%, Days T no sig # no lo inclui en el grafico general 
gam.check(model4)

model5 <- gam(STX ~ s(Days_T), data = BEM3, method = "REML") 
plot.gam(model5,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBE blue mussel")
summary(model5)  #R-sq.(adj) =  0.811   Deviance explained = 87.8%
gam.check(model5)

model6 <- gam(STX ~ s(Days_T), data = BEM4, method = "REML") 
plot.gam(model6,xlab= "Detoxification days", residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBE blue mussel")
summary(model6)  #R-sq.(adj) =  0.804   Deviance explained =   88%
gam.check(model6)

model7 <- gam(STX ~ s(Days_T), data = BEM5, method = "REML") 
plot.gam(model7,xlab= "Detoxification days", residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBE blue mussel")
summary(model7)  
gam.check(model7) ### no lo hace pocas observaciones

model7 <- gam(STX ~ s(Days_T), data = BFM1, method = "REML") 
plot.gam(model7,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, main= "BBF blue mussel")
summary(model7)  #-sq.(adj) =  0.743   Deviance explained = 82.5%
gam.check(model7) # algo ajusta hay q arreglar los residuales 

model8 <- gam(STX ~ s(Days_T), data = PPM1, method = "REML") 
plot.gam(model8,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, main= "PP blue mussel")
summary(model8)  #R-sq.(adj) =  0.791   Deviance explained = 85.4%
gam.check(model8)

model8 <- gam(STX ~ s(Days_T), data = PPM2, method = "REML") 
plot.gam(model8,residuals=T,pch=1,all.terms=T,seWithMean=T)
summary(model8)  
gam.check(model8) #pocas observaciones no lo hace 


# Uso los datos totales, sin separar por evento toxico, saco las curvas de 5-8 observaciones para ver si mejora el ajuste 

library(readxl)
DETOX2 <- read_excel("Data/DETOX-filtrado.xlsx")

names(DETOX2)
str(DETOX2)

model0 <- gam(STX ~ s(Days), data = DETOX2, method = "REML") 
plot.gam(model0,residuals=T,pch=1,all.terms=T,seWithMean=T)
summary(model0)  # 
gam.check(model0)

#---------------------------------------------
# pruebo area y organism como variables categoricas

library(mgcv)
require(gratia)

DETOX2$area<-as.factor(DETOX2$area)
DETOX2$organism<-as.factor(DETOX2$organism)
class(DETOX2$area)
class(DETOX2$organism)
class(DETOX2$Days)

layout(1)
model1 <- gam(STX ~ s(Days) + area + organism, data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model1,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model1)  #R-sq.(adj) =   0.25   Deviance explained = 43.3% ################
gam.check(model1, pages=1)
draw(model1,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model1) # chequea el modelo, residuales, etc


model2 <- gam(STX ~ s(Days,  by = area), data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model2,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model2)  #R-sq.(adj) =   0.259   Deviance explained = 46.2%, sig BBE y PP pero los graficos no se ve nada
gam.check(model2, pages=1)
draw(model2,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model2) # chequea el modelo, residuales, etc


model3 <- gam(STX ~ s(Days,  by = organism), data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model3,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model3)  #R-sq.(adj) =   0.191   Deviance explained = 39.3%, sig cholga y meji pero los graficos no dan nada
gam.check(model3, pages=1)
draw(model3,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model3) # chequea el modelo, residuales, etc

model4 <- gam(STX ~ s(Days,  by = area) + organism, data = DETOX2,family=Gamma (link="log"), method = "REML") 
plot.gam(model4,xlab= "Detoxification days",residuals=T,pch=500,all.terms=T,seWithMean=T, pages=1)
summary(model4)  #R--sq.(adj) =  0.202   Deviance explained = 45.9%, todos sig y lindos graficos ###########
draw(model4,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model4) # chequea el modelo, residuales, etc

model5 <- gam(STX ~ s(Days,  by = organism) + area, data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model5,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model5)  #R--sq.(adj) =   0.26   Deviance explained = 44.1%, sig cholga y meji pero los graficos no dan nada
gam.check(model5, pages=1)
draw(model5,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model5) # chequea el modelo, residuales, etc

model6 <- gam(STX ~ s(Days) + area + organism, data = DETOX2,family=Gamma (link="log"), method = "REML") 
plot.gam(model6,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model6)  #R-sq.(adj) =  0.162   Deviance explained = 44.7%
draw(model6,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model6) # chequea el modelo, residuales, etc

AIC(model0,model1,model2,model3,model4,model5,model6) # modelo 6 y 4 los mejores

## Modelo con autocorrelacion  para ver si mejoran los modelos 6 y 4 ------------------##

str(DETOX2)


# Grafico Autocorrelacion

#model 6
E <- residuals(model6)
I1 <- !is.na(DETOX2$STX)
Efull <- vector(length = length(DETOX2$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

#model4
E <- residuals(model4)
I1 <- !is.na(DETOX2$STX)
Efull <- vector(length = length(DETOX2$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

#Incluyo autocorrelacion AR-1 

model6A<- gam(STX ~ s(Days) + area + organism, data = DETOX2,family=Gamma (link="log"), correlation = corARMA(form =~ Days))
plot.gam(model6A,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6A,residuals=T) 
appraise(model6A) 
summary(model6A)  #R-sq.(adj) =  0.162   Deviance explained = 44.7%


model4A <- gam(STX ~ s(Days,by = area) + organism, data = DETOX2,family=Gamma (link="log"), correlation = corARMA(form =~ Days))
plot.gam(model4A,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4A,residuals=T) 
appraise(model4A) 
summary(model4A)  #R-sq.(adj) =  0.162   Deviance explained = 49%

# no toma la autocorrelacion pero el ajuste no es tan malo 

## Graficos -------------##

require(ggplot2)

ggplot(DETOX2, aes(x = Days, y = STX,color=area)) + xlab("") + geom_line() + facet_grid( area ~ organism,scale="free_y") + geom_smooth(se=FALSE) 




### Modelos jerarquicos -------------------------------------------------------------##

## GS: A single common smoother plus group-level smoothers that have the same wiggliness

# CO2_modGS <- gam(log(uptake) ??? s(log(conc), k=5, m=2) + s(log(conc), Plant_uo, k=5, bs="fs", m=2), data=CO2, method="REML")
# M1AGS <- gam(Medulis$STX ~ s(Date, k=5, m=2) + s(Date, Area,k=5, m=2,bs="fs"), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")

model6GS<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML")
plot.gam(model6GS,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GS,residuals=T) 
appraise(model6GS) 
summary(model6GS) #R-sq.(adj) =  0.153   Deviance explained =   44%


model4GS <- gam(STX ~ s(Days,by = area) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML")
plot.gam(model4GS,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4GS,residuals=T) 
appraise(model4GS) 
summary(model4GS) # R-sq.(adj) =  0.218   Deviance explained = 47.5%


# grafico ACF 

# model6GS
E <- residuals(model6GS)
I1 <- !is.na(DETOX2$STX)
Efull <- vector(length = length(DETOX2$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

#model4GS
E <- residuals(model4GS)
I1 <- !is.na(DETOX2$STX)
Efull <- vector(length = length(DETOX2$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")


# agrego ACF a los modelos 4 y  6 GS

model6AGS<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days))
plot.gam(model6AGS,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6AGS,residuals=T) 
appraise(model6AGS) 
summary(model6AGS) # no  toma bien la autocorrelacion 


model4AGS <- gam(STX ~ s(Days,by = area) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days))
plot.gam(model4AGS,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4AGS,residuals=T) 
appraise(model4AGS) 
summary(model4AGS) # no  toma bien la autocorrelacion

# ACF anidada en area 
model4AGS <- gam(STX ~ s(Days,by = area) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days | area))
plot.gam(model4AGS,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4AGS,residuals=T) 
appraise(model4AGS) # no toma la autocorrelacion
summary(model4AGS) 


AIC(model6GS,model4GS,model6AGS,model4AGS)# el model4AGs es el mejor pero no toma la autocorrelacion


## GI A single common smoother plus group-level smoothers with differing wiggliness (Model GI)

#CO2_modGI <- gam(log(uptake) ??? s(log(conc), k=5, m=2, bs="tp") + s(log(conc), by=Plant_uo, k=5, m=1, bs="tp") + s(Plant_uo, bs="re", k=12), data=CO2, method="REML")

model6GI<- gam(STX ~ s(Days, k=5, m=2, bs="tp") + s(Days,by=area, k=5, m=1, bs="tp") + s(Days, by=organism,m=1, k=5, bs="tp") + s(area, bs="re", k=12)+ s(organism, bs="re", k=12), data = DETOX2,family=Gamma (link="log"), method="REML")
plot.gam(model6GI,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GI,residuals=T) 
appraise(model6GI) 
summary(model6GI) # R-sq.(adj) =  0.235   Deviance explained = 49.5%


#Grafico ACF
E <- residuals(model6GI)
I1 <- !is.na(DETOX2$STX)
Efull <- vector(length = length(DETOX2$STX))
Efull <- NA
Efull[I1] <- E
acf(Efull, na.action = na.pass,
    main = "Auto-correlation plot for residuals")



# agrego ACF
model6AGI<- gam(STX ~ s(Days, k=5, m=2, bs="tp") + s(Days,by=area, k=5, m=1, bs="tp") + s(Days, by=organism,m=1, k=5, bs="tp") + s(area, bs="re", k=12)+ s(organism, bs="re", k=12), data = DETOX2,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days))
plot.gam(model6AGI,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6AGI,residuals=T) # no lo grafica
appraise(model6AGI) #R-sq.(adj) =  0.235   Deviance explained = 49.5%
summary(model6AGI) # no toma la autocorrelacion 


AIC(model6GS,model4GS,model6AGS,model4AGS,model6GI,model6AGI) # modelo 6GI es el mejor


### Agrego variables ambientales al modelo --------------------------------------------------##

library(readxl)
DETOX3 <- read_excel("Data/STXmax-Totalarea-DaysT.xlsx")
names(DETOX3)
str(DETOX3)

DETOX3$area<- as.factor(DETOX3$area)
DETOX3$organism<- as.factor(DETOX3$organism)
DETOX3$Date<-as.Date(DETOX3$Date)


model7<- gam(STX ~ s(Tmed) +  s(Days, k=5, m=2, bs="tp") + s(Days,by=area, k=5, m=1, bs="tp") + s(Days, by=organism,m=1, k=5, bs="tp") + s(area, bs="re", k=12)+ s(organism, bs="re", k=12), data = DETOX3,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days))
plot.gam(model6AGI,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6AGI,residuals=T) # no lo grafica
appraise(model6AGI) 
summary(model6AGI)# R-sq.(adj) =  0.235   Deviance explained = 49.5%


model8<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs")+ s(Tmed), data = DETOX3,family=Gamma (link="log"), method="REML")
plot.gam(model8,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model8,residuals=T) 
appraise(model8) #R-sq.(adj) =  0.293   Deviance explained = 53.1%
summary(model8)  

# agrego correlacion anidada correlation = corARMA(form =~ Days | area))
model8c<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs")+ s(Tmed), data = DETOX3,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days | area))
plot.gam(model8c,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model8c,residuals=T) 
appraise(model8c) # no toma la ACF
summary(model8c)  # R-sq.(adj) =  0.293   Deviance explained = 53.1%


#solo Temperatura media
model7<- gam(STX ~ s(Tmed), data = DETOX3,family=Gamma (link="log"), method="REML")
plot.gam(model7,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model7,residuals=T) 
appraise(model7) 
summary(model7) # R-sq.(adj) =  0.129   Deviance explained = 30.7%

# DE ARCHIVO EXPLORATORIO PREVIO (detoxificacion-ambiental) el sig modelo era el mejor,le agrego la parte jerarquica 

#model0 <- gam(µg_STX ~ s(DAYS) + s(Tac_10D, k=25) + s(Vac_10D, k=35), data = DETOX3, family=Gamma (link="log"),method = "REML") 
#plot.gam(model0,residuals=T,pch=1,all.terms=T,seWithMean=T,pages=1)
#draw(model0,residuals=T) #dibuja el GAMs y los puntos son los residuales
#appraise(model0) 
#summary(model0) #R-sq.(adj) =  0.276   Deviance explained = 51.5% los 3 sig 
#gam.check(model0)


model9<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs")+ s(Tmed) + s(Viento_Med) + s(Precip), data = DETOX3,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days | area))
plot.gam(model9,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model9,residuals=T) 
appraise(model9) # no toma la autocorrelacion
summary(model9) #R-sq.(adj) =  0.327   Deviance explained = 58.3%, precip y Days (organism) no son sign


AIC(model7,model8,model8c, model9)# modelo 9 es el mejorcito

#uso Tac-10D, Vac-10D y Pac-7D
#model0 <- gam(µg_STX ~ s(DAYS) + s(Tac_10D, k=25) + s(Vac_10D, k=35), data = DETOX3, family=Gamma (link="log"),method = "REML") 


model10<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Days, organism,m=2, k=5, bs="fs")+ s(Tac_10D) + s(Vac_10D) + s(Pac_7D), data = DETOX3,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days | area))
plot.gam(model10,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model10,residuals=T) 
appraise(model10) # no toma la autocorrelacion
summary(model10) #R-sq.(adj) =  0.348   Deviance explained = 60.3%, Days,orga y prec no significativos

# saco preci y Days, organism q son no significativos

model11<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Tac_10D) + s(Vac_10D), data = DETOX3,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days | area))
plot.gam(model11,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model11,residuals=T) 
appraise(model11) # no toma la autocorrelacion
summary(model11) #R-sq.(adj) =  0.352   Deviance explained = 60.3%
gam.check(model11) # aumentar k Tac y de Vac (k=9)

# pruebo diferente k pero siguen siendo significactivas 
model11<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs") + s(Tac_10D, k=40) + s(Vac_10D,k=45), data = DETOX3,family=Gamma (link="log"), method="REML", correlation = corARMA(form =~ Days | area))
plot.gam(model11,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model11,residuals=T) 
appraise(model11) # no toma la autocorrelacion
summary(model11) #R-sq.(adj) =  0.352   Deviance explained = 60.3%
gam.check(model11)



AIC(model7,model8,model8c, model9, model10, model11) # el model11 es el mejor

#----------------------------------------------------------------------------------------------#

#  Modelo para cada curva de detoxificacion por separado, con autocorrelacion (Zhur 2009)

library(readxl)
DETOX3 <- read_excel("Data/DETOX-jerarquico.xlsx")
str(DETOX3)
names(DETOX3)

mussels <- c(DETOX3$BBEC1, DETOX3$BBEC2, DETOX3$BBEC3, DETOX3$BBEM1,DETOX3$BBEM2,DETOX3$BBEM3, DETOX3$BBEM4,DETOX3$BBFM1, DETOX3$BBFM2, DETOX3$PPM1)
Time <- rep(DETOX3$Days, 10)
ID <- factor(rep(c("DETOX3$BBEC1", "DETOX3$BBEC2", "DETOX3$BBEC3", "DETOX3$BBEM1","DETOX3$BBEM2","DETOX3$BBEM3", "DETOX3$BBEM4","DETOX3$BBFM1", "DETOX3$BBFM2", "DETOX3$PPM1"),
                 each = length(DETOX3$Days)))
library(lattice)
xyplot(mussels ??? Time | ID, col = 1)
M0<-gam(mussels ???  ID +
              s(Time, by = as.numeric(ID == "DETOX3$BBEC1")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBEC2")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBEC3")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBEM1")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBEM2")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBEM3")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBEM4")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBFM1")) +
              s(Time, by = as.numeric(ID == "DETOX3$BBFM2")) +
             s(Time, by = as.numeric(ID == "DETOX3$PPM1")), family=Gamma (link="log"), method="REML")


plot.gam(M0,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M0,residuals=T) # no lo grafica
appraise(M0) 
summary(M0)


# agrego autocorrelacion
M1<-gam(mussels ???  ID +
          s(Time, by = as.numeric(ID == "DETOX3$BBEC1")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBEC2")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBEC3")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBEM1")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBEM2")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBEM3")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBEM4")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBFM1")) +
          s(Time, by = as.numeric(ID == "DETOX3$BBFM2")) +
          s(Time, by = as.numeric(ID == "DETOX3$PPM1")), family=Gamma (link="log"), method="REML", correlation = corAR1(form =??? Time | ID ))



plot.gam(M1,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(M1,residuals=T) # no lo grafica
appraise(M1) 
summary(M1) # R-sq.(adj) =  0.789   Deviance explained = 90.6% 













#--------------------------------------------#

library(gamm4) 

# Package alternative, allows AIC type model selection for generalized models, to compare gamm models.



  
    

