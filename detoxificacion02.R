
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

DETOX2$area<-as.factor(DETOX2$area)
DETOX2$organism<-as.factor(DETOX2$organism)
class(DETOX2$area)
class(DETOX2$organism)
class(DETOX2$Date)

layout(1)
model1 <- gam(STX ~ s(Days) + area + organism, data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model1,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model1)  #R-sq.(adj) =   0.25   Deviance explained = 43.3% ################
gam.check(model1, pages=1)

model2 <- gam(STX ~ s(Days,  by = area), data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model2,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model2)  #R-sq.(adj) =   0.259   Deviance explained = 46.2%, sig BBE y PP pero los graficos no se ve nada
gam.check(model2, pages=1)


model3 <- gam(STX ~ s(Days,  by = organism), data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model3,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model3)  #R-sq.(adj) =   0.191   Deviance explained = 39.3%, sig cholga y meji pero los graficos no dan nada
gam.check(model3, pages=1)

model4 <- gam(STX ~ s(Days,  by = area) + organism, data = DETOX2,family=Gamma (link="log"), method = "REML") 
plot.gam(model4,xlab= "Detoxification days",residuals=T,pch=500,all.terms=T,seWithMean=T, pages=1)
summary(model4)  #R--sq.(adj) =  0.202   Deviance explained = 45.9%, todos sig y lindos graficos ###########
gam.check(model4, pages=1)

model5 <- gam(STX ~ s(Days,  by = organism) + area, data = DETOX2,family=Gamma, method = "REML") 
plot.gam(model5,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model5)  #R--sq.(adj) =   0.26   Deviance explained = 44.1%, sig cholga y meji pero los graficos no dan nada
gam.check(model5, pages=1)


#transformo datos a log 
model6 <- gam(STX ~ s(Days) + area + organism, data = DETOX2,family=Gamma (link="log"), method = "REML") 
plot.gam(model6,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model6)  #R-sq.(adj) =  0.162   Deviance explained = 44.7%
gam.check(model6)


AIC(model0,model1,model2,model3,model4,model5,model6) # modelo 6 y 4 los mejores


#--------------------------------------------#

library(gamm4) 

# Package alternative, allows AIC type model selection for generalized models, to compare gamm models.



  
    
