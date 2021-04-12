# Detoxificacion 

# Uso los datos totales, sin separar por evento toxico, saco las curvas de 5-8 observaciones para ver si mejora el ajuste 
# k.check() es OK CUANDO
# EDF < (k')
# p-value for the k-index (which measures pattern in the residuals) not significant
#Total deviance BUSCAR LA MENOR

library(readxl)
DETOX2 <-DETOX_filtrado
DETOX2 <- read_excel("Data/DETOX-filtrado.xlsx")

names(DETOX2)
str(DETOX2)
DETOX2$area<-as.factor(DETOX2$area)
DETOX2$organism<-as.factor(DETOX2$organism)
DETOX2$year<-as.factor(DETOX2$year)## year-F tiene q ser factor para modelo estacional

library(mgcv)
require(gratia)

#Graficos 

require(ggplot2)
theme_set(theme_bw())

ggplot(DETOX2, aes(x = Days, y = STX,color=area)) + xlab("") + facet_grid( area ~ organism,scale="free_y") + geom_line() + geom_smooth(se=FALSE) 


#---------------------------------------------
# pruebo area y organism como variables categoricas

model2 <- gam(STX ~ s(Days,  by = area), data = DETOX2,family=Gamma(link="log"), method = "REML") 
plot.gam(model2,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model2)  #R-sq.(adj) =  0.221   Deviance explained = 44.7
draw(model2,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model2) # chequea el modelo, residuales, etc


model3 <- gam(STX ~ s(Days,  by = organism), data = DETOX2,family=Gamma (link="log") , method = "REML") 
plot.gam(model3,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model3)  #R-sq.(adj) =   0.168   Deviance explained = 40.6%, sig cholga y meji 
draw(model3,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model3) # chequea el modelo, residuales, etc

model4 <- gam(STX ~ s(Days,  by = area) + organism, data = DETOX2,family=Gamma (link="log"), method = "REML") 
plot.gam(model4,xlab= "Detoxification days",residuals=T,pch=500,all.terms=T,seWithMean=T, pages=1)
summary(model4)  #R--sq.(adj) =  0.202   Deviance explained = 45.9%, todos sig y lindos graficos ###########
draw(model4,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model4) # chequea el modelo, residuales, etc

model5 <- gam(STX ~ s(Days,  by = organism) + area, data = DETOX2,family=Gamma (link="log"), method = "REML") 
plot.gam(model5,xlab= "Detoxification days",residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model5)  #R--sq.(adj) =   0.26   Deviance explained = 44.1%, sig cholga y meji pero los graficos no dan nada
draw(model5,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model5) # chequea el modelo, residuales, etc

model6 <- gam(STX ~ s(Days) + area + organism, data = DETOX2,family=Gamma (link="log"), method = "REML") 
plot.gam(model6,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
summary(model6)  #R-sq.(adj) =  0.162   Deviance explained = 44.7%
draw(model6,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(model6) # chequea el modelo, residuales, etc

AIC(model2,model3,model4,model5,model6) # modelo 6 y 4 los mejores


## Modelos jerarquicos -------------------------------------------------------------##

### GS: A single common smoother plus group-level smoothers that have the same wiggliness

# CO2_modGS <- gam(log(uptake) ??? s(log(conc), k=5, m=2) + s(log(conc), Plant_uo, k=5, bs="fs", m=2), data=CO2, method="REML")
# M1AGS <- gam(Medulis$STX ~ s(Date, k=5, m=2) + s(Date, Area,k=5, m=2,bs="fs"), na.action = na.omit,data = Medulis,family=Gamma (link="log"), method="REML")


model6GS<- gam(STX ~ s(Days, k=10, m=2) + s(Days,area, k=10, m=2,bs="fs") + s(Days, organism,m=2, k=10, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML")
plot.gam(model6GS,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GS,residuals=T) 
appraise(model6GS) 
summary(model6GS) #R-sq.(adj) =  0.153   Deviance explained =   44%

# Este es GI
#
model4GS <- gam(STX ~ s(Days, k=10, m=2) + s(Days,by = area, k=10, m=1,bs="fs") + s(Days, by=organism,m=1, k=10, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML")
plot.gam(model4GS,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4GS,residuals=T) 
appraise(model4GS) 
summary(model4GS) # R-sq.(adj) =  0.218   Deviance explained = 47.5% (mejora el ajuste)


AIC(model2,model3,model4,model5,model6,model6GS,model4GS) # modelo 6 y 4 los mejores


## Filtros datos totales por especie (paquete tidyverse) y area para ver si mejoran los modelos

library(tidyverse)
theme_set(theme_bw())


# MEJILLON (M. edulis) en areas (BBF, BBE, PP)---------------------#
#
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" )

ggplot(DETOX2M, aes(x = Days, y = STX,color=area)) + xlab("") + facet_grid( area ~ organism,scale="free_y") + geom_line() + geom_smooth(se=FALSE) 

#

DETOX2M %>% count(area,codigo) 


model6GSM<- gam(STX ~ s(Days, k=10, m=2, bs="cc") + s(Days,area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma (link="log"), method="REML")
gam.check(model6GSM)
plot.gam(model6GSM,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GSM,residuals=T) 
appraise(model6GSM) 
summary(model6GSM) #R-sq.(adj) =  0.153   Deviance explained =   38.8%
AIC(model6GSM)

# Plot de predicciones de modelo 
#
#
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model6GSM,newdata=pred, se.fit = TRUE)
ilink <- family(model6GSM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area)


## Modelo GI

model4GSM <- gam(STX ~ s(Days,k=10, m=2, bs="cc") + s(Days,by=area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma(link="log"), method="REML")
plot.gam(model4GSM,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4GSM,residuals=T) 
appraise(model4GSM) 
summary(model4GSM) # R-sq.(adj) =  0.218   Deviance explained = 45.4%
gam.check(model4GSM)

# Plot de predicciones de modelo 
#
#
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model4GSM,newdata=pred, se.fit = TRUE)
ilink <- family(model4GSM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))


# NO SE PUEDE COMPARAR CON AIC modelos ajustados con distinto nro de datos
#
# Modelos generales (sin filtrar)
AIC(model6GS,model4GS)
# Modelos filtrados 
AIC(model6GSM,model4GSM)# el mas chico es model4GSM filtrado para mejillon

#************************ HASTA ACA ***************************

## CHOLGA (A. ater)en areas (BB, BF)-------------------------------#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )

model6GSA<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
plot.gam(model6GSA,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GSA,residuals=T) 
appraise(model6GSA) 
summary(model6GSA) #R-sq.(adj) =  0.246   Deviance explained = 57.1%
k.check(model6GSA)#ok

model4GSA <- gam(STX ~ s(Days,by = area) + s(Days,area, k=5, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
plot.gam(model4GSA,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4GSA,residuals=T) 
appraise(model4GSA) #R-sq.(adj) =  0.266   Deviance explained = 59.2%
summary(model4GSA)

# Modelos GS filtrados cholga vs generales (sin filtrar)
AIC(model6GS,model4GS,model6GSA,model4GSA) # los modelos filtrados (cholga) son mejores que los globales 


## BBE 2 Organismos para 1 area ------------------------------------#
DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )

model6GSBBE<- gam(STX ~ s(Days, k=5, m=2) + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
plot.gam(model6GSBBE,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GSBBE,residuals=T) 
appraise(model6GSBBE) 
summary(model6GSBBE) #R-sq.(adj) =  0.153   Deviance explained =   35.7%
k.check(model6GSBBE)

model4GSBBE <- gam(STX ~ s(Days,by = organism) + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
plot.gam(model4GSBBE,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model4GSBBE,residuals=T) 
appraise(model4GSBBE) 
summary(model4GSBBE) #R-sq.(adj) =  0.165   Deviance explained = 43.3%
k.check(model4GSBBE)


## GI A single common smoother plus group-level smoothers with differing wiggliness (Model GI)

#CO2_modGI <- gam(log(uptake) ??? s(log(conc), k=5, m=2, bs="tp") + s(log(conc), by=Plant_uo, k=5, m=1, bs="tp") + s(Plant_uo, bs="re", k=12), data=CO2, method="REML")

model6GI<- gam(STX ~ s(Days, k=5, m=2, bs="tp") + s(Days,by=area, k=5, m=1, bs="tp") + s(Days, by=organism,m=1, k=5, bs="tp") + s(area, bs="re", k=12)+ s(organism, bs="re", k=12), data = DETOX2,family=Gamma (link="log"), method="REML")
plot.gam(model6GI,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GI,residuals=T) # no lo grafica 
appraise(model6GI) 
summary(model6GI) # R-sq.(adj) =  0.235   Deviance explained = 49.5%

# MEJILLON (M. edulis) en areas (BBF, BBE, PP)---------------------#
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" )
model6GIM<- gam(STX ~ s(Days, k=5, m=2, bs="tp") + s(Days,by=area, k=5, m=1, bs="tp") + s(area, bs="re", k=12), data = DETOX2M,family=Gamma (link="log"), method="REML")
plot.gam(model6GIM,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GIM,residuals=T) #  no grafica 
appraise(model6GIM) 
summary(model6GIM) #R-sq.(adj) =  0.191   Deviance explained = 43.1%

# CHOLGA (A. ater) en areas (BBB, BBE)---------------------#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
model6GIA<- gam(STX ~ s(Days, k=5, m=2, bs="tp") + s(Days,by=area, k=5, m=1, bs="tp") + s(area, bs="re", k=12), data = DETOX2A, family=Gamma (link="log"), method="REML")
plot.gam(model6GIA,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GIA,residuals=T) # no acepta el modelo 
appraise(model6GIA) 
summary(model6GIA)

## BBE 2 Organismos para 1 area ------------------------------------#
DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )

model6GIBBE<- gam(STX ~ s(Days, k=5, m=2, bs="tp") + s(Days, by=organism,m=1, k=5, bs="tp") + s(organism, bs="re", k=12), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
plot.gam(model6GIBBE,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
draw(model6GIBBE,residuals=T) # no lo grafica 
appraise(model6GIBBE) 
summary(model6GIBBE) #R-sq.(adj) =  0.154   Deviance explained = 40.8%


#comparacion de modelos para mejillon (GS/GI/filtrados)
AIC(model6GIM,model6GI,model6GS,model4GS,model6GSM,model4GSM) # mejores los modelos filtrados Gs y GI (model4GSM, model6GIM)

#comparacion de modelos para cholga (GS/GI/filtrados)
AIC(model6GI,model6GS,model4GS,model6GSA,model4GSA) # mejores los modelos filtrados Gs (model4GSA, model6GSA)

#comparacion de modelos para BBE (GS/GI/filtrados)
AIC(model6GIBBE,model6GI,model6GS,model4GS,model6GSBBE,model4GSBBE)#  mejores modeloS filtrados GI y GS (6GI, 6GS, 4GS)


## Modelos jerarquicos (incluyendo ESTACIONALIDAD) -------------------------------------------------------------##

### GS (siguiendo ejemplo zooplancton)
#zoo_daph_modGS <- gam(density_adj ~  s(day, bs="cc", k=10) + s(day, lake, k=10, bs="fs", xt=list(bs="cc")) +s(lake, year_f, bs="re"), data=daphnia_train, knots=list(day=c(0, 365)),family=Gamma(link="log"), method="REML", drop.unused.levels=FALSE)

# Agregar año de inicio 
#
str(DETOX2)
require(lubridate)
dd <- DETOX2 %>% group_by(area,organism,codigo) %>% summarise(year_ini=min(lubridate::year(Date)))

DETOX2 <- inner_join(DETOX2,dd) %>% mutate(year_ini=as.factor(year_ini))


MGS <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGS, residuals = TRUE)
appraise(MGS)
summary(MGS) #R-sq.(adj) =  0.367   Deviance explained = 59.4%
k.check(MGS) ## edf< k y p value no sig p-value (measures remaining pattern in the residuals) .. esto es OK


AIC(model6GS,model4GS,MGS) # mejor modelo (<AIC) respecto de los anteriores generales (sin filtrar)


#################### DESDE ACA LEO
#
# MEJILLON (M. edulis) en areas (BBF, BBE, PP)--------------------- Modelo GS 
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) 
ggplot(DETOX2M, aes(x = Days, y = STX,color=area)) + xlab("") + facet_grid( area ~ year_ini,scale="free_y") + geom_point() + geom_smooth(se=FALSE) 

# Si eliminamos los que tienen pocos datos 
#DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) %>% group_by(year,area) %>% filter( n()>10) %>% ungroup()

# Ver maximo para knots
max(DETOX2M$Days)

MGSM <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM, residuals = TRUE)
appraise(MGSM)
summary(MGSM) #R-sq.(adj) =  0.439   Deviance explained = 81.3%
k.check(MGSM) # ok pero residuales no tan lindos en los extremos 

#
# Lo que pasa aca es que al poner los knots en 365 que es mayor que el maximo efectivamente es como si no lo tomara ciclico, porque el 
# smoother va a ser igual al dia 0 en el dia 365, por los datos veo que las curvas no terminan y empiezan igual
# por eso creo que está bien asi
#
# Comparacion de modelos filtrados para mejillon 
AIC(model6GSM,model4GSM,MGSM) # MGSM es claramente mejor q todos los demas AIC (1909)

# dan todos significativos !!!!!!!!!!!!!!!1
# ver que hacer con el termino que da no significativo s(Days, area) !!!!!!!!!!

# Plot de predicciones de modelo 
#
pred <-distinct(DETOX2M, area,year_ini) %>% group_by(area,year_ini) %>% do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2M$Days))))
p1<- predict(MGSM,newdata=pred, se.fit = TRUE) 
ilink <- family(MGSM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
 facet_wrap(year_ini~area) 


#
# En la prediccion se ve que le erra mal para BBE 2010 es porque los datos no empiezan desde Days=0!!!!!!!!!!!!!!!
#

# Pruebo modelo sin global
#
MSM <- gam(STX ~  s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM, residuals = TRUE)
appraise(MGSM)
summary(MGSM) #R-sq.(adj) =  0.439   Deviance explained = 81.3%
k.check(MGSM) # ok pero residuales no tan lindos en los extremos 

# Plot de predicciones de modelo 
#
pred <-distinct(DETOX2M, area,year_ini) %>% group_by(area,year_ini) %>% do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2M$Days))))
p1<- predict(MSM,newdata=pred, se.fit = TRUE) 
ilink <- family(MSM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 


AIC(MSM,MGSM)

# 
# Da un poco mejor sin global pero no hay mucha diferencia
# 

# CHOLGA (A. ater) en areas (BBB, BBE)--------------------- Muy pocos datos en BBB!!!!!!!!!!!!! 
# 
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
max(DETOX2A$Days)
MGSA <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 218)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSA, residuals = TRUE)
appraise(MGSA)
summary(MGSA) #R-sq.(adj) =  0.358   Deviance explained = 64.5% todo significativos 
k.check(MGSA) #ok


AIC(model6GSA,model4GSA,MGSA)# claramente el AIC baja cn datos filtrados para cholga e inluyendo estacionalidad 

## BBE 2 Organismos para 1 area ------------------------------------#
#zoo_daph_modGS <- gam(density_adj ~  s(day, bs="cc", k=10) + s(day, lake, k=10, bs="fs", xt=list(bs="cc")) +s(lake, year_f, bs="re"), data=daphnia_train, knots=list(day=c(0, 365)),family=Gamma(link="log"), method="REML", drop.unused.levels=FALSE)

DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )
MGSBBE <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, organism, k=10, bs="fs", xt=list(bs="cc"))+ s(organism, year, bs="re"),na.action = na.omit,data = DETOX2BBE, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSBBE, residuals = TRUE)
appraise(MGSBBE) # pas mal 
summary(MGSBBE) #R-sq.(adj) =   0.34   Deviance explained = 72.3% 
k.check(MGSBBE)

AIC(model6GSBBE,model4GSBBE,MGSBBE) # MGSBBE el mejor 

### GI (siguiendo ejemplo zooplancton)

#zoo_daph_modGI <- gam(density_adj???s(day, bs="cc", k=10) +s(lake, bs="re") + s(day, by=lake, k=10, bs="cc") + s(lake, year_f, bs="re"), data=daphnia_train, knots=list(day=c(0, 365)), family=Gamma(link ="log"), method="REML", drop.unused.levels=FALSE)

MGI <- gam(STX ~ s(Days, bs="cc", k=10) + s(area, bs="re") + s(Days, by=area, k=10, bs="cc") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGI, residuals = TRUE)## no grafica 
plot.gam(MGI,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(MGI) # los residuales se ven muy bien
summary(MGI)# R-sq.(adj) =  0.372   Deviance explained = 60.9% pero solo los dias significativos
k.check(MGI) #ok

# MEJILLON (M. edulis) en areas (BBF, BBE, PP)--------------------- 
#
# No hay suficientes datos 
#
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" )
MGIM <- gam(STX ~ s(Days, bs="cc", k=10) + s(area, bs="re") + s(Days, by=area, k=10, bs="cc") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIM, residuals = TRUE) ## no grafica 
plot.gam(MGIM,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(MGIM) # los residuales se ven bien
summary(MGIM) #R-sq.(adj) =  0.441   Deviance explained = 81.2%
k.check(MGIM) #ok

#
# Error in gam.reparam(UrS, sp, grderiv) : 
# NA/NaN/Inf in foreign function call (arg 3)
#
# This usually means an overfitted/unstable model. 


# Plot de predicciones de modelo 
#
pred <-distinct(DETOX2M, area,year) %>% group_by(area,year) %>% do(tibble(area=.$area,year=.$year,Days=0:(max(DETOX2M$Days))))
p1<- predict(MGIM,newdata=pred, se.fit = TRUE) 
ilink <- family(MGIM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year~area) 


# GS vs GI (con y sin estacionalidad)
AIC(model6GS,model4GS,MGS,MGI)# MGS, MGI (c/estacionalidad) son mejores 

# GS, GI  filtrados para mejillon, c/ y s/estacionalidad
AIC(MGSM,MGIM)# MGSM, MGIM (c/estacionalidad y filtrados para mejillon son mejores 

# CHOLGA (A. ater) en areas (BBB, BBE)---------------------#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
MGIA <- gam(STX ~ s(Days, bs="cc", k=10) + +s(area, bs="re") + s(Days, by=area, k=10, bs="cc") + s(area, year, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIA, residuals = TRUE) ## no lo acepta
plot.gam(MGIA,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(MGIA) # 
summary(MGIA) #
k.check(MGIA)

## BBE 2 Organismos para 1 area ------------------------------------#
#zoo_daph_modGI <- gam(density_adj???s(day, bs="cc", k=10) +s(lake, bs="re") + s(day, by=lake, k=10, bs="cc") + s(lake, year_f, bs="re"), data=daphnia_train, knots=list(day=c(0, 365)), family=Gamma(link ="log"), method="REML", drop.unused.levels=FALSE)

DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )
MGIBBE <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, bs="re") + s(Days, by=organism, k=10, bs="cc")+ s(organism, year, bs="re"),na.action = na.omit,data = DETOX2BBE, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIBBE, residuals = TRUE) # no grafica 
plot.gam(MGIBBE, residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(MGIBBE) # pas mal 
summary(MGIBBE) #R-sq.(adj) =  0.358   Deviance explained = 72.3% 
k.check(MGIBBE)







### PRUEBA DATA TRAIN, DATA TEST, FUNCION PREDICT -----------------------------------#
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" )


STX_train <- subset(DETOX2M, codigo%%2==1) #selecciono curvas impares para correr modelo, incluye todas las areas (BBE, PP, BBF)
STX_test  <- subset(DETOX2M, codigo%%2==0) # pares para testear (BBE, BBE, no incluye PP porque no hay otra curva para esa area)

view(STX_train)
view(STX_test)


# G con curvas impares (BBE, BBF, PP)

#zoo_daph_modG <- gam(density_adj ~ s(day, bs="cc", k=10) + s(lake, bs="re") + s(lake, year_f, bs="re"),data=daphnia_train, knots=list(day=c(0, 365)),family=Gamma(link="log"), method="REML",drop.unused.levels=FALSE)

MGM_train <- gam(STX ~ s(Days, bs="cc", k=10) + s(area, bs"re") + s(area, year, bs="re"),na.action = na.omit,data = STX_train, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGM_train, residuals = TRUE)
appraise(MGM_train)
summary(MGM_train) #ERROR
k.check(MGM_train)



# GS con curvas impares (BBE, BBF, PP)
MGSM_train <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year, bs="re"),na.action = na.omit,data = STX_train, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM_train, residuals = TRUE)
appraise(MGSM_train)
summary(MGSM_train) #R-sq.(adj) =  0.555   Deviance explained = 74.7%
k.check(MGSM_train) # Dasy, area no es significativo ver si se puede sacar

# GI con curvas impares (BBE, BBF, PP)
MGIM_train <- gam(STX ~ s(Days, bs="cc", k=10) + +s(area, bs="re") + s(Days, by=area, k=10, bs="cc") + s(area, year, bs="re"),na.action = na.omit,data = STX_train, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIM_train, residuals = TRUE) ## no grafica 
plot.gam(MGIM_train,residuals=T,pch=1,all.terms=T,seWithMean=T, pages=1)
appraise(MGIM_train) 
summary(MGIM_train)
k.check(MGIM_train) #ERROR 


#Checking residuals and qqplots for GAM fits

#qqplot, using gratia's qq_plot function, with simulated confidence intervals
pltG <- qq_plot(MGM_train, method = "simulate")+
  labs(subtitle = NULL, title=NULL)
pltGS <- qq_plot(MGSM_train, method = "simulate")+
  labs(subtitle = NULL, title=NULL, y=NULL)
pltGI <- qq_plot(MGIM_train, method = "simulate")+
  labs(subtitle = NULL, title=NULL, y=NULL)

plot_grid(pltG, pltGS,pltGI, 
          ncol = 3, 
          align = "hv", 
          axis = "lrtb",labels=c("a","b","c"))


plot(pltGS)

#Create synthetic data to use to compare predictions (del data train creo un subset para usar predict)
STX_plot_data <- expand.grid(Days = 1:259, 
                              area = factor(levels(STX_train$area)),
                              year = 2009)
#VER ESTO ESTA MAL PLANTEADO!!!!!

#extract predicted values and standard errors for both models. the 
#exclude ="s(lake,year_f)" term indicates that predictions should be made 
#excluding the effect of the lake-by-year random effect (effectively making
#predictions averaging over year-lake  effects).

STX_modG_fit <- predict(MGM_train, 
                         newdata = STX_plot_data, 
                         se.fit = TRUE, 
                         exclude = "s(area,year)")
STX_modGS_fit <- predict(MGSM_train, 
                          newdata = STX_plot_data, 
                          se.fit = TRUE, 
                          exclude = "s(area,year)")
STX_modGI_fit <- predict(MGIM_train, 
                          newdata = daph_plot_data, 
                          se.fit = TRUE, 
                          exclude = "s(area,year)")


STX_plot_data$modG_fit <- as.numeric(STX_modG_fit$fit)
STX_plot_data$modGS_fit <- as.numeric(STX_modGS_fit$fit)
STX_plot_data$modGI_fit <- as.numeric(STX_modGI_fit$fit)

STX_plot_data <- gather(STX_plot_data, 
                         key = model, 
                         value = fit, 
                         modGS_fit)

STX_plot_data <- mutate(STX_plot_data, 
                        se = c(as.numeric(STX_modGS_fit$se.fit),
                        upper = exp(fit + (2 * se)),
                        lower = exp(fit - (2 * se)))
                        
                         
                                    
STX_plot_model_labels = paste("Model", c("G","GS","GI"))
STX_plot_model_labels = factor(STX_plot_model_labels, 
                                levels= STX_plot_model_labels)

STX_plot <- ggplot(STX_plot_data, aes(x=Days))+
  facet_wrap(~area, nrow = 3)+
  geom_point(data= STX_train, aes(x = Days, y = STX), size=0.06)+
  geom_point(data= STX_test, aes(x = Days, y = STX),size=0.06,col="grey")+
  geom_line(aes(x = Days, y = fit, colour = model))
 

STX_plot
















































#----------------------------------------------#--------------------------------#-----------------#
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

#model0 <- gam(?g_STX ~ s(DAYS) + s(Tac_10D, k=25) + s(Vac_10D, k=35), data = DETOX3, family=Gamma (link="log"),method = "REML") 
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
#model0 <- gam(?g_STX ~ s(DAYS) + s(Tac_10D, k=25) + s(Vac_10D, k=35), data = DETOX3, family=Gamma (link="log"),method = "REML") 


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



  
    

