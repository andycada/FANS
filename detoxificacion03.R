# Detoxificacion

# Paquetes
library(readxl)
library(mgcv)
require(gratia)

require(ggplot2)

library(tidyverse)
require(lubridate)

DETOX2 <- read_excel("Data/DETOX-filtrado.xlsx")

names(DETOX2)
str(DETOX2)
DETOX2$area<-as.factor(DETOX2$area)
DETOX2$organism<-as.factor(DETOX2$organism)
DETOX2$year<-as.factor(DETOX2$year)## year-F tiene q ser factor para modelo estacional

##############------ Modelos jerarquicos ---------------------------####

## GS (Global)
model6GS<- gam(STX ~ s(Days, k=10, m=2) + s(Days,area, k=10, m=2,bs="fs") + s(Days, organism,m=2, k=10, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML")
draw(model6GS,residuals=T) 

## GI (Global)
model4GS <- gam(STX ~ s(Days, k=10, m=2) + s(Days,by = area, k=10, m=1,bs="fs") + s(Days, by=organism,m=1, k=10, bs="fs"), data = DETOX2,family=Gamma (link="log"), method="REML")
draw(model4GS,residuals=T)


# MEJILLON (M. edulis) en areas (BBF, BBE, PP)---------------------#
theme_set(theme_bw())
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" )
DETOX2M %>% count(area,codigo) 

##  GS (Mejillon)
model6GSM<- gam(STX ~ s(Days, k=10, m=2, bs="cc") + s(Days,area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma (link="log"), method="REML")
draw(model6GSM,residuals=T) 

# Plot (prediccion) 
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model6GSM,newdata=pred, se.fit = TRUE)
ilink <- family(model6GSM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area)

##  GI (Mejillon)
model4GSM <- gam(STX ~ s(Days,k=10, m=2, bs="cc") + s(Days,by=area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma(link="log"), method="REML")
draw(model4GSM,residuals=T) 
appraise(model4GSM) 

# Plot (prediccion) 
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model4GSM,newdata=pred, se.fit = TRUE)
ilink <- family(model4GSM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))

# CHOLGA (A. ater)en areas (BB, BF)-------------------------------#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )

## GS (Cholga)
model6GSA<- gam(STX ~ s(Days, k=5, m=2) + s(Days,area, k=5, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model6GSA,residuals=T) 

## GI (Cholga)
model4GSA <- gam(STX ~ s(Days,by = area) + s(Days,area, k=5, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model4GSA,residuals=T) 

# BBE 2 Organismos para 1 area ------------------------------------#
DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )

## GS
model6GSBBE<- gam(STX ~ s(Days, k=5, m=2) + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model6GSBBE,residuals=T) 

## GI
model4GSBBE <- gam(STX ~ s(Days,by = organism) + s(Days, organism,m=2, k=5, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model4GSBBE,residuals=T) 

###############------ Modelos jerarquicos (C/estacionalidad) ---------------------------####

# Agregar año de inicio 

require(lubridate)
dd <- DETOX2 %>% group_by(area,organism,codigo) %>% summarise(year_ini=min(lubridate::year(Date)))

DETOX2 <- inner_join(DETOX2,dd) %>% mutate(year_ini=as.factor(year_ini))

## GS (Global)

MGS <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGS, residuals = TRUE)

## GI (global)
MGI <- gam(STX ~ s(Days, bs="cc", k=10) + s(area, bs="re") + s(Days, by=area, k=10, bs="cc") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGI, residuals = TRUE)

# MEJILLON (M. edulis) en areas (BBF, BBE, PP)---------------------#
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) 
# Si eliminamos los que tienen pocos datos 
#DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) %>% group_by(year,area) %>% filter( n()>10) %>% ungroup()

## GS (Mejillon)
MGSM <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM, residuals = TRUE)

# Plot (prediccion) 
#
pred <-distinct(DETOX2M, area,year_ini) %>% group_by(area,year_ini) %>% do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2M$Days))))
p1<- predict(MGSM,newdata=pred, se.fit = TRUE) 
ilink <- family(MGSM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 

## GI (mejillon) 
# No hay suficientes datos 
#
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" )
MGIM <- gam(STX ~ s(Days, bs="cc", k=10) + s(area, bs="re") + s(Days, by=area, k=10, bs="cc") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)

# # Plot (prediccion)  
#
pred <-distinct(DETOX2M, area,year) %>% group_by(area,year) %>% do(tibble(area=.$area,year=.$year,Days=0:(max(DETOX2M$Days))))
p1<- predict(MGIM,newdata=pred, se.fit = TRUE) 
ilink <- family(MGIM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year~area) 



## S (sin smoother global)

MSM <- gam(STX ~  s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM, residuals = TRUE)

# Plot (prediccion) 
#
pred <-distinct(DETOX2M, area,year_ini) %>% group_by(area,year_ini) %>% do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2M$Days))))
p1<- predict(MSM,newdata=pred, se.fit = TRUE) 
ilink <- family(MSM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 

#Da un poco mejor sin global pero no hay mucha diferencia

# CHOLGA (A. ater) en areas (BBB, BBE)---------------------#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
max(DETOX2A$Days)

## GS (cholga)
MGSA <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 218)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSA, residuals = TRUE)

## GI (Cholga)
MGIA <- gam(STX ~ s(Days, bs="cc", k=10) + +s(area, bs="re") + s(Days, by=area, k=10, bs="cc") + s(area, year, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 218)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIA, residuals = TRUE)

## BBE 2 Organismos para 1 area ------------------------------------#
DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )
max(DETOX2BBE$Days)

## GS (BBE)
MGSBBE <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, organism, k=10, bs="fs", xt=list(bs="cc"))+ s(organism, year, bs="re"),na.action = na.omit,data = DETOX2BBE, knots=list(Days=c(0, 259)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSBBE, residuals = TRUE)

## GI(BBE)
MGIBBE <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, bs="re") + s(Days, by=organism, k=10, bs="cc")+ s(organism, year, bs="re"),na.action = na.omit,data = DETOX2BBE, knots=list(Days=c(0, 259)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIBBE, residuals = TRUE)
