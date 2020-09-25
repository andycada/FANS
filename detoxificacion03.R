# Detoxificacion

# Paquetes
library(readxl)
library(mgcv)
require(gratia)
require(ggplot2)
library(tidyverse)
require(lubridate)

DETOX2 <- read_excel("Data/DETOX-filtrado.xlsx")

str(DETOX2)
DETOX2$area<-as.factor(DETOX2$area)
DETOX2$organism<-as.factor(DETOX2$organism)
DETOX2$year<-as.factor(DETOX2$year)## year-F tiene q ser factor para modelo estacional

##############------ Modelos jerarquicos ---------------------------####

# MEJILLON (M. edulis) en areas (BBF, BBE, PP)---------------------#
theme_set(theme_bw())
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" )

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
model4GIM <- gam(STX ~ s(Days,k=10, m=2, bs="cc") + s(Days,by=area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma(link="log"), method="REML")
draw(model4GIM,residuals=T) 

# Plot (prediccion) 
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model4GIM,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))

# CHOLGA (A. ater)en areas (BB, BF)-------------------------------#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )

## GS (Cholga)
model6GSA<- gam(STX ~ s(Days, k=10, m=2) + s(Days,area, k=10, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model6GSA,residuals=T) 

## GI (Cholga)
model4GIA <- gam(STX ~ s(Days,by = area) + s(Days,area, k=10, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model4GIA,residuals=T) 

# Plot (prediccion) 
pred <- tibble(Days=rep(seq(from=min(DETOX2A$Days), to=max(DETOX2A$Days)),3), area =rep( unique(DETOX2A$area),each=max(DETOX2A$Days)+1) )
p1<- predict(model4GIA,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIA)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX), shape=21,size=0.8) +  theme_bw() + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))


# BBE 2 Organismos para 1 area ------------------------------------#
DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )

## GS
model6GSBBE<- gam(STX ~ s(Days, k=10, m=2) + s(Days, organism,m=2, k=10, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model6GSBBE,residuals=T) 

## GI
model4GIBBE <- gam(STX ~ s(Days,by = organism) + s(Days, organism,m=2, k=10, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model4GIBBE,residuals=T) 

###############------ Modelos jerarquicos (C/estacionalidad) ---------------------------####

# Agregar año de inicio 

require(lubridate)
dd <- DETOX2 %>% group_by(area,organism,codigo) %>% summarise(year_ini=min(lubridate::year(Date)))

DETOX2 <- inner_join(DETOX2,dd) %>% mutate(year_ini=as.factor(year_ini))

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

## I (sin smoother global)

MGIM <- gam(STX ~ s(Days, by=area, k=10, bs="cc") + s(area, bs="re") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)


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
