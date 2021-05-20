
## Modelling PSP outbreaks dynamics 

# The codes for sampling areas are PC=Potter Cove AB= Admiral Bay

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
#DETOX2$year<-as.factor(DETOX2$year)## year-F tiene q ser factor para modelo estacional


## 1. Modelling Variability among areas

## 1.1. Blue mussel (M.edulis) data

theme_set(theme_bw())
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) %>% group_by(codigo) %>% mutate(year_ini=min(year))
ggplot(DETOX2M, aes(x = Days, y = STX,color=area)) + xlab("") + facet_wrap( area ~ year_ini,scale="free_y") + geom_point() + geom_smooth(se=FALSE) + scale_color_brewer(palette="Dark2",guide=NULL) 



# Model GS: SACANDO EL TERMINO DEL ERROR SI FUNCIONA, puse este en el paper 
model6GSM<- gam(STX ~ s(Days, k=10, m=2, bs="tp") + s(Days,area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma (link="log"), method="REML")
draw(model6GSM,residuals=T) 
appraise(model6GSM) 
summary(model6GSM) #R-sq.(adj) =  0.153   Deviance explained =   49.1%

# Model GI
#
model4GIM <- gam(STX ~ s(Days,k=10, m=2, bs="tp") + s(Days,by=area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma(link="log"), method="REML")
draw(model4GIM,residuals=T) 
appraise(model4GIM) 
summary(model4GIM)


### PLOT (PREDICTION)

## Model GS
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model6GSM,newdata=pred, se.fit = TRUE)
ilink <- family(model6GSM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area)


## Model GI
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model4GIM,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))

# MODEL SELECTION

AIC(model6GSM,model4GIM) 

## 1.2. Magellan mussel (A.ater) data

DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
ggplot(DETOX2A, aes(x = Days, y = STX,color=area)) + xlab("") + facet_grid( area ~ organism,scale="free_y") + geom_point() + geom_smooth(se=FALSE) 

## Model GS 
model6GSA<- gam(STX ~ s(Days, k=10, m=2) + s(Days,area, k=10, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model6GSA,residuals=T) 
summary(model6GSA)

# Model G (only global smoother)
model6GA<- gam(STX ~ s(Days, k=10, m=2), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model6GA,residuals=T) 
summary(model6GA)

# Model S (without global smoother)
model6SA<- gam(STX ~ s(Days,area, k=10, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model6SA,residuals=T) 
summary(model6SA)

# Model GI 
model4GIA <- gam(STX ~ s(Days,by = area) + s(Days,area, k=10, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model4GIA,residuals=T) 
summary(model4GIA)

### PLOT (PREDICTION)

## Model GS 
pred <- tibble(Days= 0:30, area =rep( "BB-B",each=31) )
pred <- bind_rows(pred, tibble(Days=0:259, area =rep( "BB-E",each=260) ))# 2 repeticiones porque hay 2 areas unique(DETOX2A$area) te dice que hay 2

p1<- predict(model6GSA,newdata=pred, se.fit = TRUE)
ilink <- family(model6GSA)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))

## Model S (PAPER)                                                                     |
pred <- tibble(Days= 0:30, area =rep( "BB-B",each=31) )
pred <- bind_rows(pred, tibble(Days=0:259, area =rep( "BB-E",each=260) ))

p1<- predict(model6SA,newdata=pred, se.fit = TRUE)
ilink <- family(model6SA)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))


## Model GI 
pred <- tibble(Days=rep(seq(from=min(DETOX2A$Days), to=max(DETOX2A$Days)),2), area =rep( unique(DETOX2A$area),each=max(DETOX2A$Days)+1) )
p1<- predict(model4GIA,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIA)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))

# MODEL SELECTION

AIC(model6GSA,model6GA,model6SA,model4GIA)

## 2. Modelling interspecific Variability 

## BB-E data

DETOX2BBE<- DETOX2 %>% filter(area == "BB-E" )
ggplot(DETOX2BBE, aes(x = Days, y = STX,color=organism)) + xlab("") + facet_wrap( area ~ year_ini,scale="free_y") + geom_point() + geom_smooth(se=FALSE) + scale_color_brewer(palette="Dark2",guide=NULL) 

# Model GS
model6GSBBE<- gam(STX ~ s(Days, k=10, m=2) + s(Days, organism,m=2, k=10, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model6GSBBE,residuals=T) 


# Model I (without global smoother)
model4GIBBE <- gam(STX ~ s(Days,by = organism) + s(Days, organism,m=2, k=10, bs="fs") + s(organism, bs="re", k=12), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model4GIBBE,residuals=T) 
summary(model4GIBBE)


# Model GI **lo toma bien pero no grafica (PAPER)
model4GIBBE2<- gam(STX ~ s(Days, k=10, m=2, bs="tp") + s(Days, by=organism,m=1, k=10, bs="tp") + s(organism, bs="re", k=12), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model4GIBBE2,residuals=T) # no lo grafica y la prediccion es similar

### PLOT (PREDICTION)

## Model GS 
pred <- tibble(Days=rep(seq(from=min(DETOX2BBE$Days), to=max(DETOX2BBE$Days)),2), organism =rep( unique(DETOX2BBE$organism),each=max(DETOX2BBE$Days)+1) )
p1<- predict(model6GSBBE,newdata=pred, se.fit = TRUE)
ilink <- family(model6GSBBE)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2BBE, aes(x = Days, y = STX,color=organism), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~organism) + coord_cartesian(ylim=c(0,4000))


## Model I 
pred <- tibble(Days=rep(seq(from=min(DETOX2BBE$Days), to=max(DETOX2BBE$Days)),2), organism =rep( unique(DETOX2BBE$organism),each=max(DETOX2BBE$Days)+1) )
p1<- predict(model4GIBBE,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIBBE)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2BBE, aes(x = Days, y = STX,color=organism), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~organism) + coord_cartesian(ylim=c(0,4000))


## Model GI 
pred <- tibble(Days=rep(seq(from=min(DETOX2BBE$Days), to=max(DETOX2BBE$Days)),2), organism =rep( unique(DETOX2BBE$organism),each=max(DETOX2BBE$Days)+1) )
p1<- predict(model4GIBBE2,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIBBE2)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2BBE, aes(x = Days, y = STX,color=organism), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~organism) + coord_cartesian(ylim=c(0,4000))


# MODEL SELECTION

AIC(model6GSBBE,model4GIBBE,model4GIBBE2) 


## 3. Modelling seasonality  

# Agregar año de inicio 

require(lubridate)
dd <- DETOX2 %>% group_by(area,organism,codigo) %>% summarise(year_ini=min(lubridate::year(Date)))

DETOX2 <- inner_join(DETOX2,dd) %>% mutate(year_ini=as.factor(year_ini))

## 3.1. Blue mussel (M.edulis) data

DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) 
# Si eliminamos los que tienen pocos datos 
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) %>% group_by(year,area) %>% filter( n()>10) %>% ungroup()

# Model GS 
MGSM <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM, residuals = TRUE)
summary(MGSM)


# Model S 
MSM <- gam(STX ~  s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MSM, residuals = TRUE)
summary(MSM) # mejor este por deviance y AIC


### PLOT (PREDICTION)

## Model GS 
pred <-distinct(DETOX2M, area,year_ini) %>% group_by(area,year_ini) %>% do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2M$Days))))
p1<- predict(MGSM,newdata=pred, se.fit = TRUE) 
ilink <- family(MGSM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 


## Model S 
pred <-distinct(DETOX2M, area,year_ini) %>% group_by(area,year_ini) %>% do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2M$Days))))
p1<- predict(MSM,newdata=pred, se.fit = TRUE) 
ilink <- family(MSM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +  
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area)
ggsave("Figures/Medulis_S_byYear.png",width=6,height=4,units="in",dpi=600)

# MODEL SELECTION

AIC (MGSM,MSM) # Da un poco mejor MSM, sin global pero no hay mucha diferencia (es mejor me quedo cn este)


## 3.2. Magellan mussel (A.ater) data
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
max(DETOX2A$Days)
ggplot(DETOX2A, aes(x = Days, y = STX,color=area)) + xlab("") + facet_wrap( area ~ year_ini,scale="free_y") + geom_point() + geom_smooth(se=FALSE) + scale_color_brewer(palette="Dark2",guide=NULL) 

# Model GS 
MGSA <- gam(STX ~ s(Days, bs="tp", k=20) + s(Days, area, k=10, bs="fs", xt=list(bs="tp"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 218)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSA, residuals = TRUE)
summary(MGSA) # s(Days,area) No Sig

# Model S 
MSA <- gam(STX ~  s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM, residuals = TRUE)
summary(MSA) # todos Significativos



### PLOT (PREDICTION)

## Model GS 
pred <-distinct(DETOX2A, area,year_ini) %>% group_by(area,year_ini) %>% 
  do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2A$Days))))

p1<- predict(MGSA,newdata=pred, se.fit = TRUE) 
ilink <- family(MGSA)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +  
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 


## Model S
pred <-distinct(DETOX2A, area,year_ini) %>% group_by(area,year_ini) %>% 
  do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2A$Days))))

p1<- predict(MSA,newdata=pred, se.fit = TRUE) 
ilink <- family(MSA)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +  
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 

# MODEL SELECTION

AIC (MGSA,MSA)


