# Detoxificacion
#
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
#DETOX2$year<-as.factor(DETOX2$year)## year-F tiene q ser factor para modelo estacional

##############------ Modelos jerarquicos ---------------------------####
#
# MEJILLON (M. edulis) en areas (BBF, BBE, PP)---------------------#
#
theme_set(theme_bw())
DETOX2M<- DETOX2 %>% filter(organism == "M. edulis" ) %>% group_by(codigo) %>% mutate(year_ini=min(year))
ggplot(DETOX2M, aes(x = Days, y = STX,color=area)) + xlab("") + facet_wrap( area ~ year_ini,scale="free_y") + geom_point() + geom_smooth(se=FALSE) + scale_color_brewer(palette="Dark2",guide=NULL) 


#
# AHora estoy viendo claramente lo que pasa las curvas son muy distintas para cada año!!!!!!!!!!! pero parecidas entre si por año
# claramente las condiciones ambientales son parecidas en cada año
#


##  GS (Mejillon)

model6GSM<- gam(STX ~ s(Days, k=10, m=2, bs="tp") + s(Days,area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma (link="log"), method="REML")
draw(model6GSM,residuals=T) 

# GS Plot (prediccion) 
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model6GSM,newdata=pred, se.fit = TRUE)
ilink <- family(model6GSM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area)
ggsave("Figures/Medulis_GS_gam.png",width=6,height=4,units="in",dpi=600)

##  GI (Mejillon)
#
model4GIM <- gam(STX ~ s(Days,k=10, m=2, bs="tp") + s(Days,by=area, k=10, m=1,bs="fs"), data = DETOX2M,family=Gamma(link="log"), method="REML")
draw(model4GIM,residuals=T) 

AIC(model4GIM,model6GSM)

#  GI Plot (prediccion) 
#
pred <- tibble(Days=rep(seq(from=min(DETOX2M$Days), to=max(DETOX2M$Days)),3), area =rep( unique(DETOX2M$area),each=max(DETOX2M$Days)+1) )
p1<- predict(model4GIM,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIM)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))
ggsave("Figures/Medulis_GI_gam.png",width=6,height=4,units="in",dpi=600)

#
# CHOLGA (A. ater)en areas (BB, BF)-------------------------------#
#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
ggplot(DETOX2A, aes(x = Days, y = STX,color=area)) + xlab("") + facet_grid( area ~ organism,scale="free_y") + geom_point() + geom_smooth(se=FALSE) 

## GS (Cholga)
#
model6GSA<- gam(STX ~ s(Days, k=10, m=2) + s(Days,area, k=10, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model6GSA,residuals=T) 
summary(model6GSA)

## GI (Cholga)
#
model4GIA <- gam(STX ~ s(Days,by = area) + s(Days,area, k=10, m=2,bs="fs"), data = DETOX2A,family=Gamma (link="log"), method="REML")
draw(model4GIA,residuals=T) 
summary(model4GIA)
AIC(model6GSA,model4GIA)

# Plot (prediccion) 
#                                2 repeticiones porque hay 2 areas --------------->   unique(DETOX2A$area) te dice que hay 2           
#                                                                         |
pred <- tibble(Days= 0:30, area =rep( "BBB",each=31) )
pred <- bind_rows(pred, tibble(Days=0:259, area =rep( "BBE",each=260) ))

p1<- predict(model6GSA,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIA)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))
ggsave("Figures/Aater_GS_gam.png",width=6,height=4,units="in",dpi=600)

# GI Plot prediccion) 
pred <- tibble(Days=rep(seq(from=min(DETOX2A$Days), to=max(DETOX2A$Days)),2), area =rep( unique(DETOX2A$area),each=max(DETOX2A$Days)+1) )
p1<- predict(model4GIA,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIA)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~area) + coord_cartesian(ylim=c(0,4000))
ggsave("Figures/Aater_GI_gam.png",width=6,height=4,units="in",dpi=600)

#
# 
#

# BBE 2 Organismos para 1 area ------------------------------------#
DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )
ggplot(DETOX2BBE, aes(x = Days, y = STX,color=organism)) + xlab("") + facet_wrap( area ~ year_ini,scale="free_y") + geom_point() + geom_smooth(se=FALSE) + scale_color_brewer(palette="Dark2",guide=NULL) 

## GS
model6GSBBE<- gam(STX ~ s(Days, k=10, m=2) + s(Days, organism,m=2, k=10, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model6GSBBE,residuals=T) 

pred <- tibble(Days=rep(seq(from=min(DETOX2BBE$Days), to=max(DETOX2BBE$Days)),2), organism =rep( unique(DETOX2BBE$organism),each=max(DETOX2BBE$Days)+1) )
p1<- predict(model6GSBBE,newdata=pred, se.fit = TRUE)
ilink <- family(model6GSBBE)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2BBE, aes(x = Days, y = STX,color=organism), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~organism) + coord_cartesian(ylim=c(0,4000))
ggsave("Figures/BBE_GS_gam.png",width=6,height=4,units="in",dpi=600)


## GI
model4GIBBE <- gam(STX ~ s(Days,by = organism) + s(Days, organism,m=2, k=10, bs="fs"), data = DETOX2BBE,family=Gamma (link="log"), method="REML")
draw(model4GIBBE,residuals=T) 

AIC(model4GIBBE,model6GSBBE)

# GI Plot prediccion) 
#
pred <- tibble(Days=rep(seq(from=min(DETOX2BBE$Days), to=max(DETOX2BBE$Days)),2), organism =rep( unique(DETOX2BBE$organism),each=max(DETOX2BBE$Days)+1) )
p1<- predict(model4GIBBE,newdata=pred, se.fit = TRUE)
ilink <- family(model4GIBBE)$linkinv 

pred <- pred %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2BBE, aes(x = Days, y = STX,color=organism), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +
  geom_line(data=pred,aes( x= Days, y= fit )) +  
  geom_ribbon(data=pred,aes(x=Days,ymin=lcl,ymax=ucl),alpha=0.3) + facet_wrap(~organism) + coord_cartesian(ylim=c(0,4000))
ggsave("Figures/BBE_GI_gam.png",width=6,height=4,units="in",dpi=600)


#
###############------ Modelos jerarquicos (C/estacionalidad) ---------------------------####
#
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
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) + 
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 
ggsave("Figures/Medulis_GS_byYear.png",width=6,height=4,units="in",dpi=600)

#
# GS Siempre hace el mismo patron por AREA
# 


## GI (mejillon) 
#
# No hay suficientes datos tenes solo una curva para cada caso 
#
MGIM <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, by=area,  bs="cc") + s(area, bs="re") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)

## S (sin smoother global)

MSM <- gam(STX ~  s(Days, area, k=10, bs="fs", xt=list(bs="cc"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSM, residuals = TRUE)

# Plot (prediccion) 
#
pred <-distinct(DETOX2M, area,year_ini) %>% group_by(area,year_ini) %>% do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2M$Days))))
p1<- predict(MSM,newdata=pred, se.fit = TRUE) 
ilink <- family(MSM)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2M, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +  
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 
ggsave("Figures/Medulis_S_byYear.png",width=6,height=4,units="in",dpi=600)

# Da un poco mejor sin global pero no hay mucha diferencia

## I (sin smoother global)
# No hay datos
MGIM <- gam(STX ~ s(Days, by=area, k=10, bs="cc") + s(area, bs="re") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2M, knots=list(Days=c(0, 365)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)


# CHOLGA (A. ater) en areas (BBB, BBE)---------------------#
DETOX2A<- DETOX2 %>% filter(organism == "A. ater" )
max(DETOX2A$Days)
ggplot(DETOX2A, aes(x = Days, y = STX,color=area)) + xlab("") + facet_wrap( area ~ year_ini,scale="free_y") + geom_point() + geom_smooth(se=FALSE) + scale_color_brewer(palette="Dark2",guide=NULL) 

## GS (cholga)
MGSA <- gam(STX ~ s(Days, bs="tp", k=20) + s(Days, area, k=10, bs="fs", xt=list(bs="tp"))+ s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 218)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSA, residuals = TRUE)
summary(MGSA)
## GI (Cholga)
MGIA <- gam(STX ~ s(Days, bs="tp", k=10) + +s(area, bs="re") + s(Days, by=area, k=10, bs="tp") + s(area, year_ini, bs="re"),na.action = na.omit,data = DETOX2A, knots=list(Days=c(0, 218)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIA, residuals = TRUE)

# GS Plot (Cholga) 
#
pred <-distinct(DETOX2A, area,year_ini) %>% group_by(area,year_ini) %>% 
  do(tibble(area=.$area,year_ini=.$year_ini,Days=0:(max(DETOX2A$Days))))

p1<- predict(MGSA,newdata=pred, se.fit = TRUE) 
ilink <- family(MGSA)$linkinv 

pred <- pred %>% ungroup() %>% mutate(fit=p1$fit,se.fit =p1$se.fit, ucl=ilink(fit + (1.96 * se.fit)), lcl = ilink(fit - (1.96 * se.fit)), fit=ilink(fit)  )
ggplot() + geom_point(data=DETOX2A, aes(x = Days, y = STX,color=area), shape=21,size=0.8) +  
  scale_color_brewer(palette="Dark2",guide=NULL) +  
  geom_line(data=pred,aes( x= Days, y= fit )) +  scale_y_log10() +
  facet_wrap(year_ini~area) 
ggsave("Figures/Aater_GS_byYear.png",width=6,height=4,units="in",dpi=600)





## BBE 2 Organismos para 1 area ------------------------------------#
DETOX2BBE<- DETOX2 %>% filter(area == "BBE" )
max(DETOX2BBE$Days)

## GS (BBE)
MGSBBE <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, organism, k=10, bs="fs", xt=list(bs="cc"))+ s(organism, year, bs="re"),na.action = na.omit,data = DETOX2BBE, knots=list(Days=c(0, 259)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGSBBE, residuals = TRUE)

## GI(BBE)
MGIBBE <- gam(STX ~ s(Days, bs="cc", k=10) + s(Days, bs="re") + s(Days, by=organism, k=10, bs="cc")+ s(organism, year, bs="re"),na.action = na.omit,data = DETOX2BBE, knots=list(Days=c(0, 259)),family=Gamma (link="log"), method="REML", drop.unused.levels=FALSE)
draw(MGIBBE, residuals = TRUE)

