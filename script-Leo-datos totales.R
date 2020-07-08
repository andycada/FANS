
  title: "ToxicAlgae"
author: "Leonardo A. Saravia"


  # Cargo archivo de Eventos toxicos (2005-2017)
  
  # Dias: duracion de la veda, T desde q el mejillon supera los 80 µg STX/100 g tejido, hasta que tiene valores por debajo de ese umbral
  # Conc: STXmax concentracion maxima de toxina (STXmax) alcanzada duante el evento toxico
  # area: BBE (Bahia Brown Entrada), BBF (Bahia Brown Fondo), BBB (BB Bajo), PP (Punta Parana)	
  # organism: Mejillon (M. edulis), Cholga  (A. ater)

    library(readxl)

 
  
  # Exploracion para ver que se puede usar para ajustar los datos de toxinas
  
  # Leer los datos y plotearlos

require(tidyverse)
theme_set(theme_bw())
require(drc)
cor <- read_excel("Data/DataTot.xlsx")
ggplot(cor, aes(x = Dias, y = Conc) ) +
geom_line() + theme_bw() + geom_smooth(se=FALSE)


## Usar el paquete `drc` para ajustar la curva

#
# f(x) = d / (1+exp(b(x - e)))
#
model1 <- drm(Conc ~ Dias, fct=lgaussian() , data = cor)
plot(model1)
summary(model1)
model2 <- drm(Conc ~ Dias, fct=multi2() , data = cor)
plot(model2)
AIC(model1,model2)


## Usar GAM para ajustar la curva

library(mgcv)
require(gratia)
library(draw)

gam_mod <- gam(Conc ~ s(Dias,k=20), data = cor,family=Gamma(link="log"), method="REML")
# Plot the results
summary(gam_mod)
draw(gam_mod,residuals=T) #dibuja el GAMs y los puntos son los residuales
appraise(gam_mod) # chequea el modelo, residuales, etc
cor <- add_fitted(cor,gam_mod,value="pred")
ggplot(cor) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(aes(x=Dias, y = pred), color = "red") + scale_y_log10() # dibuja los datos y le agrega la curva en rojo que predice el modelo

gam_mod1 <- gam(Conc ~ s(Dias,k=20), data = cor, method="REML")
summary(gam_mod1)
draw(gam_mod1,residuals=TRUE)
appraise(gam_mod1)
cor <- add_fitted(cor,gam_mod1,value="pred1")
ggplot(cor) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(aes(x=Dias, y = pred), color = "red") +
  geom_line(aes(x=Dias, y = pred1), color = "brown") + scale_y_log10()
AIC(gam_mod,gam_mod1) 


## Usar una doble exponencial   ##FALLAS DE COVERGENCIA REVISAR###

 # doble exponencial conocida como distribución de Laplace

$$f(t) = a \exp \left(- \frac{|t-c|}{b} \right)$$ 
  Donde $a$ mide la altura del pico, $b$ la pendiente y $c$ la ubicación

#Ejemplos con mas de un set de datos y ggplot <https://labangelov.wordpress.com/2017/05/17/enzyme-kinetics-with-r/>
  
require(propagate)
cor[which.max(cor$Conc),]
#
# Laplace
#
nonlin <- function(t, a, b, c) { a * (exp(-(abs(t-c) / b))) }
nlsfit1 <- nls(Conc ~ nonlin(Dias, a, b, c), data=cor, start=list(a=55, b=2, c=75),algorithm = "port")
summary(nlsfit1)
nlsfit1 <- nls(Conc ~ nonlin(Dias, a, b, c), data=cor, start=list(a=3500, b=5, c=75),algorithm = "port")
pred <- predict(nlsfit1, newdata = data.frame(Dias = cor$Dias))
#pred$summary
df <- cbind(cor, pred)
ggplot(df) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(aes(x=Dias, y = pred), color = "red") + theme_bw()
#
# Double exponencial con diferentes pendientes
#
nonlin <- function(t, a, bl, br, c,d) {
  ifelse(t < c,d + (a * exp((t-c) / bl)), d +( a * exp(-(t-c) / br)))
}
nlsfit2 <- nls(Conc ~ nonlin(Dias, a, bl, br, c,d), data=cor, start=list(a=3000, bl=3, br=2, c=75,d=100),algorithm = "port")
nlsfit2
AIC(nlsfit1,nlsfit2)
pred <- predict(nlsfit2, newdata = data.frame(Dias = 0:max(cor$Dias)))
df2 <- data.frame(Dias = 0:max(cor$Dias), pred=pred)
ggplot(cor) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(data=df2,aes(x=Dias, y = pred), color = "red") + scale_y_log10()
#
# Tranformando el tiempo a logaritmo 
#
nonlin <- function(t, a, b, c) { a * (exp(-(abs(log(t)-c) / b))) }
nlsfit <- nls(Conc ~ nonlin(Dias, a, b, c), data=cor, start=list(a=2500, b=1, c=4))
AIC(nlsfit1,nlsfit2,nlsfit)
pred <- predict(nlsfit, newdata = data.frame(Dias = cor$Dias))
df3 <- cbind(cor, pred)
ggplot(df3) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(aes(x=Dias, y = pred), color = "red") + theme_bw()
#
# Usando una LogNormal
#
nonlin <- function(t, a, b, c) { a * (exp(-(log(t)-c)^2 / b)) }
nlsfit4 <- nls(Conc ~ nonlin(Dias, a, b, c), data=cor, start=list(a=3500, b=1.2, c=4.1),algorithm = "port")
AIC(nlsfit1,nlsfit2,nlsfit,nlsfit4)
pred <- predict(nlsfit4, newdata = data.frame(Dias = cor$Dias))
df4 <- cbind(cor, pred)
ggplot(df4) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
geom_line(aes(x=Dias, y = pred), color = "red") 
#
# Gumbel distribution
#
nonlin <- function(t, a, b, c) { (a/b) * exp( -(t-c)/b - exp(-(t-c)/b) ) }
nlsfit5 <- nls(Conc ~ nonlin(Dias, a, b, c), data=cor, start=list(a=3500, b=50, c=75),algorithm = "port")
AIC(nlsfit1,nlsfit2,nlsfit,nlsfit4,nlsfit5)
pred <- predict(nlsfit5, newdata = data.frame(Dias = 0:max(cor$Dias)))
df4 <- data.frame(Dias = 0:max(cor$Dias), pred=pred)
ggplot(cor) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(data=df4,aes(x=Dias, y = pred), color = "red") 
#
#

