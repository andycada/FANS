
  #  Eventos toxicos (2005-2017)
  
  # Dias: duracion de la veda, T desde q el mejillon supera los 80 ?g STX/100 g tejido, hasta que tiene valores por debajo de ese umbral
  # Conc: STXmax concentracion maxima de toxina (STXmax) alcanzada duante el evento toxico
  # area: BBE (Bahia Brown Entrada), BBF (Bahia Brown Fondo), BBB (BB Bajo), PP (Punta Parana)	
  # organism: Mejillon (M. edulis), Cholga  (A. ater)


# Paquetes
library(readxl)
library(mgcv)
require(gratia)
require(ggplot2)
library(tidyverse)
require(lubridate)
library(broom)
 
  
  # Exploracion para ver que se puede usar para ajustar los datos de toxinas
  
  # Leer los datos y plotearlos

require(tidyverse)
theme_set(theme_bw())
require(drc)
exp <- read_excel("Data/DETOX-Filt-exp.xlsx")
ggplot(exp, aes(x = Days, y = STX) ) +
geom_line() + theme_bw() + geom_smooth(se=FALSE)




#MEJILLON (M. edulis) en areas (BBF, BBE, PP)---------------------#
  
  theme_set(theme_bw())
expM<- exp %>% filter(organism == "M. edulis" ) 


# exponencial negativa 

fit <- nls(STX ~ SSasymp(Days, yf, y0, log_alpha), data = expM)

qplot(Days, STX, data = augment(fit)) + geom_line(aes(y = .fitted))
summary(fit)

# Cholga
theme_set(theme_bw())
expA<- exp %>% filter(organism == "A. ater" ) 

fit <- nls(STX ~ SSasymp(Days, yf, y0, log_alpha), data = expA)

qplot(Days, STX, data = augment(fit)) + geom_line(aes(y = .fitted))











#
# f(x) = d / (1+exp(b(x - e)))
#
model1 <- drm(STX ~ Days, fct=lgaussian() , data = expM)
plot(model1)
summary(model1)
model2 <- drm(STX ~ Days, fct=multi2() , data = expM)
plot(model2)
AIC(model1,model2)



## Usar una doble exponencial   ##FALLAS DE COVERGENCIA REVISAR###

 # doble exponencial conocida como distribuci?n de Laplace

$$f(t) = a \exp \left(- \frac{|t-c|}{b} \right)$$ 
  Donde $a$ mide la altura del pico, $b$ la pendiente y $c$ la ubicaci?n

#Ejemplos con mas de un set de datos y ggplot <https://labangelov.wordpress.com/2017/05/17/enzyme-kinetics-with-r/>
  
require(propagate)
expM[which.max(expM$STX),]
#
# Laplace
#
nonlin <- function(t, a, b, c) { a * (exp(-(abs(t-c) / b))) }
nlsfit1 <- nls(STX ~ nonlin(Days, a, b, c), data=expM, start=list(a=55, b=2, c=75),algorithm = "port")
summary(nlsfit1)
nlsfit1 <- nls(STX ~ nonlin(Days, a, b, c), data=expM, start=list(a=3500, b=5, c=75),algorithm = "port")
pred <- predict(nlsfit1, newdata = data.frame(Days = expM$Days))
#pred$summary
df <- cbind(expM, pred)
ggplot(df) + geom_point(aes(x = Days, y = STX), alpha = 0.6) +
  geom_line(aes(x=Days, y = pred), color = "red") + theme_bw()
#
# Double exponencial con diferentes pendientes
#
nonlin <- function(t, a, bl, br, c,d) {
  ifelse(t < c,d + (a * exp((t-c) / bl)), d +( a * exp(-(t-c) / br)))
}
nlsfit2 <- nls(Conc ~ nonlin(Dias, a, bl, br, c,d), data=corM, start=list(a=3000, bl=3, br=2, c=75,d=100),algorithm = "port")
nlsfit2
AIC(nlsfit1,nlsfit2)
pred <- predict(nlsfit2, newdata = data.frame(Dias = 0:max(corM$Dias)))
df2 <- data.frame(Dias = 0:max(corM$Dias), pred=pred)
ggplot(cor) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(data=df2,aes(x=Dias, y = pred), color = "red") + scale_y_log10()
#
# Tranformando el tiempo a logaritmo 
#
nonlin <- function(t, a, b, c) { a * (exp(-(abs(log(t)-c) / b))) }
nlsfit <- nls(Conc ~ nonlin(Dias, a, b, c), data=corM, start=list(a=2500, b=1, c=4))
AIC(nlsfit1,nlsfit2,nlsfit)
pred <- predict(nlsfit, newdata = data.frame(Dias = corM$Dias))
df3 <- cbind(corM, pred)
ggplot(df3) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(aes(x=Dias, y = pred), color = "red") + theme_bw()
#
# Usando una LogNormal
#
nonlin <- function(t, a, b, c) { a * (exp(-(log(t)-c)^2 / b)) }
nlsfit4 <- nls(Conc ~ nonlin(Dias, a, b, c), data=corM, start=list(a=3500, b=1.2, c=4.1),algorithm = "port")
AIC(nlsfit1,nlsfit2,nlsfit,nlsfit4)
pred <- predict(nlsfit4, newdata = data.frame(Dias = corM$Dias))
df4 <- cbind(corM, pred)
ggplot(df4) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
geom_line(aes(x=Dias, y = pred), color = "red") 
#
# Gumbel distribution
#
nonlin <- function(t, a, b, c) { (a/b) * exp( -(t-c)/b - exp(-(t-c)/b) ) }
nlsfit5 <- nls(Conc ~ nonlin(Dias, a, b, c), data=corM, start=list(a=3500, b=50, c=75),algorithm = "port")
AIC(nlsfit1,nlsfit2,nlsfit,nlsfit4,nlsfit5)
pred <- predict(nlsfit5, newdata = data.frame(Dias = 0:max(corM$Dias)))
df4 <- data.frame(Dias = 0:max(corM$Dias), pred=pred)
ggplot(corM) + geom_point(aes(x = Dias, y = Conc), alpha = 0.6) +
  geom_line(data=df4,aes(x=Dias, y = pred), color = "red") 
#
#

