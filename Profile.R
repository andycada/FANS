
## The codes for sampling areas are 
# BB-B: Bahia Brown Bajo
# BB-E: Bahia Brown Entrada
# BB-F: Bahia Brown Fondo
# PP:  Punta Parana

library(readxl)
library("pracma")
library(tidyverse)

library("akima")
data <- read_excel("Data/Perfiles.xlsx") %>% rename( Temp = T)

dd <- data %>% group_by(Area,Depth) %>% summarise(mT = mean(Temp),n=n(),sdT=sd(Temp), hi = mT+sdT, lo=mT-sdT) 

ggplot(dd, aes(y = mT, x = Depth)) + labs (x = "Depth (m)",y = "Temperature (°C)") + scale_x_reverse() +geom_line(size = 1.5) +  geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.5) +
  facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + coord_flip()+theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) + scale_color_viridis_d()

ggplot(data, aes(y = Temp, x = Depth,color=Season)) + labs (x = "Depth (m)",y = "Temperature (°C)") + scale_x_reverse() +geom_point(size = 1.5) +  facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + coord_flip()  +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) + scale_color_viridis_d() + 
  geom_line(data=dd,aes(y=mT,x=Depth),color="red")



ggplot(data, aes(y = T, x = Depth,color=Season)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 1.5) + xlim(rev(c(0, 25))) + ylim(c(8,11)) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + geom_smooth(se=FALSE)  +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 

ggplot(data, aes(x = S, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Salinity (psu)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(data, aes(x = O2, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Oxigen (mg/L)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(data, aes(x = PH, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "pH") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 

ggplot(data, aes(x = T, y = Depth,color=Date)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + xlim(c(8,11)) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + geom_smooth(se=FALSE) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
# + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)
# facet_wrap(~ Area, nrow = 4, ncol = NULL,scale="free_y")
#+ geom_curve(se=T)







BE_T <- read_excel("Data/BE_Tmean.xlsx") 

plot(BE_T$BE1, BE_T$Depth,cex=0.5,col="red",ylim=rev(c(0, 20)), xlim=c(6,12))
points(BE_T$BE4, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE7, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE10, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE13, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE16, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE19, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE21, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE24, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE27, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE33, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))
points(BE_T$BE30, BE_T$Depth,cex=0.5,col="blue",ylim=rev(c(0, 20)))

lines(BE_T$T_mean, BE_T$Depth,cex=8,col="green",ylim=rev(c(0, 20)))
lines(Depth ~ Chlorophyll, data = chla, type = "l", col = "red", lwd = 2) 

BE <- data %>% filter(Area == "BE" )
plot(BE$Temp, BE$Depth,cex=0.5,col="red",ylim=rev(c(0, 25)))
plot(BE$Salinity, BE$Depth,cex=0.5,col="red",ylim=rev(c(0, 25)))


library(dplyr)
promedio <- group_by(BE, Depth) %>%
  summarise(
    count = n(),
    mean = mean(BE$Temp, na.rm = TRUE),
    sd = sd(BE$Temp, na.rm = TRUE),
    median = median(BE$Temp, na.rm = TRUE),
    IQR = IQR(BE$Temp, na.rm = TRUE)
  )

mean(BE, by)

BF <- data %>% filter(Area == "BF" )
plot(BF$Temperature, BF$Depth,cex=0.5,col="red",ylim=rev(c(0, 25)))
plot(BF$Salinity, BF$Depth,cex=0.5,col="red",ylim=rev(c(0, 25)))

PP<- data %>% filter(Area == "PP" )
plot(PP$Temperature, PP$Depth,cex=0.5,col="red",ylim=rev(c(0, 25)))
plot(PP$Salinity, PP$Depth,cex=0.5,col="red",ylim=rev(c(0, 25)))

#BE
ggplot(BE, aes(x = Temp, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + xlim(c(8,11)) + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(BE, aes(x = BE$Temp, y = BE$Depth,color=Season)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 2) + stat_summary(fun = mean, geom = "line", size = 1) + ylim(rev(c(0, 25))) + xlim(c(7,12)) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(BE, aes(x = BE$Temp, y = BE$Depth,color=Season)) + labs (y = "Depth m",x = "Temperature (°C)") + geom_line(size = 0.7) + ylim(rev(c(0, 25)))+theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
# + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)
# facet_wrap(~ Area, nrow = 4, ncol = NULL,scale="free_y")
#+ geom_curve(se=T)

par(mfrow = c(3, 2))
par(mfrow = c(1, 1))

#mfrow=c(nrows, ncols) 
layout(1)
layout(matrix(c(1:2), 2, 1))


#BF
ggplot(BF, aes(x = Temp, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + xlim(c(8,11)) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(BF, aes(x = Salinity, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Salinity (psu)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 

#PP
ggplot(PP, aes(x = Temp, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + xlim(c(8,11)) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(PP, aes(x = Salinity, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Salinity (psu)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 


#Chla (all values and mean)

plot(BE$Temp,BE$Depth, type="point",col=rgb(0, 0.6, 0.3, 0.3),pch = 16,ylim=rev(range(BE$Depth)),ylab="",xaxt="n",xlab="",axes=T, lwd=2.5, cex.lab=3, cex.names=2.5, cex.axis=1.5)
mtext(side=2, line=2.4, 'Depth (m)', col ="black", cex=1.5)
mtext(side=3, line=2.4,expression(paste("Chl a ( ",mu,g,~L^-1,")",)), col =rgb(0, 0.6, 0.3, 1), cex = 1.3)
axis(3,line=0,col="black",col.ticks="black",col.axis="black", cex.axis=1.3) 
lines(Depth ~ Chlorophyll, data = chla, type = "l", col = "red", lwd = 2)


