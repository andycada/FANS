
# Non parametric Analisis

## The codes for sampling areas are 
# BB-B: Bahia Brown Bajo
# BB-E: Bahia Brown Entrada
# BB-F: Bahia Brown Fondo
# PP:  Punta Parana

library(readxl)
library("pracma")
library(tidyverse)

library("akima")
data <- read_excel("Data/Perfiles.xlsx") #summer 20062007
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

#SALEEEEEE
ggplot(data, aes(x = T, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + xlim(c(8,11)) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + geom_smooth(se=FALSE)  +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(data, aes(x = S, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Salinity (psu)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(data, aes(x = O2, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Oxigen (mg/L)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
ggplot(data, aes(x = PH, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "pH") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 

ggplot(data, aes(x = T, y = Depth,color=Date)) + labs (y = "Depth (m)",x = "Temperature (°C)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) + xlim(c(8,11)) + facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + geom_smooth(se=FALSE) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 

colnames(data)

ggplot(BE, aes(x = Salinity, y = Depth,color=Season)) + labs (y = "Depth (m)",x = "Salinity (psu)") + geom_point(size = 1.5) + ylim(rev(c(0, 25))) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
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


###--------------------------------------------------########

library(tidyverse)
theme_set(theme_bw())

library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))

d_sorted <- data %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

# Fig 2
theme_set(theme_light(base_size = 10, base_family = "Poppins"))
data <- data %>% bind_rows(tibble(Date=ymd("2014-01-01"),Organism="M. edulis", Area="PP",STX=NA))
ggplot(data, aes(x = Date, y = STX,color=Organism)) + labs (y = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})),x = "Year") + geom_line(size = 0.7) +  facet_wrap(~ Area, nrow = 4, ncol = NULL,scale="free_y") +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 


# Fig 3 
ggplot(d_sorted, aes(x=season,y=log(data$STX + 0.001),color=season)) + xlab("") + geom_boxplot() + scale_color_viridis_d() + labs(x = NULL , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1}))) + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank()) 


library(dplyr)
group_by(data, season) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )


# STX vs Area -------------------------------------------------------------#

STX<- data$STX
Area<-data$Area
data$season<-as.factor(data$season)


shapiro.test(STX) #p-value < 2.2e-16
shapiro.test(log(STX + 1)) #p-value < 2.2e-16


bartlett.test(STX ~ Area, data = data, na.action=na.fail) #p-value < 2.2e-16

fligner.test(STX ~ Area, data = data, na.action=na.fail)

kruskal.test(STX ~ Area, data = data, na.action=na.fail)

# pairwise Wilcoxon Rank Sum Tests (post hoc)
install.packages("PMCMRplus") 

pairwise.wilcox.test(data$STX,data$Area,p.adj="bonferroni") 

STX <- data$STX + 0.001
library(dplyr)
 group_by(data, Area) %>%
  summarise(
      count = n(),
           mean = mean(STX, na.rm = TRUE),
        sd = sd(STX, na.rm = TRUE),
         median = median(STX, na.rm = TRUE),
         IQR = IQR(STX, na.rm = TRUE)
       )
 
# STX vs Season-------------------------------------------------------------------#


bartlett.test(STX ~ season, data = data, na.action=na.fail) 


fligner.test(STX ~ season, data = data, na.action=na.fail)


kruskal.test(STX ~ season, data = data, na.action=na.fail)
 

# pairwise Wilcoxon Rank Sum Tests (post hoc)

pairwise.wilcox.test(data$STX,data$season,p.adj="bonferroni") 

STX <- data$STX + 0.001
library(dplyr)
group_by(data, season) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )


library(sciplot)

# Fig 4a

bargraph.CI(Area, STX, season, data = d_sorted, ylim =c(0,900), xlab = NA, ylab = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})), col = c("violetred4", "steelblue3","seagreen4","yellow"),legend = T, cex.leg= 0.9, ncol=1, x.leg=0.6, y.leg=850, cex.lab = 1,cex.names = 1)


#Fig 4b (Duration)
data2 <- read_excel("Data/eventos toxicos y duracion.xlsx")

d_sorted1 <- data2 %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

bargraph.CI(Area,Duration,season, data = d_sorted1, ylab = "Duration (days)", xlab = "Area", col = c("violetred4", "steelblue3","seagreen4","yellow"),legend = T, cex.leg= 0.9, ncol=1, x.leg=0.3, y.leg=150,  cex.lab = 1,cex.names = 1)

kruskal.test(Duration ~ season, data = data2, na.action=na.fail)
pairwise.wilcox.test(data2$Duration,data2$season,p.adj="bonferroni")



# STX VS SEASON (by AREA)

BBE <- data %>% filter(Area == "BB-E" )

kruskal.test(STX ~ season, data = BBE, na.action=na.fail)
pairwise.wilcox.test(BBE$STX,BBE$season,p.adj="bonferroni") #TODAS SIG  DIFERENTES menos spring winter

BBF <- data %>% filter(Area == "BB-F" )

kruskal.test(STX ~ season, data = BBF, na.action=na.fail)
pairwise.wilcox.test(BBF$STX,BBF$season,p.adj="bonferroni")

BBB <- data %>% filter(Area == "BB-B" )

kruskal.test(STX ~ season, data = BBB, na.action=na.fail)
pairwise.wilcox.test(BBB$STX,BBB$season,p.adj="bonferroni") 

PP <- data %>% filter(Area == "PP" )

kruskal.test(STX ~ season, data = PP, na.action=na.fail)#p-value = 1.778e-07
pairwise.wilcox.test(PP$STX,PP$season,p.adj="bonferroni")# todas sig menos spring autumn, autumn summer



# STX vs organism 

bargraph.CI(Organism,STX, data = data, ylab = expression(PSP ~( "?g STX eq"~ 100~ g~ tissue^{-1})), xlab = NA, space=0.5,cex.lab = 1.1, x.leg = 1.3,cex.names = 1.3, col = c("red1", "green3"))

wilcox.test(STX ~ Organism, data = data, exact = FALSE). 

wilcox.test(STX ~ Organism, data = data, exact = FALSE, alternative = "greater")
# A ater> Medulis 

# Fig 6a  
bargraph.CI(Area,STX,Organism, data = data, ylab = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})), xlab = NA, cex.lab = 1.1,cex.names = 1.25,col = c("red1", "green3"), legend=T,  x.leg=9, y.leg=200, ncol=1)

# Fig 6b   
bargraph.CI(season, STX, Organism, data = data,  xlab = NA, ylab = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})), cex.lab = 1.1,cex.names = 1.25,col = c("red1", "green3"),legend = F, x.leg=10, y.leg=500, ncol=1)


## Blue mussel (M. edulis) 
M <- data %>% filter(Organism == "M. edulis" )

kruskal.test(STX ~ Area, data = M, na.action=na.fail)
pairwise.wilcox.test(M$STX,M$Area,p.adj="bonferroni") # BBF no es sig diferente de PP


## Magellan mussel (A. ater)
C <- data %>% filter(Organism == "A. ater" )

kruskal.test(STX ~ Area, data = C, na.action=na.fail)


# Inside (BB-E) comparing mussel species 
BBE <- data %>% filter(Area == "BB-E" )


k8<-kruskal.test(STX ~ Organism, data = BBE, na.action=na.fail)
wilcox.test(STX ~ Organism, data = BBE, exact = FALSE)



# STX vs organism by season 

# winter
W <- data %>% filter(season == "winter" )

kruskal.test(STX ~ Organism, data = W, na.action=na.fail)

# autumn
A <- data %>% filter(season == "autumn" )

kruskal.test(STX ~ Organism, data = A, na.action=na.fail)

# summer
S <- data %>% filter(season == "summer" )

kruskal.test(STX ~ Organism, data = S, na.action=na.fail)

# spring
SP <- data %>% filter(season == "spring" )

Kruskal.test(STX ~ Organism, data = SP, na.action=na.fail)

# STX vs Year (mean/median) -------------------------------------------------------------# 

# Fig 5 

d_sorted <- data %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))


ggplot(d_sorted, aes(x=Year,y=log(data$STX + 0.001),color=Year)) + xlab("") + labs(x = "Year" , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1})))+ geom_boxplot() + scale_color_viridis_d() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none", panel.grid = element_blank())  

kruskal.test(STX ~ Year, data = data, na.action=na.fail)

pairwise.wilcox.test(data$STX,data$Year,p.adj="bonferroni") 


# Compute summary statistics by groups:

STX <- data$STX + 0.001

  library(dplyr)
group_by(data, Year) %>%
  summarise(
    count = n(),
    mean = mean(STX, na.rm = TRUE),
    sd = sd(STX, na.rm = TRUE),
    median = median(STX, na.rm = TRUE),
    IQR = IQR(STX, na.rm = TRUE)
  )




























