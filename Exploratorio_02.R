
# Non parametric Analisis

## The codes for sampling areas are 
# BB-B: Bahia Brown Bajo
# BB-E: Bahia Brown Entrada
# BB-F: Bahia Brown Fondo
# PP:  Punta Parana

library(readxl)
library(ggplot2)
library(cowplot)
library(lubridate)
data <- read_excel("Data/totalR.xlsx")


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
data <- data %>% mutate(Date=as_date(Date)) %>%  bind_rows(tibble(Date=ymd("2014-01-01"),Organism="M. edulis", Area="PP",STX=NA))
# ggplot(data, aes(x = Date, y = STX,color=Organism)) + labs (y = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})),x = "Year") + geom_line(size = 0.8) +  facet_wrap(~ Area, nrow = 4, ncol = NULL,scale="free_y") +   theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) + theme(legend.position = "top", panel.grid = element_blank()) 



G1 <- data %>% filter(Area == "BB-B" ) %>% 
  ggplot( aes(x = Date, y = STX,color=Organism)) + labs(y=NULL, x=NULL) + geom_line(size = 0.7) + 
  coord_cartesian(xlim=c(ymd("2004-06-30"),ymd("2020-06-30")),expand=FALSE) +
  scale_y_continuous(limits = c(0, 350))+theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) + theme(legend.position = "none", panel.grid = element_blank()) 

G2 <- data %>% filter(Area == "BB-E" ) %>% 
  ggplot( aes(x = Date, y = STX,color=Organism)) + labs(y = NULL, x=NULL) + geom_line(size = 0.7) +
  coord_cartesian(xlim=c(ymd("2004-06-30"),ymd("2020-06-30")),expand=FALSE)+
  scale_y_continuous(limits = c(0, 5000))+ theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) + theme(legend.position = "top", panel.grid = element_blank()) 

G3 <- data %>% filter(Area == "BB-F" ) %>% 
  ggplot( aes(x = Date, y = STX,color=Organism)) + labs (y = NULL , x=NULL) + geom_line(size = 0.7) +
  coord_cartesian(xlim=c(ymd("2004-06-30"),ymd("2020-06-30")),expand=FALSE) +
  scale_y_continuous(limits = c(0, 1500))+ theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) + theme(legend.position = "top", panel.grid = element_blank()) 

G4 <- data %>% filter(Area == "PP" ) %>%
  ggplot( aes(x = Date, y = STX,color=Organism)) + labs(y=NULL, x = "Year") + geom_line(size = 0.7) +
  coord_cartesian(xlim=c(ymd("2004-06-30"),ymd("2020-06-30")),expand=FALSE)+
  scale_y_continuous(limits = c(0, 5000))+ theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) + theme(legend.position = "top", panel.grid = element_blank()) 


pg <- plot_grid(G1 + theme(legend.position="none"),
          G2 + theme(legend.position="none"),
          G3 + theme(legend.position="none"),
          G4 + theme(legend.position="none"), nrow = 4,ncol = 1)

legend <- get_legend(
  G2 + guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
) 

pg <- plot_grid(pg, legend, ncol = 1, rel_heights = c(1, .1)) 
pg


# Fig 3 
ggplot(d_sorted, aes(x=season,y=log(data$STX + 0.001),color=season)) + xlab("") + geom_boxplot() + labs(x = NULL , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1}))) + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 5)+ theme(legend.position = "none",panel.grid = element_blank()) 


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

ss <- d_sorted %>% group_by(Area,season) %>% summarise(mS = mean(STX),n=n(),sdS=se(STX), hi = mS+sdS, lo=mS-sdS) 
G1<- ggplot(ss, aes(y = mS, x = Area, fill=season)) + labs (y = NULL,x = NULL) + scale_y_continuous(limits = c(0, 650))+ geom_bar(stat="identity", position=position_dodge())+ scale_color_viridis_d() + geom_errorbar(aes(ymin = mS - sdS, ymax = mS + sdS), width=0.9, stat="identity", position=position_dodge()) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank())

#scale_y_continuous(limits = c(0, 90))
#ggplot(data, aes(x = Date, y = STX,color=Organism)) + labs (y = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})),x = "Year") + geom_line(size = 0.7) +  facet_wrap(~ Area, nrow = 4, ncol = NULL,scale="free_y") +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
#ggplot(d_sorted, aes(x=Area,y=STX,color=season)) + xlab("") + geom_col(stat = "identity") + scale_fill_continuous(type = "viridis") + labs(x = NULL , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1}))) + theme(legend.position = "none",panel.grid = element_blank()) 
#ggplot(d_sorted, aes(x=Area,y=STX,color=season)) + xlab("") + geom_bar(stat = "count") + scale_fill_continuous(type = "viridis") + labs(x = NULL , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1}))) + theme(legend.position = "none",panel.grid = element_blank()) 
#sSTX <- d_sorted %>% group_by(season,Organism) %>% summarise(mSTX = mean(STX),n=n(),sdSTX=sd(STX), hi = mSTX+sdSTX, lo=mSTX-sdSXT) 
#ggplot(sSTX, aes(y = mSTX, x = season)) + labs (x = "Depth (m)",y = "Salinity (psu)") + scale_x_reverse() +geom_line(size = 1.5) +  geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.5) +
#  facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + coord_flip()+theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) + scale_color_viridis_d()
#ggplot(data, aes(y = S, x = Depth,color=Season)) + labs (x = "Depth (m)",y = "Salinity (psu)") + scale_x_reverse() +geom_point(size = 1.5) +  facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + coord_flip()  +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) + scale_color_viridis_d() + 
 # geom_line(data=ss,aes(y=mS,x=Depth),color="blue",size=2)
#facet_wrap(~ season, nrow = NULL, ncol = 4L,scale="free_y") + coord_flip()+theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) 
#IrisPlot <- ggplot(Iris_summary, aes(Species, mean_PL)) 
#geom_col() + 
#geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2)
#+ geom_errorbar(aes(ymin = mS - sdS, ymax = mS + sdS), width=0.2)

#ggplot(data, aes(y = S, x = Depth,color=Season)) + labs (x = "Depth (m)",y = "Salinity (psu)") + scale_x_reverse() +geom_point(size = 1.5) +  facet_wrap(~ Area, nrow = NULL, ncol = 3L,scale="free_y") + coord_flip()  +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank()) + scale_color_viridis_d() + 
#  geom_line(data=ss,aes(y=mS,x=Depth),color="blue",size=2)


#geom_bar(stat = "identity") + scale_fill_continuous(type = "viridis")


#Fig 4b (Duration)
data2 <- read_excel("Data/eventos toxicos y duracion.xlsx")

d_sorted1 <- data2 %>%
  mutate(season = fct_relevel(season,c("summer","autumn","winter", "spring")))

ss <- d_sorted1 %>% group_by(Area,season) %>% summarise(mS = mean(Duration),n=n(),sdS=se(Duration), hi = mS+sdS, lo=mS-sdS) 
G2 <-ggplot(ss, aes(y = mS, x = Area, fill=season)) + labs (y = NULL,x = "Area") + geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +scale_y_continuous(limits = c(0, 180))+ scale_color_viridis_d() + geom_errorbar(aes(ymin = mS - sdS, ymax = mS + sdS), width=0.9, stat="identity", position=position_dodge()) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank())

#geom_col(position = position_dodge2(width = 0.9, preserve = "single"))

kruskal.test(Duration ~ season, data = data2, na.action=na.fail)
pairwise.wilcox.test(data2$Duration,data2$season,p.adj="bonferroni")

### ubicar todos juntos 
pg <- plot_grid(G1 + theme(legend.position="none"),
                G2 + theme(legend.position="none"), nrow = 2,ncol = 1)

legend <- get_legend(
  G2 + guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top", panel.grid = element_blank())
) 

pg <- plot_grid(pg, legend, ncol = 1,rel_heights = c(1, .1)) 
pg






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
#bargraph.CI(Area,STX,Organism, data = data, ylab = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})), xlab = NA, cex.lab = 1.1,cex.names = 1.25,col = c("violetred4", "aquamarine3"), legend=T,  x.leg=9, y.leg=200, ncol=1)

ss <- d_sorted %>% group_by(Area,Organism) %>% summarise(mS = mean(STX),n=n(),sdS=se(STX), hi = mS+sdS, lo=mS-sdS) 
#ggplot(ss, aes(y = mS, x = Area, fill=Organism)) + labs (y = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})),x = "Area") + geom_bar(stat="identity", position=position_dodge())+geom_errorbar(aes(ymin = mS - sdS, ymax = mS + sdS), width=0.9, stat="identity", position=position_dodge()) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank())

G1 <-ggplot(ss, aes(y = mS, x = Area, fill=Organism)) + labs (y = NULL,x = "Area") + geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+geom_errorbar(aes(ymin = mS - sdS, ymax = mS + sdS), width=0.9, stat="identity", position=position_dodge()) + theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) + theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank())

#geom_col(position = position_dodge2(width = 0.9, preserve = "single")) 

# Fig 6b   
#bargraph.CI(season, STX, Organism, data = data,  xlab = NA, ylab = expression(PSP ~( "µg STX eq"~ 100~ g~ tissue^{-1})), cex.lab = 1.1,cex.names = 1.25,col = c("violetred4", "aquamarine3"),legend = F, x.leg=10, y.leg=500, ncol=1)

#colours()
#install.packages("viridis")
#library(viridis)

ss <- d_sorted %>% group_by(season,Organism) %>% summarise(mS = mean(STX),n=n(),sdS=se(STX), hi = mS+sdS, lo=mS-sdS) 
G2<- ggplot(ss, aes(y = mS, x = season, fill=Organism)) + theme(legend.text = element_text(face = c(rep("italic", 5), rep("plain", 5)))) +labs (y = NULL,x = "Season") + scale_y_continuous(limits = c(0, 580))+geom_bar(stat="identity", position=position_dodge())+geom_errorbar(aes(ymin = mS - sdS, ymax = mS + sdS), width=0.9, stat="identity", position=position_dodge()) +theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank())
 

### ubicar todos juntos 
pg <- plot_grid(G1 + theme(legend.position="none"),
                G2 + theme(legend.position="none"), nrow = 2,ncol = 1)

legend <- get_legend(
  G2 + guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top", panel.grid = element_blank())
) 

pg <- plot_grid(pg, legend, ncol = 1,rel_heights = c(1, .1)) 
pg


#theme(legend.title=NULL,legend.position = "top", panel.grid = element_blank())
#pg <- plot_grid(pg, legend, ncol = 1,labels = c("a)","b)"),label_size = 12, label_y= 0.55, rel_heights = c(1, .1)) 

#ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
#geom_bar(stat="identity", position=position_dodge())

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


ggplot(d_sorted, aes(x=Year,y=log(data$STX + 0.001),color=Year)) + xlab("") + labs(x = "Year" , y = expression(log_PSP ~ ("µg STX eq"~ 100~ g~ tissue^{-1})))+ geom_boxplot() + geom_hline(aes(yintercept = 1.903), color = "red", size = 0.6)+ stat_summary(fun = mean, geom = "point", size = 3)+ theme(legend.position = "none", panel.grid = element_blank())  

#scale_color_viridis_d()
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




























