#################
# Draw Flowering time and CMD.anom on Year
#################
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)
library(nlme)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggeffects)
library(lmtest)
library(egg)
library(cowplot)
library(viridis)
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors
wna_anom <- read.csv("Data/wna_all.csv", header=T) #Imports climate dataset by site/year

#Date of Flowering Vs Year
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
#Make 12 seprate graphs for year vs floweirng
site.marker<-unique(Res_Flower_all$Site.Lat)
for(i in 1:12) {
  Ref_flower_filter<- Res_Flower_all %>% filter(Site.Lat==as.character(site.marker[i]))
  Res_flower_all_plot<-ggplot(Ref_flower_filter, aes(Year, y=visregRes, colour=Drought))+
    geom_jitter(aes(colour=Drought), size=0.2)+
    stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Drought,fill=Drought), size = 1)+
    #facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
    #scale_x_discrete(limits = Res_Flower_all$Year) +
    scale_y_continuous(name="Date of Flowering", limits=c(75,120))+
    scale_x_continuous(limits=c(2010,2016))+
    scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
    scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
    theme_classic()
  Res_flower_all_plot <- Res_flower_all_plot + theme(#legend.position = "none",
                                                     axis.title.x=element_blank(),
                                                     axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
                                                     axis.ticks.x = element_blank(),
                                                     #axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
                                                     axis.text.y = element_text(size=12,face="bold"),
                                                     axis.title.y = element_text(color="black", size=12,vjust = 0, face="bold",hjust=0.5))
  assign(paste("year_flower",i,sep="_"),Res_flower_all_plot)
}

#print out graphs site 1 through to 12
year_flower_1







