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
Res_flower_all_plot <- Res_flower_all_plot + theme(legend.position = "none",
                                                   axis.title.x=element_blank(),
                                                   axis.text.x = element_blank(),
#                                                   axis.ticks.x = element_blank(),
#                           axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
                            axis.text.y = element_text(size=12,face="bold"),
                            axis.title.y = element_text(color="black", size=12,vjust = 0, face="bold",hjust=0.5))
assign(paste("year_flower",i,sep="_"),Res_flower_all_plot)
}

#CMD.anom versus Year
site.year.marker<-c("S02","S11","S07","S10","S08","S32","S29","S18","S17","S16","S36","S15")
for(i in 1:12) {
  wna_filter<- wna_anom %>% filter(Site==as.character(site.year.marker[i]))
anom.year <- ggplot(wna_filter, aes(Year,CMD.anom,color=Site))+
  geom_point(color="black",size=0)+
  geom_line(color="black",size=1.2)+
  theme_classic()
anom.year<-anom.year + theme(legend.position = "none",
                               axis.text.x = element_text(size=11, face="bold", angle=0,hjust=0.5),
                               axis.text.y = element_text(size=10,face="bold"),
                               axis.title.x=element_blank(),
                               axis.title.y = element_text(color="black", size=11,vjust = 0, face="bold",hjust=0.4))+ 
scale_y_continuous(name="CMDA",limits=c(-200,200))
assign(paste("year_CMD",i,sep="_"),anom.year)
}


#With Y Axis titles

#EGG
fl_CMD_1<-ggarrange(year_flower_1,year_CMD_1+theme(axis.text.x = element_blank()),ncol=1,nrow=2, heights=c(3,1.1)) #gives y axis
fl_CMD_2<-ggarrange(year_flower_2+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_2+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_3<-ggarrange(year_flower_3+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_3+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_4<-ggarrange(year_flower_4+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_4+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank())
                    ,ncol=1,nrow=2, heights=c(3,1.1))

fl_CMD_5<-ggarrange(year_flower_5,year_CMD_5+theme(axis.text.x = element_blank()),ncol=1,nrow=2, heights=c(3,1.1))#gives y axis
fl_CMD_6<-ggarrange(year_flower_6+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_6+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_7<-ggarrange(year_flower_7+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_7+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_8<-ggarrange(year_flower_8+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_8+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))

fl_CMD_9<-ggarrange(year_flower_9,year_CMD_9,ncol=1,nrow=2, heights=c(3,1))
fl_CMD_10<-ggarrange(year_flower_10+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                     year_CMD_10+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_11<-ggarrange(year_flower_11+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                     year_CMD_11+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_12<-ggarrange(year_flower_12+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                     year_CMD_12+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),ncol=1,nrow=2, heights=c(3,1.1))

#CowPlots
plot_grid(fl_CMD_1,fl_CMD_2,fl_CMD_3,fl_CMD_4,fl_CMD_5,fl_CMD_6,fl_CMD_7,fl_CMD_8,fl_CMD_9,fl_CMD_10,fl_CMD_11,fl_CMD_12,
          labels = c('A','B','C','D','E','F','G','H','I','J','K','L'), label_x = .2, hjust = 0,label_size = 12)

#No Y Axis titles

#EGG
#fl_CMD_1<-ggarrange(year_flower_1,year_CMD_1+theme(axis.text.x = element_blank()),ncol=1,nrow=2, heights=c(3,1.1)) #gives y axis
fl_CMD_1<-ggarrange(year_flower_1+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_1+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_2<-ggarrange(year_flower_2+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_2+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_3<-ggarrange(year_flower_3+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_3+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_4<-ggarrange(year_flower_4+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_4+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank())
                    ,ncol=1,nrow=2, heights=c(3,1.1))

#fl_CMD_5<-ggarrange(year_flower_5,year_CMD_5+theme(axis.text.x = element_blank()),ncol=1,nrow=2, heights=c(3,1))
fl_CMD_5<-ggarrange(year_flower_5+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_5+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_6<-ggarrange(year_flower_6+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_6+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_7<-ggarrange(year_flower_7+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_7+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_8<-ggarrange(year_flower_8+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_8+theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.text.x = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))

#fl_CMD_9<-ggarrange(year_flower_9,year_CMD_9,ncol=1,nrow=2, heights=c(3,1))
fl_CMD_9<-ggarrange(year_flower_9+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    year_CMD_9+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                    ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_10<-ggarrange(year_flower_10+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                     year_CMD_10+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_11<-ggarrange(year_flower_11+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                     year_CMD_11+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),ncol=1,nrow=2, heights=c(3,1.1))
fl_CMD_12<-ggarrange(year_flower_12+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),
                     year_CMD_12+theme(axis.title.y=element_blank(),axis.text.y = element_blank()),ncol=1,nrow=2, heights=c(3,1.1))

#CowPlots
plot_grid(fl_CMD_1,fl_CMD_2,fl_CMD_3,fl_CMD_4,fl_CMD_5,fl_CMD_6,fl_CMD_7,fl_CMD_8,fl_CMD_9,fl_CMD_10,fl_CMD_11,fl_CMD_12,
          labels = c('A','B','C','D','E','F','G','H','I','J','K','L'), label_x = .08, hjust = 0,label_size = 12)