#################
# Seperate Graphs per Site
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
y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factors
y5<-y5 %>% mutate(Region = ifelse(Latitude > 40, "North", "South_Centre"))
#wna_anom <- read.csv("Data/wna_all.csv", header=T) #Imports climate dataset by site/year


#SLA Vs Year _ North
fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W) #Row bind wet and dry residuals into one data frame

Ref_SLA_filter<- Res_SLA_all %>% filter(Region=="North")
Res_SLA_all_plot<-ggplot(Ref_SLA_filter, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.8)+
  stat_smooth(method = "lm",aes(colour=Drought,fill=Drought), size = 1)+
  #facet_wrap(.~Region, labeller = labeller(Region=Site_Labs))+
  #scale_x_discrete(limits = Res_SLA_all$Year) +
  scale_y_continuous(name="SLA",limits=c(100,400))+
  scale_x_continuous(limits=c(2010,2016))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_SLA_all_plot + theme(legend.position = "none",
                            axis.title.x=element_blank(),
                            #axis.ticks.x = element_blank(),
                            axis.text.x = element_text(size=0, face="bold", angle=0,hjust=0.5),
                            axis.text.y = element_text(size=0,face="bold"),
                            axis.title.y = element_text(color="black", size=0,vjust = 0, face="bold",hjust=0.5))

ggsave("SLA_North.pdf", width = 4.5, height = 5, units = "in")

#SLA Vs Year _ South_Centre

Ref_SLA_filter<- Res_SLA_all %>% filter(Region=="South_Centre")
Res_SLA_all_plot<-ggplot(Ref_SLA_filter, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.8)+
  stat_smooth(method = "lm",aes(colour=Drought,fill=Drought), size = 1)+
  #facet_wrap(.~Region, labeller = labeller(Region=Site_Labs))+
  #scale_x_discrete(limits = Res_SLA_all$Year) +
  scale_y_continuous(name="SLA",limits=c(100,400))+
  scale_x_continuous(limits=c(2010,2016))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_SLA_all_plot + theme(legend.position = "none",
                         axis.title.x=element_blank(),
                         #axis.ticks.x = element_blank(),
                         axis.text.x = element_text(size=0, face="bold", angle=0,hjust=0.5),
                         axis.text.y = element_text(size=0,face="bold"),
                         axis.title.y = element_text(color="black", size=0,vjust = 0, face="bold",hjust=0.5))
ggsave("SLA_South.pdf", width = 4.5, height = 5, units = "in")





#Date of Flowering Vs Year - North
fullmod.exp <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame


Ref_flower_filter<- Res_Flower_all %>% filter(Region=="North")
Res_flower_all_plot<-ggplot(Ref_flower_filter, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.4)+
  stat_smooth(method = "lm",aes(colour=Drought,fill=Drought), size = 1)+
  #facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  #scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_x_continuous(limits=c(2010,2016))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
 Res_flower_all_plot + theme(legend.position = "none",
                                                   axis.title.x=element_blank(),
                                                   #axis.ticks.x = element_blank(),
                                                   axis.text.x = element_text(size=26, face="bold", angle=0,hjust=0.5),
                                                   axis.text.y = element_text(size=26,face="bold"),
                                                   axis.title.y = element_text(color="black", size=30,vjust = 0, face="bold",hjust=0.5))
 ggsave("Flowering_North.pdf", width = 7, height = 6, units = "in")
 
 #Date of Flowering Vs Year - South_Centre
 Ref_flower_filter<- Res_Flower_all %>% filter(Region=="South_Centre")
 Res_flower_all_plot<-ggplot(Ref_flower_filter, aes(Year, y=visregRes, colour=Drought))+
   geom_jitter(aes(colour=Drought), size=0.4)+
   stat_smooth(method = "lm",aes(colour=Drought,fill=Drought), size = 1)+
   #facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
   #scale_x_discrete(limits = Res_Flower_all$Year) +
   scale_y_continuous(name="Date of Flowering")+
   scale_x_continuous(limits=c(2010,2016))+
   scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
   scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
   theme_classic()
 Res_flower_all_plot + theme(legend.position = "none",
                             axis.title.x=element_blank(),
                             #axis.ticks.x = element_blank(),
                             axis.text.x = element_text(size=26, face="bold", angle=0,hjust=0.5),
                             axis.text.y = element_text(size=26,face="bold"),
                             axis.title.y = element_text(color="black", size=30,vjust = 0, face="bold",hjust=0.5))
 ggsave("Flowering_South.pdf", width = 7, height = 6, units = "in")
 
