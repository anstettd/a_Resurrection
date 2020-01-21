#################
# Site*Year*Drought Mixed Models
#################
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)


y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y55Family <- as.factor(y5$Family) # prep factors


######################################################################################################################
##### Date of Flowering
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5) #3way interaction model
# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year+ (1|Family) + (1|Block), data=y5)
####### Report this result #########
lrtest(fullmod.exp, no3way.exp) #3-way intraction significant, 3-way has a larger LogLik value. Retain 3-way.
####################################

#Date of Flowering Graphs
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all<-Res_Flower_all %>% filter (Site.Lat %in% c("32.9_S02","37.5_S32","41.8_S16","43.4_S15"))

#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#Set up site lables equating names to codes
Site_Labs<-c("32.9_S02"="A", "37.5_S32" = "B","41.8_S16"="C", "43.4_S15"="D")
Res_flower_all_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_all_plot <-Res_flower_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(legend.title = element_blank(), legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

######################################################################################################################
##### SLA ####
fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
# drop 3way
no3way.SLA <- lmer(SLA ~ Site.Lat*Drought + Drought*Year + Site.Lat*Year + (1|Family) + (1|Block), data=y5)
lrtest(fullmod.SLA, no3way.SLA) # accept 3-way model

#SLA Graphs
fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W) #Row bind wet and dry residuals into one data frame
Res_SLA_all<-Res_SLA_all %>% filter (Site.Lat %in% c("36.2_S10","39.4_S29","41.8_S16","43.4_S15"))

#Reorder Treatments
Res_SLA_all$Drought <- as.factor(Res_SLA_all$Drought)
Res_SLA_all$Drought <- factor(Res_SLA_all$Drought, levels=c("W", "D"))

#Set up site lables equating names to codes
Site_Labs<-c("36.2_S10"="A", "39.4_S29" = "B","41.8_S16"="C", "43.4_S15"="D")
Res_SLA_all_plot<-ggplot(Res_SLA_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_SLA_all$Year) +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_SLA_all_plot <-Res_SLA_all_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_SLA_all_plot + facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

