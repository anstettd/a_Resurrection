#################
# Site*Year*Drought Mixed Models
#################
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)
library(cowplot)


y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factors

y5<-y5 %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Center","South")))



######################################################################################################################
##### SLA ####
fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
# drop 3way
no3way.SLA <- lmer(SLA ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), 
                   data=y5)
lrtest(fullmod.SLA, no3way.SLA) # accept 3-way model

#SLA Graphs
fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
vis_flower_D<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))

#Set up site lables equating names to codes
Site_Labs<-c("North"="A", "Center"="B", "South"="C")
SLA_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=16, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))





######################################################################################################################
##### Date of Flowering
fullmod.exp <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block)  + (1|Site.Lat), data=y5) #3way interaction model
# drop 3way
no3way.exp <- lmer(Experiment_Date ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
####### Report this result #########
lrtest(fullmod.exp, no3way.exp) # No sat diff. Retain 2-way.
####################################
# drop 2ways
noDxY.exp <- lmer(Experiment_Date ~ Region*Drought + Region*Year+ (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(no3way.exp,noDxY.exp) #no difference, pick simpler model
SxYD.exp<- lmer(Experiment_Date ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.exp,SxYD.exp) #noDxY.exp supported
noRxD.exp <- lmer(Experiment_Date ~ Drought*Year+ Region*Year+ (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.exp,noRxD.exp) ###noDxY.exp supported###
noRxY.exp <- lmer(Experiment_Date ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.exp,noRxY.exp) #noDxY.exp is equivalent to noRxY.exp
lrtest(noRxY.exp,SxYD.exp) ###noRxY.exp supported###


#SxY.exp<- lmer(Experiment_Date ~ Region*Year + (1|Family) + (1|Block), data=y5)
#lrtest(SxYD.exp,SxY.exp) #Region*Year + Drought Supported

#Date of Flowering Graphs
finalmod.exp <- lmer(Experiment_Date ~ Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
vis_flower_D<-visreg(finalmod.exp, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.exp, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="D", "Center"="E", "South"="F")
Flower_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Flower_plot <-Flower_plot + theme(
  axis.text.x = element_text(size=16, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
Flower_plot <-Flower_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))


## Cowplot
plot_grid(SLA_plot,Flower_plot,ncol = 1)


