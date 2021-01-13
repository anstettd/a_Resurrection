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



############################################## Fig 2 #######################################################
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
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#Set up site lables equating names to codes
Site_Labs<-c("North"="North", "Center"="Center", "South"="South")
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
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot




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
finalmod.exp <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
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
Site_Labs<-c("North"="North", "Center"="Center", "South"="South")
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
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
Flower_plot <-Flower_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))


## Cowplot export at 8 X 7
plot_grid(SLA_plot,Flower_plot,ncol = 1)


############################################## Fig S2 #######################################################
######################################################################################################################
##### Water_Content ####
fullmod.wc <- lmer(Water_Content ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
# drop 3way
no3way.wc <- lmer(Water_Content ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(fullmod.wc, no3way.wc) # two-way model supported

# drop 2ways
noDxY.wc <- lmer(Water_Content ~ Region*Drought + Region*Year+ (1|Family) + (1|Block) + (1|Site.Lat),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(no3way.wc,noDxY.wc) #noDxY.wc supported
SxYD.wc<- lmer(Water_Content ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.wc,SxYD.wc) #SxYD.wc supported
SxY.wc<- lmer(Water_Content ~ Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noDxY.wc,SxY.wc) #no difference

#no interactions
nox.wc <- lmer(Water_Content ~ Region + Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat),
               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(SxYD.wc,nox.wc) # no interactions model significantly better.
lrtest(SxY.wc,nox.wc)  # no interactions model significantly better.
noDrought.wc <- lmer(Water_Content ~ Region + Year + (1|Family) + (1|Block) + (1|Site.Lat),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(nox.wc, noDrought.wc) # no interactions model significantly better. Retain drought in model.


#Water Content Graphs
finalmod.wc <- lmer(Water_Content ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
vis_flower_D<-visreg(finalmod.wc, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.wc, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
WC_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Water Content")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
WC_plot <-WC_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=18,vjust = 2, face="bold",hjust=0.5))
WC_plot <-WC_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))

######################################################################################################################
######## Assimilation
fullmod.A <- lmer(Assimilation ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
# drop 3way
no3way.A <- lmer(Assimilation ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(fullmod.A, no3way.A) # No difference

# drop 2ways
######## Selected Model #########
noDxY.A <- lmer(Assimilation ~ Region*Drought + Region*Year+ (1|Family) + (1|Block) + (1|Site.Lat),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
################################
lrtest(no3way.A,noDxY.A) #No difference
SxYD.A<- lmer(Assimilation ~ Region*Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat),
              control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(noDxY.A,SxYD.A) #noDxY.A marginally supported
#SxY.A<- lmer(Assimilation ~ Region*Year + (1|Family) + (1|Block), data=y5)
#lrtest(SxYD.A,SxY.A) #No difference

#no interactions
#nox.A <- lmer(Assimilation ~ Region + Year + Drought + (1|Family) + (1|Block),
#               control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
#lrtest(SxYD.A,nox.A) # no interactions model significantly better.
#noDrought.A <- lmer(Assimilation ~ Region + Year + (1|Family) + (1|Block), data=y5)
#lrtest(nox.A, noDrought.A) # no interactions model significantly better. Retain drought in model.

#noYear.A <- lmer(Assimilation ~ Region + Drought + (1|Family) + (1|Block), data=y5)

#lrtest(nox.A, noYear.A) # Marginal support for Region + Drought model



#Assimilation Graphs
finalmod.A <- lmer(Assimilation ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
vis_flower_D<-visreg(finalmod.A, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.A, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#Set up site lables equating names to codes
Site_Labs<-c("North"="D (North)", "Center"="E (Centre)", "South"="F (South)")
A_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Assimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
A_plot <-A_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=18,vjust = 2, face="bold",hjust=0.5))
A_plot <-A_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))

######################################################################################################################
######## Stomatal Conductance
fullmod.gs <- lmer(Stomatal_Conductance ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), 
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
# drop 3way
no3way.gs <- lmer(Stomatal_Conductance ~ Region*Drought + Drought*Year + Region*Year + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(fullmod.gs, no3way.gs) #keep 3-way
# drop 2ways
noSxY.gs <- lmer(Stomatal_Conductance ~ Region*Drought + Drought*Year+ (1|Family) + (1|Block) + (1|Site.Lat),
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(no3way.gs,noSxY.gs) # Remove Site X Year
DxYS.gs<- lmer(Stomatal_Conductance ~ Drought*Year + Region +  (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(noSxY.gs,DxYS.gs) # Remove Site X Drought
#no interactions
nox.gs <- lmer(Stomatal_Conductance ~ Region + Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
lrtest(DxYS.gs,nox.gs) # no interactions model significantly better.
noDrought.gs <- lmer(Stomatal_Conductance ~ Region + Year + (1|Family) + (1|Block) + (1|Site.Lat),
                     control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
lrtest(nox.gs, noDrought.gs) # Marignal evidence for main effects model. Retain main effects model. 

###############################
#no.main.gs <- lmer(Stomatal_Conductance ~ (1|Family) + (1|Block), data=y5)
#lrtest(Drought.gs, no.main.gs) # Drought better than nothing

#Stomatal Conductance Graphs
finalmod.gs <- lmer(Stomatal_Conductance ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)
vis_flower_D<-visreg(finalmod.gs, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.gs, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Region<-as.factor(Res_Flower_all$Region)
Res_Flower_all$Region<-factor(Res_Flower_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#Set up site lables equating names to codes
Site_Labs<-c("North"="G (North)", "Center"="H (Centre)", "South"="I (South)")
gs_plot<-ggplot(Res_Flower_all, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  scale_x_discrete(limits = Res_Flower_all$Year) +
  scale_y_continuous(name="Stomatal Conductance")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
gs_plot <-gs_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=18,vjust = 2, face="bold",hjust=0.5))
gs_plot <-gs_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))

## Cowplot export at 11 X 7
plot_grid(WC_plot,A_plot,gs_plot,ncol = 1)


