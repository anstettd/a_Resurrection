#################
# ggGraphs for Site x Year X Drought
#################
library(tidyverse)
#library(lsmeans)
#library(car)
#library(maptools)
library(visreg)
#library(nlme)
library(lme4)
#library(lmerTest)
#library(lmtest)
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
y3$Block <- as.factor(y3$Block) ; y3$Family <- as.factor(y3$Family) # prep factors

#Date of Flowering
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_dat_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
vis_dat_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
Res_dat_D<-vis_dat_D$res ; Res_dat_W<-vis_dat_W$res # Extract residuals
Res_dat_all<-rbind(Res_dat_D, Res_dat_W) #Row bind wet and dry residuals into one data frame

#Set up site lables equating names to codes
Res_plot.exp <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1,se = FALSE)+
  scale_y_continuous(name="Date of Flowering")+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual(values= c("32.9_S02"="#990000","34.1_S11"="#CC0000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                              "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                              "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC"))+
  theme_classic()
Res_plot.exp<- Res_plot.exp  + theme(legend.title=element_blank(),
                                     axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                                          axis.text.y = element_text(size=12,face="bold"),
                                          axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                          axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_plot.exp + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))






+
  scale_fill_discrete(breaks=c("32.9_S02","34.1_S11","34.3_S07", "36.2_S10", "36.7_S08", "37.5_S32",
                             "39.4_S29", "39.7_S18", "41.7_S17","41.8_S16", "42.3_S36", "43.4_S15"))

#legend.position = "none",
#, labeller = labeller(Site.Lat=Site_Labs)
breaks=c("32.9_S02","34.1_S11","34.3_S07", "36.2_S10", "36.7_S08", "37.5_S32",
         "39.4_S29", "39.7_S18", "41.7_S17","41.8_S16", "42.3_S36", "43.4_S15")) +
  breaks=c("43.4_S15","42.3_S36","41.8_S16","41.7_S17","39.7_S18","39.4_S29","37.5_S32",
           "36.7_S08","36.2_S10","34.3_S07","34.1_S11","32.9_S02")


