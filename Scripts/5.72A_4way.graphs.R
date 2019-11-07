#################
# ggGraphs for 4-Way interaction
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


##Date of Flowering
#main CMD effect, remove lag effect
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*CMD.anom*CMD.anom.1*Drought + (1|Family) + (1|Block), data=y3)
vis_dat_W<-visreg(fullmod.exp, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
vis_dat_D<-visreg(fullmod.exp, xvar="CMD.anom", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
Res_dat_D<-vis_dat_D$res ; Res_dat_W<-vis_dat_W$res # Extract residuals
Res_dat_all<-rbind(Res_dat_D, Res_dat_W) #Row bind wet and dry residuals into one data frame
#Replace site names
Res_dat_all$Site.Lat<- as.character(Res_dat_all$Site.Lat)
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="32.9_S02"] <- "Site 01"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="34.1_S11"] <- "Site 02"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="34.3_S07"] <- "Site 03"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="36.2_S10"] <- "Site 04"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="36.7_S08"] <- "Site 05"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="37.5_S32"] <- "Site 06"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="39.4_S29"] <- "Site 07"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="39.7_S18"] <- "Site 08"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="41.7_S17"] <- "Site 09"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="41.8_S16"] <- "Site 10"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="42.3_S36"] <- "Site 11"
Res_dat_all$Site.Lat[Res_dat_all$Site.Lat=="43.4_S15"] <- "Site 12"

#fl points
Res_plot.exp <-ggplot(Res_dat_all, aes(CMD.anom, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  geom_smooth(method="lm",aes(colour=Site.Lat,fill=Site.Lat), size = 1,se = FALSE))+
  scale_y_continuous(name="Date of Flowering")+
  scale_x_continuous()+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.exp<- Res_plot.exp  + theme(legend.title=element_blank(),
                                     legend.text=element_text(size = 12, face = "bold"),
                                     axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                     axis.text.y = element_text(size=16,face="bold"),
                                     axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                     axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.exp + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))
