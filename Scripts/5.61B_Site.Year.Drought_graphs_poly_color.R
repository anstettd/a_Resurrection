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

##Date of Flowering
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block), data=y3)
vis_dat_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
vis_dat_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
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
Res_plot.exp <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1,se = FALSE)+
  scale_y_continuous(name="Date of Flowering",limits=c(80,120))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#CC0000","Site 02"="#FF0000", "Site 03"="#FF0000", "Site 04"="#FF6600",
                               "Site 05"="#FF6600", "Site 06" = "#FFFF00", "Site 07"="#99FF66", "Site 08"="#99FF66",
                               "Site 09"="#0099FF", "Site 10"="#0099FF", "Site 11"="#6600FF", "Site 12"="#6600FF")) +
  scale_fill_manual( values= c("Site 01"="#CC0000","Site 02"="#FF0000", "Site 03"="#FF0000", "Site 04"="#FF6600",
                               "Site 05"="#FF6600", "Site 06" = "#FFFF00", "Site 07"="#99FF66", "Site 08"="#99FF66",
                               "Site 09"="#0099FF", "Site 10"="#0099FF", "Site 11"="#6600FF", "Site 12"="#6600FF"))+
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
  

#flowering error
Res_plot.exp <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
  scale_y_continuous(name="Date of Flowering",limits=c(80,120))+
  scale_x_continuous(limits=c(2010,2017))+
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



##Water Content
fullmod.wc <- lmer(Water_Content ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_dat_W<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
vis_dat_D<-visreg(fullmod.wc, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
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

#wc points
Res_plot.wc <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1,se = FALSE)+
  scale_y_continuous(name="Water Content",limits=c(0.1,0.4))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.wc<- Res_plot.wc  + theme(legend.title=element_blank(),
                                     legend.text=element_text(size = 12, face = "bold"),
                                     axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                     axis.text.y = element_text(size=16,face="bold"),
                                     axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                     axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.wc + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))


#wc error
Res_plot.wc <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
  scale_y_continuous(name="Water Content",limits=c(0.1,0.4))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.wc<- Res_plot.wc  + theme(legend.title=element_blank(),
                                     legend.text=element_text(size = 12, face = "bold"),
                                     axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                     axis.text.y = element_text(size=16,face="bold"),
                                     axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                     axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.wc + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))


##SLA
fullmod.SLA <- lmer(SLA ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_dat_W<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
vis_dat_D<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
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

#SLA points
Res_plot.SLA <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1,se = FALSE)+
  scale_y_continuous(name="SLA",limits=c(100,400))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.SLA<- Res_plot.SLA  + theme(legend.title=element_blank(),
                                   legend.text=element_text(size = 12, face = "bold"),
                                   axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                   axis.text.y = element_text(size=16,face="bold"),
                                   axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                   axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.SLA + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))


#SLA error
Res_plot.SLA <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
  scale_y_continuous(name="SLA",limits=c(100,400))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.SLA<- Res_plot.SLA  + theme(legend.title=element_blank(),
                                   legend.text=element_text(size = 12, face = "bold"),
                                   axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                   axis.text.y = element_text(size=16,face="bold"),
                                   axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                   axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.SLA + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))




#Stomatal Conductance
fullmod.gs <- lmer(Stomatal_Conductance ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_dat_W<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
vis_dat_D<-visreg(fullmod.gs, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
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

#gs points
Res_plot.gs <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1,se = FALSE)+
  scale_y_continuous(name="Stomatal Conductance", limits=c(0,1))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.gs<- Res_plot.gs  + theme(legend.title=element_blank(),
                                     legend.text=element_text(size = 12, face = "bold"),
                                     axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                     axis.text.y = element_text(size=16,face="bold"),
                                     axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                     axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.gs + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))

#gs error
Res_plot.gs <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
  scale_y_continuous(name="Stomatal Conductance", limits=c(0,1))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.gs<- Res_plot.gs  + theme(legend.title=element_blank(),
                                   legend.text=element_text(size = 12, face = "bold"),
                                   axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                   axis.text.y = element_text(size=16,face="bold"),
                                   axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                   axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.gs + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))



##Assimilation
fullmod.A <- lmer(Assimilation ~ Site.Lat*poly(Year,2)*Drought + (1|Family) + (1|Block),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y3)
vis_dat_W<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
vis_dat_D<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
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

#A points
Res_plot.A <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1,se = FALSE)+
  scale_y_continuous(name="Assimilation", limits=c(5,20))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.A<- Res_plot.A  + theme(legend.title=element_blank(),
                                   legend.text=element_text(size = 12, face = "bold"),
                                   axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                   axis.text.y = element_text(size=16,face="bold"),
                                   axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                   axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.A + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))


#A error
Res_plot.A <-ggplot(Res_dat_all, aes(Year, y=visregRes, fill=Site.Lat,colour=Site.Lat))+
  geom_jitter(aes(colour=Site.Lat), size=0.2)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
  scale_y_continuous(name="Assimilation", limits=c(5,20))+
  scale_x_continuous(limits=c(2010,2017))+
  scale_color_manual(values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC")) +
  scale_fill_manual( values= c("Site 01"="#990000","Site 02"="#CC0000", "Site 03"="#FF0000", "Site 04"="#FF6666",
                               "Site 05"="#FF9999", "Site 06" = "#FFCCCC", "Site 07"="#CCCCCC", "Site 08"="#99CCFF",
                               "Site 09"="#0099FF", "Site 10"="#0066FF", "Site 11"="#0000CC", "Site 12"="#6600CC"))+
  theme_classic()
Res_plot.A<- Res_plot.A  + theme(legend.title=element_blank(),
                                   legend.text=element_text(size = 12, face = "bold"),
                                   axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.5),
                                   axis.text.y = element_text(size=16,face="bold"),
                                   axis.title.x = element_text(color="black", size=0, vjust = 0.5, face="bold"),
                                   axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5)) 
Res_plot.A + facet_wrap(.~Drought) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=0,face="bold",hjust=0.05,vjust=-1.7))+
  guides(color=guide_legend(reverse = TRUE))




