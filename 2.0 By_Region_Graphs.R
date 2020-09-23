# 2.1 Region_Year_Drought graphs
# split 11 popln into North, Centre, South
# North = Sites H, I, J, K
# Centre = C, D, E, F, G
# South = A, B

y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset

# Flowering Time
fullmod.exp <- lmer(Experiment_Date ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_flower_D<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(fullmod.exp, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals
Res_Flower_all<-rbind(Res_flower_D, Res_flower_W) #Row bind wet and dry residuals into one data frame
Res_Flower_all$Drought <- as.factor(Res_Flower_all$Drought)
Res_Flower_all$Drought <- factor(Res_Flower_all$Drought, levels=c("W", "D"))

#new column for Region: N, C, S - nevermind 
Res_Flower_all$Region <- ifelse(Res_Flower_all$Site.Lat=="32.9_S02", "S",
                    ifelse(Res_Flower_all$Site.Lat=="34.3_S07", "S",
                           ifelse(Res_Flower_all$Site.Lat=="36.2_S10", "C",
                                  ifelse(Res_Flower_all$Site.Lat=="36.7_S08", "C",
                                         ifelse(Res_Flower_all$Site.Lat=="37.5_S32", "C",
                                                ifelse(Res_Flower_all$Site.Lat=="39.4_S29", "C",
                                                       ifelse(Res_Flower_all$Site.Lat=="39.7_S18", "C",
                                                              ifelse(Res_Flower_all$Site.Lat=="41.7_S17", "N",
                                                                     ifelse(Res_Flower_all$Site.Lat=="41.8_S16", "N",
                                                                            ifelse(Res_Flower_all$Site.Lat=="42.3_S36", "N",
                                                                                   ifelse(Res_Flower_all$Site.Lat=="43.4_S15","N",  
                                                                                          NA    )))))))))))

#Res_flower_N
Res_Flower_N <- Res_Flower_all[which(Res_Flower_all$Region=="N"),]
Res_flower_N_plot<-ggplot(Res_Flower_N, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat)+
  scale_x_discrete(limits = Res_Flower_N$Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_N_plot <-Res_flower_N_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_N_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#Res_flower_C
Res_Flower_C <- Res_Flower_all[which(Res_Flower_all$Region=="C"),]
Res_flower_C_plot<-ggplot(Res_Flower_C, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat)+
  scale_x_discrete(limits = Res_Flower_C$Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_C_plot <-Res_flower_C_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_C_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#Res_flower_S
Res_Flower_S <- Res_Flower_all[which(Res_Flower_all$Region=="S"),]
Res_flower_S_plot<-ggplot(Res_Flower_S, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat)+
  scale_x_discrete(limits = Res_Flower_S$Year) +
  scale_y_continuous(name="Date of Flowering")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_flower_S_plot <-Res_flower_S_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_flower_S_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

### SLA 

fullmod.SLA <- lmer(SLA ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes


Res_SLA_all$Region <- ifelse(Res_SLA_all$Site.Lat=="32.9_S02", "S",
                             ifelse(Res_SLA_all$Site.Lat=="34.3_S07", "S",
                                    ifelse(Res_SLA_all$Site.Lat=="36.2_S10", "C",
                                           ifelse(Res_SLA_all$Site.Lat=="36.7_S08", "C",
                                                  ifelse(Res_SLA_all$Site.Lat=="37.5_S32", "C",
                                                         ifelse(Res_SLA_all$Site.Lat=="39.4_S29", "C",
                                                                ifelse(Res_SLA_all$Site.Lat=="39.7_S18", "C",
                                                                       ifelse(Res_SLA_all$Site.Lat=="41.7_S17", "N",
                                                                              ifelse(Res_SLA_all$Site.Lat=="41.8_S16", "N",
                                                                                     ifelse(Res_SLA_all$Site.Lat=="42.3_S36", "N",
                                                                                            ifelse(Res_SLA_all$Site.Lat=="43.4_S15","N",  
                                                                                                   NA    )))))))))))

# SLA northern
Res_SLA_N <- Res_SLA_all[which(Res_SLA_all$Region=="N"),]

Res_SLA_N_plot<-ggplot(Res_SLA_N, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_SLA_N$Year) +
  scale_y_continuous(name="SLA")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_SLA_N_plot <-Res_SLA_N_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_SLA_N_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#SLA Central
Res_SLA_C <- Res_SLA_all[which(Res_SLA_all$Region=="C"),]

Res_SLA_C_plot<-ggplot(Res_SLA_C, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_SLA_C$Year) +
  scale_y_continuous(name="SLA")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_SLA_C_plot <-Res_SLA_C_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_SLA_C_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#SLA South
Res_SLA_N <- Res_SLA_all[which(Res_SLA_all$Region=="S"),]

Res_SLA_N_plot<-ggplot(Res_SLA_N, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat, labeller = labeller(Site.Lat=Site_Labs))+
  scale_x_discrete(limits = Res_SLA_N$Year) +
  scale_y_continuous(name="SLA")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_SLA_N_plot <-Res_SLA_N_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_SLA_N_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))


# Assimilation
fullmod.A <- lmer(Assimilation ~ Site.Lat*Year*Drought + (1|Family) + (1|Block), data=y5)
vis_assimilation_D<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="D")) #set up visreg for Drought
vis_assimilation_W<-visreg(fullmod.A, xvar="Year", by="Site.Lat", cond=list(Drought="W")) #set up visreg for Wet
Res_assimilation_D<-vis_assimilation_D$res ; Res_assimilation_W<-vis_assimilation_W$res # Extract residuals
Res_assimilation_all<-rbind(Res_assimilation_D, Res_assimilation_W) #Row bind wet and dry residuals into one data frame
#Set up site lables equating names to codes
Res_assimilation_all$Region <- ifelse(Res_assimilation_all$Site.Lat=="32.9_S02", "S",
                                      ifelse(Res_assimilation_all$Site.Lat=="34.3_S07", "S",
                                             ifelse(Res_assimilation_all$Site.Lat=="36.2_S10", "C",
                                                    ifelse(Res_assimilation_all$Site.Lat=="36.7_S08", "C",
                                                           ifelse(Res_assimilation_all$Site.Lat=="37.5_S32", "C",
                                                                  ifelse(Res_assimilation_all$Site.Lat=="39.4_S29", "C",
                                                                         ifelse(Res_assimilation_all$Site.Lat=="39.7_S18", "C",
                                                                                ifelse(Res_assimilation_all$Site.Lat=="41.7_S17", "N",
                                                                                       ifelse(Res_assimilation_all$Site.Lat=="41.8_S16", "N",
                                                                                              ifelse(Res_assimilation_all$Site.Lat=="42.3_S36", "N",
                                                                                                     ifelse(Res_assimilation_all$Site.Lat=="43.4_S15","N",  
                                                                                                            NA    )))))))))))

#Assimilation N
Res_assimilation_N <- Res_assimilation_all[which(Res_assimilation_all$Region=="N"),]
Res_assimilation_N_plot<-ggplot(Res_assimilation_N, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat)+
  scale_x_discrete(limits = Res_assimilation_N$Year) +
  scale_y_continuous(name="Carbon Assimilation Rate")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_assimilation_N_plot <-Res_assimilation_N_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_assimilation_N_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#Res_assimilation_C
Res_assimilation_C <- Res_assimilation_all[which(Res_assimilation_all$Region=="C"),]
Res_assimilation_C_plot<-ggplot(Res_assimilation_C, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat)+
  scale_x_discrete(limits = Res_assimilation_C$Year) +
  scale_y_continuous(name="Carbon Assimilation Rate")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_assimilation_C_plot <-Res_assimilation_C_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_assimilation_C_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#Res_assimilation_S
Res_assimilation_S <- Res_assimilation_all[which(Res_assimilation_all$Region=="S"),]
Res_assimilation_S_plot<-ggplot(Res_assimilation_S, aes(Year, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Site.Lat)+
  scale_x_discrete(limits = Res_assimilation_S$Year) +
  scale_y_continuous(name="Carbon Assimilation Rate")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
Res_assimilation_S_plot <-Res_assimilation_S_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
  axis.text.y = element_text(size=12,face="bold"),
  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
Res_assimilation_S_plot + facet_wrap(.~Site.Lat) +
  theme(legend.title = element_blank(), legend.position = c(0.85,0.1),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

