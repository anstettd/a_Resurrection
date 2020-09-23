#################
# Slopes graphs
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
library(glmmTMB)
library(cowplot)

slopes.rapid <- read.csv("Data/slopes.year.12.csv", header=T) #Imports main dataset

slopes.rapid$Region <- ifelse(slopes.rapid$Site.Lat=="32.9_S02", "1",
                              ifelse(slopes.rapid$Site.Lat=="34.3_S07", "2",
                                     ifelse(slopes.rapid$Site.Lat=="36.2_S10", "3",
                                            ifelse(slopes.rapid$Site.Lat=="34.1_S11","4",
                                            ifelse(slopes.rapid$Site.Lat=="36.7_S08", "5",
                                                   ifelse(slopes.rapid$Site.Lat=="37.5_S32", "6",
                                                          ifelse(slopes.rapid$Site.Lat=="39.4_S29", "7",
                                                                 ifelse(slopes.rapid$Site.Lat=="39.7_S18", "8",
                                                                        ifelse(slopes.rapid$Site.Lat=="41.7_S17", "9",
                                                                               ifelse(slopes.rapid$Site.Lat=="41.8_S16", "10",
                                                                                      ifelse(slopes.rapid$Site.Lat=="42.3_S36", "11",
                                                                                             ifelse(slopes.rapid$Site.Lat=="43.4_S15","12",  
                                                                                                    NA    ))))))))))))

#FTvsSLA in W and D
plot1<-ggplot(slopes.rapid, aes(SLA_Wet, y=Flowering_Wet, label=rownames(Region)))+
  geom_point()+
  geom_text(hjust =-0.3, size=6, aes(label=Region))+
  ylim(-1,2.8)+
  xlim(-11,13)+
  ylab("Flowering Date/Time")+
  xlab("Specific Leaf Area/Time")+
 # annotate("text", x=1, y=1, label="Later FT, Thicker/Narrower")+
  theme_classic()
plot1<- plot1 + theme(axis.title.x= element_text(size=18))+
  theme(axis.title.y=element_text(size=18))+
  theme(axis.text = element_text(size=18))+
  geom_hline(yintercept = 0, linetype ="dashed")+
  geom_vline(xintercept = 0, linetype ="dashed")
plot2<-ggplot(slopes.rapid, aes(SLA_Dry, y=Flowering_Dry, label=rownames(Region)))+
  geom_point()+
  geom_text(hjust =-0.3, size=6, aes(label=Region))+
  ylim(-1,2.8)+
  xlim(-20,10.5)+
  ylab("Flowering Date/Time")+
  xlab("Specific Leaf Area/Time")+
  theme_classic()
plot2<- plot2 + theme(axis.title.x= element_text(size=18))+
  theme(axis.title.y=element_text(size=18))+
  theme(axis.text = element_text(size=18))+
  geom_hline(yintercept = 0, linetype ="dashed")+
  geom_vline(xintercept = 0, linetype ="dashed")

plot_grid(plot1, plot2, labels = c("Wet","Dry"),
          label_size=12,
          hjust=-2.5)

#FTvsAssimilation W and D
plot3<-ggplot(slopes.rapid, aes(Assimilation_Wet, y=Flowering_Wet, label=rownames(Region)))+
  geom_point()+
  geom_text(hjust =-0.3, size=6, aes(label=Region))+
  ylab("Flowering Date/Time")+
  xlab("Carbon Assimilation/Time")+
  xlim(-1,1.5)+
  theme_classic()
plot3<- plot3 + theme(axis.title.x= element_text(size=18))+
  theme(axis.title.y=element_text(size=18))+
  theme(axis.text = element_text(size=18))+
  geom_hline(yintercept = 0, linetype ="dashed")+
  geom_vline(xintercept = 0, linetype ="dashed")
plot4<-ggplot(slopes.rapid, aes(Assimilation_Dry, y=Flowering_Dry, label=rownames(Region)))+
  geom_point()+
  geom_text(hjust =-0.3, size=6, aes(label=Region))+
  ylab("Flowering Date/Time")+
  xlab("Carbon Assimilation/Time")+
  xlim(-0.9,0.5)+
  theme_classic()
plot4<- plot4 + theme(axis.title.x= element_text(size=18))+
  theme(axis.title.y=element_text(size=18))+
  theme(axis.text = element_text(size=18))+
  geom_hline(yintercept = 0, linetype ="dashed")+
  geom_vline(xintercept = 0, linetype ="dashed")

plot_grid(plot1, plot2, plot3, plot4, labels = c("Wet","Dry", "Wet", "Dry"),
          label_size=12,
          hjust=-2.5)

