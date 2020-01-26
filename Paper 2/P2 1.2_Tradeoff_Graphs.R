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

slopes.rapid <- read.csv("Data/slopes.year.csv", header=T) #Imports main dataset

boxplot(slopes.rapid$Flowering_Dry ~ slopes.rapid$SLA_Dry)
ggplot(slopes.rapid, )

#Flowering time:SLA Wet
ggplot(slopes.rapid, aes(SLA_Wet, y=Flowering_Wet, colour = Site.Lat))+
  geom_point(aes(colour=Site.Lat), size=2)+
  stat_smooth(method = "lm",aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
scale_color_manual(values= c("32.9_S02"="#990000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                             "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                             "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual( values= c("32.9_S02"="#990000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "43.4_S15"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC"))+
  theme_classic()

#Flowering:Assimilation Wet
ggplot(slopes.rapid, aes(Assimilation_Wet, y=Flowering_Wet, colour = Site.Lat))+
  geom_point(aes(colour=Site.Lat), size=2)+
  stat_smooth(method = "lm",aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
  scale_color_manual(values= c("32.9_S02"="#990000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual( values= c("32.9_S02"="#990000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "43.4_S15"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC"))+
  theme_classic()

#Flowering:Water Content Wet
ggplot(slopes.rapid, aes(Water_Content_Wet, y=Flowering_Wet, colour = Site.Lat))+
  geom_point(aes(colour=Site.Lat), size=2)+
  stat_smooth(method = "lm",aes(colour=Site.Lat,fill=Site.Lat), size = 1)+
  scale_color_manual(values= c("32.9_S02"="#990000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "39.7_S18"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC")) +
  scale_fill_manual( values= c("32.9_S02"="#990000", "34.3_S07"="#FF0000", "36.2_S10"="#FF6666",
                               "36.7_S08"="#FF9999", "37.5_S32" = "#FFCCCC", "39.4_S29"="#CCCCCC", "43.4_S15"="#99CCFF",
                               "41.7_S17"="#0099FF", "41.8_S16"="#0066FF", "42.3_S36"="#0000CC", "43.4_S15"="#6600CC"))+
  theme_classic()
