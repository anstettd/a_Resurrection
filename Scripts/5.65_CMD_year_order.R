#################
# CMDA with year lables
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

y3 <- read.csv("Data/y3.csv", header=T)
#### Set up data frames
trait.means.w <- data.frame() #Set up Wet Treatment data frame
y3.w<- y3 %>% filter(Drought=="W") #Filter for Wet treatment data

### get trait and CMDA means
means.y3W <- y3.w %>% group_by(ID_Year,Site, Year, Latitude, Longitude, Drought) %>%
  summarise_at(c("CMD.anom", "CMD.anom.1", "Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass"), mean, na.rm=TRUE)

means.y3W <-means.y3W %>% mutate(Order_Year=Year-Year)

#CMD_Year lable
CMD_year_lable <- ggplot(means.y3W, aes(CMD.anom,Order_Year, label=Year))+
  geom_point(size=1)+
  geom_text_repel(
    nudge_y      = 0.05,
    direction    = "x",
    angle        = 90,
    vjust        = 0,
    segment.size = 0.2
  )
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+ theme_minimal()
CMD_year_lable <- CMD_year_lable + theme(legend.text = element_text(size = 12, face = "bold"),
                                         axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                                         axis.text.y = element_text(size=14,face="bold"),
                                         axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) 
CMD_year_lable + facet_wrap( ~ Site, ncol=4)





