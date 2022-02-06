##########################################################################################################
## Use climwin to analyse climate data
## Find window that influences SPRI
## Author Daniel Anstett
## 
##
## Last Modified November 8, 2021
##########################################################################################################
##########################################################################################################
# Clear environment
rm(list = ls())

# Get this package retrieving function
## This function will automatically load packages that you already have
## and will install packages you don't yet have then load them
ipak <- function(pkg){
  # Function written by Dr. Evan Fricke
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

# Define the packages that the script needs
myPackages <- c("tidyverse","lme4","lmtest","car","visreg","cowplot","Hmisc")

# Load the packages
ipak(myPackages)

##########################################################################################################
#Import files

y9 <- read.csv("Data/y9.csv", header=T) #Import trait & lag data

##########################################################################################################
#################### lag models ###################################
#Look at correlation across lags and multi-year averages
corr_lag <- y9 %>% select(lag0:lag0123)
rcorr(as.matrix(corr_lag)) #lag0 lag1 & lag2 not highly correated. Averages highly correated with lag0, lag1

#Trait ~ Treatment*env(lag0) + Random Effects
#Trait ~ Treatment*env(lag1) + Random Effects
#Trait ~ Treatment*env(lag2) + Random Effects
#Trait ~ Treatment*env(average (lag01) + Random Effects
#Trait ~ Treatment*env(average (lag012) + Random Effects

#Random Effects = + (1|Family) + (1|Block) + (1|Year) + (1|Site)

#lag models
sla_lag0 <- lmer(SLA ~ Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag1 <- lmer(SLA ~ Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag2 <- lmer(SLA ~ Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag01 <- lmer(SLA ~ Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag012 <- lmer(SLA ~ Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

fl_lag0 <- lmer(Experiment_Date ~ Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag1 <- lmer(Experiment_Date ~ Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag2 <- lmer(Experiment_Date ~ Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag01 <- lmer(Experiment_Date ~ Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag012 <- lmer(Experiment_Date ~ Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

wc_lag0 <- lmer(Water_Content ~ Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag1 <- lmer(Water_Content ~ Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag2 <- lmer(Water_Content ~ Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag01 <- lmer(Water_Content ~ Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag012 <- lmer(Water_Content ~ Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

a_lag0 <- lmer(Assimilation ~ Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag1 <- lmer(Assimilation ~ Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag2 <- lmer(Assimilation ~ Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag01 <- lmer(Assimilation ~ Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag012 <- lmer(Assimilation ~ Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

gz_lag0 <- lmer(Stomatal_Conductance ~ Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y9)
gz_lag1 <- lmer(Stomatal_Conductance ~ Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
gz_lag2 <- lmer(Stomatal_Conductance ~ Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
gz_lag01 <- lmer(Stomatal_Conductance ~ Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
gz_lag012 <- lmer(Stomatal_Conductance ~ Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

#################### AIC ###################################
lag_AIC <- data.frame()
#AIC
lag_AIC[1,1] <- AIC(sla_lag0)
  lag_AIC[2,1] <- AIC(sla_lag1)
  lag_AIC[3,1] <- AIC(sla_lag2)
  lag_AIC[4,1] <- AIC(sla_lag01)
  lag_AIC[5,1] <- AIC(sla_lag012)
  
  lag_AIC[1,2] <- AIC(fl_lag0)
  lag_AIC[2,2] <- AIC(fl_lag1)
  lag_AIC[3,2] <- AIC(fl_lag2)
  lag_AIC[4,2] <- AIC(fl_lag01)
  lag_AIC[5,2] <- AIC(fl_lag012)
  
  lag_AIC[1,3] <- AIC(wc_lag0)
  lag_AIC[2,3] <- AIC(wc_lag1)
  lag_AIC[3,3] <- AIC(wc_lag2)
  lag_AIC[4,3] <- AIC(wc_lag01)
  lag_AIC[5,3] <- AIC(wc_lag012)
  
  lag_AIC[1,4] <- AIC(a_lag0)
  lag_AIC[2,4] <- AIC(a_lag1)
  lag_AIC[3,4] <- AIC(a_lag2)
  lag_AIC[4,4] <- AIC(a_lag01)
  lag_AIC[5,4] <- AIC(a_lag012)
  
  lag_AIC[1,5] <- AIC(gz_lag0)
  lag_AIC[2,5] <- AIC(gz_lag1)
  lag_AIC[3,5] <- AIC(gz_lag2)
  lag_AIC[4,5] <- AIC(gz_lag01)
  lag_AIC[5,5] <- AIC(gz_lag012)
  
colnames(lag_AIC) <- c("SLA","Date of Flowering","Water Content","Assimilation","Stomatal Conductance")
rownames(lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(lag_AIC, file = "Data/lag_AIC.csv", sep = ",", row.names = T)
#Make delta AIC table
delta_AIC <- lag_AIC
delta_AIC[,1] <- lag_AIC[,1]-min(lag_AIC[,1])
delta_AIC[,2] <- lag_AIC[,2]-min(lag_AIC[,2])
delta_AIC[,3] <- lag_AIC[,3]-min(lag_AIC[,3])
delta_AIC[,4] <- lag_AIC[,4]-min(lag_AIC[,4])
delta_AIC[,5] <- lag_AIC[,5]-min(lag_AIC[,5])
write.table(delta_AIC, file = "Data/delta_AIC.csv", sep = ",", row.names = T)

########################################################################################################## 

##########################################################################################################  
#Test models that where delta AIC < 2

#SLA lag 0
sla_lag0a <- lmer(SLA ~ Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(sla_lag0,sla_lag0a) #lag0 significnatly more likely
  
#SLA lag 2
sla_lag2a <- lmer(SLA ~ Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(sla_lag2,sla_lag2a) #lag0 significnatly more likely

#SLA lag 0,1,2
sla_lag012a <- lmer(SLA ~ Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(sla_lag012,sla_lag012a) #lag0 significnatly more likely
  
#Graphs
visreg(sla_lag0, xvar="lag0", by="Drought",gg=TRUE)
visreg(sla_lag2, xvar="lag2", by="Drought",gg=TRUE)
visreg(sla_lag012, xvar="lag012", by="Drought",gg=TRUE)




#Fl lag 1
fl_lag1a <- lmer(Experiment_Date ~ Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag1,fl_lag1a) #no difference, keep simpler model
fl_lag1b <- lmer(Experiment_Date ~ Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag1a,fl_lag1b) #no difference, keep simpler model
fl_lag1c <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag1b,fl_lag1c) #keep drought

#fl lag 2
fl_lag2a <- lmer(Experiment_Date ~ Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag2,fl_lag2a) #no difference, keep simpler model
fl_lag2b <- lmer(Experiment_Date ~ Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag2a,fl_lag2b) #no difference, keep simpler model
fl_lag2c <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag2b,fl_lag2c) #keep drought

#fl lag 01
fl_lag01a <- lmer(Experiment_Date ~ Drought+lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag01,fl_lag01a) #no difference, keep simpler model
fl_lag01b <- lmer(Experiment_Date ~ Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag01a,fl_lag01b) #no difference, keep simpler model
fl_lag01c <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag01b,fl_lag01c) #keep drought

#fl lag 0,1,2
fl_lag012a <- lmer(Experiment_Date ~ Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag012,fl_lag012a) #no difference, keep simpler model
fl_lag012b <- lmer(Experiment_Date ~ Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag012a,fl_lag012b) #no difference, keep simpler model
fl_lag012c <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag012b,fl_lag012c) #keep drought

#Graphs
visreg(fl_lag1b, xvar="Drought",gg=TRUE) #same for all models













                                            










