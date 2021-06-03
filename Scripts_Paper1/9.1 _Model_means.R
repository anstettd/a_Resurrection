#################
# Model means for Site*Year*Drought Mixed Models
#################
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)
library(cowplot)

#Import datasets
y5 <- read.csv("Data/y5.csv", header=T) 
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factors

y5<-y5 %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Center","South")))

paper1_means <- read.csv("Data/paper1_means.csv", header=T) #Imports main dataset

######################################################################################################################
# SLA
######################################################################################################################

fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5) # Run 3-way model
vis_sla_D<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals

#Break up just by Region
Res_sla_W_N <- Res_sla_W %>% filter(Region=="North")
Res_sla_W_C <- Res_sla_W %>% filter(Region=="Center")
Res_sla_W_S <- Res_sla_W %>% filter(Region=="South")

#Break up Wet & Dry by region and pre-peak
North_pre_W_sla <- Res_sla_W %>% filter(Region=="North", Year==2010)
North_pre_D_sla <- Res_sla_D %>% filter(Region=="North", Year==2010)
North_peak_W_sla <- Res_sla_W %>% filter(Region=="North", Year==2016)
North_peak_D_sla <- Res_sla_D %>% filter(Region=="North", Year==2016)

Centre_pre_W_sla <- Res_sla_W %>% filter(Region=="Center", Year==2010)
Centre_pre_D_sla <- Res_sla_D %>% filter(Region=="Center", Year==2010)
Centre_peak_W_sla <- Res_sla_W %>% filter(Region=="Center", Year==2015)
Centre_peak_D_sla <- Res_sla_D %>% filter(Region=="Center", Year==2015)

South_pre_W_sla <- Res_sla_W %>% filter(Region=="South", Year==2011)
South_pre_D_sla <- Res_sla_D %>% filter(Region=="South", Year==2011)
South_peak_W_sla <- Res_sla_W %>% filter(Region=="South", Year==2014)
South_peak_D_sla <- Res_sla_D %>% filter(Region=="South", Year==2014)

#Insert Means in paper1_means dataframe
paper1_means[1,4] <- mean(North_pre_W_sla$visregRes)
paper1_means[2,4] <- mean(North_pre_D_sla$visregRes)
paper1_means[3,4] <- mean(North_peak_W_sla$visregRes)
paper1_means[4,4] <- mean(North_peak_D_sla$visregRes)

paper1_means[5,4] <- mean(Centre_pre_W_sla$visregRes)
paper1_means[6,4] <- mean(Centre_pre_D_sla$visregRes)
paper1_means[7,4] <- mean(Centre_peak_W_sla$visregRes)
paper1_means[8,4] <- mean(Centre_peak_D_sla$visregRes)

paper1_means[9,4] <- mean(South_pre_W_sla$visregRes)
paper1_means[10,4] <- mean(South_pre_D_sla$visregRes)
paper1_means[11,4] <- mean(South_peak_W_sla$visregRes)
paper1_means[12,4] <- mean(South_peak_D_sla$visregRes)


######################################################################################################################
# Date of Flowering
######################################################################################################################

finalmod.exp <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5) # Run 3-way model
vis_flower_D<-visreg(finalmod.exp, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_flower_W<-visreg(finalmod.exp, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_flower_D<-vis_flower_D$res ; Res_flower_W<-vis_flower_W$res # Extract residuals

#Break up Wet & Dry by region and pre-peak
North_pre_W_fl <- Res_flower_W %>% filter(Region=="North", Year==2010)
North_pre_D_fl <- Res_flower_D %>% filter(Region=="North", Year==2010)
North_peak_W_fl <- Res_flower_W %>% filter(Region=="North", Year==2016)
North_peak_D_fl <- Res_flower_D %>% filter(Region=="North", Year==2016)

Centre_pre_W_fl <- Res_flower_W %>% filter(Region=="Center", Year==2010)
Centre_pre_D_fl <- Res_flower_D %>% filter(Region=="Center", Year==2010)
Centre_peak_W_fl <- Res_flower_W %>% filter(Region=="Center", Year==2015)
Centre_peak_D_fl <- Res_flower_D %>% filter(Region=="Center", Year==2015)

South_pre_W_fl <- Res_flower_W %>% filter(Region=="South", Year==2011)
South_pre_D_fl <- Res_flower_D %>% filter(Region=="South", Year==2011)
South_peak_W_fl <- Res_flower_W %>% filter(Region=="South", Year==2014)
South_peak_D_fl <- Res_flower_D %>% filter(Region=="South", Year==2014)

#Insert Means in paper1_means dataframe
paper1_means[1,5] <- mean(North_pre_W_fl$visregRes)
paper1_means[2,5] <- mean(North_pre_D_fl$visregRes)
paper1_means[3,5] <- mean(North_peak_W_fl$visregRes)
paper1_means[4,5] <- mean(North_peak_D_fl$visregRes)

paper1_means[5,5] <- mean(Centre_pre_W_fl$visregRes)
paper1_means[6,5] <- mean(Centre_pre_D_fl$visregRes)
paper1_means[7,5] <- mean(Centre_peak_W_fl$visregRes)
paper1_means[8,5] <- mean(Centre_peak_D_fl$visregRes)

paper1_means[9,5] <- mean(South_pre_W_fl$visregRes)
paper1_means[10,5] <- mean(South_pre_D_fl$visregRes)
paper1_means[11,5] <- mean(South_peak_W_fl$visregRes)
paper1_means[12,5] <- mean(South_peak_D_fl$visregRes)


######################################################################################################################
# Assimilation
######################################################################################################################

finalmod.A <- lmer(Assimilation ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), # Run 3-way model
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
vis_A_D<-visreg(finalmod.A, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_A_W<-visreg(finalmod.A, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_A_D<-vis_A_D$res ; Res_A_W<-vis_A_W$res # Extract residuals


#Break up Wet & Dry by region and pre-peak
North_pre_W_a <- Res_A_W %>% filter(Region=="North", Year==2010)
North_pre_D_a <- Res_A_D %>% filter(Region=="North", Year==2010)
North_peak_W_a <- Res_A_W %>% filter(Region=="North", Year==2016)
North_peak_D_a <- Res_A_D %>% filter(Region=="North", Year==2016)

Centre_pre_W_a <- Res_A_W %>% filter(Region=="Center", Year==2010)
Centre_pre_D_a <- Res_A_D %>% filter(Region=="Center", Year==2010)
Centre_peak_W_a <- Res_A_W %>% filter(Region=="Center", Year==2015)
Centre_peak_D_a <- Res_A_D %>% filter(Region=="Center", Year==2015)

South_pre_W_a <- Res_A_W %>% filter(Region=="South", Year==2011)
South_pre_D_a <- Res_A_D %>% filter(Region=="South", Year==2011)
South_peak_W_a <- Res_A_W %>% filter(Region=="South", Year==2014)
South_peak_D_a <- Res_A_D %>% filter(Region=="South", Year==2014)

#Insert Means in paper1_means dataframe
paper1_means[1,6] <- mean(North_pre_W_a$visregRes)
paper1_means[2,6] <- mean(North_pre_D_a$visregRes)
paper1_means[3,6] <- mean(North_peak_W_a$visregRes)
paper1_means[4,6] <- mean(North_peak_D_a$visregRes)

paper1_means[5,6] <- mean(Centre_pre_W_a$visregRes)
paper1_means[6,6] <- mean(Centre_pre_D_a$visregRes)
paper1_means[7,6] <- mean(Centre_peak_W_a$visregRes)
paper1_means[8,6] <- mean(Centre_peak_D_a$visregRes)

paper1_means[9,6] <- mean(South_pre_W_a$visregRes)
paper1_means[10,6] <- mean(South_pre_D_a$visregRes)
paper1_means[11,6] <- mean(South_peak_W_a$visregRes)
paper1_means[12,6] <- mean(South_peak_D_a$visregRes)

######################################################################################################################
# Stomatal Conductance
######################################################################################################################

finalmod.gs <- lmer(Stomatal_Conductance ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), data=y5)  # Run 3-way model
vis_gs_D<-visreg(finalmod.gs, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_gs_W<-visreg(finalmod.gs, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_gs_D<-vis_gs_D$res ; Res_gs_W<-vis_gs_W$res # Extract residuals

#Break up Wet & Dry by region and pre-peak
North_pre_W_gs <- Res_gs_W %>% filter(Region=="North", Year==2010)
North_pre_D_gs <- Res_gs_D %>% filter(Region=="North", Year==2010)
North_peak_W_gs <- Res_gs_W %>% filter(Region=="North", Year==2016)
North_peak_D_gs <- Res_gs_D %>% filter(Region=="North", Year==2016)

Centre_pre_W_gs <- Res_gs_W %>% filter(Region=="Center", Year==2010)
Centre_pre_D_gs <- Res_gs_D %>% filter(Region=="Center", Year==2010)
Centre_peak_W_gs <- Res_gs_W %>% filter(Region=="Center", Year==2015)
Centre_peak_D_gs <- Res_gs_D %>% filter(Region=="Center", Year==2015)

South_pre_W_gs <- Res_gs_W %>% filter(Region=="South", Year==2011)
South_pre_D_gs <- Res_gs_D %>% filter(Region=="South", Year==2011)
South_peak_W_gs <- Res_gs_W %>% filter(Region=="South", Year==2014)
South_peak_D_gs <- Res_gs_D %>% filter(Region=="South", Year==2014)

#Insert Means in paper1_means dataframe
paper1_means[1,7] <- mean(North_pre_W_gs$visregRes)
paper1_means[2,7] <- mean(North_pre_D_gs$visregRes)
paper1_means[3,7] <- mean(North_peak_W_gs$visregRes)
paper1_means[4,7] <- mean(North_peak_D_gs$visregRes)

paper1_means[5,7] <- mean(Centre_pre_W_gs$visregRes)
paper1_means[6,7] <- mean(Centre_pre_D_gs$visregRes)
paper1_means[7,7] <- mean(Centre_peak_W_gs$visregRes)
paper1_means[8,7] <- mean(Centre_peak_D_gs$visregRes)

paper1_means[9,7] <- mean(South_pre_W_gs$visregRes)
paper1_means[10,7] <- mean(South_pre_D_gs$visregRes)
paper1_means[11,7] <- mean(South_peak_W_gs$visregRes)
paper1_means[12,7] <- mean(South_peak_D_gs$visregRes)


######################################################################################################################
# Water_Content 
######################################################################################################################

finalmod.wc <- lmer(Water_Content ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat), # Run 3-way model
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y5)
vis_WC_D<-visreg(finalmod.wc, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_WC_W<-visreg(finalmod.wc, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_WC_D<-vis_WC_D$res ; Res_WC_W<-vis_WC_W$res # Extract residuals

#Break up Wet & Dry by region and pre-peak
North_pre_W_wc <- Res_WC_W %>% filter(Region=="North", Year==2010)
North_pre_D_wc <- Res_WC_D %>% filter(Region=="North", Year==2010)
North_peak_W_wc <- Res_WC_W %>% filter(Region=="North", Year==2016)
North_peak_D_wc <- Res_WC_D %>% filter(Region=="North", Year==2016)

Centre_pre_W_wc <- Res_WC_W %>% filter(Region=="Center", Year==2010)
Centre_pre_D_wc <- Res_WC_D %>% filter(Region=="Center", Year==2010)
Centre_peak_W_wc <- Res_WC_W %>% filter(Region=="Center", Year==2015)
Centre_peak_D_wc <- Res_WC_D %>% filter(Region=="Center", Year==2015)

South_pre_W_wc <- Res_WC_W %>% filter(Region=="South", Year==2011)
South_pre_D_wc <- Res_WC_D %>% filter(Region=="South", Year==2011)
South_peak_W_wc <- Res_WC_W %>% filter(Region=="South", Year==2014)
South_peak_D_wc <- Res_WC_D %>% filter(Region=="South", Year==2014)

#Insert Means in paper1_means dataframe
paper1_means[1,8] <- mean(North_pre_W_wc$visregRes)
paper1_means[2,8] <- mean(North_pre_D_wc$visregRes)
paper1_means[3,8] <- mean(North_peak_W_wc$visregRes)
paper1_means[4,8] <- mean(North_peak_D_wc$visregRes)

paper1_means[5,8] <- mean(Centre_pre_W_wc$visregRes)
paper1_means[6,8] <- mean(Centre_pre_D_wc$visregRes)
paper1_means[7,8] <- mean(Centre_peak_W_wc$visregRes)
paper1_means[8,8] <- mean(Centre_peak_D_wc$visregRes)

paper1_means[9,8] <- mean(South_pre_W_wc$visregRes)
paper1_means[10,8] <- mean(South_pre_D_wc$visregRes)
paper1_means[11,8] <- mean(South_peak_W_wc$visregRes)
paper1_means[12,8] <- mean(South_peak_D_wc$visregRes)

write_csv(paper1_means, "Data/paper1_means_complete.csv")








