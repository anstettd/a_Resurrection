#################
# Slopes by Region 
#################
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)

y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factorsa

y5<-y5 %>% mutate(Region = ifelse(Latitude >= 40, "1.North", 
                                  ifelse((Latitude >35) & (Latitude <40), "2.Center","3.South")))
region.vec<-c("1.North", "2.Center", "3.South")
slopes.region<-data.frame(region.vec) #sets up site and site lat for slopes data frame
colnames(slopes.region)[1]<-"Region"


#SLA Vs Year
fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_SLA_D<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_SLA_W<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_SLA_D<-vis_SLA_D$res ; Res_SLA_W<-vis_SLA_W$res # Extract residuals
#Res_SLA_all<-rbind(Res_SLA_D, Res_SLA_W)

#Ref_SLA_filter<-Res_SLA_all %>% filter(Drought=="W")
#Wet
#1.North
Ref_SLA_filter<-Res_SLA_W %>% filter(Region=="1.North")
Ref_SLA_filter<- Ref_SLA_filter %>% mutate(Res.scale=scale(visregRes))
lm_SLA_D_S<-lm(Res.scale~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[1,2]<-summary_SLA_D$coefficients[2,1]
#Centre
Ref_SLA_filter<- Res_SLA_W %>% filter(Region=="2.Center")
Ref_SLA_filter<- Ref_SLA_filter %>% mutate(Res.scale=scale(visregRes))
lm_SLA_D_S<-lm(Res.scale~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[2,2]<-summary_SLA_D$coefficients[2,1]
#3.South
Ref_SLA_filter<- Res_SLA_W %>% filter(Region=="3.South")
Ref_SLA_filter<- Ref_SLA_filter %>% mutate(Res.scale=scale(visregRes))
lm_SLA_D_S<-lm(Res.scale~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[3,2]<-summary_SLA_D$coefficients[2,1]
colnames(slopes.region)[2]<-"SLA_Wet"

#Ref_SLA_filter<-Res_SLA_all %>% filter(Drought=="D")
#Drought
#1.North
Ref_SLA_filter<-Res_SLA_D %>% filter(Region=="1.North")
Ref_SLA_filter<- Ref_SLA_filter %>% mutate(Res.scale=scale(visregRes))
lm_SLA_D_S<-lm(Res.scale~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[1,3]<-summary_SLA_D$coefficients[2,1]
#Centre
Ref_SLA_filter<- Res_SLA_D %>% filter(Region=="2.Center")
Ref_SLA_filter<- Ref_SLA_filter %>% mutate(Res.scale=scale(visregRes))
lm_SLA_D_S<-lm(Res.scale~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[2,3]<-summary_SLA_D$coefficients[2,1]
#3.South
Ref_SLA_filter<- Res_SLA_D %>% filter(Region=="3.South")
Ref_SLA_filter<- Ref_SLA_filter %>% mutate(Res.scale=scale(visregRes))
lm_SLA_D_S<-lm(Res.scale~Year, data=Ref_SLA_filter)
summary_SLA_D<-summary(lm_SLA_D_S)
slopes.region[3,3]<-summary_SLA_D$coefficients[2,1]
colnames(slopes.region)[3]<-"SLA_Dry"

#Date of Flowering Vs Year
fullmod.FT <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_FT_D<-visreg(fullmod.FT, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_FT_W<-visreg(fullmod.FT, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_FT_D<-vis_FT_D$res ; Res_FT_W<-vis_FT_W$res # Extract residuals

#Wet
#1.North
Ref_FT_filter<-Res_FT_W %>% filter(Region=="1.North")
Ref_FT_filter<- Ref_FT_filter %>% mutate(Res.scale=scale(visregRes))
lm_FT_D_S<-lm(Res.scale~Year, data=Ref_FT_filter)
summary_FT_D<-summary(lm_FT_D_S)
slopes.region[1,4]<-summary_FT_D$coefficients[2,1]
#Centre
Ref_FT_filter<- Res_FT_W %>% filter(Region=="2.Center")
Ref_FT_filter<- Ref_FT_filter %>% mutate(Res.scale=scale(visregRes))
lm_FT_D_S<-lm(Res.scale~Year, data=Ref_FT_filter)
summary_FT_D<-summary(lm_FT_D_S)
slopes.region[2,4]<-summary_FT_D$coefficients[2,1]
#3.South
Ref_FT_filter<- Res_FT_W %>% filter(Region=="3.South")
Ref_FT_filter<- Ref_FT_filter %>% mutate(Res.scale=scale(visregRes))
lm_FT_D_S<-lm(Res.scale~Year, data=Ref_FT_filter)
summary_FT_D<-summary(lm_FT_D_S)
slopes.region[3,4]<-summary_FT_D$coefficients[2,1]
colnames(slopes.region)[4]<-"FT_Wet"

#Drought
#1.North
Ref_FT_filter<-Res_FT_D %>% filter(Region=="1.North")
Ref_FT_filter<- Ref_FT_filter %>% mutate(Res.scale=scale(visregRes))
lm_FT_D_S<-lm(Res.scale~Year, data=Ref_FT_filter)
summary_FT_D<-summary(lm_FT_D_S)
slopes.region[1,5]<-summary_FT_D$coefficients[2,1]
#Centre
Ref_FT_filter<- Res_FT_D %>% filter(Region=="2.Center")
Ref_FT_filter<- Ref_FT_filter %>% mutate(Res.scale=scale(visregRes))
lm_FT_D_S<-lm(Res.scale~Year, data=Ref_FT_filter)
summary_FT_D<-summary(lm_FT_D_S)
slopes.region[2,5]<-summary_FT_D$coefficients[2,1]
#3.South
Ref_FT_filter<- Res_FT_D %>% filter(Region=="3.South")
Ref_FT_filter<- Ref_FT_filter %>% mutate(Res.scale=scale(visregRes))
lm_FT_D_S<-lm(Res.scale~Year, data=Ref_FT_filter)
summary_FT_D<-summary(lm_FT_D_S)
slopes.region[3,5]<-summary_FT_D$coefficients[2,1]
colnames(slopes.region)[5]<-"FT_Dry"


#Water Content Vs Year
fullmod.WC <- lmer(Water_Content ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_WC_D<-visreg(fullmod.WC, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_WC_W<-visreg(fullmod.WC, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_WC_D<-vis_WC_D$res ; Res_WC_W<-vis_WC_W$res # Extract residuals

#Wet
#1.North
Ref_WC_filter<-Res_WC_W %>% filter(Region=="1.North")
Ref_WC_filter<- Ref_WC_filter %>% mutate(Res.scale=scale(visregRes))
lm_WC_D_S<-lm(Res.scale~Year, data=Ref_WC_filter)
summary_WC_D<-summary(lm_WC_D_S)
slopes.region[1,6]<-summary_WC_D$coefficients[2,1]
#Centre
Ref_WC_filter<- Res_WC_W %>% filter(Region=="2.Center")
Ref_WC_filter<- Ref_WC_filter %>% mutate(Res.scale=scale(visregRes))
lm_WC_D_S<-lm(Res.scale~Year, data=Ref_WC_filter)
summary_WC_D<-summary(lm_WC_D_S)
slopes.region[2,6]<-summary_WC_D$coefficients[2,1]
#3.South
Ref_WC_filter<- Res_WC_W %>% filter(Region=="3.South")
Ref_WC_filter<- Ref_WC_filter %>% mutate(Res.scale=scale(visregRes))
lm_WC_D_S<-lm(Res.scale~Year, data=Ref_WC_filter)
summary_WC_D<-summary(lm_WC_D_S)
slopes.region[3,6]<-summary_WC_D$coefficients[2,1]
colnames(slopes.region)[6]<-"WC_Wet"

#Drought
#1.North
Ref_WC_filter<-Res_WC_D %>% filter(Region=="1.North")
Ref_WC_filter<- Ref_WC_filter %>% mutate(Res.scale=scale(visregRes))
lm_WC_D_S<-lm(Res.scale~Year, data=Ref_WC_filter)
summary_WC_D<-summary(lm_WC_D_S)
slopes.region[1,7]<-summary_WC_D$coefficients[2,1]
#Centre
Ref_WC_filter<- Res_WC_D %>% filter(Region=="2.Center")
Ref_WC_filter<- Ref_WC_filter %>% mutate(Res.scale=scale(visregRes))
lm_WC_D_S<-lm(Res.scale~Year, data=Ref_WC_filter)
summary_WC_D<-summary(lm_WC_D_S)
slopes.region[2,7]<-summary_WC_D$coefficients[2,1]
#3.South
Ref_WC_filter<- Res_WC_D %>% filter(Region=="3.South")
Ref_WC_filter<- Ref_WC_filter %>% mutate(Res.scale=scale(visregRes))
lm_WC_D_S<-lm(Res.scale~Year, data=Ref_WC_filter)
summary_WC_D<-summary(lm_WC_D_S)
slopes.region[3,7]<-summary_WC_D$coefficients[2,1]
colnames(slopes.region)[7]<-"WC_Dry"



#Assimilation Vs Year
fullmod.A <- lmer(Assimilation ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_A_D<-visreg(fullmod.A, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_A_W<-visreg(fullmod.A, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_A_D<-vis_A_D$res ; Res_A_W<-vis_A_W$res # Extract residuals

#Wet
#1.North
Ref_A_filter<-Res_A_W %>% filter(Region=="1.North")
Ref_A_filter<- Ref_A_filter %>% mutate(Res.scale=scale(visregRes))
lm_A_D_S<-lm(Res.scale~Year, data=Ref_A_filter)
summary_A_D<-summary(lm_A_D_S)
slopes.region[1,8]<-summary_A_D$coefficients[2,1]
#Centre
Ref_A_filter<- Res_A_W %>% filter(Region=="2.Center")
Ref_A_filter<- Ref_A_filter %>% mutate(Res.scale=scale(visregRes))
lm_A_D_S<-lm(Res.scale~Year, data=Ref_A_filter)
summary_A_D<-summary(lm_A_D_S)
slopes.region[2,8]<-summary_A_D$coefficients[2,1]
#3.South
Ref_A_filter<- Res_A_W %>% filter(Region=="3.South")
Ref_A_filter<- Ref_A_filter %>% mutate(Res.scale=scale(visregRes))
lm_A_D_S<-lm(Res.scale~Year, data=Ref_A_filter)
summary_A_D<-summary(lm_A_D_S)
slopes.region[3,8]<-summary_A_D$coefficients[2,1]
colnames(slopes.region)[8]<-"A_Wet"

#Drought
#1.North
Ref_A_filter<-Res_A_D %>% filter(Region=="1.North")
Ref_A_filter<- Ref_A_filter %>% mutate(Res.scale=scale(visregRes))
lm_A_D_S<-lm(Res.scale~Year, data=Ref_A_filter)
summary_A_D<-summary(lm_A_D_S)
slopes.region[1,9]<-summary_A_D$coefficients[2,1]
#Centre
Ref_A_filter<- Res_A_D %>% filter(Region=="2.Center")
Ref_A_filter<- Ref_A_filter %>% mutate(Res.scale=scale(visregRes))
lm_A_D_S<-lm(Res.scale~Year, data=Ref_A_filter)
summary_A_D<-summary(lm_A_D_S)
slopes.region[2,9]<-summary_A_D$coefficients[2,1]
#3.South
Ref_A_filter<- Res_A_D %>% filter(Region=="3.South")
Ref_A_filter<- Ref_A_filter %>% mutate(Res.scale=scale(visregRes))
lm_A_D_S<-lm(Res.scale~Year, data=Ref_A_filter)
summary_A_D<-summary(lm_A_D_S)
slopes.region[3,9]<-summary_A_D$coefficients[2,1]
colnames(slopes.region)[9]<-"A_Dry"



#Stomatal Conductance Vs Year
fullmod.gs <- lmer(Stomatal_Conductance ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                   control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
vis_gs_D<-visreg(fullmod.gs, xvar="Year", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_gs_W<-visreg(fullmod.gs, xvar="Year", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_gs_D<-vis_gs_D$res ; Res_gs_W<-vis_gs_W$res # Extract residuals

#Wet
#1.North
Ref_gs_filter<-Res_gs_W %>% filter(Region=="1.North")
Ref_gs_filter<- Ref_gs_filter %>% mutate(Res.scale=scale(visregRes))
lm_gs_D_S<-lm(Res.scale~Year, data=Ref_gs_filter)
summary_gs_D<-summary(lm_gs_D_S)
slopes.region[1,10]<-summary_gs_D$coefficients[2,1]
#Centre
Ref_gs_filter<- Res_gs_W %>% filter(Region=="2.Center")
Ref_gs_filter<- Ref_gs_filter %>% mutate(Res.scale=scale(visregRes))
lm_gs_D_S<-lm(Res.scale~Year, data=Ref_gs_filter)
summary_gs_D<-summary(lm_gs_D_S)
slopes.region[2,10]<-summary_gs_D$coefficients[2,1]
#3.South
Ref_gs_filter<- Res_gs_W %>% filter(Region=="3.South")
Ref_gs_filter<- Ref_gs_filter %>% mutate(Res.scale=scale(visregRes))
lm_gs_D_S<-lm(Res.scale~Year, data=Ref_gs_filter)
summary_gs_D<-summary(lm_gs_D_S)
slopes.region[3,10]<-summary_gs_D$coefficients[2,1]
colnames(slopes.region)[10]<-"gs_Wet"

#Drought
#1.North
Ref_gs_filter<-Res_gs_D %>% filter(Region=="1.North")
Ref_gs_filter<- Ref_gs_filter %>% mutate(Res.scale=scale(visregRes))
lm_gs_D_S<-lm(Res.scale~Year, data=Ref_gs_filter)
summary_gs_D<-summary(lm_gs_D_S)
slopes.region[1,11]<-summary_gs_D$coefficients[2,1]
#Centre
Ref_gs_filter<- Res_gs_D %>% filter(Region=="2.Center")
Ref_gs_filter<- Ref_gs_filter %>% mutate(Res.scale=scale(visregRes))
lm_gs_D_S<-lm(Res.scale~Year, data=Ref_gs_filter)
summary_gs_D<-summary(lm_gs_D_S)
slopes.region[2,11]<-summary_gs_D$coefficients[2,1]
#3.South
Ref_gs_filter<- Res_gs_D %>% filter(Region=="3.South")
Ref_gs_filter<- Ref_gs_filter %>% mutate(Res.scale=scale(visregRes))
lm_gs_D_S<-lm(Res.scale~Year, data=Ref_gs_filter)
summary_gs_D<-summary(lm_gs_D_S)
slopes.region[3,11]<-summary_gs_D$coefficients[2,1]
colnames(slopes.region)[11]<-"gs_Dry"





###################

slopes.g<-slopes.region %>% gather(key,slope.r,2:11) %>% 
  separate(key, into=c("variable","Treatment"), sep = "_") %>%
  mutate(Drought=Treatment) %>%
  mutate(Region2=Region) %>% 
  mutate(Drought2=Treatment) %>%
  mutate(Trait=variable) %>% 
  mutate(order.var=ifelse(variable == "SLA", 1, 
                         ifelse(variable =="FT", 2,
                                ifelse(variable =="WC", 3,
                                       ifelse(variable =="A", 4,5))))) %>%
  arrange(Region) %>%
    arrange(order.var) %>%
  unite("all.dat",Region2,Treatment)%>%
  unite("Trait_D",Trait,Drought2)

#slopes.SLA<-slopes.g %>% filter(variable=="SLA")
#slopes.FT<-slopes.g %>% filter(variable=="FT")
#slopes.WC<-slopes.g %>% filter(variable=="WC")
#slopes.A<-slopes.g %>% filter(variable=="A")
#slopes.gs<-slopes.g %>% filter(variable=="gs")

slopes.g$variable<-as.factor(slopes.g$variable)
slopes.g$variable<-factor(slopes.g$variable,levels=c("SLA","FT","WC","A","gs"))
slopes.g$Trait_D<-as.factor(slopes.g$Trait_D)
slopes.g$Trait_D<-factor(slopes.g$Trait_D,levels=c("SLA_Wet","SLA_Dry","FT_Wet","FT_Dry","WC_Wet", "WC_Dry",
                                                   "A_Wet","A_Dry","gs_Wet","gs_Dry"))
slopes.g$Drought<-as.factor(slopes.g$Drought)
slopes.g$Drought<-factor(slopes.g$Drought,levels=c("Wet","Dry"))
slopes.g$all.dat<-as.factor(slopes.g$all.dat)
slopes.g$all.dat<-factor(slopes.g$all.dat,levels=c("1.North_Wet", "1.North_Dry","2.Center_Wet","2.Center_Dry",
   "3.South_Wet","3.South_Dry"))

#var_Labs<-c("SLA"="SLA", "FT"="Flowering Time", "WC"="Water Content", "A"="Assimilation","gs"="Stomatal Conductance")

write.csv(slopes.g, "slopes.g.csv")




# Slope By Trait
ggplot(slopes.g, aes(x=all.dat, y=slope.r, fill=Drought)) + 
         geom_bar(stat = "identity")+
  scale_fill_manual(values= c("Wet"="#006600","Dry"="#FF7700")) +
  scale_y_continuous(name="Slope")+
  theme( axis.title.x=element_blank(),
          #axis.ticks.x = element_blank(),
          axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.08,vjust = 0.5),
          axis.text.y = element_text(size=12,face="bold"),
          axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))+
  facet_wrap(.~variable,nrow=3,ncol=2) +
  theme(legend.title = element_blank(),legend.text = element_text(size=10,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=0))+
  scale_x_discrete(labels=c("1.North_Wet" = "North", "1.North_Dry" = "", 
                            "2.Center_Wet" = "Center","2.Center_Dry" = "",
                            "3.South_Wet" = "South","3.South_Dry" = ""))
ggsave("Slopes_trait.pdf", width = 7, height = 7, units = "in")

Region_Labs<-c("1.North"="North", "2.Center"="Centre", "3.South"="South")
# Slopes by Region
ggplot(slopes.g, aes(x=Trait_D, y=slope.r, fill=Drought)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values= c("Wet"="#006600","Dry"="#FF7700")) +
  scale_y_continuous(name="Slope")+
  theme( axis.title.x=element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.x = element_text(size=12, face="bold", angle=0,hjust=-0.6,vjust = 0.5),
         axis.text.y = element_text(size=12,face="bold"),
         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))+
  facet_wrap(.~Region,nrow=3,ncol=1, labeller = labeller(Region=Region_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=10,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=0))+
  scale_x_discrete(labels=c("SLA_Wet" = "SLA", "SLA_Dry" = "", 
                            "FT_Wet" = "FT", "FT_Dry" = "", 
                            "WC_Wet" = "WC", "WC_Dry" = "", 
                            "A_Wet" = "A", "A_Dry" = "", 
                            "gs_Wet" = "gs", "gs_Dry" = ""))
                
ggsave("Slopes_region.pdf", width = 7, height = 7, units = "in")


