#################
# Slopes by Region 
# Please NOTE: Must run entire script at once (or each section sequentially) to get correct tabulation. 
# Please clear the env if you are re-running the code
#################
library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)

y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
slope.reg <- read.csv("Data/slopes.region.csv", header=T) #Imports 
y5$Block <- as.factor(y5$Block) ; y5$Family <- as.factor(y5$Family) # prep factors
y5<-y5 %>% mutate(Region = ifelse(Latitude >= 40, "1.North", 
                                  ifelse((Latitude >35) & (Latitude <40), "2.Center","3.South")))
treatment.v<-c("W", "D")
region.v<-c("1.North", "2.Center", "3.South")
order.row<-1

#SLA Vs Year
fullmod.SLA <- lmer(SLA ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
for (i in 1:2){
  vis_SLA<-visreg(fullmod.SLA, xvar="Year", by="Region", cond=list(Drought=treatment.v[i]))
  Res_SLA<-vis_SLA$res
  for (j in 1:3){
  Ref_SLA_filter<-Res_SLA %>% filter(Region==region.v[j])
  Ref_SLA_filter<- Ref_SLA_filter %>% mutate(Res.scale=scale(visregRes))
  lm_SLA<-lm(Res.scale~Year, data=Ref_SLA_filter)
  summary_SLA<-summary(lm_SLA)
  slope.reg[order.row,4]<-summary_SLA$coefficients[2,1]
  slope.reg[order.row,5]<-summary_SLA$coefficients[2,2]
  order.row<-order.row+1
  }
}

#FT Vs Year
fullmod.FT <- lmer(Experiment_Date ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
for (i in 1:2){
  vis_FT<-visreg(fullmod.FT, xvar="Year", by="Region", cond=list(Drought=treatment.v[i]))
  Res_FT<-vis_FT$res
  for (j in 1:3){
    Ref_FT_filter<-Res_FT %>% filter(Region==region.v[j])
    Ref_FT_filter<- Ref_FT_filter %>% mutate(Res.scale=scale(visregRes))
    lm_FT<-lm(Res.scale~Year, data=Ref_FT_filter)
    summary_FT<-summary(lm_FT)
    slope.reg[order.row,4]<-summary_FT$coefficients[2,1]
    slope.reg[order.row,5]<-summary_FT$coefficients[2,2]
    order.row<-order.row+1
  }
}


#WC Vs Year
fullmod.WC <- lmer(Water_Content ~ Region + Year + Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)

for (i in 1:2){
  vis_WC<-visreg(fullmod.WC, xvar="Year", by="Region", cond=list(Drought=treatment.v[i]))
  Res_WC<-vis_WC$res
  for (j in 1:3){
    Ref_WC_filter<-Res_WC %>% filter(Region==region.v[j])
    Ref_WC_filter<- Ref_WC_filter %>% mutate(Res.scale=scale(visregRes))
    lm_WC<-lm(Res.scale~Year, data=Ref_WC_filter)
    summary_WC<-summary(lm_WC)
    slope.reg[order.row,4]<-summary_WC$coefficients[2,1]
    slope.reg[order.row,5]<-summary_WC$coefficients[2,2]
    order.row<-order.row+1
  }
}


#Assimilation Vs Year
fullmod.A <- lmer(Assimilation ~ Region*Year*Drought + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
for (i in 1:2){
  vis_A<-visreg(fullmod.A, xvar="Year", by="Region", cond=list(Drought=treatment.v[i]))
  Res_A<-vis_A$res
  for (j in 1:3){
    Ref_A_filter<-Res_A %>% filter(Region==region.v[j])
    Ref_A_filter<- Ref_A_filter %>% mutate(Res.scale=scale(visregRes))
    lm_A<-lm(Res.scale~Year, data=Ref_A_filter)
    summary_A<-summary(lm_A)
    slope.reg[order.row,4]<-summary_A$coefficients[2,1]
    slope.reg[order.row,5]<-summary_A$coefficients[2,2]
    order.row<-order.row+1
  }
}

#Stomatal Conductance Vs Year
fullmod.gs <- lmer(Stomatal_Conductance ~ Region*Year*Drought  + (1|Family) + (1|Block) + (1|Site.Lat),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=y5)
for (i in 1:2){
  vis_gs<-visreg(fullmod.gs, xvar="Year", by="Region", cond=list(Drought=treatment.v[i]))
  Res_gs<-vis_gs$res
  for (j in 1:3){
    Ref_gs_filter<-Res_gs %>% filter(Region==region.v[j])
    Ref_gs_filter<- Ref_gs_filter %>% mutate(Res.scale=scale(visregRes))
    lm_gs<-lm(Res.scale~Year, data=Ref_gs_filter)
    summary_gs<-summary(lm_gs)
    slope.reg[order.row,4]<-summary_gs$coefficients[2,1]
    slope.reg[order.row,5]<-summary_gs$coefficients[2,2]
    order.row<-order.row+1
  }
}

#Add in dummy varaible to organize X-axis into showing Wet and Dry seperately per region
slope.reg<- slope.reg %>% mutate(x_order=paste(Region,Drought,sep="_"))

#Create dummy variable to sort variables in non-aphabtical order
slope.reg<- slope.reg %>% mutate(order.var=ifelse(Variable == "SLA", 1, 
                        ifelse(Variable =="FT", 2,
                               ifelse(Variable =="WC", 3,
                                      ifelse(Variable =="A", 4,5)))))

#Set correct order for all levels
slope.reg$Variable<-as.factor(slope.reg$Variable)
slope.reg$Variable<-factor(slope.reg$Variable,levels=c("SLA","FT","WC","A","gs"))
#slope.reg$Trait_D<-as.factor(slope.reg$Trait_D)
#slope.reg$Trait_D<-factor(slope.reg$Trait_D,levels=c("SLA_Wet","SLA_Dry","FT_Wet","FT_Dry","WC_Wet", "WC_Dry",
#                                                   "A_Wet","A_Dry","gs_Wet","gs_Dry"))
slope.reg$Drought<-as.factor(slope.reg$Drought)
slope.reg$Drought<-factor(slope.reg$Drought,levels=c("Wet","Dry"))
slope.reg$x_order<-as.factor(slope.reg$x_order)
slope.reg$x_order<-factor(slope.reg$x_order,levels=c("1.North_Wet", "1.North_Dry","2.Center_Wet","2.Center_Dry",
                                                   "3.South_Wet","3.South_Dry"))


# Slope By Trait
Trait_Labs<-c("SLA"="(A) SLA", "FT"=" (B) Date of Flowering", "WC"="(C) Water Content", 
              "A"="(D) Assimilation","gs"="(E) Stomatal Conductance")
ggplot(slope.reg, aes(x=x_order, y=Slopes, fill=Drought)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values= c("Wet"="#006600","Dry"="#FF7700")) +
  scale_y_continuous(name="Slope")+
  geom_errorbar(mapping=aes(ymin=Slopes-Slopes_STDER, ymax=Slopes+Slopes_STDER), width=0.2, size=1)+
  geom_hline(yintercept=0)+
  theme( axis.title.x=element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.08,vjust = 0.5),
         axis.text.y = element_text(size=12,face="bold"),
         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))+
  facet_wrap(.~Variable,nrow=3,ncol=2,labeller = labeller(Variable=Trait_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=14,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=0))+
  scale_x_discrete(labels=c("1.North_Wet" = "North", "1.North_Dry" = "", 
                            "2.Center_Wet" = "Center","2.Center_Dry" = "",
                            "3.South_Wet" = "South","3.South_Dry" = ""))

#ggsave("Slopes_all_traits.pdf", width = 7, height = 7, units = "in")



# Slope for SLA & FT
slope.SLA.FT<-slope.reg %>% filter(Variable=="SLA" | Variable=="FT")
Trait_Labs<-c("SLA"="A   SLA", "FT"="B   Date of Flowering")
ggplot(slope.SLA.FT, aes(x=x_order, y=Slopes, fill=Drought)) + 
  geom_bar(stat = "identity")+
  scale_fill_manual(values= c("Wet"="#006600","Dry"="#FF7700")) +
  scale_y_continuous(name="Slope",limits=c(-0.2,0.2))+
  geom_errorbar(mapping=aes(ymin=Slopes-Slopes_STDER, ymax=Slopes+Slopes_STDER), width=0.2, size=1)+
  geom_hline(yintercept=0)+
  theme( axis.title.x=element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.x = element_text(size=16, face="bold", angle=0,hjust=0.15,vjust = 0.5),
         axis.text.y = element_text(size=12,face="bold"),
         axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))+
  facet_wrap(.~Variable,nrow=3,ncol=2,labeller = labeller(Variable=Trait_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=14,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=0))+
  scale_x_discrete(labels=c("1.North_Wet" = "North", "1.North_Dry" = "", 
                            "2.Center_Wet" = "Center","2.Center_Dry" = "",
                            "3.South_Wet" = "South","3.South_Dry" = ""))
#ggsave("Slopes_SLA_FT.pdf", width = 7, height = 4.5, units = "in")


