#################
# Generate climate Fig 1, Paper 1
#################
library(tidyverse)



### Fig 1D, make CMDA plot per site.lat 2010 - 2016 ###

wna.all<-read.csv('Data/wna_all.csv', header=T) #Imports CMDA for all site/year cominbations
wna.all<-wna.all %>% filter(Site!="S11")
wna.all<-wna.all %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Centre","South"))) %>%
  mutate(Site.Lat=paste(Latitude,Site,sep="_")) #Set up Site.Lat
wna.all$Region<-as.factor(wna.all$Region) #Make Region appear in logical order
wna.all$Region<-factor(wna.all$Region,levels=c("North","Centre","South"))
Site.lable<-c("32.89928_S02"="1","34.28425_S07"="2","36.20081_S10"="3","36.69096_S08"="4", 
              "37.539_S32"="5","39.39442_S29"="6","39.74298_S18"="7", "41.66546_S17"="8", 
              "41.80979_S16"="9","42.27411_S36"="10","43.37876_S15"="11")

#Make Graph
CMDA_Lat<-ggplot(wna.all, aes(x=Site.Lat, y=CMD.anom,fill=Region))+ 
  geom_boxplot()+
  scale_y_continuous(name="CMDA")+
  xlab("Site")+
  geom_hline(yintercept = 0, color="brown", size=0.8)+
  coord_flip()+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
CMDA_Lat
CMDA_Lat+ theme(legend.position = "none",
                         axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                         #axis.ticks.x = element_blank(),
                         axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
                         axis.text.y = element_text(size=14,face="bold"),
                         axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5))+
  scale_x_discrete(labels=Site.lable)
ggsave("Fig 1D.pdf", width = 4, height = 7, units = "in")










  
  Res_SLA_all_plot<-ggplot(Ref_SLA_filter, aes(Year, y=visregRes, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.8)+
  stat_smooth(method = "lm",aes(colour=Drought,fill=Drought), size = 1)+
  #facet_wrap(.~Region, labeller = labeller(Region=Site_Labs))+
  #scale_x_discrete(limits = Res_SLA_all$Year) +
  scale_y_continuous(name="SLA",limits=c(100,400))+
  scale_x_continuous(limits=c(2010,2016))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
Res_SLA_all_plot + theme(legend.position = "none",
                         axis.title.x=element_blank(),
                         #axis.ticks.x = element_blank(),
                         axis.text.x = element_text(size=0, face="bold", angle=0,hjust=0.5),
                         axis.text.y = element_text(size=0,face="bold"),
                         axis.title.y = element_text(color="black", size=0,vjust = 0, face="bold",hjust=0.5))
ggsave("SLA_Center.pdf", width = 4.5, height = 5, units = "in")

