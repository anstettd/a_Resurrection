#################
# Multivarate by region
#################
library(tidyverse)
library(ggfortify)
library(lme4)
library(lmtest)
library(car)
library(visreg)

y5 <- read.csv("Data/y5.csv", header=T) #Imports main dataset
y5$Block <- as.factor(y5$Block) ; y55Family <- as.factor(y5$Family) # prep factors
y5<-y5 %>% mutate(Region = ifelse(Latitude > 40, "North", "South_Centre"))
#trait.means.w <- data.frame() #Set up Wet Treatment data frame
#trait.means.d <- data.frame() #Set up Drought Treatment data frame
y5.w<- y5 %>% filter(Drought=="W") #Filter for Wet treatment data
y5.d<- y5 %>% filter(Drought=="D") #Filter for Drought treatment data
# get traits means for drought across year-region
site.year.means.wet <- y5.w %>% 
  group_by(Region, ID_Year, Year, Site.Lat, Drought) %>% 
  summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass"), mean, na.rm=TRUE)%>%
  ungroup()
#write.csv(site.year.means.wet ,'Data/region.means.w.csv') #Export file
site.year.means.dry <- y5.d %>% 
  group_by(Region, ID_Year, Year, Site.Lat, Drought) %>% 
  summarise_at(c("Experiment_Date", "Water_Content", "SLA", "Stomatal_Conductance", "Assimilation", "Biomass"), mean, na.rm=TRUE)%>%
  ungroup()
#write.csv(site.year.means.dry,'Data/region.means.d.csv') #Export file

#Assess correlation among response variables
pc1 <- prcomp(na.omit(y5[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
pc1_summary<-summary(pc1)
pc1_summary
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (46%)", ylab="PC2 (21%)")
pc1_scree<-pc1_summary$importance[2,]
plot_scree<-data.frame()
plot_scree[1,1]<-"PC1"
plot_scree[2,1]<-"PC2"
plot_scree[3,1]<-"PC3"
plot_scree[4,1]<-"PC4"
plot_scree[5,1]<-"PC5"
plot_scree[1,2]<-pc1_scree[1]
plot_scree[2,2]<-pc1_scree[2]
plot_scree[3,2]<-pc1_scree[3]
plot_scree[4,2]<-pc1_scree[4]
plot_scree[5,2]<-pc1_scree[5]
colnames(plot_scree)<-c("PC_Axis", "Variance_Explained")
#Make Scree Plot
scree_bar<-ggplot(plot_scree, aes(x=PC_Axis, y=Variance_Explained))+ geom_bar(stat = "identity")+
  scale_y_continuous(name="Variance Explained")+ scale_x_discrete(name="PC_Axis")+theme_classic() 
scree_bar + theme(axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0),
                  axis.text.y = element_text(size=12,face="bold"),
                  axis.title.x = element_text(color="black", size=12, vjust = 0, face="bold"),
                  axis.title.y = element_text(color="black", size=12, face="bold",vjust = 1.5,hjust=0.45))

#convert means into pc axes
tmeans.w.redu <- site.year.means.wet %>% select(Experiment_Date,Water_Content,SLA,Stomatal_Conductance,Assimilation)
pc.mean.values.w <- scale(tmeans.w.redu , pc1$center, pc1$scale) %*% pc1$rotation 
pc.means.w<-site.year.means.wet %>% select("Region","ID_Year","Site.Lat","Year","Drought")
pc.means.w<-cbind(pc.means.w,pc.mean.values.w)
#write.csv(pc.means.w,'Data/region.pc.means.w.csv') #Export file

tmeans.d.redu <- site.year.means.dry %>% select("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")
pc.mean.values.d <- scale(tmeans.d.redu , pc1$center, pc1$scale) %*% pc1$rotation 
pc.means.d<-site.year.means.dry %>% select("Region", "ID_Year","Site.Lat","Year","Drought")
pc.means.d<-cbind(pc.means.d,pc.mean.values.d)
#write.csv(pc.means.d,'Data/region.pc.means.d.csv') #Export file

pc.means <- rbind(pc.means.w,pc.means.d)
pc.means$Year <- as.factor(pc.means$Year)

#Make 4 pannel graph
pc.plot.mean <- ggplot(pc.means, aes(PC1,PC2,color=Drought, label=Year))+
  geom_point(size=1)+
  geom_path(arrow = arrow(angle = 15, type = "open"))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))
pc.plot.mean <- pc.plot.mean +
  scale_x_continuous(name="PC1 (57.7%)") +
  scale_y_continuous(name="PC2 (22.5%)") +
  theme_classic()+
  theme(legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
        axis.text.y = element_text(size=14,face="bold"),
        axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
        axis.title.y = element_text(color="black", size=16,vjust = 0, face="bold"))
#pc.plot.mean <- pc.plot.mean + theme(legend.text = element_text(size = 12, face = "bold"))
pc.plot.mean + facet_wrap( ~ Region)+ 
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))












