#################
# Put site/year means into PCA coordinate system
#################
library(tidyverse)
library(ggfortify)
library(tidyverse)

y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
trait.means.w <- read.csv("Data/trait.means.w.csv", header=T)
trait.means.d <- read.csv("Data/trait.means.d.csv", header=T)
trait.means.w <- trait.means.w %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_"))
trait.means.d <- trait.means.d %>% mutate(Site.Lat = paste(round(Latitude,1), Site, sep="_"))

#Assess correlation among response variables
pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
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



#convert wet means into pc axes
tmeans.w.redu <- trait.means.w %>% select("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")
pc.mean.values.w <- scale(tmeans.w.redu , pc1$center, pc1$scale) %*% pc1$rotation 
pc.means.w<-trait.means.w %>% select("ID_Year","Site.Lat","Site","Year","Drought")
pc.means.w<-cbind(pc.means.w,pc.mean.values.w)
write.csv(pc.means.w,'Data/pc.means.w.csv') #Export file

#convert drought means into pc axes
tmeans.d.redu <- trait.means.d %>% select("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")
pc.mean.values.d <- scale(tmeans.d.redu , pc1$center, pc1$scale) %*% pc1$rotation 
pc.means.d<-trait.means.d %>% select("ID_Year","Site.Lat","Site","Year","Drought")
pc.means.d<-cbind(pc.means.d,pc.mean.values.d)
write.csv(pc.means.d,'Data/pc.means.d.csv') #Export file