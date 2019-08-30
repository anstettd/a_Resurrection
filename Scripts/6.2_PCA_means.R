#################
# Make 12 pannel PCA graphs with site/year means only
#################
library(tidyverse)
library(ggrepel)
library(cowplot)
pc.means.w <- read.csv("Data/pc.means.w.csv", header=T)
pc.means.d <- read.csv("Data/pc.means.d.csv", header=T)
y3 <- read.csv("Data/y3.csv", header=T) #Imports main dataset
pc.means <- rbind(pc.means.w,pc.means.d)
pc.means$Year <- as.factor(pc.means$Year)


#Assess correlation among response variables
pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (51.7%)", ylab="PC2 (22.5%)")

#PCA Means Graph
pc_Labs<-c("32.9_S02"="A", "34.1_S11"="B", "34.3_S07"="C", "36.2_S10"="D","36.7_S08"="E", "37.5_S32" = "F", 
           "39.4_S29"="G", "39.7_S18"="H", "41.7_S17"="I", "41.8_S16"="J", "42.3_S36"="K", "43.4_S15"="L")
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
pc.plot.mean + facet_wrap( ~ Site.Lat, labeller = labeller(Site.Lat=pc_Labs), ncol=4)+ 
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))
        
        



#Cow Plot
site.year.marker<-c("S02","S11","S07","S10","S08","S32","S29","S18","S17","S16","S36","S15")
for(i in 1:12) {
  pc.mean_filter<- pc.means %>% filter(Site==as.character(site.year.marker[i]))
  pc.plot.mean <- ggplot(pc.mean_filter, aes(PC1,PC2,color=Drought, label=Year))+
    geom_point(size=1)+
    geom_path(arrow = arrow(angle = 15, type = "open"))+
    scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))
  pc.plot.mean <- pc.plot.mean +
    scale_x_continuous(name="PC1 (57.7%)",limits=c(-2.5,3))+
    scale_y_continuous(name="PC2 (22.5%)",limits=c(-1.5, 1.5))+
    theme_classic()+
    theme(legend.title = element_blank(),legend.position = "none")+
    theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0),
          axis.text.y = element_text(size=14,face="bold"),
          axis.title.x = element_text(color="black", size=16, vjust = 0, face="bold"),
          axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold"))
  assign(paste("PC_cow",i,sep="_"),pc.plot.mean)
}

PC_cow_2<-PC_cow_2 + theme(axis.title=element_blank(),axis.text=element_blank(), axis.ticks=element_blank())

plot_grid(PC_cow_1,PC_cow_2,PC_cow_3,PC_cow_4,PC_cow_5,PC_cow_6,PC_cow_7,PC_cow_8,PC_cow_9,PC_cow_10,PC_cow_11,PC_cow_12,
          labels = c('A','B','C','D','E','F','G','H','I','J','K','L'), label_x = .23, hjust = 0,label_size = 12)








pc.plot.mean <- pc.plot.mean + geom_text(aes(nudge_x, nudge_y, label = Year, size = 3.5))



#########################################################################################################################
#PCA Means Graph with Year lable
pc.plot.mean <- ggplot(pc.means, aes(PC1,PC2,color=Drought, label=Year))+
  geom_point(size=1)+
  geom_text_repel(size=3)+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))
pc.plot.mean <- pc.plot.mean + theme(legend.text = element_text(size = 12, face = "bold"),
                                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                                     axis.text.y = element_text(size=14,face="bold"),
                                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="PC1 48%") +
  scale_y_continuous(name="PC2 21%")
pc.plot.mean + facet_wrap( ~ Site, ncol=4)



#Graphs of individual sites
#pc.meansS2<-pc.means %>% filter(Site=="S02")
#pc.plot.mean.S2 <- ggplot(pc.meansS2, aes(PC1,PC2,color=Drought,shape=Year))+
#  geom_point()+
#  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
#  theme_classic()
#pc.plot.mean.S2 + theme(legend.text = element_text(size = 12, face = "bold"),
#                  axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
#                  axis.text.y = element_text(size=14,face="bold"),
#                  axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
#                  axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
#  scale_x_continuous(name="PC1 48%") +
#  scale_y_continuous(name="PC2 21%") 
