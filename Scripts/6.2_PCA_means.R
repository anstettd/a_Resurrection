#################
# Make 12 pannel PCA graphs with site/year means only
#################
library(tidyverse)
library(ggrepel)
pc.means.w <- read.csv("Data/pc.means.w.csv", header=T)
pc.means.d <- read.csv("Data/pc.means.d.csv", header=T)
pc.means <- rbind(pc.means.w,pc.means.d)
pc.means$Year <- as.factor(pc.means$Year) 

#Assess correlation among response variables
pc1 <- prcomp(na.omit(y3[,c("Experiment_Date","Water_Content","SLA","Stomatal_Conductance","Assimilation","Biomass")]), scale=T)
summary(pc1)
biplot(pc1, scale=0, col=c("black", "red"), xlab = "PC1 (46%)", ylab="PC2 (21%)")


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

#PCA Means Graph
pc.plot.mean <- ggplot(pc.means, aes(PC1,PC2,color=Drought, label=Year))+
  geom_point(size=1)+
  geom_line(arrow = arrow(angle = 15, type = "open"))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))
pc.plot.mean <- pc.plot.mean + theme(legend.text = element_text(size = 12, face = "bold"),
                                     axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                                     axis.text.y = element_text(size=14,face="bold"),
                                     axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                     axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="PC1 48%") +
  scale_y_continuous(name="PC2 21%")
pc.plot.mean + facet_wrap( ~ Site, ncol=4)



pc.plot.mean <- pc.plot.mean + geom_text(aes(nudge_x, nudge_y, label = Year, size = 3.5))






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
