#### PROJECT: Mimulus cardinalis demography 2010-2014
#### PURPOSE: Plot climate data for M. cardinalis study populations
#### Climate data were derived from Climate WNA v. 5.41
#### AUTHOR: Seema Sheth
#### DATE LAST MODIFIED: 20171127

# remove objects and clear workspace
rm(list = ls(all=TRUE))

require(plyr)
require(dplyr)
require(ggplot2)
require(gridExtra)
library(RColorBrewer)

# set working directory
setwd("/Users/ssheth/Google Drive/demography_PNAS_November2017")

# read in .csv file of climate WNA data for each site and create new grouping variable for year
dat=read.csv("Data/DemoSitesWeather_ClimateWNAOutput_1951-2014YT.csv")
dat=subset(dat,Year<2015)

# select relevant columns
dat=arrange(dat,Year) %>% select(ID1,Latitude,MAT,MAP,Year)

# create a column coloring site based on latitude
lat_cols=colorRampPalette(brewer.pal(11,"Spectral"))
dat$Col=lat_cols(32)[as.numeric(cut(dat$Latitude,breaks = 32))]
site.cols=subset(dat,select=c("Latitude","Col")) %>% unique() %>% arrange(Latitude)
site.cols$transparent <- adjustcolor(site.cols$Col, alpha.f = 0.75) 

# subset climate data from historical (1951-2000) vs. study years (2010-2014)
clim_hist=subset(dat,Year<2001)  %>% arrange(-Latitude)
clim_hist$Population=rev(rep(seq(1,32),each=50))
clim_study=subset(dat,Year>2009) %>% arrange(-Latitude)
clim_study$Population=rev(rep(seq(1,32),each=5))

#********************
### FIGURE 2B,C ###
#********************

# mean annual temperature
mat_plot=ggplot(clim_hist, aes(group=Population,x=Population, y=MAT, fill=Col,colour=Col)) + geom_boxplot(notch=TRUE,size=1.3,width=0.75) + 
  guides(fill=FALSE,color=FALSE) + coord_flip() + scale_fill_identity(clim_hist$Col) +
  scale_color_identity(clim_hist$Col) + # use this for colored lines
  geom_point(aes(y=MAT,x=Population),clim_study,shape=22,size=1.5,fill="black",colour="black") +
  ylab(expression(paste("MAT (",degree~C,")"))) +
  scale_x_continuous(breaks=c(3,8,13,18,23,28),
                     labels=c("30","25","20","15","10","5")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size=18),
                     axis.title=element_text(size=20)) +
  ggtitle('B') + theme(plot.title=element_text(hjust=0,size=18))

# mean annual precipitation
map_plot=ggplot(clim_hist, aes(group=Population,x=Population, y=MAP, fill=Col,colour=Col)) + geom_boxplot(notch=TRUE,size=1.3,width=0.75) + 
  guides(fill=FALSE,color=FALSE) + coord_flip() + scale_fill_identity(clim_hist$Col) +
  scale_y_log10() +
  scale_color_identity(clim_hist$Col) + # use this for colored lines
  geom_point(aes(y=MAP,x=Population),clim_study,shape=22,size=1.5,fill="black",colour="black") +
  ylab("MAP (mm)") +
  scale_x_continuous(breaks=c(3,8,13,18,23,28),
                     labels=c("30","25","20","15","10","5")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size=18),
                     axis.title=element_text(size=20),axis.title.y=element_blank()) +
  ggtitle('C') + theme(plot.title=element_text(hjust=0,size=18))

# print to pdf
pdf("Figures/Fig2bc_MAT_MAP.pdf",width=8,height=8)    
grid.arrange(mat_plot,map_plot,ncol=2, nrow=1, widths=c(1,1),heights=1)
dev.off()

#********************
### SUPPLEMENTARY FIGURE S4,S5 ###
#********************

# mean annual precipitation, Fig. S4
# print to pdf
pdf(file="Figures/FigS4_MAP.pdf",width=11,height=8.5)
par(las=1,bty="l",xpd=NA,cex.lab=2.3,mar=c(5,6,4,4.5)+.1)

boxplot(clim_hist$MAP~clim_hist$Latitude,col=site.cols$transparent,notch=TRUE,ylab="",xlab="MAP (mm)",horizontal=TRUE,cex=0.8,yaxt="n",cex.lab=2.3,cex.axis=2,ylim=c(min((dat$MAP)),max((dat$MAP))),log="x")

# add y-axis
axis(side=2,at=c(3,8,13,18,23,28),labels=c("30","25","20","15","10","5"),las=2,cex.axis=2)
mtext("C",side=3,cex=1.5,adj=-0.22)

# add y-axis title
title(ylab="Population",cex.lab=2.3,line=4)

# climate during study years
#2010
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2010)
  text((dat.site$MAP),(dat.site$Population-.5),labels="0",cex=1.2)
}
#2011
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2011)
  text((dat.site$MAP),(dat.site$Population-.5),labels="1",cex=1.2)
}
#2012
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2012)
  text((dat.site$MAP),(dat.site$Population-.5),labels="2",cex=1.2)
}
#2013
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2013)
  text((dat.site$MAP),(dat.site$Population-.5),labels="3",cex=1.2)
}
#2014
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2014)
  text((dat.site$MAP),(dat.site$Population-.5),labels="4",cex=1.2)
}

dev.off()

# mean annual temperature, Fig. S5
# print to pdf
pdf(file="Figures/FigS5_MAT.pdf",width=11,height=8.5)
par(las=1,bty="l",xpd=NA,cex.lab=2.3,mar=c(5,6,4,4.5)+.1)

boxplot(clim_hist$MAT~clim_hist$Latitude,col=site.cols$transparent,notch=TRUE,ylab="",xlab=expression(paste("MAT (",degree~C,")")),horizontal=TRUE,cex=0.8,yaxt="n",cex.lab=2.3,cex.axis=2,ylim=c(min(dat$MAT),max(dat$MAT)))

# add y-axis
axis(side=2,at=c(3,8,13,18,23,28),labels=c("30","25","20","15","10","5"),las=2,cex.axis=2)

# add y-axis title
title(ylab="Population",cex.lab=2.3,line=4)

# climate during study years
#2010
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2010)
  text(dat.site$MAT,(dat.site$Population-.5),labels="0",cex=1.2)
}
#2011
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2011)
  text(dat.site$MAT,(dat.site$Population-.5),labels="1",cex=1.2)
}
#2012
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2012)
  text(dat.site$MAT,(dat.site$Population-.5),labels="2",cex=1.2)
}
#2013
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2013)
  text(dat.site$MAT,(dat.site$Population-.5),labels="3",cex=1.2)
}
#2014
for (i in 1:length(clim_study$Population)) {
  dat.site=subset(clim_study,ID1==ID1[i]&Year==2014)
  text(dat.site$MAT,(dat.site$Population-.5),labels="4",cex=1.2)
}

# turn off plotting device
dev.off()

#********************
### SUPPLEMENTARY FIGURE S6 ###
#********************

# calculate 50yr historical mean climate (1951-2010)
mean_clim_hist=ddply(clim_hist, "Population", summarise,
      mean_MAT = mean(MAT),
      mean_MAP=mean(log10(MAP)))

# calculate temperature and precipitation anomalies (clim in study yr - 50yr historical mean climate)
clim_study=join(clim_study,mean_clim_hist)
clim_study$MAT_anomaly=clim_study$MAT-clim_study$mean_MAT
clim_study$MAP_anomaly=log10(clim_study$MAP)-clim_study$mean_MAP

mat_anomaly=ggplot(clim_study,aes(group=Population,y=Population, x=MAT_anomaly, fill=Col,colour=Col,shape=factor(Year))) + geom_line(size=1.5) + geom_point(size=5,col="black") +
guides(fill="none",color="none",shape="none") + scale_fill_identity(clim_study$Col) +
  scale_color_identity(clim_study$Col) + # use this for colored lines
  scale_shape_manual(values=c(21,22,23,24,25)) +
  xlab(expression(paste("MAT (",degree~C,") anomaly"))) +
  scale_y_continuous(breaks=c(3,8,13,18,23,28),
                     labels=c("30","25","20","15","10","5")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size=18),
                     axis.title=element_text(size=20)) +
  geom_vline(xintercept=0,linetype="dashed") +
  ggtitle('A') + theme(plot.title=element_text(hjust=0,size=18))

map_anomaly=ggplot(clim_study,aes(group=Population,y=Population, x=MAP_anomaly, fill=Col,colour=Col,shape=factor(Year)))  + geom_line(size=1.5) +geom_point(size=5,col="black") +
  scale_fill_identity(clim_study$Col) +
  scale_color_identity(clim_study$Col) + # use this for colored lines
  scale_shape_manual(values=c(21,22,23,24,25)) +
  xlab(expression(paste("log"[10]," ","(MAP [mm]) anomaly"))) +
  scale_y_continuous(breaks=c(3,8,13,18,23,28),
                     labels=c("30","25","20","15","10","5")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size=18),
                     axis.title=element_text(size=20),axis.title.y=element_blank(),legend.position = "right",legend.title=element_blank()) +
  geom_vline(xintercept=0,linetype="dashed") +
  ggtitle('B') + theme(plot.title=element_text(hjust=0,size=18))

# print to pdf
pdf("Figures/FigS6_MAT_MAP_anomalies.pdf",width=8,height=8)    
grid.arrange(mat_anomaly,map_anomaly,ncol=2, nrow=1, widths=c(1,1),heights=1)
dev.off()

















  
  


