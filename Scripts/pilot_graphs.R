###########
#Pilot Rescue Data Analysis
###########

setwd("/Users/daniel_anstett/Dropbox/a_Resurrection/Data")
Y <- read.csv("pilot.csv", header=T)

bench<-subset(Y, Treatment=="A"|Treatment=="B"|Treatment=="C"|Treatment=="D")
names(bench)
attach(bench)


#Flower Graphs
boxplot(Flower~Pilot_ID, las=2, names = c("S2_2010", "S2_2014","S7_2011","S7_2015","S15_2010","S15_2016",
            "S18_2010","S18_2016","S32_2010","S32_2016","S36_2011","S36_2016"))
#Survival Graphs
boxplot(Death~Pilot_ID, las=2, names = c("S2_2010", "S2_2014","S7_2011","S7_2015","S15_2010","S15_2016",
                                          "S18_2010","S18_2016","S32_2010","S32_2016","S36_2011","S36_2016"))
#Biomass Graph
boxplot(Biomass~Pilot_ID, las=2, names = c("S2_2010", "S2_2014","S7_2011","S7_2015","S15_2010","S15_2016",
                                          "S18_2010","S18_2016","S32_2010","S32_2016","S36_2011","S36_2016"))
#Belowground Biomass Graph
boxplot(Roots~Pilot_ID, las=2, names = c("S2_2010", "S2_2014","S7_2011","S7_2015","S15_2010","S15_2016",
                                          "S18_2010","S18_2016","S32_2010","S32_2016","S36_2011","S36_2016"))











