#####Data_organization for ressurection experiment#######
library(tidyverse)
d.prep <- read.csv("Data/drought.prep.csv", header=T)
t.roseta <- read.csv("Data/timeseries.roseta.csv", header=T)
d.join<-inner_join(d.prep, t.roseta)

