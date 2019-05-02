#################
# M. carldinalis Rapid Evolution
#################
library(tidyverse)
library(lsmeans)
library(car)
library(maptools)
library(visreg)
library(ggeffects)
library(nlme)
library(ggplot2)
### Data prep
Y <- read.csv("Data/drought1.csv", header=T)

