library(tidyverse)

Y <- read.csv("/Users/daniel_anstett/Dropbox/a_Resurrection/Data/floweringtime_data_Entry.csv", header=TRUE)
Y1<-paste0(Y[,4], Y[,5], Y[,6])
endp<-length(Y1)

outmat<-c()

for (i in 1:endp){
  tmp <- as.Date(Y1[i], format = "%d%b%y")
  
  outmat[i]<-format(tmp, "%j")
}

y2<-Y %>% 
  select(Family,Block,Treatment)
y2[,4]<-outmat
write.csv(y2,"Data/flower_date.csv")

