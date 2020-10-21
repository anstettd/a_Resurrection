########
#Generation of Germination Tables
########

library(tidyverse)
setwd("~/Documents/Daniel")

m_ger<-read.csv('germination.csv', header=T) #Imports germination data
m_ger <- m_ger %>% filter(Site != "S11") #remove site 11
m_ger <- m_ger %>%  #change Site codes into accending ID
  mutate(ID = ifelse(Site == "S02", 1,
                     ifelse(Site == "S07", 2, 
                            ifelse(Site == "S10", 3, 
                                   ifelse(Site == "S08", 4, 
                                          ifelse(Site == "S32", 5, 
                                                 ifelse(Site == "S29", 6, 
                                                        ifelse(Site == "S18", 7, 
                                                               ifelse(Site == "S17", 8, 
                                                                      ifelse(Site == "S16", 9,
                                                                             ifelse(Site == "S36", 10, 11)))))))))))
m_ger <- m_ger %>% mutate(ID.Year=paste(ID,Year,sep="_")) # make ID.Year


ger <- unique(m_ger$ID.Year)# ; ger <- as.data.frame(ger) #grap unique ID Year

ger.dat<-data.frame()

for (i in 1:length(ger)){ 
  input_1 <- tally(m_ger %>% filter(m_ger$ID.Year==ger[i],Germinated==0))
  input_2 <- tally(m_ger %>% filter(m_ger$ID.Year==ger[i],Germinated==1))
  input_3 <- tally(m_ger %>% filter(m_ger$ID.Year==ger[i]))
  ger.dat[i,1]<-ger[i]
  ger.dat[i,2]<-input_1
  ger.dat[i,3]<-input_2
  ger.dat[i,4]<-input_3
  }
colnames(ger.dat) <- c("Site.Year","No_Germination","Germination","Total")
ger.dat<- ger.dat %>% separate(Site.Year,c("Site","Year")) #Seperate Site and Year
ger.dat<- ger.dat %>% mutate (Percent_Germination=round(100*Germination/Total,1)) #calc percent
write.csv(ger.dat,"Germination_output.csv")

