#################
# Graph PDSI
#################
library(tidyverse)

#see drought-readme.txt for row codes 
#ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/
#https://www.ncdc.noaa.gov/monitoring-references/maps/images/us-climate-divisions-names.jpg

pdsi <- read.csv("Data/PDSI.csv", header=T)

#merge State and Division ID
pdsi <- pdsi %>% mutate(Division = paste(State, Div, sep = "_"))

#Filter out wanted Divisions
pdsi <- pdsi %>% filter (Division=="CA_1" | Division=="CA_2" | Division=="CA_5" | Division=="CA_6" 
                         | Division=="OR_3")

#Take Mean of JJA
pdsi <- pdsi %>% mutate(PDSI = rowMeans(select(pdsi, m_6,m_7,m_8)))

#Select June, July, August
pdsi <- pdsi %>% select(Division,Year,PDSI)

#Give Divisions order and names that make sense
pdsi <- pdsi %>% mutate(Reg=ifelse(Division =="CA_6", "A  South Coast", 
                                   ifelse((Division=="CA_5"), "B   San Joaquin", 
                                          ifelse((Division=="CA_2"), "C   Sacramento",
                                                 ifelse((Division=="CA_1"), "D   North Coast", "E   Southwest Oregon")))))



#Filter only to 1980 to 2020
pdsi <- pdsi %>%filter(Year>1979) %>% filter(Year<2017)

#Make Graph since 1980
pdsi_graph <-ggplot(pdsi , aes(x=Year, y=PDSI))+ 
  geom_line()+
  facet_wrap(.~Reg)+
  theme(axis.title = element_text(size = 18))+
  theme(axis.text = element_text(size = 14))+
  theme(strip.text = element_text(size = 15))
pdsi_graph 

ggsave("pdsi_1980.pdf", width = 12, height = 7, units = "in")




#Make Graph all years
pdsi_graph <-ggplot(pdsi , aes(x=Year, y=PDSI))+ 
  geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(.~Reg)
pdsi_graph 

ggsave("pdsi_1895.pdf", width = 12, height = 7, units = "in")


