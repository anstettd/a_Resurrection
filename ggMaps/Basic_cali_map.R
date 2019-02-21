library(maptools)
library(maps)
library(tidyverse)
library(plyr)





#Map generation
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders

mp <- ggplot() +   mapWorld +
  coord_equal() +  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_rect(colour = "black")) #get rid of grey grid

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color="blue", size=3) 
mp