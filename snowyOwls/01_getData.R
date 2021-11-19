library(tidyverse)
library(raster)
library(FedData)

counties <- getData("GADM", country="USA", level=2) 
door <- counties %>%
  subset(counties$NAME_1 == "Wisconsin" & counties$NAME_2 == "Door") 

nlcd <- get_nlcd(
  template = door,
  label = "Door County", 
  year = 2019,
  dataset = "landcover"
)

