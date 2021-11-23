library(raster)
library(FedData)


nlcd <- get_nlcd(
  template = door,
  label = "Door County", 
  year = 2019,
  dataset = "landcover"
)