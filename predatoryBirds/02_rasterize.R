library(tidyverse)
library(raster)

makeRaster <- function(state, county, res = 0.005, counties = getData("GADM", country="USA", level=2)){
  county <- counties %>%
    subset(counties$NAME_1 == state & counties$NAME_2 == county)
  
  raster(ext = extent(county), resolution = res)
}

getCountyExtent <- function(state, county, counties = getData("GADM", country="USA", level=2)){
  counties %>%
    subset(counties$NAME_1 == state & counties$NAME_2 == county) %>%
    extent() %>%
    attributes()
}

countObs <- function(obs, rast){
  rasterize(x = obs, y = rast, field = 1, fun = 'count') %>%
    rasterToPoints() %>%
    as_tibble() %>%
    rename(lng = x, lat = y, count = layer)
}


