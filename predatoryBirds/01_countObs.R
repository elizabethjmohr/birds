library(tidyverse)
library(raster)
library(sp)

makeRaster <- function(state, county, res = 0.0005, counties = getData("GADM", country="USA", level=2)){
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

countObs <- function(obsDF, rast){
  rasterize(x = obsDF, y = rast, field = 1, fun = 'count') %>%
    rasterToPoints() %>%
    as_tibble() %>%
    rename(lng = x, lat = y, count = layer)
}


