library(leaflet)
library(tidyverse)
library(lubridate)
library(htmltools)
library(htmlwidgets)
source("./predatoryBirds/01_readData.R")
source("./predatoryBirds/02_rasterize.R")

#### Set parameters of visualization ####
resolution = 0.005
circleScale = 30
county = "Dane"
state = "Wisconsin"
lng1 = -89.543437; lat1 = 43.192304; lng2 = -89.234618; lat2 = 43.015585
minZoom = 10; maxZoom = 15

#### Make grid####
rast <- makeRaster(state = state, 
                   county = county,
                   res = resolution)
values(rast) <- 1
polygons <- rasterToPolygons(rast)

#### Read in bird data, summarize observations by raster grid ####
birdNames <- c("perfal", "baleag", "grhowl", "shshaw", "rolhaw")
map(birdNames, function(name) assign(name, 
                                     readData(fileDir = paste0("ebd_US-WI-025_",name,"_relOct-2021"), 
                                              fileName = paste0("ebd_US-WI-025_",name,"_relOct-2021",".txt"))%>%
                                       filter(year(`OBSERVATION DATE`) >= 2010) %>%
                                       countObs(rast),
                                     envir = .GlobalEnv))

#### Make and render map ####
map <- leaflet(options = leafletOptions(minZoom = minZoom, maxZoom = maxZoom)) %>%
  fitBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>%
  addTiles() %>%
  addPolygons(data = polygons,
              fill = TRUE,
              fillColor = "transparent",
              stroke = FALSE,
              highlightOptions = highlightOptions(fillColor = "grey")) %>%
  addCircles(data = perfal,
             lng = ~lng, 
             lat = ~lat, 
             label = ~htmlEscape(paste(count)),
             radius = ~sqrt(count)*circleScale, 
             popup = ~htmlEscape(paste(count, "observations")),
             group = "Peregrine Falcons") %>%
  addCircles(data = baleag,
             lng = ~lng, 
             lat = ~lat, 
             label = ~htmlEscape(paste(count)),
             radius = ~sqrt(count)*circleScale, 
             popup =  ~htmlEscape(paste(count, "observations")),
             group = "Bald Eagles") %>%
  addCircles(data = grhowl,
             lng = ~lng, 
             lat = ~lat, 
             label = ~htmlEscape(paste(count)),
             radius = ~sqrt(count)*circleScale, 
             popup =  ~htmlEscape(paste(count, "observations")),
             group = "Great Horned Owls") %>%
  addCircles(data = shshaw,
             lng = ~lng, 
             lat = ~lat, 
             label = ~htmlEscape(paste(count)),
             radius = ~sqrt(count)*circleScale, 
             popup =  ~htmlEscape(paste(count, "observations")),
             group = "Sharp Shinned Hawk") %>%
  addCircles(data = rolhaw,
             lng = ~lng, 
             lat = ~lat, 
             label = ~htmlEscape(paste(count)),
             radius = ~sqrt(count)*circleScale, 
             popup =  ~htmlEscape(paste(count, "observations")),
             group = "Rough-Legged Hawk") %>%
  addLayersControl(baseGroups = c("Peregrine Falcons", 
                                     "Bald Eagles", 
                                     "Great Horned Owls", 
                                     "Sharp Shinned Hawk", 
                                     "Rough-Legged Hawk"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar(position = "bottomright")

saveWidget(map, "../website/content/project/birdsOfPrey/map.html")



  
  
