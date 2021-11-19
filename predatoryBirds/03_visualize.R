library(leaflet)
library(tidyverse)
library(htmltools)
source("./predatoryBirds/01_readData.R")
source("./predatoryBirds/02_rasterize.R")

#### Set parameters of visualization ####
resolution = 0.005
circleScale = 30
lng1 = -89.543437; lat1 = 43.192304; lng2 = -89.234618; lat2 = 43.015585
minZoom = 10; maxZoom = 15

#### Read in data ####
dataDir = "../../../data/ebird"
map2(birdNames, function(name) assign(name, prepObsData(dataDir, fileDir, paste0(name, ".txt"))))

#### Make and render map ####
leaflet(options = leafletOptions(minZoom = minZoom, maxZoom = maxZoom)) %>%
  fitBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>%
  addTiles() %>%
  addCircles(data = falcons,
             lng = ~lng, 
             lat = ~lat, 
             radius = ~sqrt(count)*circleScale, 
             popup = ~count,
             group = "Peregrine Falcons") %>%
  addCircles(data = eagles,
             lng = ~lng, 
             lat = ~lat, 
             radius = ~sqrt(count)*circleScale, 
             popup = ~count,
             group = "Bald Eagles") %>%
  addCircles(data = owls,
             lng = ~lng, 
             lat = ~lat, 
             radius = ~sqrt(count)*circleScale, 
             popup = ~count,
             group = "Great Horned Owls") %>%
  addCircles(data = ssHawks,
             lng = ~lng, 
             lat = ~lat, 
             radius = ~sqrt(count)*circleScale, 
             popup = ~count,
             group = "Sharp Shinned Hawk") %>%
  addCircles(data = rlHawks,
             lng = ~lng, 
             lat = ~lat, 
             radius = ~sqrt(count)*circleScale, 
             popup = ~count,
             group = "Rough-Legged Hawk") %>%
  addLayersControl(overlayGroups = c("Peregrine Falcons", 
                                     "Bald Eagles", 
                                     "Great Horned Owls", 
                                     "Sharp Shinned Hawk", 
                                     "Rough-Legged Hawk"))

### Montgomery County Alabama test ###
resolution = 0.1
rast <- makeRaster(state = "Alabama", 
                   county = "Montgomery",
                   res = resolution)
values(rast) <- 1
polygons <- rasterToPolygons(rast)
extent <- getCountyExtent("Alabama", "Montgomery")

observations <- read_tsv(file = "../../../data/ebird/sample/ebd_US-AL-101_202103_202103_relMar-2021.txt") 

robins <- observations %>%
  filter(`COMMON NAME` == "American Robin") %>%
  with(cbind(LONGITUDE, LATITUDE)) %>%
  SpatialPoints() %>%
  countObs(rast)
  
bluejays <- observations %>%
    filter(`COMMON NAME` == "Blue Jay") %>%
    with(cbind(LONGITUDE, LATITUDE)) %>%
    SpatialPoints() %>%
    countObs(rast)

leaflet(options = leafletOptions(minZoom = 10, maxZoom = 25))%>%
  fitBounds(lng1 = extent$xmin, lat1 = extent$ymin, lng2 = extent$xmax, lat2 = extent$ymax) %>%
  addTiles() %>%
  addPolygons(data = polygons,
              fill = TRUE,
              fillColor = "transparent",
              stroke = FALSE,
              highlightOptions = highlightOptions(fillColor = "grey")) %>%
  addCircles(data = robins,
             lng = ~lng, 
             lat = ~lat, 
             radius = ~sqrt(count)*500, 
             label = ~htmlEscape(paste("Total Observations: ", count)), 
             group = "Robins", 
             color = "#DF536B") %>%
  addCircles(data = bluejays,
             lng = ~lng, 
             lat = ~lat, 
             radius = ~sqrt(count)*500, 
             label = ~htmlEscape(paste("Total Observations: ", count)), 
             group = "Bluejays", 
             color = "#61D04F") %>%
  addLayersControl(overlayGroups = c("Robins", 
                                     "Bluejays"))

  
  
