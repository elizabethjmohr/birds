library(tidyverse)
library(sp)

prepObsData <- function(dataDir, fileDir, fileName){
  read_tsv(file = paste0("./", dataDir, "/", fileDir, "/", fileName)) %>%
    with(cbind(LONGITUDE, LATITUDE)) %>%
    SpatialPoints() %>%
    countObs(rast)
}