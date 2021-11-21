library(tidyverse)
library(sp)

readData <- function(dataDir = "../../../data/ebird", fileDir, fileName){
  observations <- read_tsv(file = paste0("./", dataDir, "/", fileDir, "/", fileName)) 
  points <- SpatialPointsDataFrame(coords = cbind(observations$LONGITUDE, observations$LATITUDE),
                                   data = tibble(species = observations$`COMMON NAME`),
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
  return(points)
}

commonNames <- c("Bald Eagle", "Great Horned Owl", "Sharp-shinned Hawk", "Rough-legged Hawk", "Peregrine Falcon")

ebd_file <- "../../../data/ebird/ebd_relOct-2021.txt"
event_file <- "../../../data/ebird/event_relOct-2021.txt"
ebd_output_file <- "../../../data/ebird/ebd_snoowl_complete_relOct-2021.txt"
event_output_file <- "../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.txt"

birdsOfPrey_complete_data <- input_file %>%
  auk_ebd(file_sampling = event_file) %>%
  auk_species(species = commonNames) %>%
  auk_state(state = ebird_states %>% 
              filter(country == "United States", state == "Wisconsin") %>% 
              pull(state_code)) %>%
  auk_county("US-WI-025") %>%
  auk_complete()%>%
  auk_filter(file = ebd_output_file, 
             file_sampling = event_output_file) %>%
  read_ebd() 

birdsOfPrey_zf <- auk_zerofill(birdsOfPrey_complete_data)%>%
  collapse_zerofill(zf) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
save("birdsOfPrey_zf", file = "./birdsOfPrey/birdsOfPrey_zf.Rds")  

