library(tidyverse)
library(auk)

ebd_file <- "../../../data/ebird/ebd_relOct-2021.txt"
event_file <- "../../../data/ebird/event_relOct-2021.txt"
ebd_output_file <- "../../../data/ebird/ebd_snoowl_complete_relOct-2021.txt"
event_output_file <- "../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.txt"
snowyOwl_complete_data <- input_file %>%
  auk_ebd(file_sampling = f_smp) %>%
  auk_species(species = "Snowy Owl") %>%
  auk_state(state = ebird_states %>% 
              filter(country == "United States", state %in% state.name[state.region == "Northeast"|state.region == "North Central"]) %>% 
              pull(state_code)) %>%
  auk_complete()%>%
  auk_filter(file = ebd_output_file, 
             file_sampling = event_output_file) %>%
  read_ebd() 

snowyOwl_zf <- auk_zerofill(snowyOwl_complete_data)%>%
  collapse_zerofill(zf) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
save("snowyOwl_zf", file = "./snowyOwls/snowyOwl_zf.Rds")  

owlObservations <- read_ebd("../../../data/ebird/ebd_snoowl1_relOct-2021/ebd_snoowl1_relOct-2021.txt") %>%
  filter(STATE %in% states) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)





