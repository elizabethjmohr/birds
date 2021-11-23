library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(cowplot)

ebd_zf <- read_csv("../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.csv")
states <- states(cb = TRUE) %>%
  filter(NAME  %in% state.name[state.region == "Northeast"|state.region == "North Central"])%>%
  st_transform(4326)

snowyOwl_sf <- ebd_zf %>%
  filter(species_observed) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

snowyOwl_zf_sf <-  ebd_zf %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

colors <- c("eBird checklists" = "#555555", 
            "Snowy Owl sightings" = "#4daf4a")
alphas <- c("eBird checklists" = 0.025, 
            "Snowy Owl sightings" = 1.0)
sizes <- c("eBird checklists" = 0.1, 
           "Snowy Owl sightings" = 0.3)

sightingsMap <- ggplot(states) +
  geom_sf()+
  geom_sf(data = snowyOwl_zf_sf %>% filter(year(observation_date)>= 2009,
                                           (month(observation_date) <=4 | month(observation_date) >=10)),
          aes(color = "eBird checklists", 
              alpha = "eBird checklists", 
              size = "eBird checklists"))+
  geom_sf(data = snowyOwl_sf %>% filter(year(observation_date)>= 2009,
                                        (month(observation_date) <=4 | month(observation_date) >=10)), 
          aes(color = "Snowy Owl sightings", 
              alpha = "Snowy Owl sightings",
              size = "Snowy Owl sightings")) +
  coord_sf(crs = 5070) +
  scale_color_manual(values = colors)+
  scale_alpha_manual(values = alphas)+
  scale_size_manual(values = sizes)+
  guides(size = "none", alpha = "none")+
  theme_map()+
  theme(legend.position = "top", legend.title = element_blank())

ggsave(filename = "sightings.png", 
       path = "../website/content/project/snowyOwls/",
       plot = sightingsMap,
       device = "png",
       width = 12, 
       height = 7,
       units = "in")

sightingsVsTime 


