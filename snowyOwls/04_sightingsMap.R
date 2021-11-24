library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
library(cowplot)
library(ggiraph)
library(htmlwidgets)

ebd_zf <- read_csv("../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.csv")
states <- states(cb = TRUE) %>%
  filter(NAME  %in% state.name[state.region == "Northeast"|state.region == "North Central"])%>%
  st_transform(4326)

snowyOwl_sf <- ebd_zf %>%
  filter(species_observed) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(Year = year(observation_date))

snowyOwl_zf_sf <-  ebd_zf %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

colors <- c("eBird checklists" = "#555555", 
            "Snowy Owl sightings" = "#2b8cbe")
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
       width = 8, 
       height = 4,
       units = "in")

## Wisconsin sightings##
getSeasonRecords <- function(observations, startYear, endYear){
  filteredObs <- observations %>%
    filter((year(observation_date) == startYear & month(observation_date)>=10) |(year(observation_date) == endYear & month(observation_date)<=4)) %>%
    mutate(season =  paste0(startYear,"-",endYear))
  return(filteredObs)
}
snowyOwl_sf_WI <- snowyOwl_sf %>% 
  filter(state == "Wisconsin")

facets <- tibble(
  startYears = seq(2009, 2020, by = 1),
  endYears = seq(2010, 2021, by = 1), 
  text = paste0(startYears, "-", endYears)
)

observationsByYear <- map2_dfr(facets$startYears, 
                               facets$endYears, 
                               getSeasonRecords,
                               observations = snowyOwl_sf_WI)
sightingsMapInt <- ggplot(states %>% filter(NAME == "Wisconsin")) +
  geom_sf()+
  geom_sf_interactive(data = observationsByYear, 
                      aes(data_id = season),
                      color = "#2b8cbe",
                      alpha = 0.7,
                      size = 1.0) +
  coord_sf(crs = 5070) +
  guides(size = "none", alpha = "none")+
  theme_map()+
  theme(legend.position = "top", 
        legend.title = element_blank(),
        plot.background = element_rect("transparent", colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))

sightingsPerSeason <- observationsByYear %>%
  group_by(season) %>%
  summarize(n = n())

sightingsVsTime <- ggplot(sightingsPerSeason) + 
  geom_col_interactive(aes(data_id = season, x = season, y = n)) + 
  theme_minimal_hgrid() +
  # scale_x_continuous()+
  ylab("Number of sightings") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
        plot.background = element_rect("transparent", colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))

interactiveSightings <- girafe(ggobj = plot_grid(sightingsMapInt, 
                                                 sightingsVsTime, 
                                                 nrow = 1, 
                                                 rel_widths = c(0.7,1)),
                               options = list(
                                 opts_hover_inv(css = "opacity:0.3;")
                               ),
                               width_svg = 8,
                               height_svg = 3.5,
                               bg = "transparent")
saveWidget(interactiveSightings, 
           "../website/content/project/snowyOwls/interactiveSightingsWI.html",
           background = "transparent")
