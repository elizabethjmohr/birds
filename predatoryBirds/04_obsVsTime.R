library(tidyverse)
library(lubridate)
library(ggthemes)
library(auk)
library(cowplot)
library(plotly)

birdNames <- c("perfal", "baleag", "grhowl", "shshaw", "rolhaw")
observations <- map(birdNames, function(name) read_ebd(paste0("../../../data/ebird/ebd_US-WI-025_", 
                                                              name,
                                                              "_relOct-2021/ebd_US-WI-025_", 
                                                              name, 
                                                              "_relOct-2021.txt"))) %>%
  bind_rows()

load("./birdsOfPrey/birdsOfPrey_zf.Rds")

# Summarize observations by bird and year
observationSummary <- observations %>%
  mutate(Year = year(observation_date)) %>%
  group_by(common_name, Year) %>%
  summarise(count = n()) %>%
  mutate(common_name = factor(common_name, 
                              levels = c("Bald Eagle", "Great Horned Owl", "Sharp-shinned Hawk", "Rough-legged Hawk", "Peregrine Falcon")))

numberOfChecklists <- birdsOfPrey_zf %>%
  mutate(Year = year(observation_date)) %>%
  group_by(Year) %>%
  summarise(checklists = length(unique(sampling_event_identifier)))

checklistPercents <-  birdsOfPrey_zf %>%
  mutate(Year = year(observation_date)) %>%
  group_by(common_name, Year) %>%
  summarise(percent = 100*mean(species_observed))
  
# Plot total number of observations per year
p <- ggplot(observationSummary, aes(x = Year, y = count, color = common_name)) + 
  geom_line(size = 0.8) +
  geom_point(size = 2)+
  ylab("# of Observations")+
  xlab(NULL)+
  theme_minimal_grid() + 
  scale_x_continuous(limits = c(2010, 2020),
                     breaks = c(2010, 2015, 2020))+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 12))+
  scale_color_colorblind(labels = c("Bald Eagle", 
                                    "Great Horned Owl", 
                                    "Sharp-shinned Hawk", 
                                    "Rough-legged Hawk",
                                    "Peregrine Falcon")) + 
  guides(color = guide_legend(title = "Species"))

ggsave(filename = "observations.png", 
       path = "../website/content/project/birdsOfPrey/",
       plot = p,
       device = "png",
       width = 8, 
       height = 3.5,
       units = "in")
# Plot observations from complete checklists, 
# normalized by number of checklists
p2 <- ggplot(checklistPercents, aes(x = Year, y = percent, color = common_name)) + 
  geom_line(size = 0.8) +
  geom_point(size = 2)+
  ylab("Percent of checklists")+
  xlab(NULL)+
  theme_minimal_grid() + 
  scale_x_continuous(limits = c(2010, 2020),
                     breaks = c(2010, 2015, 2020))+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 12))+
  scale_color_colorblind(labels = c("Bald Eagle", 
                                    "Great Horned Owl", 
                                    "Sharp-shinned Hawk", 
                                    "Rough-legged Hawk",
                                    "Peregrine Falcon")) + 
  guides(color = guide_legend(title = "Species"))

# Plot number of checklists per year

# Plot histograms for each year with duration_minutes()

