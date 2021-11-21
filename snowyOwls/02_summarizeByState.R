library(tidyverse)
library(lubridate)
library(sf)
library(tigris)

load("./snowyOwls/snowyOwl_zf.Rds")

getPointDensity <- function(observations, polygons, startYear, endYear){
    filteredObs <- observations %>%
    filter((year(observation_date) == startYear & month(observation_date)>=10) |(year(observation_date) == endYear & month(observation_date)<=4) )
  polygons$counts <- lengths(st_intersects(polygons, filteredObs)) 
  polygons$season <- paste0(startYear,"-",endYear)
  return(polygons)
}

getChecklistPercent <- function(zf, polygons, startYear, endYear){
  filteredObs <- zf %>% 
    select(common_name, species_observed, observation_date) %>%
    filter((year(observation_date) == startYear & month(observation_date)>=10) |(year(observation_date) == endYear & month(observation_date)<=4) )
  intersections <- st_intersects(polygons, filteredObs)
  polygons$percentOfChecklists <- map(intersections, function(int) mean(filteredObs$species_observed[int]))
  polygons$season <- paste0(startYear,"-",endYear)
  return(polygons)
}

facets <- tibble(
  startYears = seq(2012, 2020, by = 1),
  endYears = seq(2013, 2021, by = 1), 
  text = paste0(startYears, "-", endYears)
)

states <- states(cb = TRUE) %>%
  filter(NAME  %in% state.name[state.region == "Northeast"|state.region == "North Central"])%>%
  st_transform(4326)

countsByYear <- map2_dfr(facets$startYears, 
                         facets$endYears, 
                         getPointDensity,
                         observations = owlObservations,
                         polygons = states) %>%
  mutate(countsPerOneHundredSqKm = ((1e8)*counts/ALAND))

checklistPercentsByYear <- map2_dfr(facets$startYears, 
                                    facets$endYears, 
                                    getChecklistPercent,
                                    observations = snowyOwl_zf,
                                    polygons = states)