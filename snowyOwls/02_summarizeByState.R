library(tidyverse)
library(lubridate)
library(sf)
library(tigris)

ebd_zf <- read_csv("../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.csv")
states <- states(cb = TRUE) %>%
  filter(NAME  %in% state.name[state.region == "Northeast"|state.region == "North Central"])%>%
  st_transform(4326)

snowyOwl_sf <- ebd_zf %>%
  filter(species_observed) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

snowyOwl_zf_sf <-  ebd_zf %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

getPointDensity <- function(observations, polygons, startYear, endYear){
  filteredObs <- observations %>%
    filter((year(observation_date) == startYear & month(observation_date)>=10) |(year(observation_date) == endYear & month(observation_date)<=4) )
  polygons$counts <- lengths(st_intersects(polygons, filteredObs)) 
  polygons$season <- paste0(startYear,"-",endYear)
  return(polygons)
}

getChecklistPercent <- function(zf, polygons, startYear, endYear){
  filteredObs <- zf %>% 
    filter((year(observation_date) == startYear & month(observation_date)>=10) |(year(observation_date) == endYear & month(observation_date)<=4) )
  intersections <- st_intersects(polygons, filteredObs)
  polygons$percent <- 100*map_dbl(intersections, function(int) mean(filteredObs$species_observed[int]))
  polygons$season <- paste0(startYear,"-",endYear)
  return(polygons)
}

facets <- tibble(
  startYears = seq(2009, 2020, by = 1),
  endYears = seq(2010, 2021, by = 1), 
  text = paste0(startYears, "-", endYears)
)

countsByYear <- map2_dfr(facets$startYears, 
                         facets$endYears, 
                         getPointDensity,
                         observations = snowyOwl_sf,
                         polygons = states) %>%
  mutate(countsPerOneHundredSqKm = ((1e8)*counts/ALAND)) 
cutoffs <- quantile(countsByYear$countsPerOneHundredSqKm, c(0.2, 0.4, 0.6, 0.8))

countsByYear <- countsByYear %>%
  mutate(bin = case_when(
    countsPerOneHundredSqKm < cutoffs[1] ~ "<0.002",
    cutoffs[1] <= countsPerOneHundredSqKm & countsPerOneHundredSqKm < cutoffs[2] ~ "0.002-0.03",
    cutoffs[2] <= countsPerOneHundredSqKm & countsPerOneHundredSqKm< cutoffs[3] ~ "0.03-0.1",
    cutoffs[3] <= countsPerOneHundredSqKm & countsPerOneHundredSqKm < cutoffs[4] ~ "0.1-0.5",
    countsPerOneHundredSqKm >= cutoffs[4] ~ ">0.5")
  ) %>%
  mutate(bin = factor(bin, levels = c("<0.002","0.002-0.03","0.03-0.1","0.1-0.5", ">0.5")))

checklistPercentsByYear <- map2_dfr(facets$startYears, 
                                    facets$endYears, 
                                    getChecklistPercent,
                                    zf = snowyOwl_zf_sf,
                                    polygons = states)
cutoffs <- quantile(checklistPercentsByYear$percent, c(0.2, 0.4, 0.6, 0.8))
checklistPercentsByYear <- checklistPercentsByYear %>%
  mutate(bin = case_when(
    percent < cutoffs[1] ~ "<0.02%",
    percent >= cutoffs[1] & percent <cutoffs[2] ~ "0.02%-0.1%",
    percent >= cutoffs[2] & percent< cutoffs[3] ~ "0.1%-0.5%",
    percent >= cutoffs[3] & percent <cutoffs[4] ~ "0.5%-1.1%",
    percent >cutoffs[4]  ~ ">1.1%",
  )) %>%
  mutate(bin = factor(bin, levels = c("<0.02%", "0.02%-0.1%", "0.1%-0.5%", "0.5%-1.1%",">1.1%")))
