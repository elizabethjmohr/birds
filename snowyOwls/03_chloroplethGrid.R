library(viridis)
library(RColorBrewer)
source("./snowyOwls/02_summarizeByState.R")

quartz(width = 7, height = 5)
ggplot(countsByYear %>% mutate(rate_cut = cut_number(countsPerOneHundredSqKm, n = 5)),
       aes(fill = rate_cut))+
  geom_sf()+
  coord_sf(crs = 5070)+ # USA Contiguous Albers Equal Area Conic
  scale_x_continuous(breaks = c(-95, -85, -75))+
  scale_fill_viridis(discrete = TRUE,
                    labels = c("<0.01", "0.1-0.02", "0.02-0.05", "0.05-1.0", "1.0-2.0", "2.0+"))+
  facet_wrap(~season)+
  guides(fill = guide_legend(reverse = TRUE, title = "Observations per \n100 square km"))

quartz(width = 7, height = 5)
ggplot(checklistPercentsByYear %>% mutate(rate_cut = cut_number(percentOfChecklists, n = 6)),
       aes(fill = rate_cut))+
  geom_sf()+
  coord_sf(crs = 5070)+ # USA Contiguous Albers Equal Area Conic
  scale_x_continuous(breaks = c(-95, -85, -75))+
  scale_fill_manual(values = viridis(6))+
  facet_wrap(~season)+
  guides(fill = guide_legend(reverse = TRUE, title = "% of checklists"))

