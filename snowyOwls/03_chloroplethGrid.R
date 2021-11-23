library(RColorBrewer)
source("./snowyOwls/02_summarizeByState.R")

densityChloropleth <- ggplot(countsByYear,aes(fill = bin))+
  geom_sf()+
  coord_sf(crs = 5070)+ # USA Contiguous Albers Equal Area Conic
  scale_x_continuous(breaks = c(-95, -85, -75))+
  scale_fill_brewer(palette = "Blues")+
  facet_wrap(~season, nrow = 4)+
  guides(fill = guide_legend(reverse = TRUE, title = "Observations per \n100 square km"))

ggsave(filename = "chloropleth1.png", 
       path = "../website/content/project/snowyOwls/",
       plot = densityChloropleth,
       device = "png",
       width = 10, 
       height = 7.5,
       units = "in",
       bg='transparent')

checkListPercentChloropleth <- ggplot(checklistPercentsByYear, aes(fill = bin))+
  geom_sf()+
  coord_sf(crs = 5070)+ # USA Contiguous Albers Equal Area Conic
  scale_fill_brewer(palette = "Blues")+
  facet_wrap(~season, nrow = 4)+
  guides(fill = guide_legend(reverse = TRUE, title = "% of checklists"))

ggsave(filename = "chloropleth2.png", 
       path = "../website/content/project/snowyOwls/",
       plot = checkListPercentChloropleth ,
       device = "png",
       width = 10, 
       height = 7.5,
       units = "in",
       bg='transparent')
