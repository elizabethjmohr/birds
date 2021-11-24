library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)
library(ggiraph)

ebd_zf <- read_csv("../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.csv")
ebd_zf_2010_2019 <- ebd_zf %>%
  filter(year(observation_date) < 2020, year(observation_date) > 2009)

populations <- readxl::read_xlsx(path = "./nst-est2019-01.xlsx", 
                                 range ="A10:M60",
                                 col_names = c("Area", "2010 Census", "Estimates Base", as.character(2010:2019))) %>%
  mutate(Area = substring(Area, 2)) %>%
  pivot_longer(cols = 4:13, 
               names_to = "year",
               values_to = "population") %>%
  rename(state = "Area") %>%
  mutate(year = as.numeric(year))

checkListCounts <- ebd_zf_2010_2019  %>%
  mutate(year = year(observation_date)) %>%
  group_by(state, year) %>%
  summarize(checklistCount = n()) %>%
  left_join(populations, by = c("state", "year")) %>%
  mutate(checklistsPerThousand = 1000*checklistCount/population)

plot <- ggplot(checkListCounts, 
       aes(x = year, y = checklistsPerThousand, group = state)) + 
  geom_point_interactive(aes(data_id = state,
                             tooltip = state), 
                         color = "#065535")+
  geom_line_interactive(aes(data_id = state,
                            tooltip = state),
                        color = "#065535") +
  scale_x_continuous(breaks = 2010:2019)+
  ylab("Checklists per 1,000 residents") + 
  theme_minimal_grid()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))

checklistCountsInt <- girafe(ggobj = plot,
       options = list(
         opts_hover_inv(css = "opacity:0.1;"),
         opts_hover(css = "stroke-width:2;")
       ),
       width_svg = 8,
       height_svg = 5,
       bg = "transparent")

saveWidget(checklistCountsInt, 
           "../website/content/project/snowyOwls/checklistCountsInt.html",
           background = "transparent")

lineChart <- plot_ly(checkListCounts,
                     x = ~year,
                     y = ~checklistsPerThousand, 
                     type = 'scatter',
                     mode = 'lines') %>%
  add_trace(type = "scatter", 
            mode = 'markers', 
            marker = list(color = 'rgba(49,130,189, 1)'), 
            hoverinfo = 'text',
            text = ~paste(state, ":", round(checklistsPerThousand, 0))) %>%
  layout(yaxis = list(title = "Checklists per 1,000 residents"),
         xaxis = list(title = ""),
         showlegend = FALSE)

htmlwidgets::saveWidget(lineChart, "../website/content/project/snowyOwls/lineChart.html")

# Alternate version

checkListCountsWide <- checkListCounts %>%
  select(state, year, checklistsPerThousand) %>%
  ungroup() %>%
  pivot_wider(id_cols = 1:2, 
              names_from = year, 
              values_from = checklistsPerThousand) %>%
  arrange(`2019`)

heatmap <- plot_ly(
  y = checkListCountsWide$state, 
  x = as.character(2010:2019),
  z = (as.matrix(checkListCountsWide[,2:11])), 
  colorscale = "YlOrRd",
  type = "heatmap",
  hovertemplate = paste("%{y},", "%{x}", "<extra></extra>"),
  reversescale=TRUE,
  xgap = 3, 
  ygap = 3) %>%
  add_annotations(x = checkListCounts$year,
                  y = checkListCounts$state,
                  text = as.character(round(checkListCounts$checklistsPerThousand, 1)),
                  showarrow = FALSE,
                  font = list(color = "black")) %>%
  layout(showlegend = FALSE)

htmlwidgets::saveWidget(heatmap, "../website/content/project/snowyOwls/heatmap.html")
