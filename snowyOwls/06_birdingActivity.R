library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)

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

ggplot(checkListCounts, aes(x = year, y = checklistsPerThousand)) + 
  geom_line(aes(color = state))

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
