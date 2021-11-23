library(tidyverse)
library(auk)

# Specify output file directory
f_ebd <- "../../../data/ebird/ebd_snoowl_complete_relOct-2021.txt"
f_sampling <- "../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.txt"

# Make auk_ebd object and specify filters
ebd_filters <- auk_ebd("../../../data/ebird/ebd_snoowl1_relOct-2021/ebd_snoowl1_relOct-2021.txt", 
                       file_sampling = "../../../data/ebird/ebd_sampling_relOct-2021/ebd_sampling_relOct-2021.txt") %>%
  auk_species("Snowy Owl") %>%
  auk_species(species = "Snowy Owl") %>%
  auk_state(state = ebird_states %>% 
              filter(country == "United States", state %in% state.name[state.region == "Northeast"|state.region == "North Central"]) %>% 
              pull(state_code)) %>%
  auk_complete()

# Filter data
auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)

# Produce zero-filled data
ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse = TRUE)
write_csv(ebd_zf, file = "../../../data/ebird/ebd_snoowl_zerofill_relOct-2021.csv")






