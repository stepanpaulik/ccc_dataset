library(tidyverse)
metadata = read_rds(file = "../data/US_metadata.rds")

metadata = metadata %>%
  mutate(merits_admissibility = case_when(
    str_detect(as.character(type_verdict), "vyhověno|zamítnuto") ~ "merits",
    str_detect(as.character(type_verdict), "procesní") & !str_detect(as.character(type_verdict), "vyhověno|zamítnuto|odmítnutno") ~ "procedural",
    .default = "admissibility")) %>%
  relocate(merits_admissibility, .after = type_verdict)

metadata %>%
  mutate(across(where(is.list), ~as.character(.x))) %>%
  write_csv(., file = "../data/US_metadata.csv")
data = read_csv(file = "../data/US_metadata.csv")
unique(data$dissenting_opinion)
