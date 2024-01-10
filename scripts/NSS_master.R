library(tidyverse)
source("scripts/WR_decisions_NSS.R")

# NSS
NSS_IDs = read_rds("../data/NSS_IDs.rds")
NSS_metadata = read_rds("../data/NSS_metadata.rds")

latest_date = max(NSS_metadata$date_decision, na.rm = TRUE) %>% 
  format("%Y-%m-%d")

NSS_IDs_new = get_NSS_ID(latest_date = latest_date)

c(NSS_IDs, NSS_IDs_new) %>%
  write_rds(., file = "../data/NSS_applied_laws.rds")

get_NSS_metadata(NSS_IDs = NSS_IDs_new) %>%
  bind_rows(NSS_metadata, .) %>%
  write_rds(., file = "../data/NSS_metadata.rds")

get_NSS_texts(NSS_IDs = NSS_IDs_new) %>%
  bind_rows(read_rds("../data/NSS_texts.rds"), .) %>%
  write_rds(., file = "../data/NSS_texts.rds")

get_applied_judgments(NSS_IDs = NSS_IDs_new) %>%
  bind_rows(read_rds("../data/NSS_applied_judgments.rds"),.) %>%
  write_rds(., file = "../data/NSS_applied_judgments.rds")

get_applied_laws(NSS_IDs = NSS_IDs_new) %>%
  bind_rows(read_rds("../data/NSS_applied_laws.rds"),.) %>%
  write_rds(., file = "../data/NSS_IDs.rds")


