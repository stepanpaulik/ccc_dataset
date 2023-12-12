source("scripts/WR_decisions_US.R")
# source("scripts/WR_decisions_NSS.R")

#US
US_IDs_new = read_rds(file = "../data/US_IDs.rds")
metadata = read_rds(file = "../data/US_metadata.rds")
texts = read_rds(file = "../data/US_texts.rds")

US_IDs_new = get_urls(decision_date = as.character(max(US_metadata$date_decision)))
c(read_rds(file = "../data/US_IDs.rds"),US_IDs_new) %>%
  write_rds(., file = "../data/US_IDs.rds")

metadata_new2 = get_metadata(US_IDs_new[1:50])
bind_rows(metadata, metadata_new) %>%
  write_rds(., file = "../data/US_metadata.rds")

texts_new = get_texts(metadata = metadata_new)
bind_rows(texts, texts_new) %>%
  write_rds(., file = "../data/US_texts.rds")


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

metadata %>%




