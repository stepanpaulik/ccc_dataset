library(tidyverse)
source("scripts/US_web_scraping.R")
source("scripts/US_supporting_functions.R")
# source("scripts/WR_decisions_NSS.R")

#US
metadata = read_rds(file = "../data/US_metadata.rds") 
texts = read_rds(file = "../data/US_texts.rds")
judges = readr::read_rds("../data/US_judges.rds")

US_IDs_new = get_urls(as.character(max(US_metadata$date_decision)))
US_IDs = c(read_rds(file = "../data/US_IDs.rds"),US_IDs_new)
write_rds(US_IDs, file = "../data/US_IDs.rds")
  

metadata_new = get_metadata(US_IDs_new)
metadata = bind_rows(metadata, metadata_new)
metadata = metadata %>%
  get_compositions(metadata = ., texts = texts, judges = judges)
readr::write_rds(metadata, file = "../data/US_metadata.rds")

texts_new = get_texts(metadata = metadata_new)
texts = bind_rows(texts, texts_new)
readr::write_rds(texts_new, file = "../data/US_texts.rds")

dissents = get_dissents(metadata, texts, judges)
readr::write_rds(dissents, file = "../data/US_dissents.rds")


