library(tidyverse)
source("scripts/ccc_web_scraping.R")
source("scripts/ccc_supporting_functions.R")

# LOAD CURRENT DATA -------------------------------------------------------
metadata = read_rds(file = "../data/ccc_dataset/rds/ccc_metadata.rds")
texts = read_rds(file = "../data/ccc_dataset/rds/ccc_texts.rds")

# UPDATE ------------------------------------------------------------------
ccc_IDs_new = get_urls(as.character(max(ccc_metadata$date_decision)))
ccc_IDs = c(read_rds(file = "../data/ccc_dataset/rds/ccc_IDs.rds"),ccc_IDs_new)
write_rds(ccc_IDs, file = "../data/ccc_dataset/rds/ccc_IDs.rds")

metadata_new = get_metadata(ccc_IDs_new)
metadata = bind_rows(metadata, metadata_new) %>%
  mutate(doc_id = make.unique(doc_id))
metadata = get_compositions(metadata = metadata, texts = texts, judges = judges)


texts_new = get_texts(metadata = metadata %>%
                        filter(!doc_id %in% texts$doc_id))
texts = bind_rows(texts, texts_new)

# WRITE UPDATED DATA ------------------------------------------------------
readr::write_rds(metadata, file = "../data/ccc_dataset/rds/ccc_metadata.rds")
readr::write_rds(texts, file = "../data/ccc_dataset/rds/ccc_texts.rds")
