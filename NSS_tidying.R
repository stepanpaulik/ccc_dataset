xfun::pkg_attach2("tidyverse", "RMySQL", "lubridate", "foreach", "iterators")

# Load data
source("supporting_functions.R")
load("data/NSS_metadata.RData")
load("data/NSS_texts.RData")

NSS_judges <- foreach(judge_rapporteur = NSS_metadata$judge_rapporteur, .combine = "c") %do% {
  return(paste0(word(judge_rapporteur, 2), word(judge_rapporteur, 1)) %>% str_replace(",", " ") %>% str_to_title())
} %>% unique()

NSS_applied_judgments <- NSS_metadata %>% select(doc_id, applied_judgments)


str <- NSS_applied_judgments$applied_judgments[1] %>% str_replace(string = ., pattern = fixed('list(list'), replacement = "list") %>% str_replace(string = ., pattern = fixed(')))'), replacement = "))") %>% as.data.frame()

