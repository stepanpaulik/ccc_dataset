library(tidyverse)

# Load data
source("supporting_functions.R")
NSS_metadata = read_rds("../data/NSS_metadata.rds")
NSS_texts = read_rds("../data/NSS_texts.rds")
NSS_judges = read_rds("../data/NSS_judges.rds")

NSS_judges <- foreach(judge_rapporteur = NSS_metadata$judge_rapporteur, .combine = "c") %do% {
  return(paste0(word(judge_rapporteur, 2), word(judge_rapporteur, 1)) %>% str_replace(",", " ") %>% str_to_title())
} %>% unique()

NSS_metadata = NSS_metadata %>%
  mutate(judge_rapporteur_name = paste0(word(judge_rapporteur, 2), word(judge_rapporteur, 1)) %>% str_replace(",", " ") %>% str_to_title())

# NSS_judges = NSS_judges %>%
#   mutate(judge_id = paste0("J:", row_number(judge_name)))

# Random prep for courts_and_judges
# NSS_df = NSS_metadata %>%
#   filter(type_decision == "Rozsudek") %>%
#   mutate(outcome = case_when(
#     grepl("zrušeno", type_verdict) ~ "granted",
#     .default = "rejected"
#   ),
#   outcome = factor(outcome),
#   phd = case_when(
#     grepl("Ph.D.", lawyer) ~ TRUE,
#     .default = FALSE)) %>%
#   select(c(doc_id, lawyer, outcome, phd)) %>%
#   group_by(phd) %>%
#   mutate(ability = rnorm(n(), mean = phd + 4, sd = 1)) %>%
#   ungroup() %>%
#   drop_na()