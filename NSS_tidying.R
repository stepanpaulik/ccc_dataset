xfun::pkg_attach2("tidyverse", "RMySQL", "lubridate", "foreach", "iterators", "tidymodels")
library(tidyverse)

# Load data
source("supporting_functions.R")
load("../data/NSS_metadata.RData")
load("data/NSS_texts.RData")

NSS_judges <- foreach(judge_rapporteur = NSS_metadata$judge_rapporteur, .combine = "c") %do% {
  return(paste0(word(judge_rapporteur, 2), word(judge_rapporteur, 1)) %>% str_replace(",", " ") %>% str_to_title())
} %>% unique()

NSS_applied_judgments <- NSS_metadata %>% select(doc_id, applied_judgments)


str <- NSS_applied_judgments$applied_judgments[1] %>% str_replace(string = ., pattern = fixed('list(list'), replacement = "list") %>% str_replace(string = ., pattern = fixed(')))'), replacement = "))") %>% as.data.frame()

# Random prep for courts_and_judges
NSS_df = NSS_metadata %>%
  filter(type_decision == "Rozsudek") %>%
  mutate(outcome = case_when(
    grepl("zrušeno", type_verdict) ~ "granted",
    .default = "rejected"
  ),
  outcome = factor(outcome),
  phd = case_when(
    grepl("Ph.D.", lawyer) ~ TRUE,
    .default = FALSE)) %>%
  select(c(doc_id, lawyer, outcome, phd)) %>%
  group_by(phd) %>%
  mutate(ability = rnorm(n(), mean = phd + 4, sd = 1)) %>%
  ungroup() %>%
  drop_na()

saveRDS(NSS_df, file = "../data/NSS_df.rds")

NSS_df = NSS_df %>%
  select(-phd)
  

NSS_df %>% 
  ggplot() +
  geom_density(aes(x = ability, group = phd, color = phd))

logit <- glm(outcome ~ phd, data = NSS_df, family = "binomial")


summary(logit)
