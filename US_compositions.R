library(tidyverse)
library(foreach)
library(parallel)

# Parallelise
library(doMC)
registerDoMC(cores = parallel::detectCores() - 2)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

# Load data
US_metadata = readr::read_rds("../data/US_metadata.rds")
US_texts = readr::read_rds("../data/US_texts.rds")
US_judges = readr::read_rds("../data/US_judges.rds")
source("supporting_functions.R")

# Save data
write_rds(US_compositions, file = "../data/US_compositions.rds")

# The mean function
get_compositions = function(texts, judges_grepl, judges_names, judges_id) {
   data_compositions = foreach(i = seq_along(texts$doc_id), .combine = "rbind") %:% 
    foreach(j = seq_along(judges_grepl), .combine = "rbind", .packages = c("tidyverse")) %dopar% {
      if (grepl(judges_grepl[j], texts[i,"text"], ignore.case = TRUE)) {
        output = tibble("doc_id" = texts$doc_id[i],
             "judge" = judges_names[j],
              "judge_id" = judges_id[j])
        return(output)
      }
    } 
  return(data_compositions)
}

#Functions call
US_compositions = get_compositions(texts = US_texts, judges_grepl = US_judges$name_lemmatized, judges_names = US_judges$judge_name, judges_id = US_judges$judge_id)
US_compositions = remove_procedural(data = compositions, data_metadata = US_metadata)


# Data check
data_compositions_check = US_compositions %>% select(doc_id, judge) %>% group_by(doc_id) %>% summarise(count = n(judge))
data_compositions_check$doc_id = data_compositions_check$doc_id %>% as.character()
data_compositions_check = US_metadata %>% select(doc_id, formation) %>% left_join(., data_compositions_check) %>% mutate_all(~replace(., is.na(.), 0))

# Visualization
ggplot(data = data_compositions_check, mapping = aes(x = count)) +
  geom_bar(position = "dodge")

# Create a sample
sample = list()
sample$four = data_compositions_check %>% filter(count == 4) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$one = data_compositions_check %>% filter(count == 1) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$five = data_compositions_check %>% filter(count == 5) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$zero = data_compositions_check %>% filter(count == 0) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")