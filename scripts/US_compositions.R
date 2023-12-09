library(tidyverse)
library(furrr)

# Load data
US_texts = readr::read_rds("../data/US_texts.rds")
US_judges = readr::read_rds("../data/US_judges.rds")
US_metadata = readr::read_rds("../data/US_metadata.rds")


get_compositions = function(metadata, texts, judges){ 
  # Paralelise
  plan(multisession, workers = parallel::detectCores() - 2)
  
  tictoc::tic()
  metadata = left_join(metadata, texts, by = join_by(doc_id)) %>%
    mutate(composition = future_pmap(., function(doc_id, text, ...) judges %>%
                                       filter(str_detect(string = text, pattern = judge_name_lemmatized)) %>%
                                       select(judge_name, judge_id))) %>%
    select(-text)
  tictoc::toc()
  return(metadata)
}

US_metadata = get_compositions(US_metadata, US_texts, US_judges)

# Save data
write_rds(US_compositions, file = "../data/US_compositions.rds")


# 
# # Completion check
# data_compositions_check = US_compositions %>% select(doc_id, judge) %>% group_by(doc_id) %>% summarise(count = n(judge))
# data_compositions_check$doc_id = data_compositions_check$doc_id %>% as.character()
# data_compositions_check = US_metadata %>% select(doc_id, formation) %>% left_join(., data_compositions_check) %>% mutate_all(~replace(., is.na(.), 0))
# 
# # Visualization
# ggplot(data = data_compositions_check, mapping = aes(x = count)) +
#   geom_bar(position = "dodge")
# 
# # Create a sample
# sample = list()
# sample$four = data_compositions_check %>% filter(count == 4) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
# sample$one = data_compositions_check %>% filter(count == 1) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
# sample$five = data_compositions_check %>% filter(count == 5) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
# sample$zero = data_compositions_check %>% filter(count == 0) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")