library(tidyverse)

# Load data
US_texts = readr::read_rds("../data/US_texts.rds")
US_metadata = readr::read_rds("../data/US_metadata.rds")
US_judges = readr::read_rds("../data/US_judges.rds")

find_judges = function(x, judges, texts){
  judges %>%
    filter(str_detect(string = x, pattern = judge_name_lemmatized)) %>%
    select(judge_name, judge_id) %>%
    bind_cols(., texts %>%
                filter(text == x)) %>%
    select(-text) %>%
    relocate(doc_id)
}

US_compositions = map_dfr(.x = US_texts$text, ~find_judges(x = .x, judges = US_judges, texts = US_texts), .progress = TRUE)

# Save data
write_rds(US_compositions, file = "../data/US_compositions.rds")


# 
# # Data check
# data_compositions_check = US_compositions %>% select(doc_id, judge) %>% group_by(doc_id) %>% summarise(count = n(judge))
# data_compositions_check$doc_id = data_compositions_check$doc_id %>% as.character()
# data_compositions_check = US_metadata %>% select(doc_id, formation) %>% left_join(., data_compositions_check) %>% mutate_all(~replace(., is.na(.), 0))
# 
# # Visualization
# ggplot(data = data_compositions_check, mapping = aes(x = count)) +
#   geom_bar(position = "dodge")

# Create a sample
sample = list()
sample$four = data_compositions_check %>% filter(count == 4) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$one = data_compositions_check %>% filter(count == 1) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$five = data_compositions_check %>% filter(count == 5) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$zero = data_compositions_check %>% filter(count == 0) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")