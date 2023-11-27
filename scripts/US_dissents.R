library(tidyverse)

# Load data
US_metadata = readr::read_rds("../data/US_metadata.rds") %>%
  filter(!is.na(dissenting_opinion)) %>%
  unnest(dissenting_opinion) %>%
  select(doc_id, dissenting_opinion)
US_texts = readr::read_rds("../data/US_texts.rds") %>%
  filter(doc_id %in% US_metadata$doc_id)
US_judges = readr::read_rds("../data/US_judges.rds") %>%
  mutate(initials = paste0(substring(word(judge_name, 1), 1, 1), ".\\s*", substring(word(judge_name, 2), 1, 1), "."))

# The terms detecting paragraphs starting the dissenting opinion
dissent_term = paste("Odlišné stanovisko",
                "ODLIŠNÉ STANOVISKO",
                "O d l i š n é   s t a n o v i s k o",
                "O d l i š n é  s t a n o v i s k o",
                "Odlišné  stanovisko",
                "O d l i š n é s t a n o v i s k o",
                "O D L I Š N É  S T A N O V I S K O",
                "Odlišné doplňující stanovisko",
                "Odlišné-doplňující stanovisko", "Konkurující stanovisko", "K odlišnému stanovisku", sep = "|")

US_dissents = US_texts %>%
  mutate(paragraph = str_split(string = text, pattern = "\n+")) %>%
  select(-text) %>%
  unnest(paragraph) %>%
  group_by(doc_id) %>%
  mutate(paragraph_id = row_number()) %>%
  ungroup() %>%
  filter(str_detect(string = paragraph, pattern = dissent_term)) %>% # Up until here the code splits up the texts into paragraphs and filters those which contain the dissent_term strings
  mutate(dissenting_judge = pmap(., function(doc_id, paragraph, ...) US_judges %>%
                                    filter(str_detect(string = paragraph, pattern = judge_name_lemmatized)|str_detect(string = paragraph, pattern = initials)) %>%
                                    select(judge_name, judge_id))) %>% # A parallel map to detect the judge names/initials in the paragraphs and to keep the doc_id as well
  group_by(doc_id) %>%
  mutate(dissenting_group = row_number()) %>% # Finally assign an identifier of whether the dissent was written together or separately
  ungroup() %>%
  unnest(dissenting_judge) %>%
  select(-c(paragraph, paragraph_id)) %>%
  rename(dissenting_judge = judge_name,
         dissenting_judge_id = judge_id) %>%
  group_by(doc_id) %>%
  arrange(desc(dissenting_group)) %>% # removes duplicates because of the "We join the dissent of...", the first occurring name is joined to the larger group
  distinct(dissenting_judge, .keep_all = TRUE) %>%
  arrange(dissenting_group) %>%
  ungroup() %>%
  left_join(US_metadata, ., by = join_by(doc_id, dissenting_opinion == dissenting_judge))

# Save data
readr::write_rds(US_dissents, file = "../data/US_dissents.rds")




