library(tidyverse)
library(furrr)

get_separate_opinions = function(metadata, texts, judges){
  future::plan(multisession, workers = parallel::detectCores() - 2)
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
  
  metadata %<>%
    filter(!is.na(separate_opinion)) %>%
    unnest(separate_opinion) %>%
    select(doc_id, separate_opinion)
  
  data = texts %>%
    filter(doc_id %in% metadata$doc_id) %>%
    mutate(paragraph = str_split(string = text, pattern = "\n+")) %>%
    select(-text) %>%
    unnest(paragraph) %>%
    group_by(doc_id) %>%
    mutate(paragraph_id = row_number()) %>%
    ungroup() %>%
    filter(str_detect(string = paragraph, pattern = dissent_term)) %>% # Up until here the code splits up the texts into paragraphs and filters those which contain the dissent_term strings
    mutate(dissenting_judge = future_pmap(., function(doc_id, paragraph, ...) judges %>%
                                     filter(str_detect(string = paragraph, pattern = judge_name_lemmatized)|str_detect(string = paragraph, pattern = judge_initials)) %>%
                                     select(judge_name, judge_id) %>%
                                     distinct())) %>% # A parallel map to detect the judge names/initials in the paragraphs and to keep the doc_id as well
    group_by(doc_id) %>%
    mutate(dissenting_group = row_number()) %>% # Finally assign an identifier of whether the dissent was written together or separately
    ungroup() %>%
    unnest(dissenting_judge) %>%
    select(-c(paragraph, paragraph_id)) %>%
    rename(dissenting_judge_name = judge_name,
           dissenting_judge_id = judge_id) %>%
    group_by(doc_id) %>%
    arrange(desc(dissenting_group)) %>% # removes duplicates because of the "We join the dissent of...", the first occurring name is joined to the larger group
    distinct(dissenting_judge_name, .keep_all = TRUE) %>%
    arrange(dissenting_group) %>%
    ungroup() %>%
    left_join(metadata, ., by = join_by(doc_id, separate_opinion == dissenting_judge_name)) %>%
    rename(dissenting_judge_name = separate_opinion)
  return(data)
}

get_compositions = function(metadata, texts, judges){ 
  future::plan(multisession, workers = parallel::detectCores() - 2)
  
  data = left_join(metadata, texts, by = join_by(doc_id)) %>%
    mutate(composition = future_pmap(., function(doc_id, text, ...) judges %>%
                                       filter(str_detect(string = text, pattern = judge_name_lemmatized)) %>%
                                       select(judge_name, judge_id) %>%
                                       distinct())) %>%
    mutate(
           citations = future_pmap(., function(doc_id, text, ...) str_extract_all(string = text, pattern = '[A-Za-z]{1,2}\\.\\s*ÚS\\s*\\d{1,4}\\/\\d{1,4}') %>%
                                     as_tibble_col(column_name = "citations") %>%
                                     distinct())) %>%
    select(-text)
  return(data)
}
