library(tidyverse)
library(furrr)

get_dissenting_opinions = function(metadata, texts, judges){
  future::plan(multisession, workers = parallel::detectCores() - 2)
  # The terms detecting paragraphs starting the dissenting opinion
  dissent_term_vec = c("Odlišné stanovisko",
                       "ODLIŠNÉ STANOVISKO",
                       "O d l i š n é   s t a n o v i s k o",
                       "O d l i š n é  s t a n o v i s k o",
                       "Odlišné  stanovisko",
                       "O d l i š n é s t a n o v i s k o",
                       "O D L I Š N É  S T A N O V I S K O",
                       "Odlišné doplňující stanovisko",
                       "Odlišné-doplňující stanovisko", 
                       "Konkurující stanovisko", 
                       "K odlišnému stanovisku",
                       "K tomuto odlišnému stanovisku",
                       "Odlišné stanovisko:", 
                       "Odlišné stanovisko k nálezu pléna Ústavního soudu ČR", 
                       "Odlišné stanovisko č. 1", 
                       "Odlišné stanovisko č. 2", 
                       "Odlišné stanovisko č. 3", 
                       "Odlišné stanovisko č. 4",
                       "Odlišné stanovisko č. 5",
                       "ODLIŠNÉ STANOVISKO č. 1",
                       "ODLIŠNÉ STANOVISKO č. 2")
  dissent_term = paste("Odlišné stanovisko",
                       "ODLIŠNÉ STANOVISKO",
                       "O d l i š n é   s t a n o v i s k o",
                       "O d l i š n é  s t a n o v i s k o",
                       "Odlišné  stanovisko",
                       "O d l i š n é s t a n o v i s k o",
                       "O D L I Š N É  S T A N O V I S K O",
                       "Odlišné doplňující stanovisko",
                       "Odlišné-doplňující stanovisko", 
                       "Konkurující stanovisko", 
                       "K odlišnému stanovisku",
                       "K tomuto odlišnému stanovisku", sep = "|")
  
  metadata = metadata |>
    filter(!is.na(dissenting_opinion)) |>
    unnest(dissenting_opinion) |>
    select(doc_id, dissenting_opinion)
  
  data = texts |>
    filter(doc_id %in% metadata$doc_id) |>
    mutate(paragraph = str_split(string = text, pattern = "\n+")) |>
    select(-text) |>
    unnest(paragraph) |>
    group_by(doc_id) |>
    mutate(paragraph_id = row_number()) |>
    ungroup() |>
    filter((str_detect(string = paragraph, pattern = dissent_term) | str_trim(lag(paragraph)) %in% dissent_term_vec) & !(str_trim(paragraph) %in% dissent_term_vec)) |>
    filter() 
  
  # Up until here the code splits up the texts into paragraphs and filters those which contain the dissent_term strings
  
  data = data %>%
    mutate(dissenting_judge = pmap(., function(doc_id, paragraph, ...) judges |>
                                     filter(str_detect(string = paragraph, pattern = judge_name_lemmatized)|str_detect(string = paragraph, pattern = judge_initials)) |>
                                     select(judge_name) |>
                                     distinct())) |> # A parallel map to detect the judge names/initials in the paragraphs and to keep the doc_id as well
    unnest(dissenting_judge) |> # Gets rid of empty rows
    nest(dissenting_judge = c(judge_name)) |>
    group_by(doc_id) |>
    mutate(dissenting_group = row_number()) |> # Finally assign an identifier of whether the dissent was written together or separately
    ungroup() |>
    select(-c(paragraph, paragraph_id)) |>
    unnest(dissenting_judge) |>
    rename(dissenting_judge_name = judge_name) |>
    group_by(doc_id) |>
    arrange(desc(dissenting_group)) |> # removes duplicates because of the "We join the dissent of...", the first occurring name is joined to the larger group
    distinct(dissenting_judge_name, .keep_all = TRUE) |>
    arrange(dissenting_group) |>
    ungroup() 
  
  # Finally join back with the metadata and judges tables
  data = data |>
    right_join(metadata, by = join_by(doc_id, dissenting_judge_name == dissenting_opinion)) |>
    left_join(judges |> select(judge_name, judge_id) |> distinct(), by = join_by(dissenting_judge_name == judge_name)) |>
    rename(
      # dissenting_judge_name = dissenting_opinion,
           dissenting_judge_id = judge_id) |>
    relocate(dissenting_judge_id, .after = dissenting_judge_name)
  return(data)
}

get_compositions = function(metadata, texts, judges){ 
  data = left_join(metadata, texts, by = join_by(doc_id)) %>%
    mutate(composition = pmap(., function(doc_id, text, ...) {
      text = text |>
        str_split("\\n\\n") |>
        unlist()
      
      text_temp = paste0(head(text, 3), collapse = " ")
      
      
      found_judges = judges |>
        select(judge_name, judge_id, judge_name_lemmatized) |>
        filter(str_detect(string = text_temp, pattern = judge_name_lemmatized)) |>
        distinct() |>
        mutate(position = str_locate(string = text_temp, pattern = judge_name_lemmatized) |> as_tibble()) |>
        select(-judge_name_lemmatized)
      
      if(nrow(found_judges) == 4) {found_judges = found_judges |>
        filter(position[[1]] != max(position[[1]]))}
      
      found_judges |>
        select(judge_name, judge_id)
    }, .progress = TRUE)) |>
    select(-text)
  return(data)
}

get_citations = function(metadata, texts){
  data = left_join(metadata, texts, by = join_by(doc_id)) |>
    mutate(
      citations = str_extract_all(string = text, pattern = '[A-Za-z]{1,2}\\.\\s*ÚS\\s*\\d{1,4}\\/\\d{1,4}')) |>
    select(-text)
  return(data)
}
