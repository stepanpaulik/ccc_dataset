US_metadata = US_metadata %>% 
  left_join(., US_judges %>% select(judge_name, judge_id), by = join_by(judge_rapporteur == judge_name)) %>%
  rename(judge_rapporteur_name = judge_rapporteur,
         judge_rapporteur_id = judge_id)

# Insert commas instead of non-existing spaces
US_metadata$applicant %<>% modify(.x = ., ~ str_replace_all(string = .x, pattern = "POSTĚŽOVATEL", replacement = "PO, STĚŽOVATEL")) %>% str_trim()
US_metadata$applicant %<>% modify(.x = ., ~ str_replace_all(string = .x, pattern = "FOSTĚŽOVATEL", replacement = "FO, STĚŽOVATEL")) %>% str_trim()
US_metadata$applicant %<>% modify(.x = ., ~ str_replace_all(string = .x, pattern = "STRANAPOLITICKÁ", replacement = "STRANA, POLITICKÁ")) %>% str_trim()
US_metadata$applicant %<>% modify_if(.x = ., .p = grepl(pattern = "[A-Za-z]STĚŽOVATEL", x = .x), ~ str_replace_all(string = .x, pattern = "STĚŽOVATEL", replacement = ", STĚŽOVATEL")) %>% str_trim()
unique(US_metadata$applicant)

