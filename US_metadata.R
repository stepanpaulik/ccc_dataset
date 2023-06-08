xfun::pkg_attach2("udpipe","tidyverse", "utf8", "uchardet")

# Load data
US_metadata = readRDS("../data/US_metadata.rds")
US_texts = readRDS("../data/US_texts.rds")
US_compositions = readRDS("../data/US_compositions.rds")
US_judges = readRDS("../data/US_judges.rds")
US_dissents = readRDS("../data/US_dissents.rds")


# Save data
saveRDS(US_metadata, file = "../data/US_metadata.rds")
saveRDS(US_texts, file = "../data/US_texts.rds")
saveRDS(US_compositions, file = "../data/US_compositions.rds")
saveRDS(US_judges, file = "../data/US_judges.rds")
saveRDS(US_dissents, file = "../data/US_dissents.rds")



distinct = US_metadata %>% filter(year_cc > 2003) %>% n_distinct()

# Clean useless columns and rename
# US_metadata = US_metadata %>% subset(select = -c(2,4,5,8,10,24)) %>% rename(
#   doc_id = identifikator_evropske_judikatury,
#   case_id = spisova_znacka,
#   popular_name = popularni_nazev,
#   date_decision = datum_rozhodnuti,
#   date_submission = datum_podani,
#   type_decision = forma_rozhodnuti,
#   type_proceedings = typ_rizeni,
#   importance = vyznam,
#   applicant = navrhovatel,
#   concerned_body = dotceny_organ,
#   judge_rapporteur = soudce_zpravodaj,
#   disputed_act = napadeny_akt,
#   type_verdict = typ_vyroku,
#   concerned_constitutional_acts = dotcene_ustavni_zakony_a_mezinarodni_smlouvy,
#   concerned_acts = ostatni_dotcene_predpisy,
#   dissenting_opinion = odlisne_stanovisko,
#   subject_proceedings = predmet_rizeni,
#   field_register = vecny_rejstrik,
#   note = poznamka,
#   url_adress = url_adresa
# )

US_metadata = US_metadata %>% rename(year_decision = year_cc)

# Clean doc_id
US_metadata$doc_id = str_replace_all(US_metadata$doc_id ,"\\.", ":")
US_texts$doc_id = str_replace_all(US_texts$doc_id ,"\\.", ":")
join = left_join(US_metadata, US_texts)

# Split into texts and metadata
US_texts = US_metadata %>% subset(select = c(doc_id, decisions_texts))
US_metadata = US_metadata %>% subset(select = -c(decisions_texts))
US_texts = US_texts %>% rename(
  texts = decisions_texts
)

# Lubridate the dates -- DON'T MAKE THE SAME MISTAKE WITH y/Y! --
US_metadata$date_decision = as.Date(US_metadata$date_decision, format = "%d. %m. %Y")
US_metadata$date_submission = as.Date(US_metadata$date_submission, format = "%d. %m. %Y")
US_metadata$length_proceedings = interval(US_metadata$date_submission, US_metadata$date_decision) %>% as.numeric('days')

# Text normalization
US_texts$text = US_texts$text %>% utf8_normalize(map_quote = TRUE)

# Add outcome as binary variable
add_outcome = function(US_metadata, outcome = "vyhověno") {
  US_metadata$outcome = ifelse(grepl(outcome, US_metadata$type_verdict), "granted", "rejected")
  return(US_metadata)
}

# Fix and check duplicates
US_texts$doc_id = make.names(US_texts$doc_id, unique = TRUE)
US_metadata$doc_id = make.names(US_metadata$doc_id, unique = TRUE)
data_duplicates = US_texts %>% group_by(doc_id) %>% filter(n()>1)

# Add formation
US_metadata = US_metadata %>%
  mutate(formation = case_when(
    grepl(":Pl:" , doc_id) ~ "Plenum",
    grepl(":1:US:", doc_id) ~ "First Chamber",
    grepl(":2:US:", doc_id) ~ "Second Chamber",
    grepl(":3:US:", doc_id) ~ "Third Chamber",
    grepl(":4:US:", doc_id) ~ "Fourth Chamber"
  ))

US_metadata = US_metadata %>% mutate_all(~replace(., . == "", NA))

# Insert commas instead of non-existing spaces
US_metadata$applicant %<>% modify(.x = ., ~ str_replace_all(string = .x, pattern = "POSTĚŽOVATEL", replacement = "PO, STĚŽOVATEL")) %>% str_trim()
US_metadata$applicant %<>% modify(.x = ., ~ str_replace_all(string = .x, pattern = "FOSTĚŽOVATEL", replacement = "FO, STĚŽOVATEL")) %>% str_trim()
US_metadata$applicant %<>% modify(.x = ., ~ str_replace_all(string = .x, pattern = "STRANAPOLITICKÁ", replacement = "STRANA, POLITICKÁ")) %>% str_trim()
US_metadata$applicant %<>% modify_if(.x = ., .p = grepl(pattern = "[A-Za-z]STĚŽOVATEL", x = .x), ~ str_replace_all(string = .x, pattern = "STĚŽOVATEL", replacement = ", STĚŽOVATEL")) %>% str_trim()
unique(US_metadata$applicant)


# DATA PREP
# Split the texts into paragraphs and index them
# paragraphs_split = function(US_texts) {
#     # Create temporary object with doc_id + text split up into paragraphs
#   judgments_annotated_paragraphs_temp = US_texts %>% group_by(doc_id) %>% summarise(paragraphs = str_split(text, pattern = "\n\n"))
#   
#   # The meat of the function: nested foreach loop
#   judgments_annotated_paragraphs = foreach(i = seq(judgments_annotated_paragraphs_temp$doc_id), .combine='rbind') %:%
#     foreach(j = 1:length(judgments_annotated_paragraphs_temp$paragraphs[[i]]), .combine = 'rbind') %do% {
#       paragraph_temp = judgments_annotated_paragraphs_temp$paragraphs[[i]][j] %>% str_trim(side = "both")
#       location_temp = str_locate(string = US_texts$text[US_texts$doc_id == judgments_annotated_paragraphs_temp$doc_id[i]], pattern = fixed(judgments_annotated_paragraphs_temp$paragraphs[[i]][j])) %>% as.list()
#       text_length = str_length(US_texts$text[US_texts$doc_id == judgments_annotated_paragraphs_temp$doc_id[i]])
#       output = list(
#         "doc_id" = judgments_annotated_paragraphs_temp$doc_id[i], 
#         "paragraph_id" = j, 
#         "paragraph_text" = paragraph_temp, 
#         "paragraph_start" = as.numeric(location_temp[1])/text_length, 
#         "paragraph_end" = as.numeric(location_temp[2])/text_length, 
#         "paragraph_length" = str_length(paragraph_temp)/text_length
#       )
#       return(output)
#     } %>% as_tibble() %>% df_unlist()
# 
#   # Drop NA values
#   judgments_annotated_paragraphs = judgments_annotated_paragraphs  
#   
#   return(judgments_annotated_paragraphs)
# }
# 
# # Run the function and save the file
# judgments_annotated_paragraphs = paragraphs_split(US_texts = US_texts)
# save(judgments_annotated_paragraphs, file = "data/US_texts_paragraphs.RData")





