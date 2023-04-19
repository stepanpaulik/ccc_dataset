xfun::pkg_attach2("udpipe","tidyverse", "RMySQL", "lubridate", "utf8", "uchardet")
# Load data
load("data/US_compositions.RData")
load("data/US_texts.RData")
load("data/US_metadata.RData")
load("data/US_judges.RData")
load("data/US_dissents.RData")

# Save data
save(US_metadata, file = "data/US_metadata.RData")
save(US_texts, file = "data/US_texts.RData")
save(data_ud, file = "models/US_UDmodel.RData")

distinct <- US_metadata %>% filter(year_cc > 2003) %>% n_distinct()

# Clean useless columns and rename
# US_metadata <- US_metadata %>% subset(select = -c(2,4,5,8,10,24)) %>% rename(
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

US_metadata <- US_metadata %>% rename(year_decision = year_cc)

# Clean doc_id
US_metadata$doc_id <- str_replace_all(US_metadata$doc_id ,"\\.", ":")
US_texts$doc_id <- str_replace_all(US_texts$doc_id ,"\\.", ":")
join <- left_join(US_metadata, US_texts)

# Split into texts and metadata
US_texts <- US_metadata %>% subset(select = c(doc_id, decisions_texts))
US_metadata <- US_metadata %>% subset(select = -c(decisions_texts))
US_texts <- US_texts %>% rename(
  texts = decisions_texts
)

# Lubridate the dates -- DON'T MAKE THE SAME MISTAKE WITH y/Y! --
US_metadata$date_decision <- as.Date(US_metadata$date_decision, format = "%d. %m. %Y")
US_metadata$date_submission <- as.Date(US_metadata$date_submission, format = "%d. %m. %Y")
US_metadata$length_proceedings <- interval(US_metadata$date_submission, US_metadata$date_decision) %>% as.numeric('days')

# Text normalization
US_texts$text <- US_texts$text %>% utf8_normalize(map_quote = TRUE)

# Add outcome as binary variable
add_outcome <- function(US_metadata, outcome = "vyhověno") {
  US_metadata$outcome <- ifelse(grepl(outcome, US_metadata$type_verdict), "granted", "rejected")
  return(US_metadata)
}

# Fix and check duplicates
US_texts$doc_id <- make.names(US_texts$doc_id, unique = TRUE)
US_metadata$doc_id <- make.names(US_metadata$doc_id, unique = TRUE)
data_duplicates <- US_texts %>% group_by(doc_id) %>% filter(n()>1)

# Add formation
US_metadata <- US_metadata %>%
  mutate(formation = case_when(
    grepl(":Pl:" , doc_id) ~ "Plenum",
    grepl(":1:US:", doc_id) ~ "First Chamber",
    grepl(":2:US:", doc_id) ~ "Second Chamber",
    grepl(":3:US:", doc_id) ~ "Third Chamber",
    grepl(":4:US:", doc_id) ~ "Fourth Chamber"
  ))









