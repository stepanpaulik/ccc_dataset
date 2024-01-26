library(tidyverse)
source("scripts/ccc_web_scraping.R")
source("scripts/ccc_supporting_functions.R")
# source("scripts/WR_decisions_NSS.R")

# LOAD CURRENT DATA -------------------------------------------------------
metadata = read_rds(file = "../data/ccc_dataset/ccc_metadata.rds")
texts = read_rds(file = "../data/ccc_dataset/ccc_texts.rds")
judges = readr::read_rds("../data/ccc_dataset/ccc_judges.rds")
clerks = readr::read_rds("../data/ccc_dataset/ccc_clerks.rds")

# UPDATE ------------------------------------------------------------------
ccc_IDs_new = get_urls(as.character(max(ccc_metadata$date_decision)))
ccc_IDs = c(read_rds(file = "../data/ccc_dataset/ccc_IDs.rds"),ccc_IDs_new)
write_rds(ccc_IDs, file = "../data/ccc_dataset/ccc_IDs.rds")
  
metadata_new = get_metadata(ccc_IDs_new)
metadata = bind_rows(metadata, metadata_new)
metadata = metadata %>%
  get_compositions(metadata = ., texts = texts, judges = judges)
readr::write_rds(metadata, file = "../data/ccc_dataset/ccc_metadata.rds")

texts_new = get_texts(metadata = metadata_new)
texts = bind_rows(texts, texts_new)
readr::write_rds(texts_new, file = "../data/ccc_dataset/ccc_texts.rds")


# ADDITIONAL DATA ---------------------------------------------------------
separate_opinions = get_separate_opinions(metadata, texts, judges)
readr::write_rds(separate_opinions, file = "../data/ccc_dataset/ccc_separate_opinions.rds")

references = bind_rows(
  metadata %>%
    select(doc_id, concerned_acts) %>%
    unnest(concerned_acts) %>% 
    mutate(act_type = "ordinary_act") %>%
    rename(concerned_act = concerned_acts),
  metadata %>%
    select(doc_id, concerned_constitutional_acts) %>%
    unnest(concerned_constitutional_acts) %>% 
    mutate(act_type = "constitutional_act") %>%
    rename(concerned_act = concerned_constitutional_acts),
  metadata %>%
    select(doc_id, citations) %>%
    unnest(citations) %>% 
    unnest(citations) %>%
    mutate(act_type = "caselaw_reference") %>%
    rename(concerned_act = citations)
) %>%
  drop_na() %>%
  write_rds(file = "../data/ccc_dataset/ccc_references.rds")

parties = bind_rows(
  metadata %>%
    select(doc_id, applicant) %>%
    unnest(applicant) %>% 
    mutate(party_type = "applicant") %>%
    rename(party = applicant),
  metadata %>%
    select(doc_id, concerned_body) %>%
    unnest(concerned_body) %>% 
    mutate(party_type = "concerned_body") %>%
    rename(party = concerned_body)
) %>%
  drop_na() %>%
  mutate(party_kind = case_when(
    str_detect(string = party, pattern = "PREZIDENT REPUBLIKY") ~ "president_republic",
    str_detect(string = party, pattern = "STĚŽOVATEL - FO") ~ "natural_person",
    str_detect(string = party, pattern = "STĚŽOVATEL - PO") ~ "legal_person",
    str_detect(string = party, pattern = "OBEC") ~ "municipality",
    str_detect(string = party, pattern = "SOUD") ~ "court",
    str_detect(string = party, pattern = "ÚŘAD|STÁTNÍ ORGÁN|JINÝ ORGÁN VEŘEJNÉ MOCI") ~ "state_authority",
    str_detect(string = party, pattern = "VLÁDA") ~ "government",
    str_detect(string = party, pattern = "KRAJ") ~ "region",
    str_detect(string = party, pattern = "MINISTERSTVO") ~ "ministry",
    str_detect(string = party, pattern = "SKUPINA POSLANCŮ|SENÁTORŮ") ~ "parliament_representatives",
    str_detect(string = party, pattern = "SENÁT PARLAMENTU ČR") ~ "parliament_senate",
    str_detect(string = party, pattern = "POSLANECKÁ SNĚMOVNA PARLAMENTU ČR") ~ "parliament_chamber_of_deputies",
    str_detect(string = party, pattern = "POLICIE") ~ "police",
    str_detect(string = party, pattern = "STÁTNÍ ZASTUPITELSTVÍ") ~ "state_prosecution",
    .default = NA
  )) %>%
  write_rds(file = "../data/ccc_dataset/ccc_parties.rds")

subject_matter = bind_rows(
  metadata %>%
    select(doc_id, subject_proceedings) %>%
    unnest(subject_proceedings) %>% 
    mutate(source = "subject_proceedings") %>%
    rename(subject_matter = subject_proceedings),
  metadata %>%
    select(doc_id, field_register) %>%
    unnest(field_register) %>% 
    mutate(source = "field_register") %>%
    rename(subject_matter = field_register)
) %>%
  drop_na() %>%
  write_rds(file = "../data/ccc_dataset/ccc_subject_matter.rds")

disputed_act = metadata %>%
  select(doc_id, disputed_act) %>%
  unnest(disputed_act) %>%
  mutate(type_disputed_act = case_when(
    str_detect(string = disputed_act, pattern = "obecně závazná vyhláška") ~ "municipal_statute",
    str_detect(string = disputed_act, pattern = "zákon") ~ "statute",
    str_detect(string = disputed_act, pattern = "vyhláška") ~ "regulation",
    str_detect(string = disputed_act, pattern = "rozhodnutí správní") ~ "administrative_decision",
    str_detect(string = disputed_act, pattern = "nařízení") ~ "decree",
    str_detect(string = disputed_act, pattern = "opatření obecné povahy") ~ "general_measure",
    str_detect(string = disputed_act, pattern = "ostatní (nezařaditelné)") ~ "other",
    str_detect(string = disputed_act, pattern = "usnesení vlády") ~ "government_resolution",
    str_detect(string = disputed_act, pattern = "rozhodnutí") & !str_detect(string = disputed_act, pattern = "soud") ~ "decision",
    str_detect(string = disputed_act, pattern = "rozhodnutí") & str_detect(string = disputed_act, pattern = "jiné")  ~ "decision",
    str_detect(string = disputed_act, pattern = "rozhodnutí") & str_detect(string = disputed_act, pattern = "soud")  ~ "court_decision",
    .default = "other"
  )) %>%
  drop_na() %>%
  write_rds(., file = "../data/ccc_dataset/ccc_disputed_acts.rds")

compositions = metadata %>%
  select(doc_id, composition) %>%
  unnest(composition) %>%
  write_rds(file = "../data/ccc_dataset/ccc_compositions.rds")

verdict = metadata %>%
  select(doc_id, type_verdict) %>%
  unnest(type_verdict) %>%
  mutate(verdict_kind = case_when(
    str_detect(string = type_verdict, pattern = "odmítnuto") ~ "admissibility",
    str_detect(string = type_verdict, pattern = "vyhověno|zamítnuto") ~ "merits",
    str_detect(string = type_verdict, pattern = "procesní") ~ "procedural",
    .default = "other"
  )) %>%
  write_rds(file = "../data/ccc_dataset/ccc_verdicts.rds")


