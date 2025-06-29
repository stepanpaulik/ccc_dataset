library(tidyverse)
source("scripts/ccc_web_scraping.R")
source("scripts/ccc_supporting_functions.R")

update = FALSE
write = TRUE

# This is the master file that runs the web scraping process, including automatic update of the DB.

if(update == FALSE & write == FALSE){
  # LOAD CURRENT DATA -------------------------------------------------------
  metadata = read_rds(file = "../data/ccc_database/rds/ccc_metadata.rds")
  texts = read_rds(file = "../data/ccc_database/rds/ccc_texts.rds")
  judges = readr::read_rds("../data/ccc_database/rds/ccc_judges.rds")
  clerks = readr::read_rds("../data/ccc_database/rds/ccc_clerks.rds")
  compositions = readr::read_rds("../data/ccc_database/rds/ccc_compositions.rds")
  separate_opinions = readr::read_rds("../data/ccc_database/rds/ccc_separate_opinions.rds")
  references = readr::read_rds("../data/ccc_database/rds/ccc_references.rds")
  parties = readr::read_rds("../data/ccc_database/rds/ccc_parties.rds")
  subject_matter = readr::read_rds("../data/ccc_database/rds/ccc_subject_matter.rds")
  disputed_acts = readr::read_rds("../data/ccc_database/rds/ccc_disputed_acts.rds")
  verdicts = readr::read_rds("../data/ccc_database/rds/ccc_verdicts.rds")
}

if(update == TRUE){
  metadata = read_rds(file = "../data/ccc_database/rds/metadata.rds")
  # UPDATE CURRENT DATA -----------------------------------------------------
  ccc_IDs_new = get_urls(as.character(max(metadata$date_decision))) # Reads the latest date from the current data
  ccc_IDs = c(read_rds(file = "../data/ccc_database/rds/ccc_IDs.rds"),ccc_IDs_new) # Update the identifiers used to scrape the decisions
  write_rds(ccc_IDs, file = "../data/ccc_database/rds/ccc_IDs.rds")
  
  metadata_new = get_metadata(ccc_IDs_new)
  metadata = bind_rows(metadata, metadata_new) |>
    mutate(doc_id = make.unique(doc_id))
  
  metadata = get_compositions(metadata = metadata, texts = texts, judges = judges)
  
  metadata = get_citations(metadata = metadata, texts = texts)
  
  # Unnest the compositions list-column
  compositions = metadata |>
    select(doc_id, composition) |>
    unnest(composition)
  
  # Update texts
  texts_new = get_texts(metadata = metadata |>
                          filter(!doc_id %in% texts$doc_id))
  texts = bind_rows(texts, texts_new)
  
} else{
  source("scripts/ccc_judges.R")
  ccc_IDs = read_rds("../data/ccc_database/rds/ccc_IDs.rds")
  # source("scripts/ccc_clerks.R")
  ccc_IDs = get_urls() # Call with the default "day 0" date
  write_rds(ccc_IDs, file = "../data/ccc_database/rds/ccc_IDs.rds")
  
  metadata = get_metadata(ccc_IDs)
  # write_rds(texts, file = "../data/ccc_database/rds/ccc_texts.rds")
  
  # Update texts
  texts = get_texts(metadata = metadata)
  
  metadata = get_compositions(metadata = metadata, texts = texts, judges = judges)
  
  metadata = get_citations(metadata = metadata, texts = texts)
  
  # Unnest the compositions list-column
  compositions = metadata |>
    select(doc_id, composition) |>
    unnest(composition)
  
  
}

# ADDITIONAL DATA ---------------------------------------------------------
# This creates the auxiliary tables with additional data wrangling steps
separate_opinions = get_dissenting_opinions(metadata, texts, judges)

references = bind_rows(
  metadata |>
    select(doc_id, concerned_acts) |>
    unnest(concerned_acts) |> 
    mutate(act_type = "ordinary_act") |>
    rename(concerned_act = concerned_acts),
  metadata |>
    select(doc_id, concerned_constitutional_acts) |>
    unnest(concerned_constitutional_acts) |> 
    mutate(act_type = "constitutional_act") |>
    rename(concerned_act = concerned_constitutional_acts),
  metadata |>
    select(doc_id, citations) |>
    unnest(citations) |> 
    unnest(citations) |>
    mutate(act_type = "caselaw_reference") |>
    rename(concerned_act = citations)
) |>
  drop_na()

duplicates = metadata |> group_by(doc_id) |> filter(n() > 1)
data = metadata |>
  distinct()

parties = bind_rows(
  metadata |>
    select(doc_id, applicant) |>
    unnest(applicant) |> 
    mutate(party_type = "applicant") |>
    rename(party = applicant),
  metadata |>
    select(doc_id, concerned_body) |>
    unnest(concerned_body) |> 
    mutate(party_type = "concerned_body") |>
    rename(party = concerned_body)
) |>
  drop_na() |>
  mutate(party_nature = case_when(
    str_detect(string = party, pattern = "PREZIDENT REPUBLIKY") ~ "president_republic",
    str_detect(string = party, pattern = "STĚŽOVATEL - FO") ~ "natural_person",
    str_detect(string = party, pattern = "STĚŽOVATEL - PO") ~ "legal_person",
    str_detect(string = party, pattern = "OBEC") ~ "municipality",
    str_detect(string = party, pattern = "SOUD") ~ "court",
    str_detect(string = party, pattern = "ÚŘAD|STÁTNÍ ORGÁN|JINÝ ORGÁN VEŘEJNÉ MOCI|INSPEKCE|ČESKÁ SPRÁVA SOCIÁLNÍHO ZABEZPEČENÍ|ÚSTAV|FOND|RADA PRO ROZHLASOVÉ A TELEVIZNÍ VYSÍLÁNÍ|PROFESNÍ KOMORA") ~ "state_authority",
    str_detect(string = party, pattern = "VLÁDA") ~ "government",
    str_detect(string = party, pattern = "KRAJ") ~ "region",
    str_detect(string = party, pattern = "MINISTERSTVO") ~ "ministry",
    str_detect(string = party, pattern = "SKUPINA POSLANCŮ|SENÁTORŮ") ~ "parliament_representatives",
    str_detect(string = party, pattern = "SENÁT PARLAMENTU ČR") ~ "parliament_senate",
    str_detect(string = party, pattern = "POSLANECKÁ SNĚMOVNA PARLAMENTU ČR") ~ "parliament_chamber_of_deputies",
    str_detect(string = party, pattern = "POLICIE") ~ "police",
    str_detect(string = party, pattern = "STÁTNÍ ZASTUPITELSTVÍ") ~ "state_prosecution",
    str_detect(string = party, pattern = "VEŘEJNÝ OCHRÁNCE PRÁV") ~ "ombudsperson",
    str_detect(string = party, pattern = "VOLEBNÍ STRANA") ~ "political party",
    str_detect(string = party, pattern = "ARMÁDA|BEZPEČNOSTNÍ INFORMAČNÍ SLUŽBA") ~ "defense",
    str_detect(string = party, pattern = "POSLANEC|SENÁTOR") ~ "MP",
    str_detect(string = party, pattern = "POJIŠŤOVNA") ~ "public insurance company",
    str_detect(string = party, pattern = "BANKA") ~ "central bank",
    str_detect(string = party, pattern = "SENÁT ÚS ") ~ "CCC chamber",
    str_detect(string = party, pattern = "VĚZEŇSKÁ SLUŽBA") ~ "prison service",
    .default = NA
  )) # Flattens the party data and translates them into English

subject_matter = bind_rows(
  metadata |>
    select(doc_id, subject_proceedings) |>
    unnest(subject_proceedings) |> 
    mutate(source = "subject_proceedings") |>
    rename(subject_matter = subject_proceedings),
  metadata |>
    select(doc_id, subject_register) |>
    unnest(subject_register) |> 
    mutate(source = "subject_register") |>
    rename(subject_matter = subject_register)
) |>
  drop_na()

disputed_acts = metadata |>
  select(doc_id, disputed_act) |>
  unnest(disputed_act) |>
  mutate(disputed_act_type = case_when(
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
  )) |> # Again flattening and translating the data
  drop_na()


verdicts = metadata |>
  select(doc_id, type_verdict) |>
  unnest(type_verdict) |>
  mutate(verdict_ground = case_when(
    str_detect(string = type_verdict, pattern = "odmítnuto") ~ "admissibility",
    str_detect(string = type_verdict, pattern = "vyhověno|zamítnuto") ~ "merits",
    str_detect(string = type_verdict, pattern = "procesní") ~ "procedural",
    .default = "other"
  )) |>
  rename(verdict_type = type_verdict)


# WRITE DATA ---------------------------------------------------------------
if(write == TRUE) {
  write_rds(metadata, file = "../data/ccc_database/rds/metadata.rds")
  write_rds(texts, file = "../data/ccc_database/rds/ccc_texts.rds")
  write_rds(separate_opinions, file = "../data/ccc_database/rds/ccc_separate_opinions.rds")
  write_rds(references, file = "../data/ccc_database/rds/ccc_references.rds")
  write_rds(parties, file = "../data/ccc_database/rds/ccc_parties.rds")
  write_rds(subject_matter, file = "../data/ccc_database/rds/ccc_subject_matter.rds")
  write_rds(disputed_acts, file = "../data/ccc_database/rds/ccc_disputed_acts.rds")
  write_rds(compositions, file = "../data/ccc_database/rds/ccc_compositions.rds")
  write_rds(verdicts, file = "../data/ccc_database/rds/ccc_verdicts.rds")
  
  # Transform all .rds files into CSV ---------------------------------------------------------------------
  rds2csv = function(file) {
    data = read_rds(file) |>
      mutate(across(where(is.list), as.character))
    outfile = str_replace(string = file, pattern = "\\.rds", replacement = ".csv") |>
      str_replace("../data/ccc_database/rds/", replacement = "../data/ccc_database/csv/")
    write_csv(data, file=outfile)
  }
  
  list.files(path = "../data/ccc_database/rds", pattern = ".rds", full.names = TRUE) |>
    map(rds2csv)
}


