library(RSelenium)
library(tidyverse)
library(rvest)
library(foreach)
library(httr)
library(xml2)
library(progress)
library(lubridate)
library(parallel)

# Parallelise
library(doMC)
registerDoMC(cores = parallel::detectCores() - 2)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

# 
# source("db_functions.R")
metadata = read_rds(file = "../data/US_metadata.rds")
texts = read_rds(file = "../data/US_texts.rds")
decision_addresses = read_rds(file = "../data/US_IDs.rds")


get_urls = function(decision_date = "1.1.1993") {
  
  rD = rsDriver(browser="firefox", port=as.integer(sample(x = 3000:5000, 1)), verbose=F, chromever = NULL)
  remDr = rD[["client"]]
  
  message("Scraping decision addresses")
  remDr$navigate("https://nalus.usoud.cz/Search/Search.aspx")
  remDr$findElement(using = "id", value = "ctl00_MainContent_decidedFrom")$clearElement()
  remDr$findElement(using = "id", value = "ctl00_MainContent_decidedFrom")$sendKeysToElement(list(decision_date))

  remDr$findElements("id", "ctl00_MainContent_but_search")[[1]]$clickElement()
  
  html = remDr$getPageSource()[[1]]
  
  # Get number of pages
  pages = html %>% 
    read_html() %>% 
    html_text() %>% 
    str_extract(pattern = "z celkem [0-9]+") %>% 
    str_extract(pattern = "[0-9]+") %>%
    as.numeric()
  
  numberPages = ceiling(pages/20)
  
  pb = progress_bar$new(
    format = "  scraping addresses [:bar] :percent eta: :eta",
    total = numberPages, clear = FALSE, width= 60)

  decisions_id = foreach(i = seq(numberPages), .combine = "c") %do% {
    
    html = remDr$getPageSource()[[1]]
    output = read_html(html) %>% 
      html_elements(xpath = "//a[contains(@class, 'resultData')]") %>% 
      html_attr('href') %>% 
      paste0("https://nalus.usoud.cz/Search/", .)
    
    remDr$findElements("xpath", "/html/body/form/div[4]/table/tbody/tr[4]/td/table/tbody/tr/td/a[last()]")[[1]]$clickElement()
    
    pb$tick()
    return(output)
    }
  return(decisions_id)
  remDr$close()
  rD[["server"]]$stop()
  
  message("Done with scraping URL addresses.")
}

get_metadata = function(decision_addresses){
  rD = rsDriver(browser="firefox", port=as.integer(sample(x = 3000:5000, 1)), verbose=F, chromever = NULL)
  remDr = rD[["client"]]
  
  message("Scraping metadata")
  
  pb = progress_bar$new(
    format = "scraping metadata [:bar] :percent eta: :eta",
    total = length(decision_addresses), clear = FALSE, width= 60)
  
  metadata = foreach(i = seq_along(decision_addresses), .combine = "bind_rows", .errorhandling = "remove") %do% {
    remDr$navigate(decision_addresses[i] %>% 
      str_extract("id=[0-9]+") %>% 
      paste0("https://nalus.usoud.cz/Search/Karta.aspx?", .))
    html = remDr$getPageSource()[[1]]
    html_metadata = html %>% 
      read_html() %>% 
      html_nodes(".recordCardTable") %>% 
      html_table()
    
    output = html_metadata[[1]]$X2 %>% t() %>% as_tibble()
    colnames(output) = html_metadata[[1]]$X1
    
    pb$tick()
    return(output)
  } %>%
    subset(select = -c(2,4,5,10,24)) %>% 
    rename(
      doc_id = "Identifikátor evropské judikatury",
      case_id = "Spisová značka",
      popular_name = "Populární název",
      date_decision = "Datum rozhodnutí",
      date_submission = "Datum podání",
      date_publication = "Datum vyhlášení",
      type_decision = "Forma rozhodnutí",
      type_proceedings = "Typ řízení",
      importance = "Význam",
      applicant = "Navrhovatel",
      concerned_body = "Dotčený orgán",
      judge_rapporteur_name = "Soudce zpravodaj",
      disputed_act = "Napadený akt",
      type_verdict = "Typ výroku",
      concerned_constitutional_acts = "Dotčené ústavní zákony a mezinárodní smlouvy",
      concerned_acts = "Ostatní dotčené předpisy",
      dissenting_opinion = "Odlišné stanovisko",
      subject_proceedings = "Předmět řízení",
      field_register = "Věcný rejstřík",
      note = "Poznámka",
      url_address = "URL adresa"
    ) %>%
    mutate(across(contains("date"), ~ as.Date(x = ., format = "%d. %m. %Y"))) %>% 
    mutate(formation = case_when(
      grepl(":Pl." , doc_id) ~ "Plenum",
      grepl(":1.US.", doc_id) ~ "First Chamber",
      grepl(":2.US.", doc_id) ~ "Second Chamber",
      grepl(":3.US.", doc_id) ~ "Third Chamber",
      grepl(":4.US.", doc_id) ~ "Fourth Chamber"
    ),
    length_proceeding = interval(date_submission, date_decision) %>% as.numeric('days'),
    outcome = ifelse(grepl("vyhověno", type_verdict), "granted", "rejected"),
    judge_rapporteur_name = paste0(word(judge_rapporteur_name, 2), " ", word(judge_rapporteur_name, 1)),
    doc_id = make.unique(doc_id)) %>%
    remove_rownames() %>%
    mutate(across(everything(), str_squish)) %>%
    mutate(across(everything(), ~replace(., . == "NA", NA))) %>%
    mutate(across(everything(), ~na_if(.,""))) %>%
    left_join(., read_rds(file = "../data/US_judges.rds") %>% select(judge_name, judge_id), by = join_by(judge_rapporteur_name == judge_name)) %>%
    rename(judge_rapporteur_id = judge_id) %>%
    relocate(judge_rapporteur_id, .after = judge_rapporteur_name)
    
    
  remDr$close()
  rD[["server"]]$stop()
  return(metadata)
  message("Done with scraping metadata")
}

get_texts = function(metadata) {
     texts = foreach(html = metadata$url_address, .combine = "bind_rows", .packages = c("rvest", "tidyverse")) %dopar% {
    text = html %>% 
      read_html() %>% 
      html_element(xpath='//td[@class="DocContent"]/table/tr/td') %>% 
      html_text2() %>% 
      utf8::utf8_normalize(map_quote = TRUE)
    output = tibble(
      "url_address" = html,
      "text" = text
    )
    return(output)
  } %>% left_join(metadata %>%
                   select(doc_id, url_address), .) %>%
    select(-url_address) %>%
    distinct() 
  return(texts)
  message("Finished scraping texts")
}


# Function calls
decision_addresses = get_urls(decision_date = as.character(max(US_metadata$date_decision)))
metadata_new = get_metadata(decision_addresses)
texts_new = get_texts(metadata = metadata_new)s

# Bind old and new data
US_metadata = bind_rows(metadata, metadata_new)
US_texts = bind_rows(texts, texts_new)

# Save data
write_rds(decision_addresses, file = "../data/US_IDs.rds")
write_rds(US_metadata, file = "../data/US_metadata.rds")
write_rds(texts, file = "../data/US_texts.rds")

