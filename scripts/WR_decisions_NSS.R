library(httr)
library(tidyverse)
library(rvest)
library(foreach)
library(xml2)
library(doParallel)
library(iterators)
library(RSelenium)
library(progress)

# Parallelise
library(doMC)
registerDoMC(cores = parallel::detectCores() - 2)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

# Functions
get_NSS_ID = function (starting_i = 1, latest_date = "1993-01-01") {
  
  # Get number of decisions
  rD = rsDriver(browser="firefox", port=as.integer(sample(x = 3000:5000, 1)), verbose=F, chromever = NULL,  extraCapabilities=list(acceptInsecureCerts=TRUE,acceptUntrustedCerts=TRUE))
  remDr = rD[["client"]]
  
  message("Scraping decision IDS")
  
  # Find number of pages
  remDr$navigate("https://vyhledavac.nssoud.cz")
  remDr$findElement(using = "id", value = "vyhledavaciSekce_1__vyhledavaciPodminka_0__vyhledavaciPodminkaHodnota_0__HodnotaDatumACasOd")$clearElement()
  remDr$findElement(using = "id", value = "vyhledavaciSekce_1__vyhledavaciPodminka_0__vyhledavaciPodminkaHodnota_0__HodnotaDatumACasOd")$sendKeysToElement(list(latest_date))
  remDr$findElement(using = "id", value = "vyhledavaciSekce_0__vyhledavaciPodminka_0__vyhledavaciPodminkaHodnota_0__HodnotaCiselnikPolozkyArrowBtn")$clickElement()
  remDr$findElement(using = "xpath", value = "/html/body/div/div/div/form/fieldset[1]/div[2]/div[1]/div/div/div/div/div/div[1]/div/div/ul/li[2]/span[2]/input")$clickElement()
  remDr$findElement(using = "name", value = "btSubmit")$clickElement()
  Sys.sleep(10)
  number_decisions = remDr$getPageSource()[[1]] %>% 
    read_html() %>%
    html_text() %>%
    str_extract("Počet nalezených záznamů: [0-9]+") %>%
    str_extract("\\d+") %>%
    as.numeric()
  number_pg = ceiling(number_decisions/20)
  remDr$close()
  rD[["server"]]$stop()
  
  # Reads the POST request file: at this point, I think going RSelenium way would've been more efficient but at least I tried to do it via HTTP request :)
  request_to_replace = readLines("web_scraping/Request.txt") %>% 
    str_replace(pattern = "1993-01-01", replace = latest_date)
  
  message("Succcesfully finished parallelization")
  
  NSS_IDs = foreach(i = starting_i:number_pg, .combine = "c", .packages = c("foreach", "tidyverse", "httr")) %dopar% {
    
    # Replacement of page nr. for the request
    request_replaced = str_replace(
      string = request_to_replace,
      pattern = "pageNum=1",
      replace = paste0("pageNum=", i)
    )
    
    writeLines(text = request_replaced, con = paste0("Request_temp",i,".txt"))
    filetoupload_temp = upload_file(paste0("Request_temp",i,".txt"))
    
    # HTTP post to the server with i requests
    POST_req = POST (
      url = "https://vyhledavac.nssoud.cz/Home/MyResTRowsCont",
      config = list(),
      body = filetoupload_temp,
      handle = NULL,
      content_type("application/x-www-form-urlencoded; charset=UTF-8")
    )
    
    # Removes the temporary request file
    file.remove(paste0("Request_temp",i,".txt"))
    post_cont = content(POST_req, "text")
    
    # Concantation to global variable id_ex
    NSS_ID = unique(unlist(str_extract_all(post_cont, '/DokumentDetail/Index/[0-9]+'))) %>% str_remove("/DokumentDetail/Index/")
    return(NSS_ID)
  }
  return(NSS_IDs)
}

get_NSS_metadata = function (NSS_IDs) {
  NSS_metadata = foreach(NSS_ID = NSS_IDs, .combine = rbind, .packages = c("httr", "rvest", "foreach", "tidyverse", "lubridate", "xml2")) %dopar% {
    
    html = paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
      read_html()
    
    doc_id = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
    if (length(doc_id) == 0) {
      doc_id = NA
    } else {doc_id = html_text(doc_id)}
    
    case_id = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Číslo jednací')]/../*[@class='det-textval']")
    if (length(case_id) == 0) {
      case_id = NA
    } else {case_id = html_text(case_id)}
    
    proceeding_id = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Spisová značka')]/../*[@class='det-textval']")
    if (length(proceeding_id) == 0) {
      proceeding_id = NA
    } else {proceeding_id = html_text(proceeding_id)}
    
    formation = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Soud (senát)')]/../*[@class='det-textval']")
    if (length(formation) == 0) {
      formation = NA
    } else {formation = html_text(formation)}
    
    date_submission = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Datum zahájení')]/../*[@class='det-textval']")
    if (length(date_submission) == 0) {
      date_submission = NA
    } else {date_submission = html_text(date_submission)}
    
    date_decision = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Datum skončení')]/../*[@class='det-textval']")
    if (length(date_decision) == 0) {
      date_decision = NA
    } else {date_decision = html_text(date_decision)}
    
    judge_rapporteur = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Soudce zpravodaj')]/../*[@class='det-textval']")
    if (length(judge_rapporteur) == 0) {
      judge_rapporteur = NA
    } else {judge_rapporteur = list(html_text(judge_rapporteur) %>% str_squish())}
    
    subject_matter = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Oblast úpravy')]/../*[@class='det-textval']")
    if (length(subject_matter) == 0) {
      subject_matter = NA
    } else {subject_matter = html_text(subject_matter)}
    
    type_verdict = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Výrok rozhodnutí NSS')]/../*[@class='det-textval']")
    if (length(type_verdict) == 0) {
      type_verdict = NA
    } else {type_verdict = html_text(type_verdict)}
    
    type_decision = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Druh dokumentu')]/../*[@class='det-textval']")
    if (length(type_decision) == 0) {
      type_decision = NA
    } else {type_decision = html_text(type_decision)}
    
    type_proceeding = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Typ řízení')]/../*[@class='det-textval']")
    if (length(type_proceeding) == 0) {
      type_proceeding = NA
    } else {type_proceeding = html_text(type_proceeding)}
    
    type_decision_relationship = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Rozhodnutí ve vztahu k řízení')]/../*[@class='det-textval']")
    if (length(type_decision_relationship) == 0) {
      type_decision_relationship = NA
    } else {type_decision_relationship = html_text(type_decision_relationship)}
    
    lawyer = html_elements(html, xpath ="//*[@class='det-textitle' and @title='Zástupce (zastupce)']/../*[@class='det-textval']")
    if (length(lawyer) == 0) {
      lawyer = NA
    } else {lawyer = list(html_text(lawyer) %>% str_remove(., " hlavní subjekt.*") %>% str_squish())}
    
    administrative_authority = xml_find_first(html, xpath ="//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[1]")
    if (html_text(administrative_authority) == "") {
      administrative_authority = NA
    } else {administrative_authority = html_text(administrative_authority)}
    
    date_contested_decision = xml_find_first(html, xpath ="//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[3]")
    if (html_text(date_contested_decision) == "") {
      date_contested_decision = NA
    } else {date_contested_decision = html_text(date_contested_decision)}
    
    regional_court_ID = html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[1]")
    if (length(regional_court_ID) == 0) {
      regional_court_ID = NA
    } else {regional_court_ID = html_text(regional_court_ID)}
    
    regional_court_case_ID = html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[2]")
    if (length(regional_court_case_ID) == 0) {
      regional_court_case_ID = NA
    } else {regional_court_case_ID = html_text(regional_court_case_ID)}
    
    regional_court_date_decision = html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[8]")
    if (length(regional_court_date_decision) == 0) {
      regional_court_date_decision = NA
    } else {regional_court_date_decision = html_text(regional_court_date_decision)}
    
    output = tibble(
      "doc_id" = doc_id,
      "case_id" = case_id,
      "proceeding_id" = proceeding_id,
      "formation" = formation,
      "date_submission" = date_submission,
      "date_decision" = date_decision,
      "judge_rapporteur_name" = judge_rapporteur,
      "subject_matter" = subject_matter,
      "type_verdict" = type_verdict,
      "type_decision" = type_decision,
      "type_proceeding" = type_proceeding,
      "lawyer" = lawyer,
      "type_decision_relationship" = type_decision_relationship,
      "administrative_authority" = administrative_authority,
      "date_contested_decision" = date_contested_decision,
      "regional_court_ID" = regional_court_ID,
      "regional_court_case_ID" = regional_court_case_ID,
      "regional_court_date_decision" = regional_court_date_decision,
      "url_address" = NSS_ID
    )
    return(output)
  } %>%
    mutate(across(where(is.character), str_squish)) %>%
    mutate(across(where(is.character), ~replace(., . == "NA", NA))) %>%
    distinct() %>% 
    mutate(across(contains("date"), ~as_date(x = ., format = "%d.%m.%Y"))) %>%
    remove_rownames() %>%
    nest(regional_court_case = starts_with("regional_court"))
  # Remove unnecessary whitespaces
  return(NSS_metadata)
}

get_NSS_texts = function (NSS_IDs) {
  NSS_texts = foreach(NSS_ID = NSS_IDs, .combine = "rbind", .packages = c("httr", "rvest", "foreach", "tidyverse", "lubridate", "xml2")) %dopar% {
    html_id = paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
      read_html()
    
    # HTML_text
    html_texts = paste0("https://vyhledavac.nssoud.cz/DokumentOriginal/Html/", NSS_ID) %>% 
      read_html()
    
    doc_id = html_elements(html_id, xpath = "//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
    if (length(doc_id) == 0) {
      doc_id = NA
    } else {doc_id = html_text(doc_id)}
    
    text = tryCatch(
      expr = {
        html_texts %>% 
          html_text2() %>% 
          str_squish()
      },
      error = function(e){
        return(NA)
      })
    
    
    output = tibble(
      "doc_id" = doc_id,
      "text" = text,
      "url_address" = NSS_ID
    )
    
    return(output)
  } %>%
    mutate(across(where(is.character), str_squish)) %>%
    mutate(across(where(is.character), ~replace(., . == "NA", NA))) %>%
  return(NSS_texts)
}

get_applied_judgments = function(NSS_IDs) {
  # rvest loop
  NSS_applied_judgments = foreach(NSS_ID = NSS_IDs, .combine = "rbind", .packages = c("httr", "rvest", "foreach", "tidyverse", "xml2")) %dopar% {
    
    html = paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
      read_html()
    
    length_applied_judgments = length(html_elements(html, xpath = "//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr"))
    
    doc_id = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
    
    if (length(doc_id) == 0) {
      doc_id = NA
    } else {doc_id = html_text(doc_id)}
    
    if(length_applied_judgments == 0) {
      return(NULL)
    } else {
      applied_judgments_result = foreach(i = seq(length_applied_judgments), .combine = "rbind") %do% {
        inner_length = length(html_elements(html, xpath = "//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr[1]/td"))
        output = tibble(
          "doc_id" = doc_id,
          "applied_judgment_case_id" = html_elements(html, xpath = paste0("//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr[",i,"]/td[1]")) %>% html_text2(),
          "applied_judgment_authority" = html_elements(html, xpath = paste0("//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr[",i,"]/td[", inner_length-2,"]")) %>% html_text2(),
          "applied_judgment_relationship" = html_elements(html, xpath = paste0("//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr[",i,"]/td[", inner_length-1,"]")) %>% html_text2(),
          "applied_judgment_conflict" = html_elements(html, xpath = paste0("//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr[",i,"]/td[", inner_length, "]")) %>% html_text2()
        )
        return(output)
      }
      return(applied_judgments_result)
    }
  }
  return(NSS_applied_judgments)
}

get_applied_laws = function(NSS_IDs){
  # rvest loop
  NSS_applied_laws = foreach(NSS_ID = NSS_IDs, .combine = "rbind", .packages = c("httr", "rvest", "foreach", "tidyverse", "xml2")) %dopar% {
    
    html = paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
      read_html()
    
    length_applied_laws = length(html_elements(html, xpath = "//td[@title = 'aplikovanepravnipredpisysbcl']/../../../../table/tbody/tr"))
    
    doc_id = html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']") %>% html_text()
    
    if (length(doc_id) == 0) {
      doc_id = NA
    }
    
    if(length_applied_laws == 0) {
      return(NULL)
    } else {
      applied_laws_result = foreach(i = seq(length_applied_laws), .combine = "rbind") %do% {
        output = tibble(
          "doc_id" = doc_id,
          "applied_law_§" = html_elements(html, xpath = paste0("//td[@title = 'aplikovanepravnipredpisysbcl']/../../../../table/tbody/tr[",i,"]/td[2]")) %>% html_text2(),
          "applied_law_para" = html_elements(html, xpath = paste0("//td[@title = 'aplikovanepravnipredpisysbcl']/../../../../table/tbody/tr[",i,"]/td[3]")) %>% html_text2(),
          "applied_law_letter" = html_elements(html, xpath = paste0("//td[@title = 'aplikovanepravnipredpisysbcl']/../../../../table/tbody/tr[",i,"]/td[4]")) %>% html_text2(),
          "applied_law_type" = html_elements(html, xpath = paste0("//td[@title = 'aplikovanepravnipredpisysbcl']/../../../../table/tbody/tr[",i,"]/td[5]")) %>% html_text2(),
          "applied_law_nr" = html_elements(html, xpath = paste0("//td[@title = 'aplikovanepravnipredpisysbcl']/../../../../table/tbody/tr[",i,"]/td[6]")) %>% html_text2(),
          "applied_law_year" = html_elements(html, xpath = paste0("//td[@title = 'aplikovanepravnipredpisysbcl']/../../../../table/tbody/tr[",i,"]/td[7]")) %>% html_text2()
        )
        return(output)
      }
      return(applied_laws_result)
    }
  }
  # Stop the parallel process
  parallel::stopCluster(cl = my.cluster)
  return(NSS_applied_laws)
}


  







