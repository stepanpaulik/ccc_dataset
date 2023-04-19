xfun::pkg_attach2("httr", "rvest", "progress", "rapportools", "foreach", "tidyverse", "lubridate", "xml2", "doParallel", "iterators")

# Load the data
source("supporting_functions.R")
load("data/NSS_IDs.RData")
load("data/NSS_metadata.RData")
load("data/NSS_texts.RData")
load(file = "data/NSS_applied_judgments.RData")

# Save the data
# save(NSS_IDs, file = "data/NSS_IDs.RData")
# save(NSS_metadata, file = "data/NSS_metadata.RData")
# save(NSS_texts, file = "data/NSS_texts.RData")
save(NSS_applied_judgments, file = "data/NSS_applied_judgments.RData")


yeet <- NSS_applied_judgments
NSS_applied_judgments <- NSS_applied_judgments_WIP

# Functions
get_NSS_ID <- function (starting_i = 1, number_decisions = 68658) {
  
  # Sets the upper cap for the foreach loop
  number_pg <- ceiling(number_decisions/20)
  
  pb <- progress_bar$new(
  format = "  downloading NSS IDs [:bar] :current/:total :percent eta: :eta",
  total = number_pg, clear = FALSE, width = 80)
  
  # Reads the POST request file: at this point, I think going RSelenium way would've been more efficient but at least I tried to do it via HTTP request :)
  request_to_replace <- readLines("web_scraping/Request.txt")
  # The loop
  NSS_IDs <- foreach(i = starting_i:number_pg, .combine = "c") %do% {
    
    # Replacement of page nr. for the request
    request_replaced <- str_replace (
      string = request_to_replace,
      pattern = "pageNum=1",
      replace = paste0("pageNum=", i)
    )
    
    writeLines(text = request_replaced, con = paste0("Request_temp.txt"))
    filetoupload_temp <- upload_file(paste0("Request_temp.txt"))
    
    # HTTP post to the server with i requests
    POST_req <- POST (
      url = "https://vyhledavac.nssoud.cz/Home/MyResTRowsCont",
      config = list(),
      body = filetoupload_temp,
      handle = NULL,
      content_type("application/x-www-form-urlencoded; charset=UTF-8")
    )
    
    # Removes the temporary request file
    file.remove("Request_temp.txt")
    post_cont <- content(POST_req, "text")
    
    # Concantation to global variable id_ex
    NSS_ID <- unique(unlist(str_extract_all(post_cont, '/DokumentDetail/Index/[0-9]{4,7}'))) %>% str_remove("/DokumentDetail/Index/")
    
    pb$tick()
    message(i)
    return(NSS_ID)
    Sys.sleep(1 / 100)
  }
  return(NSS_IDs)
}

get_NSS_metadata <- function (NSS_IDs, par = FALSE) {
  
  if (par == FALSE) {
    pb <- progress_bar$new(
      format = "  downloading NSS metadata [:bar] :current/:total :percent eta: :eta",
      total = length(NSS_IDs), clear = FALSE, width = 80)
    
    NSS_metadata <- foreach(NSS_ID = NSS_IDs, .combine = "rbind") %do% {
      
      html <- paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
        read_html()
      
      doc_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
      if (length(doc_id) == 0) {
        doc_id <- NA
      } else {doc_id <- html_text(doc_id)}
      
      case_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Číslo jednací')]/../*[@class='det-textval']")
      if (length(case_id) == 0) {
        case_id <- NA
      } else {case_id <- html_text(case_id)}
      
      proceeding_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Spisová značka')]/../*[@class='det-textval']")
      if (length(proceeding_id) == 0) {
        proceeding_id <- NA
      } else {proceeding_id <- html_text(proceeding_id)}
      
      formation <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Soud (senát)')]/../*[@class='det-textval']")
      if (length(formation) == 0) {
        formation <- NA
      } else {formation <- html_text(formation)}
      
      date_submission <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Datum zahájení')]/../*[@class='det-textval']")
      if (length(date_submission) == 0) {
        date_submission <- NA
      } else {date_submission <- html_text(date_submission)}
      
      date_decision <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Datum skončení')]/../*[@class='det-textval']")
      if (length(date_decision) == 0) {
        date_decision <- NA
      } else {date_decision <- html_text(date_decision)}
      
      judge_rapporteur <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Soudce zpravodaj')]/../*[@class='det-textval']")
      if (length(judge_rapporteur) == 0) {
        judge_rapporteur <- NA
      } else {judge_rapporteur <- html_text(judge_rapporteur)}
      
      subject_matter <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Oblast úpravy')]/../*[@class='det-textval']")
      if (length(subject_matter) == 0) {
        subject_matter <- NA
      } else {subject_matter <- html_text(subject_matter)}
      
      type_verdict <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Výrok rozhodnutí NSS')]/../*[@class='det-textval']")
      if (length(type_verdict) == 0) {
        type_verdict <- NA
      } else {type_verdict <- html_text(type_verdict)}
      
      type_decision <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Druh dokumentu')]/../*[@class='det-textval']")
      if (length(type_decision) == 0) {
        type_decision <- NA
      } else {type_decision <- html_text(type_decision)}
      
      type_proceeding <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Typ řízení')]/../*[@class='det-textval']")
      if (length(type_proceeding) == 0) {
        type_proceeding <- NA
      } else {type_proceeding <- html_text(type_proceeding)}
      
      type_decision_relationship <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Rozhodnutí ve vztahu k řízení')]/../*[@class='det-textval']")
      if (length(type_decision_relationship) == 0) {
        type_decision_relationship <- NA
      } else {type_decision_relationship <- html_text(type_decision_relationship)}
      
      lawyer <- html_elements(html, xpath ="//*[@class='det-textitle' and @title='Zástupce (zastupce)']/../*[@class='det-textval']")
      if (length(lawyer) == 0) {
        lawyer <- NA
      } else {lawyer <- html_text(lawyer) %>% str_remove(., " hlavní subjekt.*")}
      
      administrative_authority <- xml_find_first(html, xpath ="//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[1]")
      if (html_text(administrative_authority) == "") {
        administrative_authority <- NA
      } else {administrative_authority <- html_text(administrative_authority)}
      
      date_contested_decision <- xml_find_first(html, xpath ="//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[3]")
      if (html_text(date_contested_decision) == "") {
        date_contested_decision <- NA
      } else {date_contested_decision <- html_text(date_contested_decision)}
      
      regional_court_ID <- html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[1]")
      if (length(regional_court_ID) == 0) {
        regional_court_ID <- NA
      } else {regional_court_ID <- html_text(regional_court_ID)}
      
      regional_court_case_ID <- html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[2]")
      if (length(regional_court_case_ID) == 0) {
        regional_court_case_ID <- NA
      } else {regional_court_case_ID <- html_text(regional_court_case_ID)}
      
      regional_court_date_decision <- html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[8]")
      if (length(regional_court_date_decision) == 0) {
        regional_court_date_decision <- NA
      } else {regional_court_date_decision <- html_text(regional_court_date_decision)}
      
      output <- list(
        "doc_id" = doc_id,
        "case_id" = case_id,
        "proceeding_id" = proceeding_id,
        "formation" = formation,
        "date_submission" = date_submission,
        "date_decision" = date_decision,
        "judge_rapporteur" = judge_rapporteur,
        "subject_matter" = subject_matter,
        "type_verdict" = type_verdict,
        "type_decision" = type_decision,
        "type_proceeding" = type_proceeding,
        "type_decision_relationship" = type_decision_relationship,
        "lawyer" = lawyer,
        "administrative_authority" = administrative_authority,
        "date_contested_decision" = date_contested_decision,
        "regional_court_ID" = regional_court_ID,
        "regional_court_case_ID" = regional_court_case_ID,
        "regional_court_date_decision" = regional_court_date_decision
      )
      
      pb$tick()
      return(output)
      Sys.sleep(1 / 100)
    } %>% as.data.frame(row.names = FALSE) %>% column_as_Date()
    return(NSS_metadata)  
  } else {
    # Start the parallelezatin process
    n.cores <- parallel::detectCores() - 1
    my.cluster <- parallel::makeCluster(
      n.cores, 
      type = "PSOCK"
    )
    
    #register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my.cluster)
    
    #check if it is registered (optional)
    foreach::getDoParRegistered()
    foreach::getDoParWorkers()
    
    message("Succcesfully finished parallelization")
    
    NSS_metadata <- foreach(NSS_ID = NSS_IDs, .combine = "rbind", .packages = c("httr", "rvest", "rapportools", "foreach", "tidyverse", "lubridate", "xml2")) %dopar% {
      
      html <- paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
        read_html()
      
      doc_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
      if (length(doc_id) == 0) {
        doc_id <- NA
      } else {doc_id <- html_text(doc_id)}
      
      case_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Číslo jednací')]/../*[@class='det-textval']")
      if (length(case_id) == 0) {
        case_id <- NA
      } else {case_id <- html_text(case_id)}
      
      proceeding_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Spisová značka')]/../*[@class='det-textval']")
      if (length(proceeding_id) == 0) {
        proceeding_id <- NA
      } else {proceeding_id <- html_text(proceeding_id)}
      
      formation <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Soud (senát)')]/../*[@class='det-textval']")
      if (length(formation) == 0) {
        formation <- NA
      } else {formation <- html_text(formation)}
      
      date_submission <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Datum zahájení')]/../*[@class='det-textval']")
      if (length(date_submission) == 0) {
        date_submission <- NA
      } else {date_submission <- html_text(date_submission)}
      
      date_decision <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Datum skončení')]/../*[@class='det-textval']")
      if (length(date_decision) == 0) {
        date_decision <- NA
      } else {date_decision <- html_text(date_decision)}
      
      judge_rapporteur <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Soudce zpravodaj')]/../*[@class='det-textval']")
      if (length(judge_rapporteur) == 0) {
        judge_rapporteur <- NA
      } else {judge_rapporteur <- html_text(judge_rapporteur)}
      
      subject_matter <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Oblast úpravy')]/../*[@class='det-textval']")
      if (length(subject_matter) == 0) {
        subject_matter <- NA
      } else {subject_matter <- html_text(subject_matter)}
      
      type_verdict <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Výrok rozhodnutí NSS')]/../*[@class='det-textval']")
      if (length(type_verdict) == 0) {
        type_verdict <- NA
      } else {type_verdict <- html_text(type_verdict)}
      
      type_decision <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Druh dokumentu')]/../*[@class='det-textval']")
      if (length(type_decision) == 0) {
        type_decision <- NA
      } else {type_decision <- html_text(type_decision)}
      
      type_proceeding <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Typ řízení')]/../*[@class='det-textval']")
      if (length(type_proceeding) == 0) {
        type_proceeding <- NA
      } else {type_proceeding <- html_text(type_proceeding)}
      
      type_decision_relationship <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'Rozhodnutí ve vztahu k řízení')]/../*[@class='det-textval']")
      if (length(type_decision_relationship) == 0) {
        type_decision_relationship <- NA
      } else {type_decision_relationship <- html_text(type_decision_relationship)}
      
      lawyer <- html_elements(html, xpath ="//*[@class='det-textitle' and @title='Zástupce (zastupce)']/../*[@class='det-textval']")
      if (length(lawyer) == 0) {
        lawyer <- NA
      } else {lawyer <- html_text(lawyer) %>% str_remove(., " hlavní subjekt.*")}
      
      administrative_authority <- xml_find_first(html, xpath ="//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[1]")
      if (html_text(administrative_authority) == "") {
        administrative_authority <- NA
      } else {administrative_authority <- html_text(administrative_authority)}
      
      date_contested_decision <- xml_find_first(html, xpath ="//*[@title='nazevspravnihoorganu']/../../../tbody/tr/td[3]")
      if (html_text(date_contested_decision) == "") {
        date_contested_decision <- NA
      } else {date_contested_decision <- html_text(date_contested_decision)}
      
      regional_court_ID <- html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[1]")
      if (length(regional_court_ID) == 0) {
        regional_court_ID <- NA
      } else {regional_court_ID <- html_text(regional_court_ID)}
      
      regional_court_case_ID <- html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[2]")
      if (length(regional_court_case_ID) == 0) {
        regional_court_case_ID <- NA
      } else {regional_court_case_ID <- html_text(regional_court_case_ID)}
      
      regional_court_date_decision <- html_elements(html, xpath ="//*[@title='krajskysoud']/../../../tbody/tr/td[8]")
      if (length(regional_court_date_decision) == 0) {
        regional_court_date_decision <- NA
      } else {regional_court_date_decision <- html_text(regional_court_date_decision)}
      
      # applied_laws <- html_elements(html, xpath ="//*[@title='aplikovanepravnipredpisysbcl']/../../../../table")
      # if (length(applied_laws) == 0) {
      #   applied_laws <- NA
      # } else {applied_laws <- html_table(applied_laws) %>% as.data.frame()}
      
      output <- list(
        "doc_id" = doc_id,
        "case_id" = case_id,
        "proceeding_id" = proceeding_id,
        "formation" = formation,
        "date_submission" = date_submission,
        "date_decision" = date_decision,
        "judge_rapporteur" = judge_rapporteur,
        "subject_matter" = subject_matter,
        "type_verdict" = type_verdict,
        "type_decision" = type_decision,
        "type_proceeding" = type_proceeding,
        "type_decision_relationship" = type_decision_relationship,
        "lawyer" = lawyer,
        "administrative_authority" = administrative_authority,
        "date_contested_decision" = date_contested_decision,
        "regional_court_ID" = regional_court_ID,
        "regional_court_case_ID" = regional_court_case_ID,
        "regional_court_date_decision" = regional_court_date_decision
      )
      message(NSS_ID)
      return(output)
    } 
    # Stop the parallel process
    parallel::stopCluster(cl = my.cluster)
    
    # Remove unnecessary whitespaces
    return(NSS_metadata)
  }
  for(i in seq(ncol(NSS_metadata))) {
    NSS_metadata[,i] <- NSS_metadata[,i] %>% str_squish() %>% na_if(., "NA")
  }
  return(NSS_metadata)
}

get_NSS_texts <- function (NSS_IDs) {
  
  pb <- progress_bar$new(
    format = "  downloading [:bar] :current/:total :percent eta: :eta",
    total = length(NSS_IDs), clear = FALSE, width = 80)
  
  NSS_texts <- foreach(NSS_ID = NSS_IDs, .combine = "rbind") %do% {
    # HTML_id
    html_id <- paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
      read_html()
    
    # HTML_text
    html_texts <- paste0("https://vyhledavac.nssoud.cz/DokumentOriginal/Html/", NSS_ID) %>% 
      read_html()
    
    doc_id <- html_elements(html_id, xpath = "//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
      if (length(doc_id) == 0) {
      doc_id <- NA
      } else {doc_id <- html_text(doc_id)}
    
    text <- html_texts %>% html_text2() %>% str_squish() %>% na_if(., "NA")
    
    output <- tibble(
      "doc_id" = doc_id,
      "text" = text
    )
    
    pb$tick()
    return(output)
    Sys.sleep(1 / 100)
  }
  return(NSS_texts)
}

get_applied_judgments <- function(NSS_IDs, par = FALSE) {
  if (par == FALSE) {
    
    pb <- progress_bar$new(
      format = "  downloading [:bar] :current/:total :percent eta: :eta",
      total = length(NSS_IDs), clear = FALSE, width = 80)
    
    NSS_applied_judgments <- foreach(NSS_ID = NSS_IDs, .combine = "rbind") %do% {
        pb$tick()
      
        html <- paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
          read_html()
        
        length_applied_judgments <- length(html_elements(html, xpath = "//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr"))
        
        doc_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
        
        if (length(doc_id) == 0) {
          doc_id <- NA
        } else {doc_id <- html_text(doc_id)}
        
        if(length_applied_judgments == 0) {
          return(NULL)
        } else {
          applied_judgments_result <- foreach(i = seq(length_applied_judgments), .combine = "rbind") %do% {
            inner_length <- length(html_elements(html, xpath = "//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr[1]/td"))
            output <- tibble(
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
  } else {
    # Start the parallelezatin process
    n.cores <- parallel::detectCores() - 1
    my.cluster <- parallel::makeCluster(
      n.cores, 
      type = "PSOCK"
    )
    
    #register it to be used by %dopar%
    doParallel::registerDoParallel(cl = my.cluster)
    
    #check if it is registered (optional)
    foreach::getDoParRegistered()
    foreach::getDoParWorkers()
    
    message("Succcesfully finished parallelization")
    
    # rvest loop
    NSS_applied_judgments <- foreach(NSS_ID = NSS_IDs, .combine = "rbind", .packages = c("httr", "rvest", "rapportools", "foreach", "tidyverse", "lubridate", "xml2")) %dopar% {
  
        html <- paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
          read_html()
        
        length_applied_judgments <- length(html_elements(html, xpath = "//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr"))
        
        doc_id <- html_elements(html, xpath ="//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
        
        if (length(doc_id) == 0) {
          doc_id <- NA
        } else {doc_id <- html_text(doc_id)}
        
        if(length_applied_judgments == 0) {
          return(NULL)
        } else {
          applied_judgments_result <- foreach(i = seq(length_applied_judgments), .combine = "rbind") %do% {
            inner_length <- length(html_elements(html, xpath = "//td[@title = 'prejudikaturaoznacenivecivcelku']/../../../../table/tbody/tr[1]/td"))
            output <- tibble(
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
    
    # Stop the parallel process
    parallel::stopCluster(cl = my.cluster)
    }
  return(NSS_applied_judgments)
}


# Function calls
NSS_IDs <- get_NSS_ID(starting_i = 1, number_decisions = 70442)
NSS_metadata <- get_NSS_metadata(NSS_IDs = NSS_IDs)
NSS_texts <- get_NSS_texts(NSS_IDs = NSS_IDs)



get_NSS_texts_par <- function (NSS_IDs) {
  
  # Start the parallelezatin process
  n.cores <- parallel::detectCores() - 1
  my.cluster <- parallel::makeCluster(
    n.cores, 
    type = "PSOCK"
  )
  
  #register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster)
  
  #check if it is registered (optional)
  foreach::getDoParRegistered()
  foreach::getDoParWorkers()
  
  message("Succcesfully finished parallelization")
  
  NSS_texts <- foreach(NSS_ID = NSS_IDs, .combine = "rbind", .packages = c("httr", "rvest", "rapportools", "foreach", "tidyverse", "lubridate", "xml2")) %dopar% {
    # HTML_id
    html_id <- paste0("https://vyhledavac.nssoud.cz/DokumentDetail/Index/", NSS_ID) %>% 
      read_html()
    
    # HTML_text
    html_texts <- paste0("https://vyhledavac.nssoud.cz/DokumentOriginal/Html/", NSS_ID) %>% 
      read_html()
    
    doc_id <- html_elements(html_id, xpath = "//*[@class='det-textitle' and contains(@title, 'ECLI (ecli)')]/../*[@class='det-textval']")
    if (length(doc_id) == 0) {
      doc_id <- NA
    } else {doc_id <- html_text(doc_id)}
    
    text <- html_texts %>% html_text2() %>% str_squish() %>% na_if(., "NA")
    
    output <- tibble(
      "doc_id" = doc_id,
      "text" = text
    )
    
    return(output)
    Sys.sleep(1 / 100)
  }
  parallel::stopCluster(cl = my.cluster)
  return(NSS_texts)
}

