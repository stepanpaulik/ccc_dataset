# Load packages and data
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)

# Parallelise
library(doMC)
registerDoMC(cores = parallel::detectCores() - 2)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

# Load data
US_texts = readRDS("../data/US_texts.rds")
# US_citations = readRDS("../data/US_citations.rds")

# The function that finds all references to US caselaw by the case ID (not popular name) and saves it in a matcheable format with the metadata file
find_citations = function(texts, doc_id) {

    US_citations = foreach(i = seq_along(texts), .combine = "rbind") %do% {
    
    matches = str_extract_all(string = texts[i], pattern = 'sp\\.\\s*zn\\.\\s*[A-Za-z]{1,2}\\.\\s*ÚS\\s*\\d{1,4}\\/\\d{1,4}')
    
    if(is.null(matches)){
      return(NULL)
    } else {
      citation = foreach(j = seq_along(matches), .combine = "rbind") %do% {
        output = tibble(
          "doc_id" = doc_id[[i]],
          "matched_case_id" = matches[[j]])
        return(output)
      }
      return(citation)
    }
  }
  return(US_citations)
}


# Run the function (on the CC)
US_citations = find_citations(texts = US_texts$text, doc_id = US_texts$doc_id)
NSS_citations = find_citations(texts = NSS_texts$text, doc_id = NSS_texts$doc_id)

# Save data
saveRDS(US_citations, "../data/US_citations.rds")

