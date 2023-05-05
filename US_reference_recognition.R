# Load packages and data
xfun::pkg_attach2("tidyverse", "foreach", "progress", "doParallel", "jsonlite")

# Load data
source("supporting_functions.R")
US_metadata = readRDS("data/US_metadata.rds")
US_texts = readRDS("data/US_texts.rds")
US_citations = readRDS("data/US_citations.rds")

# Save data
saveRDS(US_citations, "data/US_citations.rds")

# The function that finds all references to US caselaw by the case ID (not popular name) and saves it in a matcheable format with the metadata file
find_citations = function(texts, doc_id) {
  
  citation = 'sp\\.\\s*zn\\.\\s*[A-Za-z]{1,2}\\.\\s*ÚS\\s*\\d{1,4}\\/\\d{1,4}'
  
  pb = progress_bar$new(
    format = "  looking for citations [:bar] :percent eta: :eta",
    total = length(doc_id), clear = FALSE, width= 60)
  
  matches = foreach(i = seq(texts), .combine = "rbind") %do% {
    matches = str_extract_all(string = texts[i], pattern = citation)
    output = tibble(
      "doc_id" = doc_id[i],
      "matches" = matches
    )
    pb$tick()
    return(output)
  }
  
  print("Done with the first loop, proceeding onto the second one.")
  
  pb = progress_bar$new(
    format = "  creating the final data frame [:bar] :percent eta: :eta",
    total = length(matches$doc_id), clear = FALSE, width= 60)
  
  US_citations = foreach(i = seq(matches$matches), .combine = "rbind") %do% {
    pb$tick()
    foreach(j = seq(matches$matches[[i]][[1]]), .combine = "rbind") %do%
    {
      output = tibble(
        "doc_id" = as.character(doc_id[i]),
        "matched_case_id" = as.character(matches$matches[[i]][[1]][j]) %>% str_remove(., "sp.\\szn.\\s")
      )
      return(output)
    } 
  }
return(US_citations)
}


# Run the function (on the CC)
US_citations = find_citations(texts = US_texts$text, doc_id = US_texts$doc_id)
NSS_citations = find_citations(texts = NSS_texts$text, doc_id = NSS_texts$doc_id)
US_citations = US_metadata %>% select(doc_id) %>% left_join(., US_citations)

