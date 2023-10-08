library(tidyverse)
library(progress)
library(foreach)

# Parallelise
library(doMC)
registerDoMC(cores = parallel::detectCores() - 2)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

# Load data
source("../supporting_functions.R")
US_texts = readr::read_rds("../data/US_texts.rds")
US_metadata = readr::read_rds("../data/US_metadata.rds")
US_judges = readr::read_rds("../data/US_judges.rds")

# Create function for extracting dissents, returns the long format
get_dissents = function(data, judges_names, judges_id) {
  
  judges_switched = map(.x = judges_names, ~ paste0(word(.x, 2), " ", word(.x, 1)))
  
  data_dissents = foreach(i = seq(data$doc_id), .combine = "bind_rows") %:% 
    foreach(j = seq(judges_switched), .combine = "bind_rows", .packages = c("tidyverse")) %dopar% {
      if (grepl(judges_switched[j], data[i,"dissenting_opinion"], ignore.case = TRUE)) {
        output = tibble("doc_id" = data$doc_id[i],
             "dissenting_judge" = judges_names[j],
              "judge_id" = judges_id[j])
        return(output)
      }
    }
  parallel::stopCluster(cl = my.cluster)
  return(data_dissents)
}

# Run the function and save the data
US_dissents = get_dissents(US_metadata, judges_names = US_judges$judge_name, judges_id = US_judges$judge_id)

# Save data
readr::write_rds(US_dissents, file = "../data/US_dissents.rds")
