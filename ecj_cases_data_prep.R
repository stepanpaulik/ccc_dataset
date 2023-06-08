xfun::pkg_attach2("textrank", "wordcloud", "ggplot2", "tidyverse", "RMySQL", "lubridate", "tm")
data_matches <- read.csv("data/matches_output.csv")
data_metadata <- read.csv("data/decisions_US_metadata.csv")
ecj_judgments <- read.csv("data/CJEU-database-platform-csv-v-0-1/CJEU-database-platform-judgments-v-0-1.csv")


# ECJ cases_name
ecj_judgments <- read.csv("data/CJEU-database-platform-csv-v-0-1/CJEU-database-platform-cases-v-0-1.csv") %>% select(cjeu_case_id,case_name) %>% rename(cjeu_proceeding_id = cjeu_case_id) %>% left_join(ecj_judgments, .)

data_matches <- data_matches %>% rename(
  doc_id = source_case_id,
  match_type = m_type,
  cjeu_proceeding_id = matched_case_code
)

matches_joined <- left_join(data_matches, ecj_judgments)
matches_joined <- left_join(matches_joined, data_metadata, by = "doc_id")

# Get years
matches_joined$year_cc <- year(matches_joined$date_decision)

# Create IDs without ECLI 
# generate_ID <- function(data_metadata) {
#   data_metadata <- arrange(data_metadata, spisova_znacka)
#   data_metadata$unique_id <- 1
#   
#   for (i in 2:length(data_metadata$spisova_znacka)){
#     if(data_metadata$spisova_znacka[i-1] == data_metadata$spisova_znacka[i]) {
#       data_metadata$unique_id[i] <- data_metadata$unique_id[i-1]+1 
#     } else {
#       data_metadata$unique_id[i] <- 1
#     }
#   
#     for (i in 1:length(data_metadata$spisova_znacka)){
#     case_id <- data_metadata$spisova_znacka[i] %>% str_extract("\\d+\\/\\d+") %>% str_replace("/", ":")
#     senate_id <- data_metadata$spisova_znacka[i] %>% str_extract("^[A-Z]{1,2}")
#     data_metadata$doc_ID2[i] <- paste0("US:", senate_id, ":", case_id,":", data_metadata$unique_id)
#   }
#   return(data_metadata)
# }
# }

# Get outcome
add_outcome <- function(data_metadata, outcome = "vyhověno") {
  data_metadata$outcome <- ifelse(grepl(outcome, data_metadata$type_verdict), "granted", "rejected")
  return(data_metadata)
}
matches_joined <- add_outcome(matches_joined)

# Regex check
matches_check <- matches_joined %>% filter(
  year_cc < 2004
) %>% subset(select = c("case_id", "doc_id", "cjeu_proceeding_id"))
write.csv(matches_check, "data/matches_check.csv", row.names = FALSE)

write.csv(matches_joined, "data/matches_joined.csv", row.names = FALSE)
