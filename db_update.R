library(tidyverse)

source("db_functions.R")

# US
US_metadata = readRDS("../data/US_metadata.rds")
source("web_scraping/WR_decisions_US.R")
decision_date = max(US_metadata$date_decision) %>% format("%d.%m.%Y")



# Run the update functions

# Load data
US_metadata = readRDS("../data/US_metadata.rds")
US_texts = readRDS("../data/US_texts.rds")
US_compositions = readRDS("../data/US_compositions.rds")
US_judges = readRDS("../data/US_judges.rds")
US_dissents = readRDS("../data/US_dissents.rds")
US_citations = readRDS("../data/US_citations.rds")


# Write the dataframes as tables in MySQL database
US_judges %>% writeNewDb(data = ., table = "US_judges")
US_metadata %>% writeNewDb(data = ., table = "US_metadata")
US_texts %>% writeNewDb(data = ., table = "US_texts")
US_compositions %>% writeNewDb(data = ., table = "US_compositions")
US_dissents %>% writeNewDb(data = ., table = "US_dissents")
US_citations %>% writeNewDb(data = ., table = "US_citations")



# Fit in the current data
# Clean useless columns and rename

# NSS
# Load the data
NSS_IDs = readr::read_rds("../data/NSS_IDs.rds")
NSS_metadata = readr::read_rds("../data/NSS_metadata.rds")
NSS_texts = readr::read_rds("../data/NSS_texts.rds")
NSS_applied_judgments = readr::read_rds("../data/NSS_applied_judgments.rds")
NSS_applied_laws = readr::read_rds("../data/NSS_applied_laws.rds")
NSS_judges = readr::read_rds("../data/NSS_judges.rds")

dbWriteTable(conn = con, value = NSS_texts, name = "NSS_texts", field.types = c(doc_id = "varchar(255)", text = "LONGTEXT"), overwrite=TRUE)
writeNewDb(NSS_applied_laws, table = "NSS_applied_laws")
writeNewDb(NSS_applied_judgments, table = "NSS_applied_judgments")
writeNewDb(NSS_judges, table = "NSS_judges")
writeNewDb(NSS_metadata, table = "NSS_metadata")

US_metadata = tbl(con, "US_metadata") %>% collect()
