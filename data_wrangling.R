library(tidyverse)

source("db_functions.R")

dbListTables(con)

# Load data
US_metadata = readRDS("../data/US_metadata.rds")
US_texts = readRDS("../data/US_texts.rds")
US_compositions = readRDS("../data/US_compositions.rds")
US_judges = readRDS("../data/US_judges.rds")
US_dissents = readRDS("../data/US_dissents.rds")

# Read_data from db
# US_metadata = tbl(conn, "US_metadata") %>% as_tibble()

# Write the dataframes as tables in MySQL database
dbWriteTable(conn = con, value = US_texts, name = "US_texts", field.types = c(doc_id = "varchar(255)", text = "LONGTEXT"), overwrite=TRUE)
US_compositions %>% writeNewDb(data = ., table = "US_compositions")
US_dissents %>% writeNewDb(data = ., table = "US_dissents")
US_metadata %>% writeNewDb(data = ., table = "US_metadata")
US_judges %>% writeNewDb(data = ., table = "US_judges")





dbReadTable(con, "US_metadata")

# Close the database connection
dbDisconnect(con)
