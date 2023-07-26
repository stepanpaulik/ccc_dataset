xfun::pkg_attach2("tidyverse", "RMariaDB", "foreach")



# SQL commmunication
# Connect to the DB
conn <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "apex_courts_database",
  username = "root",
  password = "4E5ad7d!!",
  host = "localhost",
  port = 3306
)



dbExecute(conn, "SET NAMES 'utf8'")
dbListTables(conn)

US_metadata = tbl(conn, "US_metadata")

query = US_metadata %>% 
  group_by(judge_rapporteur, year_decision) %>%
  summarise(count = n()) %>%
  compute(temporary = FALSE, name = "US_metadata", overwrite = TRUE)
query <- query[2:length(query)] %>% str_remove_all(pattern = "`")

tmp <- tempfile()
writeLines(query, tmp)

dbSendQuery(conn, query)


# Create a new table function
writeNewDb <- function(data, table) {
  dbWriteTable(
  conn,
  table,
  data,
  overwrite = TRUE,
  row.names = FALSE
  )
}

US_metadata = readRDS(file = "../data/US_metadata.rds")

# Write the dataframes as tables in MySQL database
US_metadata %>% writeNewDb(data = ., table = "US_metadata")
US_compositions %>% writeNewDb(data = ., table = "US_compositions")
US_dissents %>% writeNewDb(data = ., table = "US_dissents")

# rs <- RMariaDB::dbGetQuery(
#   conn,
#   "SELECT * FROM US_metadata WHERE judge_rapporteur = ?",
#   params = list(c("Fiala Josef"))
# )




dbWriteNewLines <- function(data, table_name){
  # Updating an existing table
  dbWriteTable(conn, "temp_table", data, append = TRUE, row.names = FALSE)
  
  # Get the query
  table_name <- deparse(substitute(data))
  dbExecute(conn, 
            paste0("INSERT IGNORE INTO ", table_name, 
                   "
          SELECT *
          FROM temp_table"))
  
  
  # Remove the temporary table
  dbRemoveTable(conn, "temp_table")
}

dbWriteNewLines(data = NSS_metadata[70:100,], table_name = "NSS_metadata")

# # Update the existing table in the database with the new data
# query <- paste0("UPDATE ", existing_table, "
#           INNER JOIN temp_table
#           ON ", existing_table, ".id = temp_table.id
#           SET existing_table.column1 = temp_table.column1,
#               existing_table.column2 = temp_table.column2,
#               existing_table.column3 = temp_table.column3")
# 
# dbSendQuery(conn, query)
``
# Write part of NSS_metadata
NSS_metadata[1:80,] %>% writeNewDb(data = ., table = "NSS_metadata")
dbExecute(conn, "ALTER TABLE NSS_metadata MODIFY regional_court_case_ID VARCHAR(50);")
dbExecute(conn, "ALTER TABLE NSS_metadata ADD PRIMARY KEY (doc_id);")



# Insert any new rows that don't already exist in the database




# Close the database connection
dbDisconnect(conn)