library(tidyverse)
library(RMariaDB)
library(DBI)

# SQL commmunication
# Connect to the DB
con = RMariaDB::dbConnect(
  RMariaDB::MariaDB(),
  dbname = "apex_courts_db",
  username = "root",
  password = keyring::key_get("localdb", "root"),
  port = 3306L
)

dbExecute(con, "SET NAMES 'utf8'")
dbGetInfo(con)
dbListTables(con)

# Create a new table function
writeNewDb = function(data, table, conn = con) {
  dbWriteTable(
  conn,
  table,
  data,
  overwrite = FALSE,
  append = TRUE,
  row.names = FALSE
  )
}

dbWriteNewLines = function(data, table_name, conn = con){
  # Updating an existing table
  dbWriteTable(conn = conn, "temp_table", data, append = TRUE, row.names = FALSE)
  
  # Get the query
  table_name = deparse(substitute(data))
  dbExecute(conn, 
            paste0("INSERT IGNORE INTO ", table_name, 
                   "
          SELECT *
          FROM temp_table"))
  
  
  # Remove the temporary table
  dbRemoveTable(conn, "temp_table")
}



