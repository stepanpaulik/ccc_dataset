xfun::pkg_attach2("httr", "xml2", "stringr", "progress", "xmlparsedata", "tidyverse", "RSelenium", "janitor")

rD <- rsDriver(browser="firefox", port=5553L, verbose=F)
remDr <- rD[["client"]]

decision_date <- "1.1.1993"
decisions_id <- c()
numberPages <- 4493
decisions_id <- read.csv("data/Decisions_US_id.csv") %>% unique()
data_metadata <- as.data.frame(read.csv("data/Decisions_US_metadata.csv"))
current_i <- 1

get_urls <- function(decision_date = "1.1.1993", numberPages = "1") {
  remDr$navigate("https://nalus.usoud.cz/Search/Search.aspx")
  remDr$findElement(using = "id", value = "ctl00_MainContent_decidedFrom")$clearElement()
  remDr$findElement(using = "id", value = "ctl00_MainContent_decidedFrom")$sendKeysToElement(list(decision_date))

  remDr$findElements("id", "ctl00_MainContent_but_search")[[1]]$clickElement()


  for (i in 1:numberPages) {
    html <- remDr$getPageSource()[[1]]
    decisions_id_temp <- read_html(html) %>% html_elements(xpath = "//a[contains(@class, 'resultData')]") %>% html_attr('href') %>% paste0("https://nalus.usoud.cz/Search/", .)
    decisions_id <<- c(decisions_id, decisions_id_temp)
    remDr$findElements("xpath", "/html/body/form/div[4]/table/tbody/tr[4]/td/table/tbody/tr/td/a[last()]")[[1]]$clickElement()
    
    print(i)
  }
  return(decisions_id)
}

# metadata_list <- c()
html_metadata_names <- c()

get_metadata <- function(starting_i = 1, decisions_id) {
  metadata_list <- c()
  html_metadata_names <- c()
  for (i in starting_i:length(decisions_id$x)) {
    remDr$navigate(decisions_id$x[i])
    html <- remDr$getPageSource()[[1]]
    
    html_metadata <- html %>% read_html() %>% 
      html_nodes(xpath="/html/body/form/div[3]/table/tbody/tr[3]/td/div/table") %>% html_table() %>% as.data.frame()
    
    metadata_list[[i]] <<- t(html_metadata[,2])
    
    current_i <<- i
    print(i)
    
    if (i == 1) {
      html_metadata_names <<- html_metadata[,1]
    }
  }
  data_metadata <- create_df_names()
  return(data_metadata)
}

create_df_names <- function () {
  data_metadata <- do.call(rbind, metadata_list)
  colnames(data_metadata) <- make_clean_names(html_metadata_names)
  data_metadata <- data_metadata %>% discard(~all(is.na(.) | . =="")) %>% as.data.frame()
  return(data_metadata)
}

get_texts <- function(starting_i = 1) {
  decisions_texts <- c()
  for (i in starting_i:length(data_metadata$doc_id)) {
    html2 <- data_metadata[i,"url_adress"]
    decisions_texts[i] <<- html2 %>% read_html() %>% html_nodes(xpath='//td[@class="DocContent"]/table/tr/td') %>% html_text2()
    
    current_i <<- i
    print(i)
  }
  data_metadata <<- cbind(data_metadata, decisions_texts)
  return(data_metadata)
}


# functions calls 
get_urls(decision_date, numberPages)
data_metadata <- get_metadata(starting_i = current_i, decisions_id = decisions_id)
data_metadata <- get_texts(starting_i = current_i)

# Save progress
write.csv(unique_IDs,"decisions_decisions_id.csv", row.names = FALSE)
save(data_metadata, file = "data/US_metadata.RData")

data_texts <- data_metadata %>% select(doc_id, decisions_texts) %>% rename(text = decisions_texts)
save(data_texts, file = "data/US_texts.RData")
