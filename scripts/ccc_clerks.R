library(tidyverse)
library(rvest)
library(foreach)
library(fuzzyjoin)

get_clerks = function(){
  # This scrapes the information automatically from the Nalus website. The scraping proceeds in two steps for the current and past clerks seprately as the way their information is stored differs
  html = "https://www.usoud.cz/asistenti-soudcu" %>%
    read_html()
  
  browser()
  
  data_p1 = foreach(i = 1:length(html %>%
                                   html_elements(xpath = '//*[@id="c689"]/h3')), .combine = "bind_rows") %do% {
                                     judge_name = html %>%
                                       html_elements(xpath = paste0('//*[@id="c689"]/h3[',i,']', collapse = "")) %>%
                                       html_text2()
                                     clerks = html %>%
                                       html_elements(xpath = paste0('//*[@id="c689"]/p[',i+1,']', collapse = "")) %>%
                                       html_text2() %>%
                                       str_split(pattern = "\n\r ")
                                     tibble(
                                       judge_name = judge_name,
                                       clerk_name = clerks
                                     )
                                   } %>%
    unnest(clerk_name) %>%
    mutate(clerk_term_start = str_extract(string = clerk_name, pattern = "\\d+. \\d+. \\d{4}") %>% as_date(format = "%d. %m. %Y"),
           clerk_term_end = NA,
           clerk_name = str_remove(string = clerk_name, pattern = "\\s\\(\\d+. \\d+. \\d{4}\\)"),
           judge_name = str_extract(judge_name, pattern = "[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+(\\s[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+)?\\s[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+"),
           clerk_name_full = clerk_name,
           clerk_name = str_extract(clerk_name_full, pattern = "[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+\\s[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+")) %>%
    regex_left_join(., read_rds("../data/ccc_database/rds/ccc_judges.rds") %>% select(judge_name, judge_name_lemmatized) %>% distinct(), by = c('judge_name' = 'judge_name_lemmatized')) %>%
    select(-c(judge_name.x, judge_name_lemmatized)) %>%
    rename(judge_name = judge_name.y) %>%
    relocate(judge_name) %>%
    drop_na(clerk_name)
  
  data_p1 <- html %>%
    # Select all <p> tags within the #c689 div
    html_elements("#c689 p") %>%
    # Filter for <p> tags that contain a <strong> element (indicating a judge's name)
    keep(~ html_element(.x, "strong") %>% length() > 0) %>%
    map_df(~ {
      judge_name_raw <- .x %>% html_element("strong") %>% html_text2()

    # IMPROVED JUDGE NAME EXTRACTION (More flexible for genitive case)
    # This regex now captures any capitalized words that appear after
    # the title phrases.
    judge_name <- str_extract(
      judge_name_raw,
      "(?:Asistenti (?:předsedy Ústavního soudu|místopředsedkyně|soudce|soudkyně)\\s+)([A-ZĽŠČŘŽČ][a-zžěščřžýěáíéúůň]+(?:\\s+[A-ZĽŠČŘŽČ][a-zžěščřžěýáíéúůň]+)*)"
    ) %>% str_squish()

    # If the above is still NA, it means the structure might be different or
    # the title was not perfectly matched. As a last resort, extract any
    # capitalized words at the end of the string.
    if(is.na(judge_name)) {
      judge_name <- str_extract(
        judge_name_raw,
        "([A-ZĽŠČŘŽČ][a-žěščřžýáíéúůň]+(?:\\s+[A-ZĽŠČŘŽČ][a-žěščřžýáíéúůň]+)*)$"
      ) %>% str_squish()
    } # Clean up any extra spaces
      
      # Get the text content of the *next* <p> sibling that doesn't contain a <strong> tag.
      # This is a more robust way to get the clerk list for the current judge.
      clerks_paragraph <- .x %>% 
        html_element(xpath = 'following-sibling::p[not(strong)][1]')
      
      clerks_raw <- character()
      if (!is.null(clerks_paragraph)) {
        # Extract text and split by <br> tags, then clean up newlines and spaces
        clerks_raw <- clerks_paragraph %>% 
          as.character() %>% 
          str_split("<br>") %>% 
          pluck(1) %>% 
          str_remove_all("<p>|<\\/p>|&nbsp;") %>% # Remove remaining HTML tags and &nbsp;
          str_squish() %>% # Trim whitespace
          .[. != ""] # Remove empty strings
      }
      
      tibble(
        judge_name = judge_name,
        clerk_name = clerks_raw
      )
    }) %>%
    unnest(clerk_name) %>%
    mutate(
      clerk_term_start = str_extract(string = clerk_name, pattern = "\\d{1,2}\\.\\s*\\d{1,2}\\.\\s*\\d{4}") %>%
        as_date(format = "%d. %m. %Y"),
      clerk_term_end = NA, # No end date in this section
      clerk_name_full = clerk_name, # Keep original for full name
      clerk_name = str_remove(string = clerk_name, pattern = "\\s*\\(\\d{1,2}\\.\\s*\\d{1,2}\\.\\s*\\d{4}\\)") %>%
        str_squish() # Remove the date and trim
    ) %>%
    regex_left_join(., read_rds("../data/ccc_database/rds/ccc_judges.rds") %>% select(judge_name, judge_name_lemmatized) %>% distinct(), by = c('judge_name' = 'judge_name_lemmatized')) %>%
    select(-c(judge_name.x, judge_name_lemmatized)) %>%
    rename(judge_name = judge_name.y) %>%
    relocate(judge_name) %>%
    drop_na(clerk_name)
  
  
  data_p2 = foreach(item = html %>% html_elements(xpath = '//*[@id="c687"]/p') %>% html_text2(), .combine = "bind_rows") %do%{
    judge_name = str_extract_all(item, '\\([A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+\\s[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+\\)') %>% map(.x = ., ~str_remove(.x, "\\(") %>% str_remove("\\)"))
    clerk_name = sub("\\d+.\\s*\\d+.\\s*\\d{4} (–|-).*", "", item) %>% str_squish()
    start = str_extract_all(item, "\\d+.\\s*\\d+.\\s*\\d{4} (–|-)") %>% map(.x = ., ~str_remove(.x, " (–|-)") %>% str_remove_all(" "))
    end = str_extract_all(item, "(–|-)\\s\\d+.\\s*\\d+.\\s*\\d{4}") %>% map(.x = ., ~str_remove(.x, "(–|-) ") %>% str_remove_all(" "))
    tibble(
      judge_name = judge_name,
      clerk_name = clerk_name,
      clerk_term_start = start,
      clerk_term_end = end
    )
  } %>%
    mutate(clerk_name_full = clerk_name,
           clerk_name = str_extract(clerk_name_full, pattern = "[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+\\s([A-ZĽŠČŘŽČ].\\s)?[A-ZĽŠČŘŽČ]{1}[a-žěščřžýáíéúůň]+")) %>% 
    slice(-1) %>%
    unnest(cols = c(judge_name, clerk_term_start, clerk_term_end)) %>%
    mutate(across(c(clerk_term_start,clerk_term_end), ~as_date(x = ., format = "%d.%m.%Y")))
  
  data = bind_rows(data_p1, data_p2) %>%
    mutate(clerk_gender = case_when(str_detect(str_sub(clerk_name, -1), "á") | clerk_name %in% c("Martina Kuloglija", "Zuzana Čierna", "Kamila Abbasi") ~ "F",
           .default = "M"),
           clerk_degree = case_when(
             str_detect(clerk_name_full, "Prof.|David Kosař") ~ "prof",
             str_detect(clerk_name_full, "doc.") ~ "doc",
             str_detect(clerk_name_full, "Ph.\\s?D.?|Martin Kopa") ~ "phd",
             str_detect(clerk_name_full, "JUDr.") ~ "judr",
             str_detect(clerk_name_full, "Mgr.|prom. práv.") ~ "mgr",
             .default = NA
           ),
           clerk_abroad = ifelse(str_detect(clerk_name_full, "LL.\\s?M.|MJur|M.\\s?Jur.|M.\\s?St.|M.\\s?Phil."), 1, 0)) %>%
    left_join(., readr::read_rds("../data/ccc_database/rds/ccc_judges.rds") %>% select(judge_name, judge_id) %>% distinct()) %>%
    relocate(judge_id) %>%
    mutate(clerk_term_end = case_when(is.na(clerk_term_end) ~ clerk_term_start %m+% years(10),
                                      .default = clerk_term_end))
  data = data %>%
    left_join(data %>% select(clerk_name) %>% distinct() %>% rowwise() %>%
                mutate(clerk_id = paste0("C:",cur_group_id()))) %>%
    relocate(clerk_id, .before = clerk_name)
  return(data)
}

data_clerks = get_clerks()

if(write == TRUE) write_rds(data_clerks, "../data/ccc_database/rds/ccc_clerks.rds")




