library(tidyverse)
library(rvest)
library(foreach)
library(fuzzyjoin)

get_clerks = function(){
  html = "https://www.usoud.cz/asistenti-soudcu" %>%
    read_html()
  
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
    regex_left_join(., read_rds("../data/ccc_dataset/ccc_judges.rds") %>% select(judge_name, judge_name_lemmatized) %>% distinct(), by = c('judge_name' = 'judge_name_lemmatized')) %>%
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
    left_join(., readr::read_rds("../data/ccc_dataset/ccc_judges.rds") %>% select(judge_name, judge_id) %>% distinct()) %>%
    relocate(judge_id) %>%
    mutate(clerk_term_end = case_when(is.na(clerk_term_end) ~ clerk_term_start %m+% years(10),
                                      .default = clerk_term_end))
  return(data)
}

data_clerks = get_clerks()
write_rds(data_clerks, "../data/ccc_dataset/ccc_clerks.rds")




