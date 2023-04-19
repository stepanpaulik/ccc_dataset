xfun::pkg_attach2("httr", "rvest", "progress", "foreach", "tidyverse", "xml2")

get_current_judges <- function() {
  html <- "https://www.nssoud.cz/o-soudu/soudci/soucasni-soudci" %>% read_html()
  
  NSS_judges <- foreach(i = seq((html_elements(html, xpath = '//*[contains(@class, "people-list")]/div'))), .combine = "rbind") %do% {
    output <- tibble(
      "judge_name" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/h3/a')) %>% html_text2() %>% str_extract("[A-ZŽČŠŘ][a-ž]+\\s[A-ZŽČŠŘ][a-ž]+"),
      "yob" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/h3/span')) %>% html_text2() %>% str_extract("\\d{4}"),
      "start" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/div[1]')) %>% html_text2() %>% str_extract("(soudcem|soudkyní) Nejvyššího správního soudu od roku \\d{4}") %>% str_extract("\\d{4}"),
      "end" = NA,
      "gender" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/div[1]')) %>% html_text2() %>% str_extract("(soudcem|soudkyní) Nejvyššího správního soudu od roku \\d{4}") %>% str_extract("(soudcem|soudkyní)") %>% str_replace_all(c("soudcem" = "M", "soudkyní" = "F"))
    )
    return(output)
  }
  return(NSS_judges)
}

NSS_judges <- get_current_judges()
NSS_judges


get_former_judges <- function() {
  html <- "https://www.nssoud.cz/o-soudu/soudci/byvali-soudci" %>% read_html()
  
  
  NSS_judges <- foreach(i = seq((html_elements(html, xpath = '//*[contains(@class, "people-list")]/div'))), .combine = "rbind") %do% {
    output <- tibble(
      "judge_name" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/h3')) %>% html_text2() %>% str_extract("[A-ZŽČŠŘ][a-ž]+\\s[A-ZŽČŠŘ][a-ž]+"),
      "yob" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/h3/span')) %>% html_text2() %>% str_extract("\\d{4}"),
      "start" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/div')) %>% html_text2() %>% str_extract("(soudcem|soudkyní) Nejvyššího správního soudu od roku \\d{4}") %>% str_extract("\\d{4}"),
      "end" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/div')) %>% html_text2() %>% str_extract("(funkce zanikla v roce \\d{4}|na funkci rezignoval[a-z]* v roce \\d{4})") %>% str_extract("\\d{4}"),
      "gender" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/div')) %>% html_text2() %>% str_extract("(soudcem|soudkyní) Nejvyššího správního soudu od roku \\d{4}") %>% str_extract("(soudcem|soudkyní)") %>% str_replace_all(c("soudcem" = "M", "soudkyní" = "F"))
    )
    return(output)
  }
  return(NSS_judges)
}

NSS_judges_former <- get_former_judges()
NSS_judges_former





