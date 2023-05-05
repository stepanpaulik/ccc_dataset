xfun::pkg_attach2("httr", "rvest", "progress", "foreach", "tidyverse", "xml2")

get_current_judges = function() {
  html = "https://www.nssoud.cz/o-soudu/soudci/soucasni-soudci" %>% read_html()
  
  data = foreach(i = seq((html_elements(html, xpath = '//*[contains(@class, "people-list")]/div'))), .combine = "rbind") %do% {
    output = tibble(
      "judge_name" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/h3/a')) %>% html_text2() %>% str_extract("[A-ZŽČŠŘ][a-ž]+\\s[A-ZŽČŠŘ][a-ž]+"),
      "yob" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/h3/span')) %>% html_text2() %>% str_extract("\\d{4}"),
      "start" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/div[1]')) %>% html_text2() %>% str_extract("(soudcem|soudkyní) Nejvyššího správního soudu od roku \\d{4}") %>% str_extract("\\d{4}"),
      "end" = NA,
      "gender" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div[2]/div[1]')) %>% html_text2() %>% str_extract("(soudcem|soudkyní) Nejvyššího správního soudu od roku \\d{4}") %>% str_extract("(soudcem|soudkyní)") %>% str_replace_all(c("soudcem" = "M", "soudkyní" = "F")),
      "temporary" = 0
    )
    return(output)
  }
  return(data)
}

NSS_judges_current = get_current_judges()
NSS_judges_current


get_former_judges = function() {
  html = "https://www.nssoud.cz/o-soudu/soudci/byvali-soudci" %>% read_html()
  
  
  data = foreach(i = seq((html_elements(html, xpath = '//*[contains(@class, "people-list")]/div'))), .combine = "rbind") %do% {
    output = tibble(
      "judge_name" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/h3')) %>% html_text2() %>% str_extract("[A-ZŽČŠŘ][a-ž]+\\s[A-ZŽČŠŘ][a-ž]+"),
      "yob" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/h3/span')) %>% html_text2() %>% str_extract("\\d{4}"),
      "start" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/div')) %>% html_text2() %>% str_extract("(soudcem|soudkyní) Nejvyššího správního soudu od roku \\d{4}") %>% str_extract("\\d{4}"),
      "end" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/div')) %>% html_text2() %>% str_extract("(funkce zanikla v roce \\d{4}|na funkci rezignoval[a-z]* v roce \\d{4})") %>% str_extract("\\d{4}"),
      "gender" = html_elements(html, xpath = paste0('//*[contains(@class, "people-list")]/div[',i,']/div/div')) %>% html_text2() %>% str_extract("(soudcem|soudkyní)") %>% str_replace_all(c("soudcem" = "M", "soudkyní" = "F")),
      "temporary" = 0
    )
    return(output)
  }
  return(data)
}

NSS_judges_former = get_former_judges()
NSS_judges_former

get_temporary_judges = function(){

  html = "https://www.nssoud.cz/o-soudu/soudci/docasne-prideleni-soudci" %>% read_html()
  judges_list = html %>% html_elements(xpath = '//div[@class="person-content"]')
  
  data = foreach(i = seq_along(judges_list), .combine = "rbind") %do% {
    start = judges_list[i] %>% html_elements(xpath = 'div[@class="person-info"]') %>% html_text2() %>% str_extract("od\\s*\\d+.\\s*\\d+.\\s*\\d{4}") %>% str_extract("\\d{4}")
    end = judges_list[i] %>% html_elements(xpath = 'div[@class="person-info"]') %>% html_text2() %>% str_extract("do\\s*\\d+.\\s*\\d+.\\s*\\d{4}") %>% str_extract("\\d{4}")
    output = tibble(
      "judge_name" = judges_list[i] %>% html_elements('h3') %>% html_text2() %>% str_extract("[A-ZŽČŠŘ][a-ž]+\\s[A-ZŽČŠŘ][a-ž]+"),
      "yob" = judges_list[i] %>% html_elements('h3') %>% html_text2() %>% str_extract("\\d{4}"),
      "start" = if_else(is.na(start), end, start),
      "end" = end,
      "gender" = judges_list[i] %>% html_elements(xpath = 'div[@class="person-info"]') %>% html_text2() %>% str_extract("(soudcem|soudkyní)") %>% str_replace_all(c("soudcem" = "M", "soudkyní" = "F")),
      "temporary" = 1
    )
    return(output)
  }
  return(data)
}

NSS_judges_temporary = get_temporary_judges()
NSS_judges_temporary

NSS_judges = rbind(NSS_judges, NSS_judges_temporary)
saveRDS(NSS_judges, file = "data/NSS_judges.rds")



