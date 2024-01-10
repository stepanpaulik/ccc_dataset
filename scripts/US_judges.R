library(tidyverse)

data_judges <- tribble(
  ~judge_name, ~judge_name_lemmatized, ~judge_yob, ~judge_gender, ~judge_uni, ~judge_degree, ~judge_profession, ~judge_term_start, ~judge_term_end,
  "Iva Brožová", "Iv[a-zěščřžýéáó]*\\s*Brožov[a-zěščřžýéáó]*", 1951, "F", "MUNI", "judr", "judge", "15.7.1993", "8.12.1999",
  "Vojtěch Cepl", "Vojtěch[a-zěščřžýéáó]*\\s*Cepl[a-zěščřžýéáó]*", 1938, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Vladimír Čermák", "Vladimír[a-zěščřžýéáó]*\\s*Čermák[a-zěščřžýéáó]*", 1929, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Miloš Holeček", "Miloš[a-zěščřžýéáó]*\\s*Holeč[a-zěščřžýéáó]*", 1950, "M", "CUNI", "judr", "judge", "15.7.1993", "15.7.2003",
  "Vladimír Jurka", "Vladimír[a-zěščřžýéáó]*\\s*Jur[a-zěščřžýéáó]*", 1948, "M", "KOM", "judr", "judge", "15.7.1993", "15.7.2003",
  "Zdeněk Kessler", "Zde[a-zěščřžýéáó]*\\s*Kessler[a-zěščřžýéáó]*", 1929, "M", "MUNI", "judr", "politician", "15.7.1993", "12.2.2003",
  "Vladimír Klokočka", "Vladimír[a-zěščřžýéáó]*\\s*Klokočk[a-zěščřžýéáó]*", 1929, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Vladimír Paul", "Vladimír[a-zěščřžýéáó]*\\s*Paul[a-zěščřžýéáó]*", 1924, "M", "CUNI", "judr", "lawyer", "15.7.1993", "3.4.2002",
  "Antonín Procházka", "Antonín[a-zěščřžýéáó]*\\s*Procházk[a-zěščřžýéáó]*", 1927, "M", "MUNI", "judr", "lawyer", "15.7.1993", "15.7.2003",
  "Vlastimil Ševčík", "Vlastimil[a-zěščřžýéáó]*\\s*Ševčík[a-zěščřžýéáó]*", 1927, "M", "MUNI", "judr", "judge", "15.7.1993", "15.12.2002",
  "Eva Zarembová", "Ev[a-zěščřžýéáó]*\\s*Zarembov[a-zěščřžýéáó]*", 1946, "F", "CUNI", "judr", "judge", "9.11.1993", "9.11.2003",
  "Pavel Varvařovský", "Pav[a-zěščřžýéáó]*\\s*Varvařovsk[a-zěščřžýéáó]*", 1945, "M", "CUNI", "judr", "judge", "29.3.1994", "29.3.2004",
  "Jiří Malenovský", "Jiří[a-zěščřžýéáó]*\\s*Malenovsk[a-zěščřžýéáó]*", 1950, "M", "MUNI", "prof", "scholar", "4.4.2000", "8.5.2004",
  "Eliška Wagnerová", "Elišk[a-zěščřžýéáó]*\\s*W(a|á)gner[a-zěščřžýéáó]*", 1948, "F", "CUNI", "phd", "scholar", "20.3.2002", "20.3.2012",
  "František Duchoň", "Františ[a-zěščřžýéáó]*\\s*Ducho[a-zěščřžýéáó]*", 1946, "M", "CUNI", "judr", "judge", "6.6.2002", "6.6.2012",
  "Dagmar Lastovecká", "Dagmar Lastoveck[a-zěščřžýéáó]*", 1951, "F", "MUNI", "judr", "politician", "29.8.2003", "29.8.2013",
  "Pavel Holländer", "Pav[a-zěščřžýéáó]*\\s*Holländer[a-zěščřžýéáó]*", 1953, "M", "KOM", "prof", "scholar", "15.7.1993", "6.8.2013",
  "Vojen Güttler", "Vojen[a-zěščřžýéáó]*\\s*G(ü|ű|u|ú)t(|t)ler[a-zěščřžýéáó]*", 1934, "M", "CUNI", "judr", "politician", c("15.7.1993", "6.8.2003"), c("15.7.2003", "6.8.2013"),
  "Miloslav Výborný", "Miloslav[a-zěščřžýéáó]*\\s*Výborn[a-zěščřžýéáó]*", 1952, "M", "CUNI", "judr", "politician", "3.6.2003", "3.6.2013",
  "Jiří Mucha", "Jiří[a-zěščřžýéáó]*\\s*Much[a-zěščřžýéáó]*", 1946, "M", "CUNI", "judr", "lawyer", "28.1.2003", "28.1.2013",
  "Jiří Nykodým", "Jiří[a-zěščřžýéáó]*\\s*Nykodým[a-zěščřžýéáó]*", 1945, "M", "CUNI", "judr", "lawyer", "17.12.2003", "17.12.2013",
  "Stanislav Balík", "Stanislav[a-zěščřžýéáó]*\\s*Balík[a-zěščřžýéáó]*", 1956, "M", "CUNI", "phd", "lawyer", "26.5.2004", "26.5.2014",
  "Michaela Židlická", "Michael[a-zěščřžýéáó]*\\s*Židlick[a-zěščřžýéáó]*", 1952, "F", "MUNI", "doc", "scholar", "16.6.2004", "16.6.2014",
  "Ivana Janů", "Ivan[a-zěščřžýéáó]*\\s*Janů", 1946, "F", "CUNI", "judr", c("politician", "judge"), c("9.11.1993", "16.9.2004"), c("9.11.2003", "16.9.2014"),
  "Vlasta Formánková", "Vlast[a-zěščřžýéáó]*\\s*Formánkov[a-zěščřžýéáó]*", 1953, "F", "CUNI", "judr", "judge", "5.8.2005", "5.8.2015",
  "Vladimír Kůrka", "Vladimír[a-zěščřžýéáó]*\\s*Kůrk[a-zěščřžýéáó]*", 1948, "M", "CUNI", "judr", "judge", "15.12.2005", "15.12.2015",
  "Jan Musil", "Jan[a-zěščřžýéáó]*\\s*Musil[a-zěščřžýéáó]*", 1941, "M", "CUNI", "prof", c("scholar","judge"), c("27.11.2003", "20.1.2014"), c("27.11.2013", "31.1.2019"),
  "Kateřina Šimáčková", "Kateřin[a-zěščřžýéáó]*\\s*Šimáčkov[a-zěščřžýéáó]*", 1966, "F", "MUNI", "phd", "judge", "7.8.2013", "10.12.2021",
  "Pavel Šámal", "Pav[a-zěščřžýéáó]*\\s*Šámal[a-zěščřžýéáó]*", 1953, "M", "CUNI", "prof", "judge", "20.2.2020", NA,
  "Josef Fiala", "Josef[a-zěščřžýéáó]*\\s*Fial[a-zěščřžýéáó]*", 1953, "M", "MUNI", "prof", "scholar", "17.12.2015", NA,
  "Jaromír Jirsa", "Jaromír[a-zěščřžýéáó]*\\s*Jirs[a-zěščřžýéáó]*", 1966, "M", "CUNI", "judr", "judge", "7.10.2015", NA,
  "David Uhlíř", "David[a-zěščřžýéáó]*\\s*Uhlíř[a-zěščřžýéáó]*", 1954, "M", "CUNI", "judr", "lawyer", "10.12.2014", NA,
  "Tomáš Lichovník", "Tomáš[a-zěščřžýéáó]*\\s*Lichovník[a-zěščřžýéáó]*", 1964, "M", "MUNI", "judr", "judge", "19.6.2014", NA,
  "Vojtěch Šimíček", "Vojtěch[a-zěščřžýéáó]*\\s*Šimíč[a-zěščřžýéáó]*", 1969, "M", "MUNI", "doc", "judge", "12.6.2014", NA,
  "Jiří Zemánek", "Jiří[a-zěščřžýéáó]*\\s*Zemán[a-zěščřžýéáó]*", 1950, "M", "CUNI", "judr", "scholar", "20.1.2014", NA,
  "Radovan Suchánek", "Radovan[a-zěščřžýéáó]*\\s*Suchán[a-zěščřžýéáó]*", 1972, "M", "CUNI", "phd", "scholar", "26.11.2013", "26.11.2023",
  "Ludvík David", "Ludvík[a-zěščřžýéáó]*\\s*David[a-zěščřžýéáó]*", 1951, "M", "MUNI", "judr", "judge", "7.8.2013", "7.8.2023",
  "Vladimír Sládeček", "Vladimír[a-zěščřžýéáó]*\\s*Sládeč[a-zěščřžýéáó]*", 1954, "M", "CUNI", "prof", "scholar", "4.6.2013", "4.6.2023",
  "Jan Filip", "Jan[a-zěščřžýéáó]*\\s* Filip[a-zěščřžýéáó]*", 1950, "M", "MUNI", "prof", "scholar", "3.5.2013", "3.5.2023",
  "Jaroslav Fenyk", "Jaroslav[a-zěščřžýéáó]*\\s*Fenyk[a-zěščřžýéáó]*", 1961, "M", "CUNI", "prof", "scholar", "3.5.2013", "3.5.2023",
  "Milada Tomková", "Milad[a-zěščřžýéáó]*\\s*Tomkov[a-zěščřžýéáó]*", 1959, "F", "CUNI", "judr", "judge", "3.5.2013", "3.5.2023",
  "Pavel Rychetský", "Pav[a-zěščřžýéáó]*\\s*Rychetsk[a-zěščřžýéáó]*", 1943, "M", "CUNI", "judr", c("politician","judge"), c("6.8.2003", "7.8.2013"), c("6.8.2013", NA),
  "Jan Svatoň", "Jan[a-zěščřžýéáó]*\\s*Svato[a-zěščřžýéáó]*", 1952, "M", "MUNI", "doc", "scholar", "15.2.2023", NA,
  "Josef Baxa", "Josef[a-zěščřžýéáó]*\\s*Bax[a-zěščřžýéáó]*", 1959, "M", "CUNI", "judr", "judge", "5.6.2023", NA,
  "Jan Wintr", "Jan[a-zěščřžýéáó]*\\s*Wintr[a-zěščřžýéáó]*", 1978, "M", "CUNI", "prof", "scholar", "5.6.2023", NA,
  "Daniela Zemanová", "Daniel[a-zěščřžýéáó]*\\s*Zeman[a-zěščřžýéáó]*", 1971, "F", "CUNI", "mgr", "judge", "5.6.2023", NA,
  "Kateřina Ronovská", "Kate[a-zěščřžýéáó]*\\s*Ronovs[a-zěščřžýéáó]*", 1974, "F", "MUNI", "prof", "scholar", "4.8.2023", NA,
  "Veronika Křesťanová", "Veroni[a-zěščřžýéáó]*\\s*Křesťan[a-zěščřžýéáó]*", 1969, "F", "CUNI", "phd", "judge", "8.8.2023", NA
) %>%
  rowwise() %>%
  mutate(judge_id = paste0("J:",cur_group_id())) %>%
  ungroup() %>%
  unnest(c(judge_term_start, judge_term_end, judge_profession)) %>%
  mutate(across(c(judge_term_start,judge_term_end), ~as_date(x = ., format = "%d.%m.%Y"))) %>%
  mutate(judge_term_court = case_when(year(judge_term_start) < 1995 ~ "1st",
                               year(judge_term_start) < 2010 ~ "2nd",
                               year(judge_term_start) < 2018 ~ "3rd",
                               year(judge_term_start) > 2019 ~ "4th"),
         judge_term_president = case_when(judge_term_court == "1st" ~ "Václav Havel",
                                          judge_term_court == "2nd" ~ "Václav Klaus",
                                          judge_term_court == "3rd" ~ "Miloš Zeman",
                                          judge_term_court == "4th" ~ "Petr Pavel")) %>%

  mutate(judge_gender = factor(judge_gender),
         judge_uni = factor(judge_uni),
         judge_degree = factor(judge_degree, 
                            ordered = TRUE,
                            levels = c("mgr", "judr", "phd", "doc", "prof"))) %>%
  relocate(judge_id) %>%
  mutate(judge_initials = paste0(substring(word(judge_name, 1), 1, 1), ".\\s*", substring(word(judge_name, 2), 1, 1), "."))
  

write_rds(data_judges, file = "../data/US_judges.rds")

#   
# reelected_judges = c("Pavel Rychetský", "Jiří Nykodým", "Ivana Janů", "Jan Musil", "Pavel Holländer", "Vojen Güttler", "Miloslav Výborný")
# 
# US_judges = US_judges %>%
#   mutate(reelection = if_else(judge_name %in% reelected_judges,1,0))
