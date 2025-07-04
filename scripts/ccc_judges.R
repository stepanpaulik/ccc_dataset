library(tidyverse)

data_judges = tribble(
  ~judge_name, ~judge_name_lemmatized, ~judge_yob, ~judge_gender, ~judge_uni, ~judge_degree, ~judge_profession, ~judge_term_start, ~judge_term_end,
  "Iva Brožová", "Iv[a-žěščřžýáíéúůň]*\\s*Brožov[a-žěščřžýáíéúůň]*", 1951, "F", "MUNI", "judr", "judge", "15.7.1993", "8.12.1999",
  "Vojtěch Cepl", "Vojtěch[a-žěščřžýáíéúůň]*\\s*Cepl[a-žěščřžýáíéúůň]*", 1938, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Vladimír Čermák", "Vladimír[a-žěščřžýáíéúůň]*\\s*Čermák[a-žěščřžýáíéúůň]*", 1929, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Miloš Holeček", "Miloš[a-žěščřžýáíéúůň]*\\s*Holeč[a-žěščřžýáíéúůň]*", 1950, "M", "CUNI", "judr", "judge", "15.7.1993", "15.7.2003",
  "Vladimír Jurka", "Vladimír[a-žěščřžýáíéúůň]*\\s*Jur[a-žěščřžýáíéúůň]*", 1948, "M", "KOM", "judr", "judge", "15.7.1993", "15.7.2003",
  "Zdeněk Kessler", "Zde[a-žěščřžýáíéúůň]*\\s*Kessler[a-žěščřžýáíéúůň]*", 1929, "M", "MUNI", "judr", "politician", "15.7.1993", "12.2.2003",
  "Vladimír Klokočka", "Vladimír[a-žěščřžýáíéúůň]*\\s*Klokočk[a-žěščřžýáíéúůň]*", 1929, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Vladimír Paul", "Vladimír[a-žěščřžýáíéúůň]*\\s*Paul[a-žěščřžýáíéúůň]*", 1924, "M", "CUNI", "judr", "lawyer", "15.7.1993", "3.4.2002",
  "Antonín Procházka", "Antonín[a-žěščřžýáíéúůň]*\\s*Procházk[a-žěščřžýáíéúůň]*", 1927, "M", "MUNI", "judr", "lawyer", "15.7.1993", "15.7.2003",
  "Vlastimil Ševčík", "Vlastimil[a-žěščřžýáíéúůň]*\\s*Ševčík[a-žěščřžýáíéúůň]*", 1927, "M", "MUNI", "judr", "judge", "15.7.1993", "15.12.2002",
  "Eva Zarembová", "Ev[a-žěščřžýáíéúůň]*\\s*Zarembov[a-žěščřžýáíéúůň]*", 1946, "F", "CUNI", "judr", "judge", "9.11.1993", "9.11.2003",
  "Pavel Varvařovský", "Pav[a-žěščřžýáíéúůň]*\\s*Varvařovsk[a-žěščřžýáíéúůň]*", 1945, "M", "CUNI", "judr", "judge", "29.3.1994", "29.3.2004",
  "Jiří Malenovský", "Jiří[a-žěščřžýáíéúůň]*\\s*Malenovsk[a-žěščřžýáíéúůň]*", 1950, "M", "MUNI", "prof", "scholar", "4.4.2000", "8.5.2004",
  "Eliška Wagnerová", "Elišk[a-žěščřžýáíéúůň]*\\s*W(a|á)gner[a-žěščřžýáíéúůň]*", 1948, "F", "CUNI", "phd", "scholar", "20.3.2002", "20.3.2012",
  "František Duchoň", "Františ[a-žěščřžýáíéúůň]*\\s*Ducho[a-žěščřžýáíéúůň]*", 1946, "M", "CUNI", "judr", "judge", "6.6.2002", "6.6.2012",
  "Dagmar Lastovecká", "Dagmar Lastoveck[a-žěščřžýáíéúůň]*", 1951, "F", "MUNI", "judr", "politician", "29.8.2003", "29.8.2013",
  "Pavel Holländer", "Pav[a-žěščřžýáíéúůň]*\\s*Holl[a-žěščřžýáíéúůňä]nder[a-žěščřžýáíéúůň]*", 1953, "M", "KOM", "prof", "scholar", c("15.7.1993", "6.8.2003"), c("15.7.2003", "6.8.2013"),
  "Vojen Güttler", "Vojen[a-žěščřžýáíéúůň]*\\s*G(ü|ű|u|ú)t(|t)ler[a-žěščřžýáíéúůň]*", 1934, "M", "CUNI", "judr", c("politician", "judge"), c("15.7.1993", "6.8.2003"), c("15.7.2003", "6.8.2013"),
  "Miloslav Výborný", "Miloslav[a-žěščřžýáíéúůň]*\\s*Výborn[a-žěščřžýáíéúůň]*", 1952, "M", "CUNI", "judr", "politician", "3.6.2003", "3.6.2013",
  "Jiří Mucha", "Jiří[a-žěščřžýáíéúůň]*\\s*Much[a-žěščřžýáíéúůň]*", 1946, "M", "CUNI", "judr", "lawyer", "28.1.2003", "28.1.2013",
  "Jiří Nykodým", "Jiří[a-žěščřžýáíéúůň]*\\s*Nykodým[a-žěščřžýáíéúůň]*", 1945, "M", "CUNI", "judr", "lawyer", "17.12.2003", "17.12.2013",
  "Stanislav Balík", "Stanislav[a-žěščřžýáíéúůň]*\\s*Balík[a-žěščřžýáíéúůň]*", 1956, "M", "CUNI", "phd", "lawyer", "26.5.2004", "26.5.2014",
  "Michaela Židlická", "Michael[a-žěščřžýáíéúůň]*\\s*Židlick[a-žěščřžýáíéúůň]*", 1952, "F", "MUNI", "doc", "scholar", "16.6.2004", "16.6.2014",
  "Ivana Janů", "Ivan[a-žěščřžýáíéúůň]*\\s*Janů", 1946, "F", "CUNI", "judr", c("politician", "judge"), c("9.11.1993", "16.9.2004"), c("9.11.2003", "16.9.2014"),
  "Vlasta Formánková", "Vlast[a-žěščřžýáíéúůň]*\\s*Formánkov[a-žěščřžýáíéúůň]*", 1953, "F", "CUNI", "judr", "judge", "5.8.2005", "5.8.2015",
  "Vladimír Kůrka", "Vladimír[a-žěščřžýáíéúůň]*\\s*Kůrk[a-žěščřžýáíéúůň]*", 1948, "M", "CUNI", "judr", "judge", "15.12.2005", "15.12.2015",
  "Jan Musil", "Jan[a-žěščřžýáíéúůň]*\\s*Musil[a-žěščřžýáíéúůň]*", 1941, "M", "CUNI", "prof", c("scholar","judge"), c("27.11.2003", "20.1.2014"), c("27.11.2013", "31.1.2019"),
  "Kateřina Šimáčková", "Kateřin[a-žěščřžýáíéúůň]*\\s*Šimáčkov[a-žěščřžýáíéúůň]*", 1966, "F", "MUNI", "phd", "judge", "7.8.2013", "10.12.2021",
  "Pavel Šámal", "Pav[a-žěščřžýáíéúůň]*\\s*Šámal[a-žěščřžýáíéúůň]*", 1953, "M", "CUNI", "prof", "judge", "20.2.2020", NA,
  "Josef Fiala", "Josef[a-žěščřžýáíéúůň]*\\s*Fial[a-žěščřžýáíéúůň]*", 1953, "M", "MUNI", "prof", "scholar", "17.12.2015", NA,
  "Jaromír Jirsa", "Jaromír[a-žěščřžýáíéúůň]*\\s*Jirs[a-žěščřžýáíéúůň]*", 1966, "M", "CUNI", "judr", "judge", "7.10.2015", NA,
  "David Uhlíř", "David[a-žěščřžýáíéúůň]*\\s*Uhlíř[a-žěščřžýáíéúůň]*", 1954, "M", "CUNI", "judr", "lawyer", "10.12.2014", "10.12.2024",
  "Tomáš Lichovník", "Tomáš[a-žěščřžýáíéúůň]*\\s*Lichovník[a-žěščřžýáíéúůň]*", 1964, "M", "MUNI", "judr", "judge", "19.6.2014", "19.6.2024",
  "Vojtěch Šimíček", "Vojtěch[a-žěščřžýáíéúůň]*\\s*Šimíč[a-žěščřžýáíéúůň]*", 1969, "M", "MUNI", "doc", "judge", "12.6.2014", "12.6.2024",
  "Jiří Zemánek", "Jiří[a-žěščřžýáíéúůň]*\\s*Zemán[a-žěščřžýáíéúůň]*", 1950, "M", "CUNI", "prof", "scholar", "20.1.2014", "20.1.2024",
  "Radovan Suchánek", "Radovan[a-žěščřžýáíéúůň]*\\s*Suchán[a-žěščřžýáíéúůň]*", 1972, "M", "CUNI", "phd", "scholar", "26.11.2013", "26.11.2023",
  "Ludvík David", "Ludvík[a-žěščřžýáíéúůň]*\\s*David[a-žěščřžýáíéúůň]*", 1951, "M", "MUNI", "judr", "judge", "7.8.2013", "7.8.2023",
  "Vladimír Sládeček", "Vladimír[a-žěščřžýáíéúůň]*\\s*Sládeč[a-žěščřžýáíéúůň]*", 1954, "M", "CUNI", "prof", "scholar", "4.6.2013", "4.6.2023",
  "Jan Filip", "Jan[a-žěščřžýáíéúůň]*\\s* Filip[a-žěščřžýáíéúůň]*", 1950, "M", "MUNI", "prof", "scholar", "3.5.2013", "3.5.2023",
  "Jaroslav Fenyk", "Jaroslav[a-žěščřžýáíéúůň]*\\s*Fenyk[a-žěščřžýáíéúůň]*", 1961, "M", "CUNI", "prof", "scholar", "3.5.2013", "3.5.2023",
  "Milada Tomková", "Milad[a-žěščřžýáíéúůň]*\\s*Tomkov[a-žěščřžýáíéúůň]*", 1959, "F", "CUNI", "judr", "judge", "3.5.2013", "3.5.2023",
  "Pavel Rychetský", "Pav[a-žěščřžýáíéúůň]*\\s*Rychetsk[a-žěščřžýáíéúůň]*", 1943, "M", "CUNI", "judr", c("politician","judge"), c("6.8.2003", "7.8.2013"), c("6.8.2013", "7.8.2023"),
  "Jan Svatoň", "Jan[a-žěščřžýáíéúůň]*\\s*Svato[a-žěščřžýáíéúůň]*", 1952, "M", "MUNI", "doc", "scholar", "15.2.2023", NA,
  "Josef Baxa", "Josef[a-žěščřžýáíéúůň]*\\s*Bax[a-žěščřžýáíéúůň]*", 1959, "M", "CUNI", "judr", "judge", "5.6.2023", NA,
  "Jan Wintr", "Jan[a-žěščřžýáíéúůň]*\\s*Wintr[a-žěščřžýáíéúůň]*", 1978, "M", "CUNI", "prof", "scholar", "5.6.2023", NA,
  "Daniela Zemanová", "Daniel[a-žěščřžýáíéúůň]*\\s*Zeman[a-žěščřžýáíéúůň]*", 1971, "F", "CUNI", "mgr", "judge", "5.6.2023", NA,
  "Kateřina Ronovská", "Kate[a-žěščřžýáíéúůň]*\\s*Ronovs[a-žěščřžýáíéúůň]*", 1974, "F", "MUNI", "prof", "scholar", "4.8.2023", NA,
  "Veronika Křesťanová", "Veroni[a-žěščřžýáíéúůň]*\\s*Křesťan[a-žěščřžýáíéúůň]*", 1969, "F", "CUNI", "phd", "judge", "8.8.2023", NA,
  "Lucie Dolanská Bányaiová", "Luci[a-žěščřžýáíéúůň]* (Dola)*[a-žěščřžýáíéúůň]* Bányai[a-žěščřžýáíéúůň]*", 1974, "F", "CUNI", "phd", "lawyer", "19.12.2023", NA,
  "Zdeněk Kühn", "Zde[a-žěščřžýáíéúůň]* K[a-žěščřžýáíéúůň]hn[a-žěščřžýáíéúůň]*", 1973, "M", "CUNI", "prof", "judge", "19.12.2023", NA,
  "Milan Hulmák", "Milan[a-žěščřžýáíéúůň]*\\s*Hulmák[a-žěščřžýáíéúůň]*", 1975, "M", "UPOL", "doc", "lawyer", "1.2.2024", NA,
  "Tomáš Langášek", "Tomáš[a-žěščřžýáíéúůň]*\\s*Langáš[a-žěščřžýáíéúůň]*", 1974, "M", "CUNI", "judr", "judge", "25.6.2024", NA,
  "Jiří Přibáň", "Jiří[a-žěščřžýáíéúůň]*\\s*Přibáň[a-žěščřžýáíéúůň]*", 1967, "M", "CUNI", "prof", "scholar", "25.6.2024", NA,
  "Dita Řepková", "Dit[a-žěščřžýáíéúůň]*\\s*Řepkov[a-žěščřžýáíéúůň]*", 1975, "F", "MUNI", "phd", "judge", "12.12.2024", NA
) |>
  rowwise() |>
  mutate(judge_id = paste0("J:",cur_group_id())) |>
  ungroup() |>
  unnest(c(judge_term_start, judge_term_end, judge_profession)) |>
  mutate(across(c(judge_term_start,judge_term_end), ~as_date(x = ., format = "%d.%m.%Y"))) |>
  mutate(judge_term_court = case_when(year(judge_term_start) < 1995 ~ "1st",
                               year(judge_term_start) < 2010 ~ "2nd",
                               year(judge_term_start) < 2021 ~ "3rd",
                               year(judge_term_start) > 2022 ~ "4th"),
         judge_term_president = case_when(judge_term_court == "1st" ~ "Václav Havel",
                                          judge_term_court == "2nd" ~ "Václav Klaus",
                                          judge_term_court == "3rd" ~ "Miloš Zeman",
                                          judge_term_court == "4th" ~ "Petr Pavel"),
         judge_gender = factor(judge_gender),
         judge_uni = factor(judge_uni),
         judge_degree = factor(judge_degree, 
                            ordered = TRUE,
                            levels = c("mgr", "judr", "phd", "doc", "prof")),
         judge_reelection = if_else(judge_name %in% c("Pavel Rychetský", "Jiří Nykodým", "Ivana Janů", "Jan Musil", "Pavel Holländer", "Vojen Güttler", "Miloslav Výborný"), true = 1, false = 0),
         judge_term_end = case_when(is.na(judge_term_end) ~ judge_term_start %m+% years(10),
                                    .default = judge_term_end)) |>
  relocate(judge_id) |>
  mutate(judge_initials = paste0(substring(word(judge_name, 1), 1, 1), ".\\s*", substring(word(judge_name, 2), 1, 1), "."))

if(write == TRUE) write_rds(data_judges, file = "../data/ccc_database/rds/ccc_judges.rds")
