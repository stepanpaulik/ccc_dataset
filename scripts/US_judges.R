library(tidyverse)

data_judges <- tribble(
  ~judge_name, ~judge_name_lemmatized, ~yob, ~gender, ~uni, ~education, ~profession, ~start, ~end,
  "Iva Brožová", "Iv[a-ž]*\\s*Brožov[a-ž]*", 1951, "F", "MUNI", "judr", "judge", "15.7.1993", "8.12.1999",
  "Vojtěch Cepl", "Vojtěch[a-ž]*\\s*Cepl[a-ž]*", 1938, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Vladimír Čermák", "Vladimír[a-ž]*\\s*Čermák[a-ž]*", 1929, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Miloš Holeček", "Miloš[a-ž]*\\s*Holeč[a-ž]*", 1950, "M", "CUNI", "judr", "judge", "15.7.1993", "15.7.2003",
  "Vladimír Jurka", "Vladimír[a-ž]*\\s*Jur[a-ž]*", 1948, "M", "KOM", "judr", "judge", "15.7.1993", "15.7.2003",
  "Zdeněk Kessler", "Zde[a-ž]*\\s*Kessler[a-ž]*", 1929, "M", "MUNI", "judr", "politician", "15.7.1993", "12.2.2003",
  "Vladimír Klokočka", "Vladimír[a-ž]*\\s*Klokočk[a-ž]*", 1929, "M", "CUNI", "prof", "scholar", "15.7.1993", "15.7.2003",
  "Vladimír Paul", "Vladimír[a-ž]*\\s*Paul[a-ž]*", 1924, "M", "CUNI", "judr", "lawyer", "15.7.1993", "3.4.2002",
  "Antonín Procházka", "Antonín[a-ž]*\\s*Procházk[a-ž]*", 1927, "M", "MUNI", "judr", "lawyer", "15.7.1993", "15.7.2003",
  "Vlastimil Ševčík", "Vlastimil[a-ž]*\\s*Ševčík[a-ž]*", 1927, "M", "MUNI", "judr", "judge", "15.7.1993", "15.12.2002",
  "Eva Zarembová", "Ev[a-ž]*\\s*Zarembov[a-ž]*", 1946, "F", "CUNI", "judr", "judge", "9.11.1993", "9.11.2003",
  "Pavel Varvařovský", "Pav[a-ž]*\\s*Varvařovsk[a-ž]*", 1945, "M", "CUNI", "judr", "judge", "29.3.1994", "29.3.2004",
  "Jiří Malenovský", "Jiří[a-ž]*\\s*Malenovsk[a-ž]*", 1950, "M", "MUNI", "prof", "scholar", "4.4.2000", "8.5.2004",
  "Eliška Wagnerová", "Elišk[a-ž]*\\s*W(a|á)gner[a-ž]*", 1948, "F", "CUNI", "phd", "scholar", "20.3.2002", "20.3.2012",
  "František Duchoň", "Františ[a-ž]*\\s*Ducho[a-ž]*", 1946, "M", "CUNI", "judr", "judge", "6.6.2002", "6.6.2012",
  "Dagmar Lastovecká", "Dagmar Lastoveck[a-ž]*", 1951, "F", "MUNI", "judr", "politician", "29.8.2003", "29.8.2013",
  "Pavel Holländer", "Pav[a-ž]*\\s*Holländer[a-ž]*", 1953, "M", "KOM", "prof", "scholar", "15.7.1993", "6.8.2013",
  "Vojen Güttler", "Vojen[a-ž]*\\s*G(ü|ű|u|ú)t(|t)ler[a-ž]*", 1934, "M", "CUNI", "judr", "politician", "15.7.1993", "6.8.2013",
  "Miloslav Výborný", "Miloslav[a-ž]*\\s*Výborn[a-ž]*", 1952, "M", "CUNI", "judr", "politician", "3.6.2003", "3.6.2013",
  "Jiří Mucha", "Jiří[a-ž]*\\s*Much[a-ž]*", 1946, "M", "CUNI", "judr", "lawyer", "28.1.2003", "28.1.2013",
  "Jiří Nykodým", "Jiří[a-ž]*\\s*Nykodým[a-ž]*", 1945, "M", "CUNI", "judr", "lawyer", "17.12.2003", "17.12.2013",
  "Stanislav Balík", "Stanislav[a-ž]*\\s*Balík[a-ž]*", 1956, "M", "CUNI", "phd", "lawyer", "26.5.2004", "26.5.2014",
  "Michaela Židlická", "Michael[a-ž]*\\s*Židlick[a-ž]*", 1952, "F", "MUNI", "doc", "scholar", "16.6.2004", "16.6.2014",
  "Ivana Janů", "Ivan[a-ž]*\\s*Janů", 1946, "F", "CUNI", "judr", "politician", c("9.11.1993", "16.9.2004"), c("9.11.2003", "16.9.2014"),
  "Vlasta Formánková", "Vlast[a-ž]*\\s*Formánkov[a-ž]*", 1953, "F", "CUNI", "judr", "judge", "5.8.2005", "5.8.2015",
  "Vladimír Kůrka", "Vladimír[a-ž]*\\s*Kůrk[a-ž]*", 1948, "M", "CUNI", "judr", "judge", "15.12.2005", "15.12.2015",
  "Jan Musil", "Jan[a-ž]*\\s*Musil[a-ž]*", 1941, "M", "CUNI", "prof", "scholar", c("27.11.2003", "20.1.2014"), c("27.11.2013", "31.1.2019"),
  "Kateřina Šimáčková", "Kateřin[a-ž]*\\s*Šimáčkov[a-ž]*", 1966, "F", "MUNI", "phd", "judge", "7.8.2013", "10.12.2021",
  "Pavel Šámal", "Pav[a-ž]*\\s*Šámal[a-ž]*", 1953, "M", "CUNI", "prof", "judge", "20.2.2020", NA,
  "Josef Fiala", "Josef[a-ž]*\\s*Fial[a-ž]*", 1953, "M", "MUNI", "prof", "scholar", "17.12.2015", NA,
  "Jaromír Jirsa", "Jaromír[a-ž]*\\s*Jirs[a-ž]*", 1966, "M", "CUNI", "judr", "judge", "7.10.2015", NA,
  "David Uhlíř", "David[a-ž]*\\s*Uhlíř[a-ž]*", 1954, "M", "CUNI", "judr", "lawyer", "10.12.2014", NA,
  "Tomáš Lichovník", "Tomáš[a-ž]*\\s*Lichovník[a-ž]*", 1964, "M", "MUNI", "judr", "judge", "19.6.2014", NA,
  "Vojtěch Šimíček", "Vojtěch[a-ž]*\\s*Šimíč[a-ž]*", 1969, "M", "MUNI", "doc", "judge", "12.6.2014", NA,
  "Jiří Zemánek", "Jiří[a-ž]*\\s*Zemán[a-ž]*", 1950, "M", "CUNI", "judr", "scholar", "20.1.2014", NA,
  "Radovan Suchánek", "Radovan[a-ž]*\\s*Suchán[a-ž]*", 1972, "M", "CUNI", "phd", "scholar", "26.11.2013", "26.11.2023",
  "Ludvík David", "Ludvík[a-ž]*\\s*David[a-ž]*", 1951, "M", "MUNI", "judr", "judge", "7.8.2013", "7.8.2023",
  "Vladimír Sládeček", "Vladimír[a-ž]*\\s*Sládeč[a-ž]*", 1954, "M", "CUNI", "prof", "scholar", "4.6.2013", "4.6.2023",
  "Jan Filip", "Jan[a-ž]*\\s* Filip[a-ž]*", 1950, "M", "MUNI", "prof", "scholar", "3.5.2013", "3.5.2023",
  "Jaroslav Fenyk", "Jaroslav[a-ž]*\\s*Fenyk[a-ž]*", 1961, "M", "CUNI", "prof", "scholar", "3.5.2013", "3.5.2023",
  "Milada Tomková", "Milad[a-ž]*\\s*Tomkov[a-ž]*", 1959, "F", "CUNI", "judr", "judge", "3.5.2013", "3.5.2023",
  "Pavel Rychetský", "Pav[a-ž]*\\s*Rychetsk[a-ž]*", 1943, "M", "CUNI", "judr", "politician", c("6.8.2003", "7.8.2013"), c("6.8.2013", NA),
  "Jan Svatoň", "Jan[a-ž]*\\s*Svatoň[a-ž]*", 1952, "M", "MUNI", "doc", "scholar", "15.2.2023", NA,
  "Josef Baxa", "Josef[a-ž]*\\s*Bax[a-ž]*", 1959, "M", "CUNI", "judr", "judge", "5.6.2023", NA,
  "Jan Wintr", "Jan[a-ž]*\\s*Wintr[a-ž]*", 1978, "M", "CUNI", "prof", "scholar", "5.6.2023", NA,
  "Daniela Zemanová", "Daniel[a-ž]*\\s*Zeman[a-ž]*", 1971, "F", "CUNI", "mgr", "judge", "5.6.2023", NA,
  "Kateřina Ronovská", "Kate[a-ž]*\\s*Ronovs[a-ž]*", 1974, "F", "MUNI", "prof", "scholar", "4.8.2023", NA,
  "Veronika Křesťanová", "Veroni[a-ž]*\\s*Křesťan[a-ž]*", 1969, "F", "CUNI", "phd", "judge", "8.8.2023", NA
) %>%
  unnest(c(start, end)) %>%
  mutate(across(c(start,end), ~as_date(x = ., format = "%d.%m.%Y"))) %>%
  nest(mandate = c(start,end)) %>%
  rowwise() %>%
  mutate(judge_id = paste0("J:",cur_group_id()),
         gender = factor(gender),
         uni = factor(uni),
         education = factor(education, 
                            ordered = TRUE,
                            levels = c("mgr", "judr", "phd", "doc", "prof"))) %>%
  relocate(judge_id)

write_rds(data_judges, file = "../data/US_judges.rds")

#   
# reelected_judges = c("Pavel Rychetský", "Jiří Nykodým", "Ivana Janů", "Jan Musil", "Pavel Holländer", "Vojen Güttler", "Miloslav Výborný")
# 
# US_judges = US_judges %>%
#   mutate(reelection = if_else(judge_name %in% reelected_judges,1,0))
