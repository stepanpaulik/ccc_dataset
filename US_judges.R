library(tidyverse)

#Save and load data
US_judges = readRDS("../data/US_judges.rds")
saveRDS(US_judges, file = "../data/US_judges.rds")

US_judges = list()


# Judges
US_judges[[1]] = list(
  "judge_id" = "J:001",
  "judge_name" = "Pavel Rychetský",
  "yob" = 1943,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "politician",
  "start" = "6.8.2003",
  "end" = NA
)

US_judges[[2]] = list(
  "judge_id" = "J:002",
  "judge_name" = "Milada Tomková",
  "yob" = 1959,
  "gender" = "F",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "3.5.2013",
  "end" = NA
)

US_judges[[3]] = list(
  "judge_id" = "J:003",
  "judge_name" = "Jaroslav Fenyk",
  "yob" = 1961,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "3.5.2012",
  "end" = NA
)

US_judges[[4]] = list(
  "judge_id" = "J:004",
  "judge_name" = "Jan Filip",
  "yob" = 1950,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "3.5.2013",
  "end" = NA
)

US_judges[[5]] = list(
  "judge_id" = "J:005",
  "judge_name" = "Vladimír Sládeček",
  "yob" = 1954,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "4.6.2023",
  "end" = NA
)

US_judges[[6]] = list(
  "judge_id" = "J:006",
  "judge_name" = "Ludvík David",
  "yob" = 1951,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "7.8.2013",
  "end" = NA
)

US_judges[[7]] = list(
  "judge_id" = "J:007",
  "judge_name" = "Radovan Suchánek",
  "yob" = 1972,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "phd",
  "profession" = "scholar",
  "start" = "26.11.2013",
  "end" = NA
)

US_judges[[8]] = list(
  "judge_id" = "J:008",
  "judge_name" = "Jiří Zemánek",
  "yob" = 1950,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "scholar",
  "start" = "20.1.2014",
  "end" = NA
)

US_judges[[9]] = list(
  "judge_id" = "J:009",
  "judge_name" = "Vojtěch Šimíček",
  "yob" = 1969,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "doc",
  "profession" = "judge",
  "start" = "12.6.2014",
  "end" = NA
)

US_judges[[10]] = list(
  "judge_id" = "J:010",
  "judge_name" = "Tomáš Lichovník",
  "yob" = 1964,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "19.6.2014",
  "end" = NA
)

US_judges[[11]] = list(
  "judge_id" = "J:011",
  "judge_name" = "David Uhlíř",
  "yob" = 1954,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "lawyer",
  "start" = "10.12.2014",
  "end" = NA
)

US_judges[[12]] = list(
  "judge_id" = "J:012",
  "judge_name" = "Jaromír Jirsa",
  "yob" = 1966,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "7.10.2015",
  "end" = NA
)

US_judges[[13]] = list(
  "judge_id" = "J:013",
  "judge_name" = "Josef Fiala",
  "yob" = 1953,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "17.12.2015",
  "end" = NA
)

US_judges[[14]] = list(
  "judge_id" = "J:014",
  "judge_name" = "Pavel Šámal",
  "yob" = 1953,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "prof",
  "profession" = "judge",
  "start" = "20.2.2020",
  "end" = NA
)

US_judges[[15]] = list(
  "judge_id" = "J:015",
  "judge_name" = "Kateřina Šimáčková",
  "yob" = 1966,
  "gender" = "F",
  "uni" = "MUNI",
  "education" = "phd",
  "profession" = "judge",
  "start" = "7.8.2013",
  "end" = "10.12.2021"
)

US_judges[[16]] = list(
  "judge_id" = "J:016",
  "judge_name" = "Jan Musil",
  "yob" = 1941,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "27.11.2003",
  "end" = "31.1.2019"
)

US_judges[[17]] = list(
  "judge_id" = "J:017",
  "judge_name" = "Vladimír Kůrka",
  "yob" = 1948,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "15.12.2005",
  "end" = "15.12.2015"
)

US_judges[[18]] = list(
  "judge_id" = "J:018",
  "judge_name" = "Vlasta Formánková",
  "yob" = 1953,
  "gender" = "F",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "5.8.2005",
  "end" = "5.8.2015"
)

US_judges[[19]] = list(
  "judge_id" = "J:019",
  "judge_name" = "Ivana Janů",
  "yob" = 1946,
  "gender" = "F",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "politician",
  "start" = "9.11.1993",
  "end" = "16.9.2014"
)

US_judges[[20]] = list(
  "judge_id" = "J:020",
  "judge_name" = "Michaela Židlická",
  "yob" = 1952,
  "gender" = "F",
  "uni" = "MUNI",
  "education" = "doc",
  "profession" = "scholar",
  "start" = "16.6.2004",
  "end" = "16.6.2014"
)

US_judges[[21]] = list(
  "judge_id" = "J:021",
  "judge_name" = "Stanislav Balík",
  "yob" = 1956,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "phd",
  "profession" = "lawyer",
  "start" = "26.5.2004",
  "end" = "26.5.2014"
)

US_judges[[22]] = list(
  "judge_id" = "J:022",
  "judge_name" = "Jiří Nykodým",
  "yob" = 1945,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "lawyer",
  "start" = "17.12.2003",
  "end" = "17.12.2013"
)

US_judges[[23]] = list(
  "judge_id" = "J:023",
  "judge_name" = "Dagmar Lastovecká",
  "yob" = 1951,
  "gender" = "F",
  "uni" = "MUNI",
  "education" = "judr",
  "profession" = "politician",
  "start" = "29.8.2003",
  "end" = "29.8.2013"
)

US_judges[[24]] = list(
  "judge_id" = "J:024",
  "judge_name" = "Pavel Holländer",
  "yob" = 1953,
  "gender" = "M",
  "uni" = "KOM",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "15.7.1993",
  "end" = "6.8.2013"
)

US_judges[[25]] = list(
  "judge_id" = "J:025",
  "judge_name" = "Vojen Güttler",
  "yob" = 1934,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "politician",
  "start" = "15.7.1993",
  "end" = "6.8.2013"
)

US_judges[[26]] = list(
  "judge_id" = "J:026",
  "judge_name" = "Miloslav Výborný",
  "yob" = 1952,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "politician",
  "start" = "3.6.2003",
  "end" = "3.6.2013"
)

US_judges[[27]] = list(
  "judge_id" = "J:027",
  "judge_name" = "Jiří Mucha",
  "yob" = 1946,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "lawyer",
  "start" = "28.1.2003",
  "end" = "28.1.2013"
)

US_judges[[28]] = list(
  "judge_id" = "J:028",
  "judge_name" = "František Duchoň",
  "yob" = 1946,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "6.6.2002",
  "end" = "6.6.2012"
)

US_judges[[29]] = list(
  "judge_id" = "J:029",
  "judge_name" = "Eliška Wagnerová",
  "yob" = 1948,
  "gender" = "F",
  "uni" = "CUNI",
  "education" = "phd",
  "profession" = "scholar",
  "start" ="20.3.2002",
  "end" = "20.3.2012"
)

US_judges[[30]] = list(
  "judge_id" = "J:030",
  "judge_name" = "Jiří Malenovský",
  "yob" = 1950,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "4.4.2000",
  "end" = "8.5.2004"
)

US_judges[[31]] = list(
  "judge_id" = "J:031",
  "judge_name" = "Pavel Varvařovský",
  "yob" = 1945,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "29.3.1994",
  "end" = "29.3.2004"
)

US_judges[[32]] = list(
  "judge_id" = "J:032",
  "judge_name" = "Eva Zarembová",
  "yob" = 1946,
  "gender" = "F",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "9.11.1993",
  "end" = "9.11.2003"
)

US_judges[[33]] = list(
  "judge_id" = "J:033",
  "judge_name" = "Vlastimil Ševčík",
  "yob" = 1927,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "15.7.1993",
  "end" = "15.12.2002"
)

US_judges[[34]] = list(
  "judge_id" = "J:034",
  "judge_name" = "Antonín Procházka",
  "yob" = 1927,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "judr",
  "profession" = "lawyer",
  "start" = "15.7.1993",
  "end" = "15.7.2003"
)

US_judges[[35]] = list(
  "judge_id" = "J:035",
  "judge_name" = "Vladimír Paul",
  "yob" = 1924,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "lawyer",
  "start" = "15.7.1993",
  "end" = "3.4.2002"
)

US_judges[[36]] = list(
  "judge_id" = "J:036",
  "judge_name" = "Vladimír Klokočka",
  "yob" = 1929,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "15.7.1993",
  "end" = "15.7.2003"
)

US_judges[[37]] = list(
  "judge_id" = "J:037",
  "judge_name" = "Zdeněk Kessler",
  "yob" = 1929,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "judr",
  "profession" = "politician",
  "start" = "15.7.1993",
  "end" = "12.2.2003"
)

US_judges[[38]] = list(
  "judge_id" = "J:038",
  "judge_name" = "Vladimír Jurka",
  "yob" = 1948,
  "gender" = "M",
  "uni" = "KOM",
  "education" = "judr",
  "profession" = "judge",
  "start" = "15.7.1993",
  "end" = "15.7.2003"
)

US_judges[[39]] = list(
  "judge_id" = "J:039",
  "judge_name" = "Miloš Holeček",
  "yob" = 1950,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "15.7.1993",
  "end" = "15.7.2003"
)

US_judges[[40]] = list(
  "judge_id" = "J:040",
  "judge_name" = "Vladimír Čermák",
  "yob" = 1929,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "15.7.1993",
  "end" = "15.7.2003"
)

US_judges[[41]] = list(
  "judge_id" = "J:041",
  "judge_name" = "Vojtěch Cepl",
  "yob" = 1938,
  "gender" = "M",
  "uni" = "CUNI",
  "education" = "prof",
  "profession" = "scholar",
  "start" = "15.7.1993",
  "end" = "15.7.2003"
)

US_judges[[42]] = list(
  "judge_id" = "J:042",
  "judge_name" = "Iva Brožová",
  "yob" = 1951,
  "gender" = "F",
  "uni" = "MUNI",
  "education" = "judr",
  "profession" = "judge",
  "start" = "15.7.1993",
  "end" = "8.12.1999"
)

US_judges[[43]] = list(
  "judge_id" = "J:043",
  "judge_name" = "Jan Svatoň",
  "yob" = 1952,
  "gender" = "M",
  "uni" = "MUNI",
  "education" = "doc",
  "profession" = "scholar",
  "start" = "15.2.2023",
  "end" = NA
)

US_judges$name_lemmatized = c("Pav[a-ž]*\\s*Rychetsk[a-ž]*", "Milad[a-ž]*\\s*Tomkov[a-ž]*", "Jaroslav[a-ž]*\\s*Fenyk[a-ž]*", "Jan[a-ž]*\\s* Filip[a-ž]*", "Vladimír[a-ž]*\\s*Sládeč[a-ž]*", "Ludvík[a-ž]*\\s*David[a-ž]*", "Radovan[a-ž]*\\s*Suchán[a-ž]*", "Jiří[a-ž]*\\s*Zemán[a-ž]*", "Vojtěch[a-ž]*\\s*Šimíč[a-ž]*", "Tomáš[a-ž]*\\s*Lichovník[a-ž]*", "David[a-ž]*\\s*Uhlíř[a-ž]*", "Jaromír[a-ž]*\\s*Jirs[a-ž]*", "Josef[a-ž]*\\s*Fial[a-ž]*", "Pav[a-ž]*\\s*Šámal[a-ž]*", "Kateřin[a-ž]*\\s*Šimáčkov[a-ž]*", "Jan[a-ž]*\\s*Musil[a-ž]*", "Vladimír[a-ž]*\\s*Kůrk[a-ž]*", "Vlast[a-ž]*\\s*Formánkov[a-ž]*", "Ivan[a-ž]*\\s*Janů", "Michael[a-ž]*\\s*Židlick[a-ž]*", "Stanislav[a-ž]*\\s*Balík[a-ž]*", "Jiří[a-ž]*\\s*Nykodým[a-ž]*", "Dagmar Lastoveck[a-ž]*", "Pav[a-ž]*\\s*Holländer[a-ž]*", "Vojen[a-ž]*\\s*G(ü|ű|u|ú)t(|t)ler[a-ž]*", "Miloslav[a-ž]*\\s*Výborn[a-ž]*", "Jiří[a-ž]*\\s*Much[a-ž]*", "Františ[a-ž]*\\s*Ducho[a-ž]*", "Elišk[a-ž]*\\s*W(a|á)gner[a-ž]*", "Jiří[a-ž]*\\s*Malenovsk[a-ž]*", "Pav[a-ž]*\\s*Varvařovsk[a-ž]*", "Ev[a-ž]*\\s*Zarembov[a-ž]*", "Vlastimil[a-ž]*\\s*Ševčík[a-ž]*", "Antonín[a-ž]*\\s*Procházk[a-ž]*", "Vladimír[a-ž]*\\s*Paul[a-ž]*", "Vladimír[a-ž]*\\s*Klokočk[a-ž]*", "Zde[a-ž]*\\s*Kessler[a-ž]*", "Vladimír[a-ž]*\\s*Jur[a-ž]*", "Miloš[a-ž]*\\s*Holeč[a-ž]*", "Vladimír[a-ž]*\\s*Čermák[a-ž]*", "Vojtěch[a-ž]*\\s*Cepl[a-ž]*", "Iv[a-ž]*\\s*Brožov[a-ž]*", "Jan[a-ž]*\\s*Svatoň[a-ž]*") %>% as.character()

reelected_judges = c("Pavel Rychetský", "Jiří Nykodým", "Ivana Janů", "Jan Musil", "Pavel Holländer", "Vojen Güttler", "Miloslav Výborný")

US_judges = US_judges %>%
  mutate(reelection = if_else(judge_name %in% reelected_judges,1,0))
