xfun::pkg_attach2("tidyverse", "campfin", "foreach", "doParallel", "ggplot2", "progress", "RMySQL", "rapportools", "foreach")

# Load data
load("data/US_compositions.RData")
load("data/US_texts.RData")
load("data/US_metadata.RData")
source("supporting_functions.R")

# Save data
save(data_metadata, file = "data/US_metadata.RData")
save(data_texts, file = "data/US_texts.RData")


# Load judges names
judges_US <- c("Pavel Rychetský", "Milada Tomková", "Jaroslav Fenyk", "Jan Filip", "Vladimír Sládeček", "Ludvík David", "Radovan Suchánek", "Jiří Zemánek", "Vojtěch Šimíček", "Tomáš Lichovník", "David Uhlíř", "Jaromír Jirsa", "Josef Fiala", "Pavel Šámal", "Kateřina Šimáčková", "Jan Musil", "Vladimír Kůrka", "Vlasta Formánková", "Ivana Janů", "Michaela Židlická", "Stanislav Balík", "Jiří Nykodým", "Dagmar Lastovecká", "Pavel Holländer", "Vojen Güttler", "Miloslav Výborný", "Jiří Mucha", "František Duchoň", "Eliška Wagnerová", "Jiří Malenovský", "Pavel Varvařovský", "Eva Zarembová", "Vlastimil Ševčík", "Antonín Procházka", "Vladimír Paul", "Vladimír Klokočka", "Zdeněk Kessler", "Vladimír Jurka", "Miloš Holeček", "Vladimír Čermák", "Vojtěch Cepl", "Iva Brožová") %>% unique()
judges_US_lemma <- c("Pav(el|la|em) Rychetsk(ý|ého|m)", "Milad(y|a|ou) Tomkov(é|á|ou)", "Jaroslav(|a|em) Fenyk(|a|em)", "Jan(|a|em) Filip(|a|em)", "Vladimír(|a|em) Sládeč(ek|ka|em|ou)", "Ludvík(|a|em) David(|a|em)", "Radovan(|a|em) Suchán(ka|ek|em)", "Jiří(|ho|m) Zemán(ka|ek|kem)", "Vojtěch(|a|em) Šimíč(ka|ek|ou|em)", "Tomáš(|e|em) Lichovník(|a|em)", "David(|a|em) Uhlíř(|e|em)", "Jaromír(|a|em) Jirs(a|y|ou)", "Josef(|a|em) Fial(a|y|ou)", "Pav(el|la|em) Šámal(|a|em)", "Kateřin(a|y|ou) Šimáčkov(é|á|ou)", "Jan(|a|em) Musil(|a|em)", "Vladimír(|a|em) Kůrk(y|a|ou)", "Vlast(y|a|ou) Formánkov(é|á|ou)", "Ivan(y|a|ou) Janů", "Michael(a|y|ou) Židlick(é|á|ou)", "Stanislav(|a|em) Balík(|a|em)", "Jiří(|ho|m) Nykodým(|a|em)", "Dagmar Lastoveck(é|á|ou)", "Pav(la|el|em) Holländer(|a|em)", "Vojen(|a|em) G(ü|ű|u|ú)t(|t)ler(|a|em)", "Miloslav(|a|em) Výborn(ý|ého|m)", "Jiří(|ho|m) Much(a|y|ou)", "Františ(ek|ka|kem) Ducho(ň|ně|něm)", "Elišk(a|y|ou) W(a|á)gner(ová|ové|ou)", "Jiří(|ho|m) Malenovsk(ý|ého|m)", "Pav(el|la|em) Varvařovsk(ý|ého|ým)", "Ev(a|y|ou) Zarembov(á|é|ou)", "Vlastimil(|a|em) Ševčík(|a|em)", "Antonín(|a|em) Procházk(a|y|ou)", "Vladimír(|a|em) Paul(|a|em)", "Vladimír(|a|em) Klokočk(a|y|ou)", "Zde(něk|ňka|ňkem) Kessler(|a|em)", "Vladimír(|a|em) Jur(ek|ky|ka|kou)", "Miloš(|e|em) Holeč(ek|ky|ka|em|ou)", "Vladimír(|a|em) Čermák(|a|em)", "Vojtěch(|a|em) Cepl(|a|em)", "Iv(a|y|ou) Brožov(á|é|ou)") %>% unique()

# The mean function
get_compositions <- function(texts, judges_grepl, judges) {
  pb <- progress_bar$new(
    format = "  creating chamber compositions [:bar] :percent eta: :eta",
    total = length(texts$doc_id)*length(judges_grepl), clear = FALSE, width= 60)
  
  judges <- switch_names(judges = judges_US)
  
  data_compositions <- foreach(i = seq(texts$doc_id), .combine = "rbind") %:% 
    foreach(j = seq(judges_grepl), .combine = "rbind") %do% {
      pb$tick()
      if (grepl(judges_grepl[j], texts[i,2], ignore.case = TRUE)) {
        list("doc_id" = as.character(texts$doc_id[i]),
             "judge" = as.character(judges[j]))
      }
    } %>% as.data.frame(row.names = NULL)
  return(data_compositions)
}

#Functions call
remove(data_compositions)
data_compositions <- get_compositions(texts = data_texts, judges_US_lemma, judges_US)
data_compositions <- remove_procedural(data = data_compositions, data_metadata = data_metadata)


# Data check
data_compositions_check <- data_compositions %>% group_by(doc_id) %>% summarise(count = n())
# Visualization
ggplot(data = data_compositions, mapping = aes(x = count)) +
  geom_bar(position = "dodge")

# Create a sample
sample <- list()
sample$four <- data_compositions_check %>% filter(count == 4) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$one <- data_compositions_check %>% filter(count == 1) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample$five <- data_compositions_check %>% filter(count == 5) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")


# SQL commmunication
conn <- dbConnect(
  RMySQL::MySQL(),
  dbname = "dataset_apexcourts",
  username = "root",
  password = "4E5ad7d!",
  host = "localhost",
  port = 3306
)

query <- paste0("CREATE TABLE compositions (
    doc_id VARCHAR(40) PRIMARY KEY,
                ",
    paste0(colnames(data_compositions[2:length(data_compositions)])," INT,
           ", collapse=""), 
    "
    FOREIGN KEY (doc_id) REFERENCES metadata(doc_id));"
)
query

dbSendQuery(conn, query)

dbWriteTable(
  conn,
  "compositions",
  data_compositions,
  overwrite = TRUE,
  row.names = FALSE
)

dbWriteTable(
  conn,
  "metadata",
  data_metadata,
  overwrite = TRUE,
  row.names = FALSE
)

data_data_compositions <- dbReadTable(
  conn,
  "compositions"
)

