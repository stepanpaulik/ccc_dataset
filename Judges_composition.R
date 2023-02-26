xfun::pkg_attach2("tidyverse", "campfin", "foreach", "doParallel", "ggplot2", "progress", "RMySQL", "rapportools")
save(data_metadata, file = "data/US_metadata.RData")
save(data_texts, file = "data/US_texts.RData")

# Load data
data_compositions <- read.csv("data/Decisions_US_compositions.csv")
load("data/US_texts.RData")
load("data/US_metadata.RData")



# Swap surnamne and name order in judge_rapporteur
for(i in 1:length(data_metadata$judge_rapporteur)){
  data_metadata$judge_rapporteur[i] <- paste0(word(data_metadata$judge_rapporteur[i], 2), "_", word(data_metadata$judge_rapporteur[i], 1))
}


clean_procedural(data_metadata, data_texts)



# With UDPipe

# # data_ud <- read.csv("data/Decisions_US_UDmodel.csv") # Load UDPipe Model
# 
# first_Nsentence <- function(data, n = 3) {
#   data_texts <- data %>% 
#     group_by(doc_id) %>% 
#     filter(between(sentence_id, 1, n) | between(sentence_id, (max(data$sentence_id) - n), max(data$sentence_id))) %>% 
#     mutate(texts = paste0(token, collapse = " ")) %>% 
#     summarise(unique(texts))
#   return(data_texts)
# }
# 
# data_texts <- first_Nsentence(data_ud)
judges_US <- c("Pavel Rychetský", "Milada Tomková", "Jaroslav Fenyk", "Jan Filip", "Vladimír Sládeček", "Ludvík David", "Radovan Suchánek", "Jiří Zemánek", "Vojtěch Šimíček", "Tomáš Lichovník", "David Uhlíř", "Jaromír Jirsa", "Josef Fiala", "Pavel Šámal", "Kateřina Šimáčková", "Jan Musil", "Vladimír Kůrka", "Vlasta Formánková", "Ivana Janů", "Michaela Židlická", "Stanislav Balík", "Jiří Nykodým", "Dagmar Lastovecká", "Pavel Holländer", "Vojen Güttler", "Miloslav Výborný", "Jiří Mucha", "František Duchoň", "Eliška Wagnerová", "Jiří Malenovský", "Pavel Varvařovský", "Eva Zarembová", "Vlastimil Ševčík", "Antonín Procházka", "Vladimír Paul", "Vladimír Klokočka", "Zdeněk Kessler", "Vladimír Jurka", "Miloš Holeček", "Vladimír Čermák", "Vojtěch Cepl", "Iva Brožová") %>% unique()
judges_US_lemma <- c("Pav(el|la|em) Rychetsk(ý|ého|m)", "Milad(y|a|ou) Tomkov(é|á|ou)", "Jaroslav(|a|em) Fenyk(|a|em)", "Jan(|a|em) Filip(|a|em)", "Vladimír(|a|em) Sládeč(ek|ka|em|ou)", "Ludvík(|a|em) David(|a|em)", "Radovan(|a|em) Suchán(ka|ek|em)", "Jiří(|ho|m) Zemán(ka|ek|kem)", "Vojtěch(|a|em) Šimíč(ka|ek|ou|em)", "Tomáš(|e|em) Lichovník(|a|em)", "David(|a|em) Uhlíř(|e|em)", "Jaromír(|a|em) Jirs(a|y|ou)", "Josef(|a|em) Fial(a|y|ou)", "Pav(el|la|em) Šámal(|a|em)", "Kateřin(a|y|ou) Šimáčkov(é|á|ou)", "Jan(|a|em) Musil(|a|em)", "Vladimír(|a|em) Kůrk(y|a|ou)", "Vlast(y|a|ou) Formánkov(é|á|ou)", "Ivan(y|a|ou) Janů", "Michael(a|y|ou) Židlick(é|á|ou)", "Stanislav(|a|em) Balík(|a|em)", "Jiří(|ho|m) Nykodým(|a|em)", "Dagmar Lastoveck(é|á|ou)", "Pav(la|el|em) Holländer(|a|em)", "Vojen(|a|em) G(ü|ű|u|ú)t(|t)ler(|a|em)", "Miloslav(|a|em) Výborn(ý|ého|m)", "Jiří(|ho|m) Much(a|y|ou)", "Františ(ek|ka|kem) Ducho(ň|ně|něm)", "Elišk(a|y|ou) W(a|á)gner(ová|ové|ou)", "Jiří(|ho|m) Malenovsk(ý|ého|m)", "Pav(el|la|em) Varvařovsk(ý|ého|ým)", "Ev(a|y|ou) Zarembov(á|é|ou)", "Vlastimil(|a|em) Ševčík(|a|em)", "Antonín(|a|em) Procházk(a|y|ou)", "Vladimír(|a|em) Paul(|a|em)", "Vladimír(|a|em) Klokočk(a|y|ou)", "Zde(něk|ňka|ňkem) Kessler(|a|em)", "Vladimír(|a|em) Jur(ek|ky|ka|kou)", "Miloš(|e|em) Holeč(ek|ky|ka|em|ou)", "Vladimír(|a|em) Čermák(|a|em)", "Vojtěch(|a|em) Cepl(|a|em)", "Iv(a|y|ou) Brožov(á|é|ou)") %>% unique()
data_compositions <- c()
get_compositions <- function(texts, judges_grepl, judges_column) {
  pb <- progress_bar$new(
    format = "  creating chamber compositions [:bar] :percent eta: :eta",
    total = length(texts$doc_id), clear = FALSE, width= 60)
  
  data_compositions <- data.frame(matrix(0, ncol = 43, nrow = length(unique(data_texts$doc_id))))
  colnames(data_compositions)[2:43] <- str_replace(str_to_lower(judges_US), " ", "_")
  colnames(data_compositions)[1] <- "doc_id"
  data_compositions$doc_id <- unique(data_texts$doc_id)
  for (i in 1:length(texts$doc_id)) {
    for (j in 1:length(judges_column)) {
      if (grepl(judges_grepl[j], texts[i,2], ignore.case = TRUE)) {
        data_compositions[data_compositions$doc_id == texts$doc_id[i], str_replace(str_to_lower(judges_column[j]), " ", "_")] <- 1
      } else {
        data_compositions[data_compositions$doc_id == texts$doc_id[i], str_replace(str_to_lower(judges_column[j]), " ", "_")] <- 0
      }
    }
    data_compositions[data_compositions$doc_id == texts$doc_id[i], str_to_lower(data_metadata$judge_rapporteur[data_metadata$doc_id == texts$doc_id[i]])] <- 1
    pb$tick()
  }
  return(data_compositions)
}



#Functions call
remove(data_compositions)
data_compositions <- get_compositions(texts = data_texts, judges_US_lemma, judges_US)


# Data check
# Add check column
data_check <- c()
data_compositions <- data_compositions %>% select(-c(data_check))
composition_check <- function(data_compositions){
  data_check <- data_compositions[,2:length(data_compositions)] %>% rowSums()
  data_compositions <- cbind(data_compositions, data_check)
  return(data_compositions)
}
data_compositions <- data_compositions %>% composition_check()

# Create a sample
sample_four <- data_compositions %>% filter(data_check == 4) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample_one <- data_compositions %>% filter(data_check == 1) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")
sample_five <- data_compositions %>% filter(data_check == 5) %>% slice_sample(n = 10) %>% left_join(., data_texts, by = "doc_id")



ggplot(data = data_compositions, mapping = aes(x = data_check)) +
  geom_bar(position = "dodge")


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




write_csv(data_compositions, "data/Decisions_US_compositions.csv")


# #Paralelization of the process
# n.cores <- parallel::detectCores() - 1
# 
# #create the cluster
# my.cluster <- parallel::makeCluster(
#   n.cores, 
#   type = "PSOCK"
# )
# 
# #check cluster definition (optional)
# print(my.cluster)
# 
# #register it to be used by %dopar%
# doParallel::registerDoParallel(cl = my.cluster)
# 
# #check if it is registered (optional)
# foreach::getDoParRegistered()
# 
# get_compositions_par <- function(data_compositions, texts_lemmatized, judges_grepl, judges_column) {
#   foreach(
#     i = 1:length(texts_lemmatized$doc_id) 
#   ) %dopar% {
#     for (j in 1:length(judges_column)) {
#       if (grepl(stringr::word(judges_grepl[j], 1), texts_lemmatized[i,2], ignore.case = TRUE) & grepl(stringr::word(judges_grepl[j], 2), texts_lemmatized[i,2], ignore.case = TRUE)) {
#         data_compositions[data_compositions$doc_id == texts_lemmatized$doc_id[i], str_replace(str_to_lower(judges_column[j]), " ", "_")] <<- 1
#       } else {
#         data_compositions[data_compositions$doc_id == texts_lemmatized$doc_id[i], str_replace(str_to_lower(judges_column[j]), " ", "_")] <<- 0
#       }
#     }
#   }
# }
# 
# get_compositions_par(data_compositions, data_texts_lemmatized, judges_US_lemma, judges_US)



# get_compositions <- function(texts, judges_grepl, judges_column) {
#   data_compositions <- data.frame(matrix(0, ncol = 43, nrow = length(unique(data_texts$doc_id))))
#   colnames(data_compositions)[2:43] <- str_replace(str_to_lower(judges_US), " ", "_")
#   colnames(data_compositions)[1] <- "doc_id"
#   data_compositions$doc_id <<- unique(data_texts$doc_id)
#   for (i in 1:length(texts$doc_id)) {
#     for (j in 1:length(judges_column)) {
#       if (grepl(word(judges_grepl[j], 1), texts[i,2], ignore.case = TRUE) & grepl(word(judges_grepl[j], 2), texts[i,2], ignore.case = TRUE)) {
#         data_compositions[data_compositions$doc_id == texts$doc_id[i], str_replace(str_to_lower(judges_column[j]), " ", "_")] <- 1
#       } else {
#         data_compositions[data_compositions$doc_id == texts$doc_id[i], str_replace(str_to_lower(judges_column[j]), " ", "_")] <- 0
#       }
#     }
#   }
# }



