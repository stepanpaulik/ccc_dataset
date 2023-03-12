xfun::pkg_attach2("tidyverse", "tidytext", "ggplot2", "quanteda", "quanteda.textmodels", "quanteda.textmodels", "progress", "plm", "lmtest", "tm", "foreach")

# Load data
load("data/US_texts.RData")
load("data/US_metadata.RData")

# Save data
save(data_metadata, file = "data/US_metadata.RData")
save(data_texts, file = "data/US_texts.RData")


# Dissent extraction ------------------------------------------------------
judges_US <- c("Pavel Rychetský", "Milada Tomková", "Jaroslav Fenyk", "Jan Filip", "Vladimír Sládeček", "Ludvík David", "Radovan Suchánek", "Jiří Zemánek", "Vojtěch Šimíček", "Tomáš Lichovník", "David Uhlíř", "Jaromír Jirsa", "Josef Fiala", "Pavel Šámal", "Kateřina Šimáčková", "Jan Musil", "Vladimír Kůrka", "Vlasta Formánková", "Ivana Janů", "Michaela Židlická", "Stanislav Balík", "Jiří Nykodým", "Dagmar Lastovecká", "Pavel Holländer", "Vojen Güttler", "Miloslav Výborný", "Jiří Mucha", "František Duchoň", "Eliška Wagnerová", "Jiří Malenovský", "Pavel Varvařovský", "Eva Zarembová", "Vlastimil Ševčík", "Antonín Procházka", "Vladimír Paul", "Vladimír Klokočka", "Zdeněk Kessler", "Vladimír Jurka", "Miloš Holeček", "Vladimír Čermák", "Vojtěch Cepl", "Iva Brožová") %>% unique()
judges_US_lemma <- c("Pav(el|la|em) Rychetsk(ý|ého|m)", "Milad(y|a|ou) Tomkov(é|á|ou)", "Jaroslav(|a|em) Fenyk(|a|em)", "Jan(|a|em) Filip(|a|em)", "Vladimír(|a|em) Sládeč(ek|ka|em|ou)", "Ludvík(|a|em) David(|a|em)", "Radovan(|a|em) Suchán(ka|ek|em)", "Jiří(|ho|m) Zemán(ka|ek|kem)", "Vojtěch(|a|em) Šimíč(ka|ek|ou|em)", "Tomáš(|e|em) Lichovník(|a|em)", "David(|a|em) Uhlíř(|e|em)", "Jaromír(|a|em) Jirs(a|y|ou)", "Josef(|a|em) Fial(a|y|ou)", "Pav(el|la|em) Šámal(|a|em)", "Kateřin(a|y|ou) Šimáčkov(é|á|ou)", "Jan(|a|em) Musil(|a|em)", "Vladimír(|a|em) Kůrk(y|a|ou)", "Vlast(y|a|ou) Formánkov(é|á|ou)", "Ivan(y|a|ou) Janů", "Michael(a|y|ou) Židlick(é|á|ou)", "Stanislav(|a|em) Balík(|a|em)", "Jiří(|ho|m) Nykodým(|a|em)", "Dagmar Lastoveck(é|á|ou)", "Pav(la|el|em) Holländer(|a|em)", "Vojen(|a|em) G(ü|ű|u|ú)t(|t)ler(|a|em)", "Miloslav(|a|em) Výborn(ý|ého|m)", "Jiří(|ho|m) Much(a|y|ou)", "Františ(ek|ka|kem) Ducho(ň|ně|něm)", "Elišk(a|y|ou) W(a|á)gner(ová|ové|ou)", "Jiří(|ho|m) Malenovsk(ý|ého|m)", "Pav(el|la|em) Varvařovsk(ý|ého|ým)", "Ev(a|y|ou) Zarembov(á|é|ou)", "Vlastimil(|a|em) Ševčík(|a|em)", "Antonín(|a|em) Procházk(a|y|ou)", "Vladimír(|a|em) Paul(|a|em)", "Vladimír(|a|em) Klokočk(a|y|ou)", "Zde(něk|ňka|ňkem) Kessler(|a|em)", "Vladimír(|a|em) Jur(ek|ky|ka|kou)", "Miloš(|e|em) Holeč(ek|ky|ka|em|ou)", "Vladimír(|a|em) Čermák(|a|em)", "Vojtěch(|a|em) Cepl(|a|em)", "Iv(a|y|ou) Brožov(á|é|ou)") %>% unique()
data_dissents <- c()

# Switch the name order of judge rapporteur
judges_US <- foreach(i = 1:length(judges_US), .combine = "c") %do% {
  paste0(word(judges_US[i], 2), " ", word(judges_US[i], 1))
}

# Create function for extracting dissents, returns the long format
get_dissents <- function(data, judges) {
  pb <- progress_bar$new(
    format = "  extracting dissents [:bar] :percent eta: :eta",
    total = length(data$doc_id)*length(judges), clear = FALSE, width= 60)
  
  data_dissents <- foreach(i = seq(data$doc_id), .combine = "rbind") %:% 
    foreach(j = seq(judges), .combine = "rbind") %do% {
      pb$tick()
      if (grepl(judges[j], data[i,"dissenting_opinion"], ignore.case = TRUE)) {
        list("doc_id" = as.character(data$doc_id[i]),
             "dissenting_judge" = as.character(judges[j]))
      }
  } %>% as.data.frame(row.names = FALSE)
  return(data_dissents)
}

# Run the function and save the data
data_dissents <- get_dissents(data_metadata, judges_US)
save(data_dissents, file = "data/US_dissents.RData")
load("data/US_dissents.RData")

# REGRESSION
# Dissent/caseload regression
data_yearly_caseload <- data_metadata %>% group_by(year_cc, judge_rapporteur) %>% summarize(count = n())

data_yearly_dissents <- data_metadata %>% 
  select(doc_id, year_cc) %>%
  left_join(data_dissents, .) %>%
  group_by(year_cc, dissenting_judge) %>%
  summarize(count_dissents = n())

data_dissents_caseload <- left_join(data_yearly_caseload, data_yearly_dissents, by = c("year_cc", "judge_rapporteur" = "dissenting_judge")) %>% mutate_all(~replace(., is.na(.), 0)) %>% filter(year_cc>2013)


fe_mod <- plm(count_dissents ~ count, 
              data = data_dissents_caseload,
              index = c("judge_rapporteur", "year_cc"), 
              model = "within")

coeftest(fe_mod, vcov. = vcovHC, type = "HC1")


# MODELS
# Wordfish model ---------------------------------------------------------------
# Filter relevant decisions
text_corpus <- data_metadata %>% 
  filter(year_cc > 2013 & grepl("Plenum", .$formation)) %>% 
  select(doc_id, judge_rapporteur) %>%
  left_join(.,data_texts)
  
# Remove data to free up RAM
remove(data_metadata, data_texts)

# Create tidy text corpus
tidy_text_corpus <- text_corpus %>%
  group_by(
    doc_id, judge_rapporteur
  ) %>%
  unnest_tokens(
    output = "word",
    input = texts,
    token = "words",
    to_lower = TRUE
  )

# Remove stop words, short words, numbers, and words with punctuation
tidy_text_corpus <- tidy_text_corpus %>%
  anti_join(
    get_stopwords(),
    by = "word"
  ) %>%
  filter(
    !str_detect(word, "[0-9]")
  ) %>%
  filter(
    str_length(word) >= 2
  ) %>%
  filter(
    !str_detect(word, "[[:punct:]]")
  )

# Make a list of procedural words to remove
to_remove <- tibble(
  word = c(
    "člán(ky|ek|ku)", "soud(u|y)", "odstav(ec|ce|ců|ci)", "rozsud(ek|ku|ky|ků)", "případ(|u|y|ů)",
    "řízení", "odvolání", "stížnost",
    "stěžovatel(ka|ky|é|e)", "zákon(|u|y)", "ústavní"
  )
)

# Remove words
tidy_text_corpus <- tidy_text_corpus %>%
  anti_join(
    to_remove,
    by = "word"
  )

# Document feature matrix ------------------------------------------------------

# Create a DFM
dfm <- tidy_text_corpus %>%
  group_by(word, doc_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  cast_dfm(
    document = doc_id,
    term = word,
    value = count
  )

# Check the dimensions
dim(dfm)

# Trim the DF
dfm_trimmed <- dfm %>%
  dfm_trim(min_termfreq = 10)

# Check the dimensions
dim(dfm_trimmed)

# Wordfish model ---------------------------------------------------------------

# Estimate model
wordfish_model <- textmodel_wordfish(dfm_trimmed)
save(wordfish_model, file = "data/wordfish_model.RData")
load("data/wordfish_model.RData")

# There are 4 parameters
# theta = estimated document positions
# beta = estimated word positions
# alpha = estimated document fixed effect (some documents are just longer than others)
# psi = estimated word fixed effect (some words are just more common)

## Interpret word estimates ----------------------------------------------------

# List of procedure-oriented words
procedural_words <- c(
  "appeal", "appellant", "unsuccessful", "error", "plea", "pleas", "unfounded",
  "erred", "annulment", "infringement", "arguments", "alleging"
)

# List of policy-oriented words
policy_words <- c(
  "worker", "workers", "social", "pension", "taxation", "income", "credit",
  "employ", "contract", "person", "work", "tax", "service", "insurance",
  "consumer", "employee", "citizen", "passenger", "passengers", "loan",
  "family", "child", "residence"
)

# Make a tibble with the word parameters
word_estimates <- tibble(
  word = colnames(dfm_trimmed),
  position = wordfish_model$beta,
  fixed_effect = wordfish_model$psi,
)

# Word type
word_estimates <- word_estimates %>%
  mutate(
    word_type = case_when(
      word %in% procedural_words ~ "Procedural word",
      word %in% policy_words ~ "Policy word",
      TRUE ~ "Other"
    ),
    word_type = factor(
      word_type,
      levels = c("Policy word", "Procedural word", "Other")
    )
  )

# We can graph these to interpret the latent dimension
plot <- ggplot(word_estimates, aes(x = position, y = fixed_effect, label = word)) +
  geom_text(alpha = 0.7) +
  geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_color_manual(values = c("#3498db", "#2ecc71", "gray80"), name = NULL) +
  scale_size_manual(values = c(4, 4, 2.5), guide = "none") +
  ggtitle("Wordfish estimates for the positions of words in CJEU judgments (2019-2021)") +
  ylab("Fixed effect") +
  xlab("Word position") +
  theme_minimal()

top <- top_n(word_estimates, -50, wt = position)

# Save plot
ggsave(plot, filename = "plots/word_positions.png", device = "png", width = 10, height = 10, scale = 1)

## Interpret document estimates ------------------------------------------------

# Create a table of document estimates
document_estimates <- tibble(
  doc_id = wordfish_model$docs,
  position = wordfish_model$theta
)

# Merge in judge rapporteur
document_estimates <- document_estimates %>%
  left_join(
    data_metadata %>%
      select(
        doc_id, judge_rapporteur
      ),
    by = "doc_id"
  )

# Collapse by judge
judge_estimates <- document_estimates %>%
  group_by(judge_rapporteur) %>%
  summarize(
    position = mean(position),
    count = n()
  ) %>%
  ungroup() %>%
  mutate(
    judge_rapporteur = judge_rapporteur %>%
      factor() %>%
      fct_reorder(position)
  ) %>%
  filter(
    count >= 10
  )

# Plot average positions by judge-rapporteur
plot <- ggplot(judge_estimates, aes(x = position, y = judge_rapporteur, size = count)) +
  geom_point(color = "#3498db") +
  geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
  scale_size_continuous(range = c(1, 4), name = "Number of judgments") +
  ggtitle("Wordfish estimates for the positions CJEU judges (2019-2021)") +
  ylab(NULL) +
  xlab("Average judge-rapporteur position") +
  theme_minimal()

# Save plot
ggsave(plot, filename = "plots/judge_positions.png", device = "png", width = 10, height = 10, scale = 1)