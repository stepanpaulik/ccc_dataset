xfun::pkg_attach2("tidyverse", "tidytext", "ggplot2", "quanteda", "quanteda.textmodels", "quanteda.textmodels", "progress", "plm", "lmtest", "tm", "foreach")

# Load data
load("data/US_texts.RData")
load("data/US_metadata.RData")
load("data/US_dissents.RData")
source("supporting_functions.R")

# Save data
# save(data_metadata, file = "data/US_metadata.RData")
# save(data_texts, file = "data/US_texts.RData")

# Create function for extracting dissents, returns the long format
get_dissents <- function(data, judges_names, judges_id) {
  pb <- progress_bar$new(
    format = "  extracting dissents [:bar] :percent eta: :eta",
    total = length(data$doc_id)*length(judges_names), clear = FALSE, width = 60)
  
  judges_switched <- switch_names(judges = judges_names)
  
  data_dissents <- foreach(i = seq(data$doc_id), .combine = "rbind") %:% 
    foreach(j = seq(judges_switched), .combine = "rbind") %do% {
      pb$tick()
      if (grepl(judges_switched[j], data[i,"dissenting_opinion"], ignore.case = TRUE)) {
        output <- list("doc_id" = as.character(data$doc_id[i]),
             "dissenting_judge" = as.character(judges_names[j]),
              "judge_id" = as.character(judges_id[j]))
        return(output)
      }
  } %>% as.data.frame(row.names = FALSE)
  return(data_dissents)
}

# Run the function and save the data
US_dissents <- get_dissents(US_metadata, judges_names = US_judges$judge_name, judges_id = US_judges$judge_id)
save(US_dissents, file = "data/US_dissents.RData")
load("data/US_dissents.RData")

# REGRESSION
# Dissent/caseload regression
US_yearly_caseload <- US_metadata %>% group_by(year_cc, judge_rapporteur) %>% summarize(count = n())

US_yearly_dissents <- US_metadata %>% 
  select(doc_id, year_cc, formation) %>%
  left_join(US_dissents, .) %>%
  group_by(year_cc, dissenting_judge) %>%
  summarize(count_dissents = n()) 


US_dissents_caseload <- left_join(US_yearly_caseload, US_yearly_dissents, by = c("year_cc", "judge_rapporteur" = "dissenting_judge")) %>% mutate_all(~replace(., is.na(.), 0)) %>% filter(year_cc>2013)


fe_mod <- plm(count_dissents ~ count, 
              data = US_dissents_caseload,
              index = c("year_cc"), 
              model = "within")

coeftest(fe_mod, vcov. = vcovHC, type = "HC1")


# MODELS
# Wordfish model ---------------------------------------------------------------
# Filter relevant decisions
text_corpus <- US_metadata %>% 
  filter(year_cc > 2013 & grepl("Plenum", .$formation)) %>% 
  select(doc_id, judge_rapporteur) %>%
  left_join(.,US_texts)
  
# Remove data to free up RAM
remove(US_metadata, US_texts)

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
save(wordfish_model, file = "models/wordfish_model.RData")
load("models/wordfish_model.RData")

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
    US_metadata %>%
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