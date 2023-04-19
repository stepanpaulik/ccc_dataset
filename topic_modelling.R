xfun::pkg_attach2("tidyverse", "tidytext", "ggplot2", "quanteda", "quanteda.textmodels", "ldatuning", "reshape2", "seededlda")

load(file = "models/US_UDmodel.RData")
load(file = "models/US_LDA.RData")
save(US_LDA, file = "models/US_LDA.RData")
save(US_dfm, file = "models/US_dfm.RData")

text_corpus <- data_ud %>% filter(upos %in% c("NOUN", "ADJ")) %>% select(doc_id, paragraph_id, lemma) %>% group_by(doc_id) %>% summarise(text = paste(lemma, collapse = " "))

# Topic modelling

# Stopwords
# Make a list of procedural words to remove
to_remove <- c(
  "člán[a-ž]*", "soud[a-ž]*", "odstav[a-ž]*", "rozsud[a-ž]*", "případ[a-ž]*",
  "řízení", "odvolání", "stížnost[a-ž]*",
  "stěžovatel[a-ž]*", "zákon[a-ž]*", "ústavní[a-ž]*", "návrh[a-ž]*", "odůvodnění[a-ž]*", "náležitost[a-ž]*", "česk[a-ž]*", "navrhovatel[a-ž]*"
)

# Creating a dtm/dfm multiple ways---

# Quanteda
quanteda_corpus <- text_corpus %>% corpus(
  docid_field = "doc_id",
  text_field = "text",
  unique_docnames = FALSE
)

tokens_US <- tokens(quanteda_corpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>% tokens_select(pattern = to_remove, valuetype = "regex", selection = "remove", min_nchar=2L)
US_dfm <- dfm(tokens_US) %>% dfm_trim(min_termfreq = 5)

# The tidy text way

# Create tidy text corpus
tidy_text_corpus <- text_corpus %>%
  group_by(
    doc_id, paragraph_id
  ) %>%
  unnest_tokens(
    output = "word",
    input = paragraph,
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

to_remove <- tibble(word = to_remove)

# Remove words
tidy_text_corpus <- tidy_text_corpus %>%
  anti_join(
    to_remove,
    by = "word"
  )

# Document feature matrix ------------------------------------------------------

# Create a dfm
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
  dfm_trim(min_termfreq = 5)

# Check the dimensions
dim(dfm_trimmed)


# LDA 
dim(US_dfm)

dtm_US <- US_dfm %>% convert(to = "topicmodels")

result2 <- ldatuning::FindTopicsNumber(
  US_dfm,
  topics = seq(from = 23, to = 26, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result2)


# number of topics
K <- 24
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
US_LDA <- textmodel_lda(US_dfm, K)

terms(US_LDA)
str(US_LDA)
tmResult <- posterior(US_LDA)
nTerms(DTM)


# Create topicnames out of the top 5 words defining them
topicNames <- apply(terms(US_LDA, 5), 2, paste, collapse=" ")

# The distribution of topics among individual documents
theta <- US_LDA$theta
dim(theta)

#You can then obtain the most likely topics using topics() and save them as a document-level variable.
head(topics(US_LDA), 20)


topicProportionExamples <- theta[exampleIds,]

# See examples distribution
exampleIds <- c(2, 100, 200)
lapply(quanteda_corpus[exampleIds], as.character)

N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)


topicModel <- topicmodels::LDA(dfm_trimmed, K, method="Gibbs", control=list(iter = 500, verbose = 25))