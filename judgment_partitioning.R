xfun::pkg_attach2("tidyverse", "ggplot2", "progress", "foreach", "jsonlite",  "word2vec", "e1071")

#Load data
source("supporting_functions.R")
load("data/US_texts.RData")
load("data/US_metadata.RData")
load("data/US_texts_paragraphs.RData")
load("data/US_dissents.RData")
load("data/US_judges.RData")

# Create sample for manual tagging
sample <- data_metadata %>% 
  filter(is.empty(dissenting_opinion)) %>% 
  slice_sample(n = 50)
sample_paragraphs <- data_metadata %>% 
  filter(!is.empty(dissenting_opinion)) %>% 
  slice_sample(n = 50) %>% 
  rbind(., sample) %>% 
  select(doc_id) %>% 
  left_join(., US_texts)
write_csv(sample_paragraphs, file = "data/US_sample_annotate.csv")


# DATA PREP
# Split the texts into paragraphs and index them
paragraphs_split <- function(US_texts) {
    # Create temporary object with doc_id + text split up into paragraphs
  data_paragraphs_temp <- US_texts %>% group_by(doc_id) %>% summarise(paragraphs = str_split(text, pattern = "\n\n"))
  
  # The meat of the function: nested foreach loop
  data_paragraphs <- foreach(i = seq(data_paragraphs_temp$doc_id), .combine='rbind') %:%
    foreach(j = 1:length(data_paragraphs_temp$paragraphs[[i]]), .combine = 'rbind') %do% {
      paragraph_temp <- data_paragraphs_temp$paragraphs[[i]][j] %>% str_trim(side = "both")
      location_temp <- str_locate(string = US_texts$text[US_texts$doc_id == data_paragraphs_temp$doc_id[i]], pattern = fixed(data_paragraphs_temp$paragraphs[[i]][j])) %>% as.list()
      text_length <- str_length(US_texts$text[US_texts$doc_id == data_paragraphs_temp$doc_id[i]])
      output <- list(
        "doc_id" = data_paragraphs_temp$doc_id[i], 
        "paragraph_id" = j, 
        "paragraph_text" = paragraph_temp, 
        "paragraph_start" = as.numeric(location_temp[1])/text_length, 
        "paragraph_end" = as.numeric(location_temp[2])/text_length, 
        "paragraph_length" = str_length(paragraph_temp)/text_length
      )
      return(output)
    } %>% as.data.frame(row.names = FALSE) 
  
  # Change to correct type
  data_paragraphs$paragraph_id <- data_paragraphs$paragraph_id %>% unlist() %>% as.numeric() 
  data_paragraphs$paragraph_start <- data_paragraphs$paragraph_start %>% unlist() %>% as.numeric()
  data_paragraphs$paragraph_end <- data_paragraphs$paragraph_end %>% unlist() %>% as.numeric() 
  data_paragraphs$paragraph_length <- data_paragraphs$paragraph_length %>% unlist() %>% as.numeric()
  data_paragraphs$paragraph_text <- data_paragraphs$paragraph_text %>% unlist() %>% as.character()
  data_paragraphs$doc_id <- data_paragraphs$doc_id  %>% unlist() %>% as.character()
  
  # Drop NA values
  data_paragraphs <- data_paragraphs  
  
  return(data_paragraphs)
}

# Run the function and save the file
data_paragraphs <- paragraphs_split(US_texts = US_texts)
save(data_paragraphs, file = "data/US_texts_paragraphs.RData")


# Word2vec
# Window parameter:  for skip-gram usually around 10, for cbow around 5
# Sample: sub-sampling of frequent words: can improve both accuracy and 
# speed for large data sets (useful values are in range 0.001 to 0.00001)
# # hs: the training algorithm: hierarchical so􏰂max (better for infrequent
# words) vs nega􏰁ve sampling (better for frequent words, better with low
#                              dimensional vectors)
word2vec_model_CBOW <- word2vec(x = US_texts$text, dim = 300)
write.word2vec(word2vec_model_CBOW, file = "models/word2vec_model_CBOW.bin")
read.word2vec(file = "models/word2vec_model_CBOW.bin")

word2vec_model_skipgram <- word2vec(x = US_texts$text, dim = 300, type = "skip-gram", window = 10)
save(word2vec_model_skipgram, file = "models/word2vec_model_CBOW.bin")

embedding <- as.matrix(word2vec_model_CBOW)
embedding <- predict(word2vec_model_CBOW, c("soud", "stěžovatel"), type = "nearest")
embedding

# SVM training

judgments_annotated <- jsonlite::fromJSON(txt = "data/US_judgments_annotated.json")
judgments_annotations <- judgments_annotated$examples %>% as.data.frame()

df <- foreach(i = seq(judgments_annotations[[6]]), .combine = "rbind") %:%
  foreach (j = seq(judgments_annotations[[6]][[i]]), .combine = "rbind") %do% {
    output <- list(
      "doc_id" = judgments_annotations$metadata$doc_id[i],
      "value" = judgments_annotations[[6]][[i]][j,4],
      "tag" = judgments_annotations[[6]][[i]][j,2],
      "start" = judgments_annotations[[6]][[i]][j,3],
      "end" = judgments_annotations[[6]][[i]][j,1]
    )
    return(output)
  } %>% as.data.frame(row.names = FALSE) %>% df_unlist() %>% drop_na() %>% as_tibble()

df$doc_id <- df$doc_id %>% make.unique()

df_doc <- df %>% select(doc_id, value) %>% rename(text = value)



doc2vec_model <- doc2vec(word2vec_model_CBOW, newdata = df_doc, type = "embedding") %>% as.data.frame() %>% rownames_to_column(var = "doc_id")


dat <- df %>% select(-value) %>% left_join(doc2vec_model, .) %>% column_to_rownames(var ="doc_id")
dat$tag <- dat$tag %>% as.factor()


# SVM learning
# Fit Support Vector Machine model to data set with e1071
svmfit <- svm(tag~., data = dat, kernel = "linear", scale = FALSE, cost = 0.01)
print(svmfit)

# find optimal cost of misclassification
tune.out <- tune(svm, tag~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
(bestmod <- tune.out$best.model)

# Create a table of misclassified observations
ypred <- predict(svmfit, dat)
(misclass <- table(predict = ypred, truth = dat$tag))

# sample training data and fit model
train <- base::sample(200,100, replace = FALSE)
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 10)
# plot classifier
plot(svmfit, dat)

# extract the best model
(bestmod <- tune.out$best.model)
(valid <- table(true = dat[-train,"y"], pred = predict(tune.out$best.model,
                                                       newx = dat[-train,])))


table(out$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
