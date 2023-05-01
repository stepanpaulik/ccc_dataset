xfun::pkg_attach2("tidyverse", "ggplot2", "progress", "foreach", "jsonlite",  "word2vec", "e1071", "caret", "tidymodels", "skimr")


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
  judgments_annotations_paragraphs_temp <- US_texts %>% group_by(doc_id) %>% summarise(paragraphs = str_split(text, pattern = "\n\n"))
  
  # The meat of the function: nested foreach loop
  judgments_annotations_paragraphs <- foreach(i = seq(judgments_annotations_paragraphs_temp$doc_id), .combine='rbind') %:%
    foreach(j = 1:length(judgments_annotations_paragraphs_temp$paragraphs[[i]]), .combine = 'rbind') %do% {
      paragraph_temp <- judgments_annotations_paragraphs_temp$paragraphs[[i]][j] %>% str_trim(side = "both")
      location_temp <- str_locate(string = US_texts$text[US_texts$doc_id == judgments_annotations_paragraphs_temp$doc_id[i]], pattern = fixed(judgments_annotations_paragraphs_temp$paragraphs[[i]][j])) %>% as.list()
      text_length <- str_length(US_texts$text[US_texts$doc_id == judgments_annotations_paragraphs_temp$doc_id[i]])
      output <- list(
        "doc_id" = judgments_annotations_paragraphs_temp$doc_id[i], 
        "paragraph_id" = j, 
        "paragraph_text" = paragraph_temp, 
        "paragraph_start" = as.numeric(location_temp[1])/text_length, 
        "paragraph_end" = as.numeric(location_temp[2])/text_length, 
        "paragraph_length" = str_length(paragraph_temp)/text_length
      )
      return(output)
    } %>% as_tibble() %>% df_unlist()

  # Drop NA values
  judgments_annotations_paragraphs <- judgments_annotations_paragraphs  
  
  return(judgments_annotations_paragraphs)
}

# Run the function and save the file
judgments_annotations_paragraphs <- paragraphs_split(US_texts = US_texts)
save(judgments_annotations_paragraphs, file = "data/US_texts_paragraphs.RData")


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
save(word2vec_model_skipgram, file = "models/word2vec_model_skipgram.bin")

embedding <- as.matrix(word2vec_model_CBOW)
embedding <- predict(word2vec_model_CBOW, c("soud", "stěžovatel"), type = "nearest")
embedding


# Load annotated data and create it into tibble of tag-level observations
judgments_annotated <- jsonlite::fromJSON(txt = "data/US_judgments_annotated.json")
judgments_annotations <- judgments_annotated$examples %>% as.data.frame()

judgments_annotations_parts <- foreach(i = seq(judgments_annotations$annotations), .combine = "rbind") %:%
  foreach (j = seq(judgments_annotations$annotations[[i]]), .combine = "rbind") %do% {
    text_length <- str_length(judgments_annotations$content[[i]])
    output <- list(
      "doc_id" = judgments_annotations$metadata$doc_id[i],
      "value" = judgments_annotations$annotations[[i]][j,4],
      "tag" = judgments_annotations$annotations[[i]][j,2],
      "start" = judgments_annotations$annotations[[i]][j,3]/text_length,
      "end" = judgments_annotations$annotations[[i]][j,1]/text_length,
      "length" = str_length(judgments_annotations$annotations[[i]][j,4])/text_length
    )
    return(output)
  } %>% as_tibble() %>% df_unlist() %>% drop_na()

judgments_annotations_paragraphs <- foreach(i = seq(nrow(judgments_annotations_parts)), .combine = "rbind") %do% {
  # Create the temporary vector of paragraph texts
  judgments_annotations_paragraphs_temp <- judgments_annotations_parts$value[i] %>% str_split(pattern = "\n\n")
  judgments_annotations_paragraphs_temp <- judgments_annotations_paragraphs_temp[[1]] %>% as.vector()
  judgments_annotations_paragraphs_temp <- judgments_annotations_paragraphs_temp[! judgments_annotations_paragraphs_temp %in% c("")]
  
  #Save the whole judgment text
  text_temp <- judgments_annotations$content[judgments_annotations$metadata$doc_id == judgments_annotations_parts$doc_id[i]]
  
  # Split the original parts tibble into paragraph level observations
  output <- foreach(j = seq(judgments_annotations_paragraphs_temp), .combine = "rbind") %do% {
    location_temp <- str_locate(string = text_temp, pattern = fixed(judgments_annotations_paragraphs_temp[j])) %>% as.list()
    text_length <- str_length(text_temp)
    output <- list(
      "doc_id" = judgments_annotations_parts$doc_id[i],
      "value" = judgments_annotations_paragraphs_temp[j],
      "tag" = judgments_annotations_parts$tag[i],
      "start" = as.numeric(location_temp[1])/text_length,
      "end" = as.numeric(location_temp[2])/text_length,
      "length" = str_length(judgments_annotations_paragraphs_temp[j])/text_length
    )
    return(output)
  } %>% as_tibble()
}


# SVM training

# Load the word2vec model for doc2vec use
word2vec_model_CBOW <- read.word2vec(file = "models/word2vec_model_CBOW.bin")

judgments_annotations_paragraphs$doc_id <- judgments_annotations_paragraphs$doc_id %>% make.unique()

# Create doc2vec model
doc2vec_df <- judgments_annotations_paragraphs %>% 
  select(doc_id, value) %>% 
  rename(text = value) %>% 
  doc2vec(word2vec_model_CBOW, newdata = ., type = "embedding") %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "doc_id") %>%
  drop_na()

# Create the final tibble for 
doc2vec_df <- judgments_annotations_paragraphs %>% 
  select(-value) %>% 
  left_join(doc2vec_df, .)

save(data = doc2vec_df, file = "data/doc2vec_df.RData")


# SVM learning
# Fit Support Vector Machine model to data set with e1071
svm_fit <- svm(factor(tag)~., data = data_analysis, kernel = "linear", scale = FALSE, cost = 0.01)
print(svm_fit)

# find optimal cost of misclassification
tune.out <- tune(svm, factor(tag)~., data = data_analysis, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
(bestmod <- tune.out$best.model)

# k-fold cross validation of the model with parameter tuning of the parameter C
set.seed(100)
trctrl <- trainControl(method = "cv", number = 6, savePredictions=TRUE)
Cgrid <- data.frame(C = 0.01)
svm_fit <- train(factor(tag)~., data = data_analysis, method = "svmLinear", trControl=trctrl, tuneLength = 0, tuneGrid = Cgrid)
svm_fit

pred <- svm_fit$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)
eachfold <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold

ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  theme_minimal()


# Create a table of misclassified observations
ypred <- predict(svm_fit, data_analysis)
(misclass <- table(predict = ypred, truth = data_analysis$tag))

# sample training data and fit model
train <- base::sample(200,100, replace = FALSE)
svm_fit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 10)
# plot classifier
plot(svm_fit, dat)

# extract the best model
(bestmod <- tune.out$best.model)
(valid <- table(true = dat[-train,"y"], pred = predict(tune.out$best.model,
                                                       newx = dat[-train,])))


table(out$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
