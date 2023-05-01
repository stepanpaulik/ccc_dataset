xfun::pkg_attach2("tidyverse", "ggplot2", "tidymodels")

load("data/doc2vec_df.RData")

doc2vec_df <- doc2vec_df %>% modify(.f = unlist) %>% as_tibble()

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(222)
# Put 3/4 of the data into the training set 
data_split <- initial_split(doc2vec_df, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
doc2vec_rec <- recipe(tag ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(tag, new_role = "outcome")
doc2vec_rec

# This creates a model object, in which you set the model specifications including the parameters to be tuned or the engine of the model
svm_mod <- svm_linear(cost = 0.01) %>%
  set_engine("kernlab") %>%
  set_mode("classification")
svm_mod

# Bind the model and the recipe to a workflow
doc2vec_wflow <- workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(doc2vec_rec)

# A first fitting without any tuning or resampling
doc2vec_fit <- doc2vec_wflow %>% 
  fit(data = train_data)

# Testing the model fit on test data
predict(doc2vec_fit, test_data)

# Measuring the accuracy of the basic model
doc2vec_aug <- 
  augment(doc2vec_fit, test_data) %>% 
  select(doc_id, tag, .pred_class) %>%
  mutate(
    tag = factor(tag),
    .pred_class = factor(.pred_class)
  )

doc2vec_pred <- predict(doc2vec_fit, test_data) %>%
  bind_cols(predict(doc2vec_fit, test_data, type = "prob")) %>%
  bind_cols(test_data %>% select(tag)) %>%
  mutate(
    tag = factor(tag)
  )

doc2vec_pred %>% 
  accuracy(truth = tag, .pred_class)

doc2vec_aug %>% 
  accuracy(truth = tag, .pred_class)

# Instead of training on the whole train data, let's do cross validation
set.seed(345)
folds <- vfold_cv(train_data, v = 6)

# This does a basic cross validation, yet without parameter tuning
doc2vec_fit_cv <- doc2vec_wflow %>% 
  fit_resamples(folds)
collect_metrics(doc2vec_fit_cv)

doc2vec_fit_cv <- doc2vec_fit_cv %>% extract_fit_parsnip()

doc2vec_aug <- augment(doc2vec_fit_cv, test_data) %>% 
  select(doc_id, tag, .pred_class) %>%
  mutate(
    tag = factor(tag),
    .pred_class = factor(.pred_class)
  )



# Tuning
# Firstly we create a grid with the tuning parameters
svm_tune <- grid_regular(cost(), levels = 5)

# We then tune the model with the tune_grid() function
doc2vec_tune <- doc2vec_wflow %>% 
  tune_grid(resamples = folds, grid = svm_tune)

# Using various yardstick functions we can see which model parameters are the best and we save them with select_best() function
doc2vec_tune %>% collect_metrics()
doc2vec_tune %>% show_best("accuracy")
best_cost <- doc2vec_tune %>% select_best("accuracy")

# Now we input the best parameters into the final workflow with the finalize_workflow() function
doc2vec_wflow_final <- 
  doc2vec_wflow %>% 
  finalize_workflow(best_cost)

# Lastly, the model is both trained and then fitted to the testing data with the last_fit() function; this function fits the finalized model on the full training data set and evaluates the finalized model on the testing data.
doc2vec_fit_final <- doc2vec_wflow %>%
  last_fit(data_split)

doc2vec_fit_final %>% collect_metrics()



doc2vec_fit_final %>%
  collect_predictions() %>% 
  roc_curve(class, .pred_PS) %>% 
  autoplot()
