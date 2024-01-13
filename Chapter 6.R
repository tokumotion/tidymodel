# Create a 80/20 training/test split of the data:
library(tidymodels); library(tidyverse); library(magrittr); library(rsample)
tidymodels_prefer()
# We pick up from last chapter
data(ames)
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price))
set.seed(502)
# Using strata on Sale_Price we make sure we respect the distribution on both 
# datasets. Only a single variable can be used in this function
ames_split <- initial_split(ames, prop = 0.8, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
# PARSNIP PACKAGE: 2 important verbs fit() and predict()
# Create a model
# First step is to encode the data in a format that the model can use, such as 
# a matrix
# HOW WE USED TO RUN MODELS BEFORE TIDYMODELS?
# Methods to estimate model variables: Ordinary Linear Regression (traditional 
# least squares) or Regularized linear regression (which ads a penalty to 
# encourage simplicity, which means less variables)
# Linear regression: lm(formula, data, ...) - stats package
# Regularized lm: stan_glm(formular, data, family = 'gaussian, ...) - rstanarm
# package (bayesian approach)
# Non-bayesian reg lm: glmnet(x = matrix, y = vector, family = 'gaussian') - 
# glmnet package. Predictor data must be in matrix
# HOW IS TIDYMODELS BETTER HERE?
# Tidymodels create a homogenous sintax and data input for all models so we can
# easily check which model works best for our data
# First, specify what model you want to run: linear reg, random forest, KNN, etc
# This shows what package should be used
# Second, declare mode of model (when needed): which is the outcome of the model
# if the model is a lm, you do not need to declare it BUT if you need a 
# classification model (such as k-means) you need to declare it (we'll see how?)
# You do not need to reference data when specifying a model
linear_reg() %>% set_engine('lm')
linear_reg() %>% set_engine('glmnet')
linear_reg() %>% set_engine('stan')
# fit() function estimates the model if you use a mode formula OR fit_xy() when 
# THE DATA IS  ALREADY PREPROCESSED
# translate() provides the original function (without parsnip)
linear_reg() %>% set_engine('lm') %>% translate()
linear_reg(penalty = 1) %>% set_engine('glmnet') %>% translate()
linear_reg() %>% set_engine('stan') %>% translate()
# HOW DOES THIS WORK????
# Predict house prices based n only longitude and latitude
# Declare the model (this is a lm) - Remember the data is already 
# log-transformed
lm_model <- linear_reg() %>% set_engine('lm')
lm_form_fit <- lm_model %>% 
# To use fit() you declare the formula as an argument
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)
# Other way to fit the model is to directly declare the variables from the 
# already preprocessed data set
lm_xy_fit <- lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )
# Both models give the same results
lm_form_fit
lm_xy_fit
# parsnip allows us to have the same argument names through different models
# HOW DOES THIS WORK IN A RANDOM FOREST?
# In parsnip, we use only 1 set of arguments:
# mtry: # of sampled predictors
# trees: # of trees
# min_n: # # of data points to split (node size)
rand_forest(trees = 10000, min_n = 5) %>% 
  set_engine('ranger') %>% 
  set_mode('regression') %>% 
  translate()
# HOW TO USE THE RESULTS OF THE MODEL
# Use extract_fit_engine()
lm_form_fit %>% extract_fit_engine()
# Now, print a covariance matrix
lm_form_fit %>% extract_fit_engine() %>% vcov()
# WATCHOUT!!! 
# Do not pass a fit element to a model prediction. 
# YES = predict(lm_form_fit)
# NO = predict(lm_form_fit$fit)
# HOW TO INTERPRET THE RESULTS?
# Use the tidy() function to have a consistent way to show the results between 
# models
tidy(lm_form_fit)
# HOW TO MAKE PREDICTIONS?
# predict() verb in parsnip have the following rules:
# The results are always a tibble, the columna names are coherent and the tibble
# has the same number of rows as the input dataset
ames_test_small <- ames_test %>% slice(1:5) # mini dataset for experiment
# Predict Sale_Price value based on long+lat data within the model
predict(lm_form_fit, new_data = ames_test_small)
# Merge this predictable results with the original data
ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, new_data = ames_test_small)) %>% 
# Here you add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, new_data = ames_test_small, type = 'pred_int'))
# NOW WE USE A DIFFERENT MODEL TO COMPARE RESULTS WITH THE SAME DATA
# We use rpart package
tree_model <- decision_tree(min_n = 2) %>% 
  set_engine('rpart') %>% 
  set_mode('regression')
tree_fit <- tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)
ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small)) 