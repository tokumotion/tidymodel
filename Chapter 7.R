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
# A MODEL WORKFLOW
# We use workflow() to streamline all the modeling processes into one coherent 
# and reproducible data pipeline
# WHAT IS NEEDED BEFORE A MODEL IS FIT?
# 1. Feature selection, either by knowing which variables are possible 
# predictors or by running a feature selection algorithm, we need to understand
# what variables can predict outcomes while maximizing simplicity in the model
# (least possible predictors)
# 2. If a predictor data is missing, instead of discarding it altogether we can
# "repopulate" it by finding correlations between the predictor and other 
# variables and estimating the predictor values.
# 3. Transform (re-scale) the predictor, like log-transform it. (PAC is a
# transformation in which we take the predictors/variables and convert them into
# new features that can be used as predictors)
# Model workflow includes data pre-processing, parameter tuning and model and 
# data post-processing. The outcome should be the final model equation
# (Remember PCA features ARE NOT CORRELATED, so we can forget colinearity)
# EXAMPLE: We need to pre-process data with PCA before we do a LR. PCA should be
# part of model workflow.
# WORKFLOW BASICS
lm_model <- linear_reg() %>% set_engine('lm')
# Use workflow() verb to add a model, you can multiple models
lm_wflow <- workflow() %>% add_model(lm_model)
lm_wflow
# We have not declared YET how the workflow needs to preprocess data
# We add a simple regression formula
lm_wflow <- lm_wflow %>% add_formula(Sale_Price ~ Longitute + Latitude)
lm_wflow
# We can fit the workflow using fit()
fit(lm_wflow, ames_train)
# When you fuck up the formula, use update_formula()
lm_wflow <- lm_wflow %>% update_formula(Sale_Price ~ Longitude + Latitude)
lm_fit <- fit(lm_wflow, ames_train)
# We can use predict with the test data
predict(lm_fit, ames_test %>% slice(1:5))
# You can remove a formula using remove_formula()
# ADDING RAW VARIABLES TO WORKFLOW()
# After you delete the formula, you use add_variables() to define outcome and
# predictor variables
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcomes = Sale_Price, predictors = c(Longitude, Latitude))
lm_wflow
# You can use a mode general formula to indicate predictors
predictors = c(ends_with('tude'))
# Or use all variables
predictors = everything()
# Now we fit
lm_fit <- fit(lm_wflow, ames_train)
lm_fit
# parsnip know how the underlying model engine use the data and you use 
# add_formula() to preprocess the data according to the needs of the underlying 
# model
# EXAMPLES
# lme4 package model: it is a regression model with random effects for subjects
library(lme4); library(nlme)
lmer(distance ~ Sex + (age | Subject), data = Orthodont)
# how we do it with parsnip
library(multilevelmod)
multilevelspec_spec <- linear_reg() %>% set_engine('lmer')
multilevel_workflow <- 
  workflow() %>% 
  add_variables(outcomes = distance, predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevelspec_spec,
            formula = distance ~ Sex + (age | Subject))
multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
multilevel_fit
# We can use strata() too for survival analysis
library(censored)
parametric_spec <- survival_reg()
parametric_workflow <- 
  workflow() %>% 
  add_variables(outcomes = c(fustat, futime), predictors = c(age,rx)) %>% 
  add_model(parametric_spec,
            formula = Surv(futime, fustat) ~ age + strata(rx))
parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit
# CREATING MULTIPLE WORKFLOWS AT ONCE
# When doing projects you need multiple model specifications with a differente 
# number of predictors
# We create a list of formulas that capture the possible predictor sets
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)
library(workflowsets)
location_models <- workflow_set(preproc = location, 
                                models = list(lm = lm_model))
location_models
extract_workflow(location_models, id = 'coords_lm')
# Create fits for each formula
location_models <- 
  location_models %>% 
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models
location_models$fit[[1]]
# EVALUATING THE TEST SET
# There is a function that will test the model versus the testing dataset
final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res
# last_fit() uses the data split from initial_split()
fitted_lm_wflow <- extract_workflow(final_lm_res)
fitted_lm_wflow
collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)
