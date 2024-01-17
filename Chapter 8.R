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

# FEATURE ENGINEERING
# Its reformatting predictor variables to make them easier for modelling.
# For the Ames dataset: we have variables that have qualitative data such as
# neighborhood, long/lat, distance to the nearest school, etc. 
# Other forms of preprocessing are: reduction of correlation between variables, 
# input missing values in predictors or reducing variance or skew from variables
# A SIMPLE recipe() FOR THE AMES HOUSING DATA
# Some predictors from Ames housing data we can use to build models
# Neighborhood - need to be converted from qualitative to quantitative
# Gross above grading area - to avoid much outliers, we log-transform
# Year built
# Type of building - need to be converted from qualitative to quantitative
# A simple lm would be build like this
# lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type,
# data = ames)
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>% 
  # we log-transform Gr_Liv_Area
  step_log(Gr_Liv_Area, base = 10) %>% 
  # we tell the recipe to transform all qualitative variables into dummy 
  # variables
  step_dummy(all_nominal_predictors())
  # all_nominal_predictors() capture all character or factor variables
simple_ames
# USING RECIPES
# We reuse the previous chapter workflow
lm_model <- linear_reg() %>% set_engine('lm')
# Use workflow() verb to add a model, you can multiple models
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcomes = Sale_Price, predictors = c(Longitude, Latitude))
# this will give me an error
lm_wflow %>% 
  add_recipe(simple_ames)
# recipe() and add_variable() DO NOT WORK TOGETHER. You need to remove the 
# add_variable()
lm_wflow <- lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)
# Now let's fit the model
lm_fit <- fit(lm_wflow, ames_train)
lm_fit
predict(lm_fit, ames_test %>% slice(1:5))
# If we need the bare model or recipe we use extract_*
lm_fit %>% extract_recipe()
lm_fit %>% extract_fit_parsnip() %>% tidy() %>%  slice(1:5)
