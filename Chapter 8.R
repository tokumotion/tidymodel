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
<<<<<<< HEAD
# HOW DATA ARE USED IN recipe()
# When you use predict() in a model which uses recipe() it does not recalculate
# the values based on the test data, for example if you normalize the data in
# the train set when you use predict() with the test set it uses the mean and sd
# of the train set
# EXAMPLES OF STEPS
# Encoding qualitative data in a numeric format: 
# step_unknown() - changes missing values to "unknown" factor level.
# step_novel() - creates a new factor level if we think a new factor can be 
# found in the new data (test data)
# step_other() - converts factors with very low frequencies into "other"
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_other(Neighborhood, threshold = 0.01)
# Interaction terms
# In ames we can see that Gr_Liv_Area and Bldg_Type vars have a relationships
# houes with more living are tend to be houses rather than apartments (therefore 
# more expensive). This interaction needs to be taken into account before 
# running the model into the recipes
ggplot(ames_train, aes(x = Gr_Liv_Area, y = log10(Sale_Price))) + 
  geom_point(alpha = 0.2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = 'lightblue') +
  scale_x_log10() + scale_y_log10() +
  labs(x = 'Gross Living Area', y = 'Sale Price (USD)')
# now we make recipe
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  # ORDER MATTERS, we need to place this step before making the dummy variables
  # dummy variables do not handle frequencies as each factor level - 1 have its
  # own column
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is alreadu log-transformed (ORDER MATTERS)
  step_interact(~ Gr_Liv_Area:starts_with('Bldg_Type_'))
  # if we want to have multiple interactions, inside the formula we use + after
  # declaring the first interaction
simple_ames
lm_wflow <- lm_wflow %>% 
  remove_recipe() %>% 
  add_recipe(simple_ames)
lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test %>% slice(1:5))
# HOW TO DEAL WITH NON LINEARITY IN DATA
# We will use ggplot to see how non-linear is the Latitude predictor. We use 
# splines which allows a lineal model to emulate a flexible non lineal pattern
# CAREFUL: THE MORE SPLINES THE MORE RISK OF OVERFITTING
library(patchwork); library(splines)
plot_smoother <- function(deg_free){
  ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) +
           geom_point(alpha = 0.2) +
           scale_y_log10() + 
           geom_smooth(
             method = lm,
             formula = y ~ ns(x, df = deg_free),
             color = 'lightblue',
             se = FALSE
           ) +
           labs(title = paste(deg_free, "Spline Terms"),
                y = 'Sale Price (USD)')
}
(plot_smoother(2) + plot_smoother(5)) / (plot_smoother(20) + plot_smoother(100))
# From the graphs we see that between 5 and 20 splines, we can catch the correct 
# degrees of freedom for the non-linearity
# Recipe with splines - step_ns()
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude, data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ Gr_Liv_Area:starts_with('Bldg_Type_')) %>% 
  step_ns(Latitude, deg_free = 20)
lm_wflow <- lm_wflow %>% 
  remove_recipe() %>% 
  add_recipe(simple_ames)
lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test %>% slice(1:5))
# Question: is Neighborhood and Latitude needed in the same model, they both
# convey geo data?
# FEATURE EXTRACTION (PRINCIPAL COMPONENT ANALYSIS)
# We want to eliminate correlation between features, we use PCA
# REMEMBER PCA assumes all predictors are in the same scale, to fix this we can
# use step_normalize() and we can then use step_pca()
# Need to put all variables in one place to avoid copypasting
SF <- reformulate(grep("SF", names(ames), value = TRUE), 
                  response = 'Sale_Price')
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + ends_with("SF"), data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ Gr_Liv_Area:starts_with('Bldg_Type_')) %>% 
  step_ns(Latitude, deg_free = 20) %>% 
  step_pca(matches('(SF$)|(Gr_Liv'))

simple_ames <- recipe(Sale_Price ~ ., data = ames_train) %>% 
  # Here we use input ALL dataset variables BUT we remove their predictor roles
  # so we can keep using the model as intended without additional variables
  remove_role(-ends_with('SF'), -c('Neighborhood', 'Gr_Liv_Area', 'Year_Built', 
                                   'Bldg_Type', 'Latitude'), 
              old_role = 'predictor') %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~ Gr_Liv_Area:starts_with('Bldg_Type_')) %>% 
  step_ns(Latitude, deg_free = 20) %>% 
  # better to use tidyselect verbs that regex
  step_pca(ends_with("SF") , contains("Gr_Liv"))
lm_wflow <- lm_wflow %>% 
  remove_recipe() %>% 
  add_recipe(simple_ames)
lm_fit <- fit(lm_wflow, ames_train)
lm_fit
predict(lm_fit, ames_test %>% slice(1:5))
