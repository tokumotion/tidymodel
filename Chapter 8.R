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
testing_data