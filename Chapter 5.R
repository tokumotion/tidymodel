### Spending Our Data ###
# Data Spending: The process of splitting data to different tasks (parameter 
# estimation, model selection + tuning, performance assessment)
# This issue is relevant depending on the amount of data available, when you
# have lots of data, you can split it into different tasks and make those splits
# exclusive (no overlap in data), BUT if you have small amounts of data you will
# have overlaps. Then the question of what % of data should be assigned to which
# task. (The question here is how much data is enough data to test for all those
# tasks)
# Book example: If you have enough data, is a good idea to split a part of it 
# and use it to find out which predictors are informative before parameter 
# estimation, BUT if you don't have enough data you will get overlaps
# WHY IS THIS IMPORTANT AT ALL?? You want to use independent data sources when 
# finding which predictors are useful and how to determine the parameters that 
# will be assigned to those predictors to avoid creating relationships between
# those processes that can bias the analysis.
# Common methods for splitting data
# Usual split: test and training. On training we do model building (parameter 
# estimation, model selection + tuning), on test we run the models selected into
# to assess mode efficacy. You only run the model on test data ONCE.
# Create a 80/20 training/test split of the data:
library(tidymodels); library(tidyverse); library(magrittr); library(rsample)
tidymodels_prefer()
# We pick up from last chapter
data(ames)
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price))
# Now we split (Simple Random Sampling)
set.seed(501)
ames_split <- initial_split(ames, prop = 0.8)
ames_split
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
# Cases when simple random sampling does not work
# Class imbalance: simple random sampling assumes that all data is normally 
# distributed. Simple random sampling does not work if one class occurs much 
# less frequently. 
# HOW DO WE FIX THIS? Stratified sampling. First you conduct the sampling for 
# each class/variable and then sum all splits into training and testing. For
# regression problems, the data can be separated into quartiles and run the 
# sampling for each quartile. Why? This helps you to keep the data distribution 
# between training and testing datasets
# HOW CAN WE USE THIS ON THE AMES DATA? We saw that the sales data is right 
# skewed or there are more cheap houses than expensive ones. We want to keep 
# that distribution between the training and testing sets.
set.seed(502)
# Using strata on Sale_Price we make sure we respect the distribution on both 
# datasets. Only a single variable can be used in this function
ames_split <- initial_split(ames, prop = 0.8, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
# WHEN RANDOM SAMPLING DOES NOT WORK? Time Series, most recent data is test set.
# Multilevel Data: When you have observations from the same individual, usually 
# through time (weight of same person through weeks, price points of same 
# product in category, daily return of stocks), you sample at the experimental 
# unit level (person, product, stock)