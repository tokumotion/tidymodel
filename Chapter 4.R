### The Ames Housing Data ###
library(tidymodels); library(tidyverse); library(magrittr); library(ggmap)
# Load ames data for use in this exercise
data(ames)
# Exploring Features of Homes in Ames
# Outcome to predict: last sale price of the house in USD
tidymodels_prefer() # Fixes conflicts between tidyverse packages
# Plot how many houses were sold at what price with a histogram
ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = 'white')
# Analysis of the data: data is right skewed, so more inexpensive houses than
# expensive ones. For modelling, we can log-transform the data, why? because 
# there is no negative data in the data set. We use log-transform to stabilize
# the variance.
ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = 'white') +
  scale_x_log10()
# Trouble: It's harder to interpret the results of the analysis.
# Units of model coefficients are difficult to interpret as also measures of 
# performance, such as root mean square mean error (RSME) why? RSME uses the 
# differences of real (observed) and predicted values. If sale price is log-
# transformed, the differences are also log-transformed. So it is harder to
# interpret a RSME in log scale.
# Take into account: from this point on, all sales data is log-transformed
ames <- ames %>% 
  mutate(Sale_Price = log10(Sale_Price))
# Log to Stamen map service for free
register_stadiamaps(key = '405afa93-be85-4625-86e2-7bfc8e9143b4', write = FALSE)
# CReate a bounding box from dataset long-lat data
bbox <- make_bbox(Longitude, Latitude, data = ames)
# Use zoom 14 for detail when downloading the map
map <- get_stadiamap(bbox = bbox, maptype = 'outdoors', zoom = 14)
ggmap(map) + 
  geom_point(data = ames,
             mapping = aes(x = Longitude, y = Latitude, 
                           color = Neighborhood),
             size = 1, alpha = 0.7)
