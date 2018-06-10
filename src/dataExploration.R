library(dplyr)    # Data manipulation
library(DT)       # data table
library(ggplot2)  # Data visualization
library(ggthemes) # Better themes
library(magrittr) # Hadleyverse
library(pander)   # Nice tables, etc.
library(readr)    # CSV file I/O, e.g. the read_csv function
library(tidyr)    # Hadleyverse

theme_set(theme_economist())


food_reviews <- read_csv("./input/Reviews.csv") %>% select(UserId, ProductId, Score) 

food_reviews %>%
  ggplot(aes(x = Score)) +
  geom_histogram(bins = 5)

food_reviews %>%
  group_by(UserId) %>%
  summarise(num_reviews = n()) %>%
  ggplot(aes(x = num_reviews)) +
  geom_histogram(bins = 50) + 
  scale_x_log10()

food_reviews %>%
  group_by(ProductId) %>%
  summarise(num_reviews = n()) %>%
  ggplot(aes(x = num_reviews)) +
  geom_histogram()