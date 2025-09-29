# install if not already
install.packages("haven")
install.packages("tidyverse")
library(dplyr)

library(haven)
library(tidyverse)
library(dplyr)
# path to .xpt file
demographics = read_xpt("../raw_data/DEMO_L.xpt")
day1_foods = read_xpt("../raw_data/DR1IFF_L.xpt")
day1_nutri = read_xpt("../raw_data/DR1TOT_L.xpt")

## Create one merged df
# merge demo + foods (many rows per person, each food reported indv.)
demo_foods <- demographics %>%
  inner_join(day1_foods, by = "SEQN") # IN NHANES SEQN is a unique ID for each participant 
#add nutrient totals to demo_foods
demo_foods_full <- demo_foods %>%
  inner_join(day1_nutri, by = "SEQN")

# quick checks of new dfs
dim(demographics)      # participants
dim(day1_foods)        # food records
dim(day1_nutri)        # nutrient totals
dim(demo_foods)        # demo + foods
dim(demo_foods_full)   # demo + foods + nutrient totals
#####################################################

#AGREGATION
# Sodium per person (aggregating foods back up to participant level)
person_sodium <- demo_foods %>%
  group_by(SEQN, INDFMPIR) %>%   # group by participant ID called SEQN and income-to-poverty ratio (INDFMPIR)
  summarise(total_sodium = sum(DR1ISODI, na.rm = TRUE),  # sum sodium across all foods 
            total_kcal   = sum(DR1IKCAL, na.rm = TRUE)) %>% # sum calories across all foods eaten
  ungroup()  # remove groupingso the result is a flat DF


# Sodium by food code (see codebook) and income ratio (food-level summary)
sodium_by_food <- demo_foods %>%
  group_by(INDFMPIR, DR1IFDCD) %>%  # group by income ratio and USDA food code
  summarise(mean_sodium = mean(DR1ISODI, na.rm = TRUE), # average sodium per food item
            n_foods     = n()) %>%  # number of food items in that group
  ungroup() # return a flat data frame

# Create an income category variable for easier comparisons
demo_foods <- demo_foods %>%
  mutate(income_cat = case_when(
    INDFMPIR < 1 ~ "Low",        # below poverty line
    INDFMPIR < 3 ~ "Middle",     # between 1 and 3
    INDFMPIR >= 3 ~ "High",      # at or above 3
    TRUE ~ NA_character_         # assign NA if missing
  ))

