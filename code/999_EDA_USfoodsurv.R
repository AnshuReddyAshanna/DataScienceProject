# install if not already
install.packages("haven")
install.packages("tidyverse")
install.packages("ggplot2")
library(dplyr)

library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
# path to .xpt file
demographics = read_xpt("../raw_data/DEMO_L.xpt")
#demographics <- read_xpt("/Users/marlon/Documents/EDUCATION/GWU/GWU Courses/GWU Fall 25/Introduction to Data Science_Darcy Morris/DataScienceProject/raw_data/DEMO_L.xpt") #MD
day1_foods = read_xpt("../raw_data/DR1IFF_L.xpt")
#day1_foods <- read_xpt("/Users/marlon/Documents/EDUCATION/GWU/GWU Courses/GWU Fall 25/Introduction to Data Science_Darcy Morris/DataScienceProject/raw_data/DR1IFF_L.xpt") #MD
day1_nutri = read_xpt("../raw_data/DR1TOT_L.xpt")
#day1_nutri <- read_xpt("/Users/marlon/Documents/EDUCATION/GWU/GWU Courses/GWU Fall 25/Introduction to Data Science_Darcy Morris/DataScienceProject/raw_data/DR1TOT_L.xpt") #MD
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


#### Testing out first visualization ###

#  avg sodium by income group
sodium_by_income <- demo_foods %>%
  group_by(income_cat) %>%
  summarise(mean_sodium = mean(DR1ISODI, na.rm = TRUE))

# quick barplot
ggplot(sodium_by_income, aes(x = income_cat, y = mean_sodium, fill = income_cat)) +
  geom_col() +
  labs(title = "Average sodium per food by income group",
       x = "Income group",
       y = "Mean sodium (mg)") +
  theme_minimal() +
  theme(legend.position = "none")


###testing out data visualtization###



#  avg sodium by income group
sodium_by_income <- demo_foods %>%
  group_by(income_cat) %>%
  summarise(mean_sodium = mean(DR1ISODI, na.rm = TRUE))

# quick barplot
ggplot(sodium_by_income, aes(x = income_cat, y = mean_sodium, fill = income_cat)) +
  geom_col() +
  labs(title = "Average sodium per food by income group",
       x = "Income group",
       y = "Mean sodium (mg)") +
  theme_minimal() +
  theme(legend.position = "none")


## Person level Sodium and Statistical tests

# 1) build a person-level dataset from food rows (already did part of this above)

person_sodium <- demo_foods %>%
  group_by(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, INDFMPIR) %>%  # keep key demo fields
  summarise(
    total_sodium = sum(DR1ISODI, na.rm = TRUE),   # mg sodium/day (sum across foods)
    total_kcal   = sum(DR1IKCAL, na.rm = TRUE),   # kcal/day
    .groups = "drop"
  ) %>%
  # adjustiung cals: sodium per 1000 kcal so groups with diff kcal are comparable
  mutate(sodium_density = if_else(total_kcal > 0, total_sodium / (total_kcal/1000), NA_real_)) %>%
  # make readable demographic categories (PLEASE DOUBBLE CHECK THIS)
  mutate(
    gender = if_else(RIAGENDR == 1, "Male", "Female"),
    age_cat = cut(RIDAGEYR, breaks = c(20, 39, 59, Inf), labels = c("20–39","40–59","60+")),
    race_cat = case_when(
      RIDRETH1 == 1 ~ "Mexican American",
      RIDRETH1 == 2 ~ "Other Hispanic",
      RIDRETH1 == 3 ~ "White",
      RIDRETH1 == 4 ~ "Black",
      RIDRETH1 == 5 ~ "Other",
      TRUE ~ NA_character_
    ),
    income_cat = case_when(
      INDFMPIR < 1 ~ "Low",
      INDFMPIR < 3 ~ "Middle",
      INDFMPIR >= 3 ~ "High",
      TRUE ~ NA_character_
    )
  )

# quick  check: how many folks have usable sodium density?
sum(!is.na(person_sodium$sodium_density))

# 2) simple tests
# t-test: sodium density by gender (just 2 groups, so t-test is fine)
t_gender <- t.test(sodium_density ~ gender, data = person_sodium)
t_gender
##  Tells us that there is no significant difference in sodium density between genders in sample

# ANOVA: sodium density by age group
a_age <- aov(sodium_density ~ age_cat, data = person_sodium)
summary(a_age)
TukeyHSD(a_age)  # post-hoc pairwise diffs

# ANOVA: by race
a_race <- aov(sodium_density ~ race_cat, data = person_sodium)
summary(a_race)

# ANOVA: income
a_income <- aov(sodium_density ~ income_cat, data = person_sodium)
summary(a_income)

# a quick viz (person-level, not per-food) and a  boxplot by income group
ggplot(person_sodium, aes(x = income_cat, y = sodium_density, fill = income_cat)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = "Sodium density by income group (Day 1)",
       x = "Income group",
       y = "Sodium (mg per 1000 kcal)") +
  theme_minimal() + theme(legend.position = "none")

##Takeaway: Age remains the vest predictor of sodium density after controlling for gender, race, and income. 

