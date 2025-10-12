## This script combines the food variables with the demographic variables
#  And creates the dataset we will use for the analysis
rm(list = ls())
#### Preliminary
# Libraries
library(dplyr)
#Libary to open xpt files
library(haven)
library(tidyverse)
library(ezids)
# To export final dataset as parquet for efficient output
library(arrow)

#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("..")

#### Open Demographics Data
demographics = read_xpt("raw_data/DEMO_L.xpt")
str(demographics)
# Keep only the variables we need
demographics_f=demographics[c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "RIDRETH3", "DMDEDUC2", "DMDMARTZ", "RIDEXPRG", "DMDHHSIZ", "WTINT2YR", "INDFMPIR")]
#Exclude pregnant women and keep only people 20 or older
demographics_f <- demographics_f %>%
  filter(RIDAGEYR >= 20, RIDEXPRG != 1 | is.na(RIDEXPRG)) %>%
  select(-RIDEXPRG)

#Drop large Demographics to save memory
rm(demographics)

#### Open Food Dataset
day1_foods = read_xpt("raw_data/DR1IFF_L.xpt")
str(day1_foods)
# Keep only the variables we need
day1_foods_f=day1_foods[c("SEQN", "WTDRD1", "DR1DAY", "DR1CCMNM", "DR1CCMTX", "DR1_020", "DR1_030Z", "DR1FS", "DR1IFDCD", "DR1IGRMS", "DR1IKCAL", "DR1IPROT", "DR1ICARB", "DR1ISUGR", "DR1IFIBE", "DR1ITFAT", "DR1ISODI")]

#Review food descriptions
food_desc = read_xpt("raw_data/drxfcd_L.xpt")
# We will not use this for now
rm(food_desc)

#Group the food nutritional file from individual-food level to individual level
str(day1_foods_f)

day1_foods_f_g <- day1_foods_f %>%
  group_by(SEQN) %>%
  summarise(
    #Weight
    WTDRD1 = mean(WTDRD1, na.rm = TRUE),
    #Food vars
    across(c("DR1IGRMS", "DR1IKCAL", "DR1IPROT", "DR1ICARB", "DR1ISUGR", "DR1IFIBE", "DR1ITFAT", "DR1ISODI"), 
           list(
             sum  = \(x) sum(x,  na.rm = TRUE),
             mean = \(x) mean(x, na.rm = TRUE),
             sd   = \(x) sd(x,   na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}"
    ),
    # Count of observations
    n_obs = n()
  )

#Free-up space
rm(day1_foods)
rm(day1_foods_f)

## Nutritional Dataset
day1_nutri = read_xpt("raw_data/DR1TOT_L.xpt")
# It does not really give us new information from what we already have
rm(day1_nutri)

## Create one merged df that combines demographics and food information
# merge demo + foods (1-1 individual level)
# Using an Inner Join as We want to apply to the Food Data the filters 
# used in the demographics dataset, and we are not interested in any possible
# observation that could appear in the food dataset but that does not have demographics
demo_foods <- demographics_f %>%
  inner_join(day1_foods_f_g, by = "SEQN") # IN NHANES SEQN is a unique ID for each participant 

#Free-up space
rm(demographics_f)
rm(day1_foods_f_g)

###### Export final datafile
write_parquet(demo_foods, "proc_data/demo_foods.parquet")

#Clean-up
rm(list = ls())

##Reading the parquet file
food <- read_parquet("proc_data/demo_foods.parquet")
str(food)
xkablesummary(food)

#EDA - Visualizations

library(ggplot2)
#Histogram for the distribution of calorie intake
ggplot(food, aes(DR1IKCAL_sum)) +
  geom_histogram(breaks=seq(0, 10000, by = 500), fill = "skyblue", color = "black", alpha = 0.9) +
  labs(title = "Distribution of Daily Calorie Intake",
       x = "Total Calories (kcal)",
       y = "Frequency")

#Histogram for the distribution of  Protein Intake
ggplot(food, aes(x = DR1IPROT_sum)) +
  geom_histogram(breaks=seq(0, 500, by = 10), fill = "lightgreen", color = "black", alpha = 0.9) +
  labs(title = "Distribution of Daily Protein Intake",
       x = "Total Protein (grams)",
       y = "Frequency")

# Histogram for the distribution of Carbohydrate Intake
ggplot(food, aes(x = DR1ICARB_sum)) +
  geom_histogram(breaks=seq(0, 1400, by = 20), fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Daily Carbohydrate Intake",
       x = "Total Carbohydrates (grams)",
       y = "Frequency")

# Creating groups based for age, gender and education
food <- food %>%
  mutate(
    AGE_GROUP = case_when(
      RIDAGEYR >= 20 & RIDAGEYR < 30 ~ "20-29",
      RIDAGEYR >= 30 & RIDAGEYR < 40 ~ "30-39",
      RIDAGEYR >= 40 & RIDAGEYR < 50 ~ "40-49",
      RIDAGEYR >= 50 & RIDAGEYR < 60 ~ "50-59",
      RIDAGEYR >= 60 ~ "60+",
      TRUE ~ NA_character_),
    
    GENDER = case_when(
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female",
      TRUE ~ NA_character_),
    
    EDUCATION = case_when(
      DMDEDUC2 == 1 ~ "Less than 9th grade",
      DMDEDUC2 == 2 ~ "9-11th grade",
      DMDEDUC2 == 3 ~ "High school",
      DMDEDUC2 == 4 ~ "Undergrduate",
      DMDEDUC2 == 5 ~ "Graduate or above",
      TRUE ~ NA_character_))

#Box plot for Protein intake by gender group
ggplot(food, aes(x = GENDER, y = DR1IPROT_sum, fill = GENDER)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Protein Intake by Gender", x = "Gender", y = "Total Protein")

#Box plot for Nutrient intake by age group
ggplot(food, aes(x = AGE_GROUP, y = DR1IKCAL_sum, fill = AGE_GROUP)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Calorie Intake by Age", x = "Age", y = "Total Calories")

#Box plot for Nutrient intake by Education group
ggplot(food, aes(x = EDUCATION, y = DR1IKCAL_sum, fill = EDUCATION)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Calorie Intake by Education Level", x = "Education Level", y = "Total Calories")

# Scatter Plot between income and total calorie intake
ggplot(food, aes(x = INDFMPIR, y = DR1IKCAL_sum)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  labs(title = "Relationship Between Income and Calorie Intake",
       x = "Income",
       y = "Total Daily Calorie Intake (kcal)")

# Scatter Plot between Sodium and Protein intake by Age
ggplot(food, aes(x = DR1ISODI_sum, y = DR1IPROT_sum, color = AGE_GROUP)) +
  geom_point(alpha = 0.7) +
  labs(title = "Sodium vs Protein Intake Across Age",
       x = "Total Sodium (g)",
       y = "Total Protein (g)")

#Pie chart of all the nutrients-Protein,Carbohydrate,Fat,Sodium,Sugar,Fiber
# Summarize total amounts for each nutrient
nutrient_composition <- food %>%
  summarise(
    Protein = sum(DR1IPROT_sum, na.rm = TRUE),
    Carbohydrates = sum(DR1ICARB_sum, na.rm = TRUE),
    Fat = sum(DR1ITFAT_sum, na.rm = TRUE),
    Sugar = sum(DR1ISUGR_sum, na.rm = TRUE),
    Fiber = sum(DR1IFIBE_sum, na.rm = TRUE),
    Sodium = sum(DR1ISODI_sum, na.rm = TRUE)) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Nutrient", values_to = "Total") %>%
  mutate(Percentage = Total / sum(Total) * 100)

# Pie chart
ggplot(nutrient_composition, aes(x = "", y = Percentage, fill = Nutrient)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),position = position_stack(vjust = 0.5), color = "black", size = 2.5) +
  labs(title = "Nutrient Composition",x = NULL, y = NULL, fill = "Nutrient") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))




