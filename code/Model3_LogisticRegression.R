#Model 3_Logistic Regression 
# Predicting a “Healthy Diet” Classification"

#### Preliminary
# Libraries
library(arrow)
library(ggplot2)
library(ezids)
library(labelled)
library(gtsummary)
library(openxlsx)
library(dplyr)
library(tidyr)
library(vctrs)
library(tibble)

#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("/Users/marlon/Documents/EDUCATION/GWU/GWU Courses/GWU Fall 25/Introduction to Data Science_Darcy Morris/ModelDataWhatWeEat")

#### Reading the parquet file
food <- read_parquet("proc_data/demo_foods.parquet")
head(food)

# Healthy Diet Dummy Variable

food <- food %>%
  mutate(
    healthy_diet =
      if_else(
        DR1IKCAL_sum <= 2000 &        # Calories
          DR1ISODI_sum < 2300 &         # Sodium (mg)
          DR1IPROT_sum >= 50 &          # Protein (g)
          DR1ISUGR_sum <= 50 &          # Sugar (g)
          DR1ICARB_sum <= 275 &         # Carbs (g)
          DR1IFIBE_sum >= 28 &          # Fiber (g)
          DR1ITFAT_sum <= 78,           # Fat (g)
        1, 0
      )
  )

### Demografic predictors

model_data <- food %>%
  select(
    healthy_diet,
    RIAGENDR,   # gender
    RIDAGEYR,   # age
    RIDRETH3,   # race (detailed)
    DMDEDUC2,   # education
    INDFMPIR    # income-to-poverty ratio
  ) %>%
  drop_na()   # logistic regression cannot run with NA


## Fit regression 


model3 <- glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR + RIDRETH3 + DMDEDUC2 + INDFMPIR,
  data = model_data,
  family = binomial
)

summary(model3)