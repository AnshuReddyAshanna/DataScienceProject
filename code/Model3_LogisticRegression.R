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

#Set Working Directory 
getwd()  
setwd("/Users/marlon/Documents/EDUCATION/GWU/GWU Courses/GWU Fall 25/Introduction to Data Science_Darcy Morris/ModelDataWhatWeEat")

#### Reading the parquet file
food <- read_parquet("proc_data/demo_foods.parquet")
head(food)

# Create a health score (0–7) based on meeting dietary benchmark
food = food %>%
  mutate(
    healthy_score =
      (DR1IKCAL_sum <= 2000) +      # calories
      (DR1ISODI_sum < 2300) +       # sodium
      (DR1IPROT_sum >= 50) +        # protein
      (DR1ISUGR_sum <= 50) +        # sugar
      (DR1ICARB_sum <= 275) +       # carbohydrates
      (DR1IFIBE_sum >= 28) +        # fiber
      (DR1ITFAT_sum <= 78),         # fat
    
    # Define healthy diet = 1 if they meet at least 4 of 7 criteria (since all would be very rare)
    healthy_diet = if_else(healthy_score >= 4, 1, 0)
  )

# Convert to factor for log regression
food$healthy_diet = factor(food$healthy_diet)

# Select demographic predictors and drop any missing values
model_data = food %>%
  select(
    healthy_diet,
    RIAGENDR,     # gender
    RIDAGEYR,     # age
    RIDRETH3,     # race/ethnicity
    DMDEDUC2,     # education
    INDFMPIR      # income-to-poverty ratio
  ) %>%
  drop_na()

# Fit logistic regression predicting the odds of having a healthy diet based on 4 criterias
model3 = glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR + RIDRETH3 + DMDEDUC2 + INDFMPIR,
  data = model_data,
  family = binomial
)

#  regression results
summary(model3)

#The model shows that sex and age are the strongest predictors of meeting at least four healthy diet criteria. 
#Women and older adults have higher odds of a healthier diet
#Education has a small effect
#income and race/ethnicity add little and are not strong predictors


# Predicted probabilities of a healthy diet
pred_prob3 = predict(model3, type = "response")

# Convert probabilities to binary predictions using a 0.5 cutoff
pred_class3 = if_else(pred_prob3 >= 0.5, 1, 0)

# Confusion matrix comparing predictions to actual values
conf_mat3 = table(pred_class3, model_data$healthy_diet)
conf_mat3

# Model accuracy
accuracy3 = mean(pred_class3 == model_data$healthy_diet)
accuracy3

#using a 0.5 cutoff the model correctly predicts a healthy diet classification about 61.3% of the time
