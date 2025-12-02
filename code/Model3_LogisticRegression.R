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
names(food)

# Create a health score (0–7) based on meeting dietary benchmark
# Calculate number of criteria met
##############################################
### Model 3: Logistic Regression – Healthy Diet
### Using 5-out-of-7 nutritional criteria
##############################################

library(dplyr)

food = food %>%
  mutate(
    # how many of the 7 benchmarks each person meets
    healthy_score =
      (DR1IKCAL_sum <= 2000) +   # calories
      (DR1ISODI_sum < 2300) +    # sodium
      (DR1IPROT_sum >= 50) +     # protein
      (DR1ISUGR_sum <= 50) +     # sugar
      (DR1ICARB_sum <= 275) +    # carbs
      (DR1IFIBE_sum >= 28) +     # fiber
      (DR1ITFAT_sum <= 78),      # fat
    
    # healthy_diet = 1 if they meet at least 5 of 7 criteria
    healthy_diet = if_else(healthy_score >= 5, 1, 0)
  )

# Check how many are classified as healthy vs not
table(food$healthy_diet)
prop.table(table(food$healthy_diet))
#76% unehealthy
##############################################
### 2. Recode demographic variables


# Marital status: Partnered / Widowed–Separated / Single
food = food %>%
  mutate(
    marital_recode = case_when(
      DMDMARTZ %in% c(1, 6) ~ "Partnered",
      DMDMARTZ %in% c(2, 3, 4) ~ "Wid/Separated",
      TRUE ~ "Single"
    )
  )

# Race/ethnicity RIDRETH3 into a few groups
food = food %>%
  mutate(
    race_recode = case_when(
      RIDRETH3 %in% c(1, 2) ~ "Hispanic",
      RIDRETH3 == 3 ~ "White",
      RIDRETH3 == 4 ~ "Black",
      TRUE ~ "Other"
    )
  )

##############################################
### 3. Prepare modeling dataset
##############################################

# Convert categorical variables to factors
food$healthy_diet   = factor(food$healthy_diet)
food$RIAGENDR       = factor(food$RIAGENDR)       # gender
food$race_recode    = factor(food$race_recode)
food$marital_recode = factor(food$marital_recode)
food$DMDEDUC2       = factor(food$DMDEDUC2)       # education

# Keep only variables we need for the model
model_data = food %>%
  select(
    healthy_diet,
    RIAGENDR,        # gender
    RIDAGEYR,        # age
    race_recode,     # recoded race/ethnicity
    marital_recode,  # recoded marital status
    DMDEDUC2,        # education
    INDFMPIR         # income-to-poverty ratio
  ) %>%
  drop_na()


##4. Build models stepwise and compare them


# Model A: only age + gender
model_A = glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR,
  data = model_data,
  family = binomial
)

# Model B: add education
model_B = glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR + DMDEDUC2,
  data = model_data,
  family = binomial
)

anova(model_A, model_B, test = "Chisq")

# Model C: add income
model_C = glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR + DMDEDUC2 + INDFMPIR,
  data = model_data,
  family = binomial
)

anova(model_B, model_C, test = "Chisq")

# Model D: add race
model_D = glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR + DMDEDUC2 + INDFMPIR + race_recode,
  data = model_data,
  family = binomial
)

anova(model_C, model_D, test = "Chisq")

# Model E: add marital status
model_E = glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR + DMDEDUC2 + INDFMPIR +
    race_recode + marital_recode,
  data = model_data,
  family = binomial
)

anova(model_D, model_E, test = "Chisq")

##############################################
### 5. Final model 
##############################################
#best final model is model D 
model3 = model_D
summary(model3)

##############################################
### 6. Evaluate model performance
##############################################

# Predicted probabilities for a healthy diet
pred_prob3 = predict(model3, type = "response")

# Turn probabilities into 0/1 using 0.5 cutoff
pred_class3 = if_else(pred_prob3 >= 0.5, 1, 0)

# Confusion matrix
conf_mat3 = table(pred_class3, model_data$healthy_diet)
conf_mat3

# Overall accuracy
accuracy3 = mean(pred_class3 == model_data$healthy_diet)
accuracy3

