##############################################
# Model 3: Logistic Regression – Healthy Diet
# Predicting a “Healthy Diet” Classification
##############################################

#### Preliminary ----

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

# Set working directory (adjust if needed)
getwd()
setwd("/Users/marlon/Documents/EDUCATION/GWU/GWU Courses/GWU Fall 25/Introduction to Data Science_Darcy Morris/ModelDataWhatWeEat")

# Read the parquet file
food = read_parquet("proc_data/demo_foods.parquet")
head(food)
names(food)

##############################################
# 1. Create healthy diet score by using 5-out-of-7 nutritional criteria
##############################################

# Note:
# - For protein and fiber, "healthy" means AT LEAST the benchmark (>= 50g protein, >= 28g fiber)
# - For all others, "healthy" means AT MOST the benchmark (≤ recommended or less)
##############################################
### Gender-Specific Healthy Diet Index
##############################################

food <- food %>%
  mutate(
    # Build gender-specific benchmarks
    healthy_score = case_when(
      # FEMALE thresholds (RIAGENDR == 2 in NHANES)
      RIAGENDR == 2 ~ (
        (DR1IKCAL_sum <= 2000) +    # calories
          (DR1ISODI_sum < 2300)  +    # sodium
          (DR1IPROT_sum >= 50)   +    # protein
          (DR1ISUGR_sum <= 50)   +    # sugar
          (DR1ICARB_sum <= 275)  +    # carbs
          (DR1IFIBE_sum >= 28)   +    # fiber
          (DR1ITFAT_sum <= 78)        # fat
      ),
      
      # MALE thresholds (RIAGENDR == 1)
      RIAGENDR == 1 ~ (
        (DR1IKCAL_sum <= 2500) +    # calories
          (DR1ISODI_sum < 2300)  +    # sodium
          (DR1IPROT_sum >= 56)   +    # protein
          (DR1ISUGR_sum <= 50)   +    # sugar
          (DR1ICARB_sum <= 300)  +    # carbs
          (DR1IFIBE_sum >= 31)   +    # fiber
          (DR1ITFAT_sum <= 88)        # fat
      )
    ),
    
    # Healthy if meeting ≥ 5 out of 7 criteria
    healthy_diet = if_else(healthy_score >= 5, 1, 0)
  )

# Check how many are classified as healthy vs not
table(food$healthy_diet)
prop.table(table(food$healthy_diet))  # ~76% unhealthy, ~24% healthy (example)

##############################################
# 2. Recode demographic variables
##############################################

# Marital status:
# We regroup as:
# 1 = Married/Living with partner  -> "Partnered" (codes 1, 6)
# 2 = Widowed/Separated            -> "Wid/Separated" (codes 2, 4)
# 3 = Single                       -> "Single" (includes divorced, never married, 77, 99, NA)
# Note: exact NHANES codes:
# 1 married, 2 widowed, 3 divorced, 4 separated, 5 never married, 6 living with partner,
# 77 refused, 99 don't know

food = food %>%
  mutate(
    marital_recode = case_when(
      DMDMARTZ %in% c(1, 6) ~ "Partnered",         # married or living with partner
      DMDMARTZ %in% c(2, 4) ~ "Wid/Separated",     # widowed or separated
      TRUE ~ "Single"                              # divorced, never married, 77, 99, NA
    )
  )

# Race / ethnicity:
# RIDRETH3 codes:
# 1 = Mexican American, 2 = Other Hispanic,
# 3 = Non-Hispanic White, 4 = Non-Hispanic Black, others = "Other"
# We combine Mexican American + Other Hispanic into one "Hispanic" group
food = food %>%
  mutate(
    race_recode = case_when(
      RIDRETH3 %in% c(1, 2) ~ "Hispanic",
      RIDRETH3 == 3 ~ "White",
      RIDRETH3 == 4 ~ "Black",
      TRUE ~ "Other"
    )
  )

# Length of time in the U.S.:
# The current dataset (names(food)) does NOT contain a years-in-US variable.
# If a variable like DMDYRUSZ or DMDYRSUS existed, we would recode it as:
# 1. Less than 4 years (1–2)
# 2. 5–15 years (3–4)
# 3. 16+ years (5–6–77–99–NA)
# To avoid errors, we only apply this recode IF such a variable exists.

if ("DMDYRUSZ" %in% names(food) | "DMDYRSUS" %in% names(food)) {
  years_var = if ("DMDYRUSZ" %in% names(food)) "DMDYRUSZ" else "DMDYRSUS"
  food = food %>%
    mutate(
      yearsUS_recode = case_when(
        .data[[years_var]] %in% c(1, 2) ~ "<4 years",
        .data[[years_var]] %in% c(3, 4) ~ "5–15 years",
        TRUE ~ "16+ years"
      )
    )
}

# Country of origin:
# The current dataset also does NOT show DMDBORN4 in names(food).
# If it existed, we would recode:
# 1 = US-born -> "US"
# otherwise    -> "Non-US"

if ("DMDBORN4" %in% names(food)) {
  food = food %>%
    mutate(
      origin_recode = case_when(
        DMDBORN4 == 1 ~ "US",
        TRUE ~ "Non-US"
      )
    )
}

##############################################
# 3. Prepare modeling dataset
##############################################

# Convert categorical variables to factors
food$healthy_diet   = factor(food$healthy_diet)
food$RIAGENDR       = factor(food$RIAGENDR)        # gender
food$race_recode    = factor(food$race_recode)     # recoded race
food$marital_recode = factor(food$marital_recode)  # recoded marital status
food$DMDEDUC2       = factor(food$DMDEDUC2)        # education

# Keep only variables we need for the model
model_data = food %>%
  select(
    healthy_diet,
    RIAGENDR,        # gender
    RIDAGEYR,        # age (numeric)
    race_recode,     # recoded race/ethnicity
    marital_recode,  # recoded marital status
    DMDEDUC2,        # education
    INDFMPIR         # income-to-poverty ratio
  ) %>%
  drop_na()

##############################################
# 4. Build models stepwise and compare them
##############################################

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

# Model E: add marital status (for testing, even if not useful)
model_E = glm(
  healthy_diet ~ RIAGENDR + RIDAGEYR + DMDEDUC2 + INDFMPIR +
    race_recode + marital_recode,
  data = model_data,
  family = binomial
)

anova(model_D, model_E, test = "Chisq")

##############################################
# 5. Final model
##############################################
# Based on the ANOVA results, Model D is chosen as the best model:
# It includes only predictors that significantly improve fit
# and avoids unstable marital-status effects.

model3 = model_D
summary(model3)

##############################################
# 6. Evaluate model performance
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

# -----------------------------
# Confusion matrix metrics
# -----------------------------

# Cutoff used
cutoff <- 0.5

# Extract values
TN <- conf_mat3["0","0"]
FP <- conf_mat3["1","0"]
FN <- conf_mat3["0","1"]
TP <- conf_mat3["1","1"]

# Accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN)

# Sensitivity (Recall)
sensitivity <- TP / (TP + FN)

# Specificity
specificity <- TN / (TN + FP)

# Precision
precision <- TP / (TP + FP)

metrics <- list(
  Cutoff_used = cutoff,
  Accuracy = accuracy,
  Sensitivity = sensitivity,
  Specificity = specificity,
  Precision = precision
)

metrics

##############################################
# 7. Vizualization 
##############################################

#Distribution by Gender
library(ggplot2)

ggplot(food, aes(x = healthy_score, fill = RIAGENDR)) +
  geom_histogram(position = "dodge", bins = 7, alpha = 0.85) +
  scale_fill_manual(
    values = c("#1f78b4", "#e31a1c"),
    labels = c("Male", "Female")
  ) +
  labs(
    title = "Distribution of Healthy Diet Scores by Gender",
    subtitle = "Scores reflect how many dietary guidelines (0–7) were met",
    x = "Healthy Diet Score",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face="bold"),
    legend.position = "right"
  )

#Predicted Probability vs Age
ggplot(model_data, aes(x = RIDAGEYR, y = pred_prob3, color = RIAGENDR)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = FALSE, size = 1.3) +
  scale_color_manual(
    values = c("#1f78b4", "#e31a1c"),
    labels = c("Male", "Female")
  ) +
  labs(
    title = "Predicted Probability of Healthy Diet by Age",
    subtitle = "Model-adjusted predictions with gender-specific diet thresholds",
    x = "Age",
    y = "Predicted Probability",
    color = "Gender"
  ) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face="bold"))

# bar chart healthy diet prevalence by race

ggplot(food, aes(x = race_recode, fill = healthy_diet)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Healthy Diet by Race",
    y = "Proportion",
    x = "Race"
  )

ggplot(food, aes(x = race_recode, fill = healthy_diet)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("#cccccc", "#1b9e77"),
    labels = c("Unhealthy", "Healthy")
  ) +
  labs(
    title = "Proportion of Healthy Diet Classification by Race",
    subtitle = "Based on gender-specific benchmarks (5 out of 7 criteria)",
    x = "Race/Ethnicity",
    y = "Proportion",
    fill = "Diet Classification"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face="bold"),
    legend.position = "right"
  )



##ROC

library(pROC)

roc_obj <- roc(model_data$healthy_diet, pred_prob3)

roc_df <- data.frame(
  tpr = roc_obj$sensitivities,
  fpr = 1 - roc_obj$specificities
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(size = 1.2, color = "#1f78b4") +
  geom_abline(linetype = "dashed", color = "grey50") +
  labs(
    title = "ROC Curve for Healthy Diet Model",
    subtitle = paste("AUC =", round(auc(roc_obj), 3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face="bold"))

#confusion matrix
library(reshape2)

cm_df <- as.data.frame(conf_mat3)
names(cm_df) <- c("Predicted", "Actual", "Freq")

ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "#6baed6", high = "#08519c") +
  labs(
    title = "Confusion Matrix (Heatmap)",
    x = "Predicted Label",
    y = "Actual Label"
  ) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face="bold"))

