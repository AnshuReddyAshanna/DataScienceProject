## This script does the Hypothesis testing: Is Sodium Intake different by ethnic groups
rm(list = ls())
#### Preliminary
# Libraries
library(arrow)
library(ggplot2)
library(ezids)
library(labelled)
library(gtsummary)
library(dplyr)
library(tidyr)
library(broom)
library(knitr)
library(lattice)
library(corrplot)
library(scales)
library("modelr")
library(car)
library(MuMIn)
library(ROCR)
library(grid)
library(caret)
library(ggthemr)
library(ggthemes)
library(gridExtra)
library(data.table)
library(tidymodels)
library(tidyverse)
library(survey)
library(wCorr)
library(treemapify)
library(Hmisc)


#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("..")

#############################################
#### Reading the parquet file
food <- read_parquet("proc_data/food_final_morevars.parquet")

#No missing in Y sample
df <- subset(food, is.finite(log_sugar) & !is.na(log_sugar))
#############################################
####### REVIEWING SUGAR VARIABLE
summary(df$log_sugar)

############################################################################
########### Correlation Variables
dffilt=df[c("log_sugar","gender", "education", "race", "timeus",
            "age", "income_pov_cat", "maritalstatus", "militar",
            "numsnacks", "numhomemeals", "dayweek")]

pairs(dffilt)

cornum=cor(dffilt[c("log_sugar", "age", "numsnacks", "numhomemeals")])
corrplot(cornum)


############################################################################
########### Linear Model

##### USE SUBSECT LEAPS
subfit <- regsubsets(
  log_sugar ~ ., data = dffilt,
  nvmax = 10,           # max number of predictors allowed
  method = "exhaustive" # or "forward", "backward", "seqrep"
)

subsum <- summary(subfit)

# Look at criteria by subset size
subsum$adjr2   # adjusted R^2
subsum$cp      # Mallows' Cp
subsum$bic     # BIC

best_r2 <- which.max(subsum$adjr2)
best_bic <- which.max(subsum$bic)

best_coefs <- coef(subfit, best_r2)
best_vars  <- names(best_coefs)[-1]  # drop "(Intercept)"
best_vars
best_coefs

## THIS IS NOT GOOD WITH CATEGORICAL VARIABLES!!!


#Benchmark
fit1 <- glm(log_sugar ~ numsnacks + numhomemeals, data = df)
summary(fit1)
# + gender
fit2 <- glm(log_sugar ~ numsnacks + numhomemeals + gender, data = df)
summary(fit2)
anova(fit1,fit2)
# + education
fit3 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education, data = df)
summary(fit3)
anova(fit1,fit2, fit3)
# + age
fit4 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + age, data = df)
summary(fit4)
anova(fit1,fit2, fit3, fit4)
# + age & age^2
fit5 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + age + I(age^2), data = df)
summary(fit4)
anova(fit1,fit2, fit3, fit4, fit5)

anova_res <- anova(fit1,fit2, fit3, fit4, fit5, test = "F")
anova_df <- as.data.frame(anova_res)
added_terms <- c(NA, "gender", "education", "age", "age^2")
result_table <- data.frame(Model = paste0("Model ", 1:5),
  Added_Term = added_terms,
  P_Value = c(NA, anova_df$`Pr(>F)`[-1]))

result_table


### New Benchmark
fit1 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education, data = df)
summary(fit1)

#+race
fit2 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race, data = df)
summary(fit2)
anova(fit1,fit2)

#+timeus
fit3 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus, data = df)
summary(fit3)
anova(fit1,fit2, fit3)

#maritalstatus
fit4 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus+maritalstatus, data = df)
summary(fit3)
anova(fit1,fit2, fit3, fit4)

anova_res <- anova(fit1,fit2, fit3, fit4, test = "F")
anova_df <- as.data.frame(anova_res)
added_terms <- c(NA, "race", "timeus", "maritalstatus")
result_table <- data.frame(Model = paste0("Model ", 1:4),
                           Added_Term = added_terms,
                           P_Value = c(NA, anova_df$`Pr(>F)`[-1]))

result_table

### New Benchmark
fit1 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus, data = df)
summary(fit1)
#+militar
fit2 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus + militar, data = df)
summary(fit2)
anova(fit1,fit2)
#+dayweek
fit2 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus + dayweek, data = df)
summary(fit2)
anova(fit1,fit2)
#+income
fit2 <- glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus + income_pov_cat, data = df)
summary(fit2)
tempf1=glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus, data = df[!is.na(df$income_pov_cat), ])
anova(tempf1,fit2)

dffinal=df[!is.na(df$income_pov_cat), ]

fitfinal=glm(log_sugar ~ numsnacks + numhomemeals + gender + education + race +timeus + income_pov_cat, data = dffinal)
summary(fitfinal)
r.squaredGLMM(fitfinal)
#Null
pchisq(2670.1, 4237, lower.tail=F)
#Fitt
pchisq(2398.4, 4219, lower.tail=F)
#Best?
pchisq(2670.1-2398.4, 4237-4219, lower.tail=F)

######### SELECTED MODEL
## Coefplot
coef_df <- tidy(fitfinal, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # significance stars
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    coef_label = sprintf("%.3f%s", estimate, sig),
    term = gsub("_", " ", term),
    term = factor(term, levels = rev(unique(term)))
  )

ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "#4DBBD5", alpha=0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = "#4DBBD5", alpha=0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color="#E64B35") +
  geom_text(aes(label = coef_label), vjust=-0.5,
            hjust = 0, size = 3.5) +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank()
  )

###### Multicollinearity
vif(fitfinal)
#Pretty good :D

# Saving the predictions:
dfwithpred <- add_predictions(dffinal,fitfinal)
#Plot predictions vs real value
ggplot(dfwithpred, aes(x = log_sugar, y = pred)) +
  geom_point(alpha = 0.5, color = "#4DBBD5", size = 2) +
  geom_line(aes(pred), colour = "#E64B35", size = 1.2) +
  labs(
    x = "Observed log(Sugar Intake)",
    y = "Predicted log(Sugar Intake)",
    title = "Observed vs. Predicted Sugar Intake"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

## Exploring Model
par(mfrow = c(1,1))
plot(fitfinal, ask=FALSE)

#IT HAS HETEROSCEDASTICITY AND THE ERRORS DIFFER FROM NORMAL AT THE TAILS