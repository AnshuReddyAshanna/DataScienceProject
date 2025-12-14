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
summary(food$sugar, na="sugar")

##### Distribution of Log of Sugar
## Weighted mean and sd for log_sugar
m_logsugar  <- with(df, weighted.mean(log_sugar, weight, na.rm = TRUE))
sd_logsugar <- with(df, {
  mu <- weighted.mean(log_sugar, weight, na.rm = TRUE)
  sqrt(weighted.mean((log_sugar - mu)^2, weight, na.rm = TRUE))
})

g2 <- ggplot(df, aes(x = log_sugar)) +
  geom_histogram(
    aes(
      y      = after_stat(density),
      weight = weight
    ),
    bins  = 30,
    alpha = 0.5,
    color = "#4DBBD5",
    fill  = "#4DBBD5"
  ) +
  geom_density(
    aes(weight = weight),
    color = "#00A087",
    size  = 1.2,
    alpha = 0.3,
    na.rm = TRUE
  ) +
  stat_function(
    fun  = dnorm,
    args = list(
      mean = m_logsugar,
      sd   = sd_logsugar
    ),
    color    = "#E64B35",
    size     = 1,
    linetype = "dashed"
  ) +
  labs(
    title    = "Distribution of Log Sugar Intake",
    subtitle = "Histogram with density curve and normal distribution overlay",
    x        = "Logarithm Sugar intake (g)",
    y        = "Density",
    caption  = "Note: Orange red dashed line represents the weighted normal distribution.\nCalculations using survey weights."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption   = element_text(hjust = 0),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

g2

####### Outliers
food_f = outlierKD2(food, sugar, rm = TRUE, boxplt = TRUE, qqplt = TRUE)

#187 observations
sum(is.na(food_f$sugar))

## Demographics for outliers
table(food_f$gender, is.na(food_f$sugar)) # More male than Female
tapply(food_f$age, is.na(food_f$sugar), mean, na.rm = TRUE) #AVG higher for non-missing
tapply(food_f$incomepovline, is.na(food_f$sugar), mean, na.rm = TRUE) #AVG higher for non-missing
table(food_f$race, is.na(food_f$sugar)) #More White

cat_plot_data <- food_f %>%
  mutate(sugar_missing = is.na(sugar)) %>%
  pivot_longer(cols = c(gender, race),
               names_to = "variable",
               values_to = "category") %>%
  group_by(variable, category) %>%
  summarise(prop_missing = mean(sugar_missing), .groups = "drop")

g3=cat_plot_data %>%
  mutate(category = str_to_title(category)) %>%
  ggplot(aes(x = category, y = prop_missing)) +
  geom_col(fill = "#4DBBD5", alpha=0.7) + 
  facet_wrap(~ variable, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Proportion Missing",
    title = "Proportion of Sugar Missing by Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave("output/sugar_outliers_catx.png", plot = g3, width = 8, height = 6, dpi = 300)

num_plot_data <- food_f %>%
  mutate(sugar_missing = ifelse(is.na(sugar), "Sugar missing", "Sugar not missing")) %>%
  pivot_longer(cols = c(age, incomepovline),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable, sugar_missing) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

g4=ggplot(num_plot_data,
          aes(x = sugar_missing, y = mean_value, fill = sugar_missing)) +
  geom_col(alpha=0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Sugar missing status",
    y = "Mean value",
    title = "Mean of Numeric Variables by Sugar Missing Status"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave("output/sugar_outliers_numx.png", plot = g4, width = 8, height = 6, dpi = 300)

#############################################
####### REVIEWING SUGAR VARIABLE LOGARITHM

####### Histrogram
m_sugar  <- with(food, weighted.mean(log_sugar, na.rm = TRUE))
sd_sugar <- with(food, {mu <- mean(log_sugar, na.rm = TRUE)
sqrt(mean((log_sugar - mu)^2, na.rm = TRUE))})

g5 <- ggplot(food, aes(x = log_sugar)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins  = 30,
    alpha = 0.5,
    color = "#4DBBD5",
    fill  = "#4DBBD5"
  ) +
  geom_density(
    color = "#00A087",
    size  = 1.2,
    alpha = 0.3,
    na.rm = TRUE
  ) +
  stat_function(
    fun  = dnorm,
    args = list(
      mean = m_sugar,
      sd   = sd_sugar
    ),
    color    = "#E64B35",
    size     = 1,
    linetype = "dashed"
  ) +
  labs(
    title    = "Distribution of Sugar Intake",
    subtitle = "Histogram with density curve and normal distribution overlay",
    x        = "Log Sugar intake",
    y        = "Density",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption   = element_text(hjust = 0),
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

ggsave("output/log_sugar_dist.png", plot = g5, width = 8, height = 6, dpi = 300)

### Outliers Log-Sugar
food_f = outlierKD2(df, log_sugar, rm = TRUE, boxplt = TRUE, qqplt = TRUE)

#160 observations
sum(is.na(food_f$log_sugar))

## Demographics for outliers
table(food_f$gender, is.na(food_f$log_sugar)) # Balanced
tapply(food_f$age, is.na(food_f$log_sugar), mean, na.rm = TRUE) #AVG higher for non-missing
tapply(food_f$incomepovline, is.na(food_f$log_sugar), mean, na.rm = TRUE) #AVG higher for non-missing
table(food_f$race, is.na(food_f$log_sugar)) #More Balanced

cat_plot_data <- food_f %>%
  mutate(sugar_missing = is.na(log_sugar)) %>%
  pivot_longer(cols = c(gender, race),
               names_to = "variable",
               values_to = "category") %>%
  group_by(variable, category) %>%
  summarise(prop_missing = mean(sugar_missing), .groups = "drop")

g6=cat_plot_data %>%
  mutate(category = str_to_title(category)) %>%
  ggplot(aes(x = category, y = prop_missing)) +
  geom_col(fill = "#4DBBD5", alpha=0.7) + 
  facet_wrap(~ variable, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Proportion Missing",
    title = "Proportion of Log Sugar Missing by Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave("output/log_sugar_outliers_catx.png", plot = g6, width = 8, height = 6, dpi = 300)

num_plot_data <- food_f %>%
  mutate(sugar_missing = ifelse(is.na(log_sugar), "Sugar missing", "Sugar not missing")) %>%
  pivot_longer(cols = c(age, incomepovline),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable, sugar_missing) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

g7=ggplot(num_plot_data,
          aes(x = sugar_missing, y = mean_value, fill = sugar_missing)) +
  geom_col(alpha=0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Sugar missing status",
    y = "Mean value",
    title = "Mean of Numeric Variables by Sugar Missing Status"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(hjust = 1),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
ggsave("output/log_sugar_outliers_numx.png", plot = g7, width = 8, height = 6, dpi = 300)

############################################################################
########### Correlation Variables
food_samp=food[c("log_sugar","gender", "age", "income_pov_cat","income_povline","race", "timeus",
                 "education", "maritalstatus", "numsnacks", "numhomemeals")]

pairs(food_samp)

cornum=cor(food[c("sugar", "age", "numsnacks")])
corrplot(cornum)


##############################################################################
############ Scatter Plot Sugar and Number of Snacks
g8= ggplot(df, aes(x = numsnacks, y = log_sugar)) +
  geom_point(
    color = "#4DBBD5",
    alpha = 0.5,
    size  = 2
  ) +
  geom_smooth(
    method = "lm",
    se     = TRUE,
    linewidth = 1,
    color = "#E64B35",
    na.rm = TRUE
  ) +
  labs(
    title = "Log Sugar Intake vs.\nNumber of Snacks",
    x     = "Number of Snacks consumed in 24h",
    y     = "Logarithm of Sugar Intake"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

ggsave("output/EDA_scat_logsugar_numsnacks.png", plot = g8, width = 8, height = 6, dpi = 300)

############################################################################
########### Linear Model
fit1 <- glm(log_sugar ~ numsnacks, data = df)
summary(fit1)

dfwithpred <- add_predictions(df,fit1)

ggplot(dfwithpred,aes(log_sugar,pred))+
  geom_point(aes(log_sugar,pred))+geom_line(aes(pred), colour="red", size=1)

fit2 <- glm(log_sugar ~ numsnacks + gender, data = df)
summary(fit2)

dfwithpred <- add_predictions(df,fit2)
ggplot(dfwithpred,aes(log_sugar,pred))+
  geom_point(aes(log_sugar,pred))+geom_line(aes(pred), colour="red", size=1)

anovaRes = anova(fit1,fit2)
anovaRes

fit25 <- glm(log_sugar ~ numsnacks*gender, data = df)
summary(fit25)

anovaRes = anova(fit1,fit2, fit25)
anovaRes
# The interaction is not improving the model

fit3 <- glm(log_sugar ~ numsnacks + gender + income_pov_cat, data = df)
summary(fit3)

##Including income diferently
df$income_pov_cat2 <- cut(
  df$incomepovline,
  breaks = c(-Inf, 3, Inf),
  labels = c(
    "<300",
    "300+"
  ),
  right = FALSE
)

fit3 <- glm(log_sugar ~ numsnacks + gender + income_pov_cat2, data = df)
summary(fit3)

fit3 <- glm(log_sugar ~ numsnacks + gender + incomepovline, data = df)
summary(fit3)

#ANOVA does not work because income has 55 less obs, still none of the dummies 
#are significant
#anovaRes = anova(fit1,fit2, fit3)
#anovaRes

fit4 <- glm(log_sugar ~ numsnacks + gender + age, data = df)
summary(fit4)
# Not significant
anovaRes = anova(fit1,fit2, fit4)
anovaRes

fit5 <- glm(log_sugar ~ numsnacks + gender + education, data = df)
summary(fit5)
anovaRes = anova(fit1,fit2, fit5)
anovaRes
# Education improves the model!! :D

fit6 <- glm(log_sugar ~ numsnacks + gender + education + race, data = df)
summary(fit6)
anovaRes = anova(fit1,fit2, fit5, fit6)
anovaRes
# Race improves the model!! White's eat more sugar

fit7 <- glm(log_sugar ~ numsnacks + gender + education + race + timeus, data = df)
summary(fit7)
anovaRes = anova(fit1,fit2, fit5, fit6, fit7)
anovaRes
# It does help a little but the p-value is polemic 0.055 if we use alpha 0.05....
vif(fit7)
# There is no multicollinearity and it does help improve a little the Adj-R2

fit8 <- glm(log_sugar ~ numsnacks + gender + education + race+ timeus+ maritalstatus, data = df)
summary(fit8)
anovaRes = anova(fit1,fit2, fit5, fit6,fit7, fit8)
anovaRes
# Not significant

fit9 <- glm(log_sugar ~ numsnacks + gender + education + race+ timeus+ maritalstatus + dayweek, data = df)
summary(fit9)
anovaRes = anova(fit1,fit2, fit5, fit6,fit7, fit9)
anovaRes
#Controling by day of the week does not improve the model

fit9 <- glm(log_sugar ~ numsnacks + gender + education + race+ timeus + numhomemeals, data = df)
summary(fit9)
anovaRes = anova(fit1,fit2, fit5, fit6,fit7, fit9)
anovaRes
## The number of home meals improves the model!!
df$log_fats=log(df$fats)
fit10 <- glm(log_sugar ~ numsnacks + gender + education + race+ timeus + numhomemeals +log_fats, data = df)
summary(fit10)
anovaRes = anova(fit1,fit2, fit5, fit6,fit7, fit9, fit10)
anovaRes
## ALSO CONTROLING FOR THE AMOUNT OF FAT CONSUMED

#The difference in days between the survey and reported consumption does not seem to matter
fit11 <- glm(log_sugar ~ numsnacks + gender + education + race+ timeus + numhomemeals +log_fats +dayssvy, data = df)
summary(fit11)

### TRYING SOME INTERACTIONS
fit11 <- glm(log_sugar ~ numsnacks + gender*education + race+ timeus + numhomemeals +log_fats, data = df)
summary(fit11)
anova(fit1,fit2, fit5, fit6,fit7, fit9, fit10, fit11)
# Interacting Gender and Education does not help
fit11 <- glm(log_sugar ~ numsnacks + gender + education + race+ timeus + numhomemeals +log_fats +militar, data = df)
summary(fit11)
anovaRes = anova(fit1,fit2, fit5, fit6,fit7, fit9, fit10, fit11)
anovaRes
#Militar status does not help


fit01 <- lm(log_sugar ~ numsnacks + gender + education + race + numhomemeals, data = df)
summary(fit01)
fit02 <- lm(log_sugar ~ gender + education + race+ timeus + numhomemeals + numsnacks, data = df)
summary(fit02)
fit03 <- lm(log_sugar ~ gender + age + I(age^2) + education + race+ timeus + numhomemeals + numsnacks, data = df)
summary(fit03)
fit04 <- lm(log_sugar ~ gender + education+race+ timeus + numhomemeals*numsnacks, data = df)
summary(fit04)

anovaRes = anova(fit02,fit04)
anovaRes


##### USE SUBSECT LEAPS

subfit <- regsubsets(
  log_sugar ~ ., data = food_samp,
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

######### SELECTED MODEL
fit02 <- glm(log_sugar ~ gender + education + race+ timeus + numhomemeals + numsnacks, data = df)
summary(fit02)
r.squaredGLMM(fit02)

## Coefplot
coef_df <- tidy(fit02, conf.int = TRUE) %>%
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
vif(fit02)
#Pretty good :D

# Saving the predictions:
dfwithpred <- add_predictions(df,fit02)
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
plot(fit02, ask=FALSE)

#IT HAS HETEROSCEDASTICITY AND THE ERRORS DIFFER FROM NORMAL AT THE TAILS


############################################################################
########### Logit Model to predict overconsumption of sugar
#According to the dietary guidelines for americans, we should consume 50 or less grams of sugar daily
food$dsugar <- factor(food$sugar > 50,
                      levels = c(FALSE, TRUE),
                      labels = c("No", "Yes"))

var_label(food$dsugar) <- "High Sugar Indicator"

# More people consume above 75% it is a little unbalanced
table(food$dsugar)

## Difference of age
ggplot(food, aes(x=dsugar, y = age,fill = dsugar)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Age Distribution by high-sugar indicator",
    x = "High-sugar indicator",
    y = "Age"
  ) +
  theme_minimal()
t.test(age ~ dsugar, data = food) #Yes - but kinda high pvalue

## Difference of income
ggplot(food, aes(x=dsugar, y = incomepovline,fill = dsugar)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Income to Poverty Ratio Distribution by high-sugar indicator",
    x = "High-sugar indicator",
    y = "Income to Poverty Ratio"
  ) +
  theme_minimal()

t.test(incomepovline ~ dsugar, data = food) #No

## Difference of snacks
ggplot(food, aes(x=dsugar, y = numsnacks,fill = dsugar)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Number of Snacks Distribution by high-sugar indicator",
    x = "High-sugar indicator",
    y = "Number of Snacks"
  ) +
  theme_minimal()
t.test(numsnacks ~ dsugar, data = food) #Yes

## Difference of meals at home
ggplot(food, aes(x=dsugar, y = numhomemeals,fill = dsugar)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(
    title = "Number of Meals at Home Distribution by high-sugar indicator",
    x = "High-sugar indicator",
    y = "Number of Meals at Home"
  ) +
  theme_minimal()
t.test(numhomemeals ~ dsugar, data = food) #Yes

## Difference Gender
sstable=xtabs(~ dsugar + gender, data = food)
ggplot(food, aes(x = gender, fill = dsugar)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("lightblue", "lightcoral"),
                    name = "High Sugar") +
  labs(title = "Proportion of High Sugar Consumption by Gender",
       x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
chisq.test(sstable) #Yes

## Difference Education
sstable=xtabs(~ dsugar + education, data = food)
ggplot(food, aes(x = education, fill = dsugar)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("lightblue", "lightcoral"),
                    name = "High Sugar") +
  labs(title = "Proportion of High Sugar Consumption by Education Level",
       x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
chisq.test(sstable) #Yes

## Difference Race
sstable=xtabs(~ dsugar + race, data = food)
ggplot(food, aes(x = race, fill = dsugar)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("lightblue", "lightcoral"),
                    name = "High Sugar") +
  labs(title = "Proportion of High Sugar Consumption by Race",
       x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
chisq.test(sstable) #Yes

## Difference Time U.S.
sstable=xtabs(~ dsugar + timeus, data = food)
ggplot(food, aes(x = timeus, fill = dsugar)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("lightblue", "lightcoral"),
                    name = "High Sugar") +
  labs(title = "Proportion of High Sugar Consumption by Time in the U.S.",
       x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
chisq.test(sstable) #Yes

## Difference Marital Status
sstable=xtabs(~ dsugar + maritalstatus, data = food)
ggplot(food, aes(x = maritalstatus, fill = dsugar)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("lightblue", "lightcoral"),
                    name = "High Sugar") +
  labs(title = "Proportion of High Sugar Consumption by Marital Status",
       x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
chisq.test(sstable) #No

######### MODEL
logit0 = glm(dsugar ~ age + numsnacks+ numhomemeals+ gender + education
             + race + timeus, data = food, family = "binomial")
summary(logit0)
r.squaredGLMM(logit0)

## Coefs for interpretation!
expcoeff=exp(coef(logit0))
expcoeff

## Confusion matrix with default 0.5 cutoff:
cutoff = 0.5
pred = ifelse(logit0$fitted.values > cutoff, 1, 0)
cm = table("Actual"= logit0$y,"Predicted" = pred)
cat("**Confusion Matrix**")
cm

TN = cm["0", "0"]
FP = cm["0", "1"]
FN = cm["1", "0"]
TP = cm["1", "1"]
Total = sum(cm)
Accuracy=(TP + TN) / Total
Precision=TP / (TP + FP)
Recall=TP / (TP + FN)
Specificity=TN / (TN + FP)

Accuracy
Precision
Recall
Specificity

#The accuracy is not so bad, but the specificity is really low, 
#I am not getting the true negatives
#This can be due to the cutoff

# prediction
food$prediction <- predict( logit0, newdata = food, type = "response" )

# distribution of the prediction score grouped by known outcome
ggplot( food, aes( prediction, color = as.factor(dsugar) ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's Predicted Score" ) + 
  scale_color_economist( name = "data", labels = c( "negative", "positive" ) ) + 
  theme_economist()

# This is such a bad model :'(

df_plot=tibble( dsugar = logit0$y, pred= logit0$fitted.values) |>
  mutate(type = case_when(
    dsugar == 1 & pred >= cutoff ~ "TP",
    dsugar == 0 & pred >= cutoff ~ "FP",
    dsugar == 1 & pred <  cutoff ~ "FN",
    dsugar == 0 & pred <  cutoff ~ "TN"
  ), type = factor(type, levels = c("FN", "FP", "TN", "TP")))

cols <- c(FN = "tomato", FP = "yellowgreen", TN = "cyan3", TP = "orchid")

ggplot(df_plot, aes(x = factor(dsugar), y = pred)) +
  geom_violin(fill = "white", color = NA, alpha = 0.6, trim = TRUE) +
  geom_jitter(aes(color = type), width  = 0.5, height = 0, alpha  = 0.6, size   = 1.8, fill   = NA, shape  = 21,) +
  geom_hline(yintercept = cutoff, color = "red", alpha = 0.6) +
  scale_color_manual(values = cols, name = "type") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = sprintf("Threshold at %.2f", cutoff), x = "survived", y = "predicted") +
  theme_minimal(base_size = 12) +
  theme( panel.background = element_rect(fill = "gray90", color = NA),
         plot.background  = element_rect(fill = "gray90", color = NA))

### 0.75 would be better but still this is so spread :(
cutoff = 0.75
pred = ifelse(logit0$fitted.values > cutoff, 1, 0)
cm = table("Actual"= logit0$y,"Predicted" = pred)
cat("**Confusion Matrix**")
cm

TN = cm["0", "0"]
FP = cm["0", "1"]
FN = cm["1", "0"]
TP = cm["1", "1"]
Total = sum(cm)
Accuracy=(TP + TN) / Total
Precision=TP / (TP + FP)
Recall=TP / (TP + FN)
Specificity=TN / (TN + FP)

Accuracy
Precision
Recall
Specificity

# Looking better
df_plot=tibble( dsugar = logit0$y, pred= logit0$fitted.values) |>
  mutate(type = case_when(
    dsugar == 1 & pred >= cutoff ~ "TP",
    dsugar == 0 & pred >= cutoff ~ "FP",
    dsugar == 1 & pred <  cutoff ~ "FN",
    dsugar == 0 & pred <  cutoff ~ "TN"
  ), type = factor(type, levels = c("FN", "FP", "TN", "TP")))

cols <- c(FN = "tomato", FP = "yellowgreen", TN = "cyan3", TP = "orchid")

ggplot(df_plot, aes(x = factor(dsugar), y = pred)) +
  geom_violin(fill = "white", color = NA, alpha = 0.6, trim = TRUE) +
  geom_jitter(aes(color = type), width  = 0.5, height = 0, alpha  = 0.6, size   = 1.8, fill   = NA, shape  = 21,) +
  geom_hline(yintercept = cutoff, color = "red", alpha = 0.6) +
  scale_color_manual(values = cols, name = "type") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = sprintf("Threshold at %.2f", cutoff), x = "survived", y = "predicted") +
  theme_minimal(base_size = 12) +
  theme( panel.background = element_rect(fill = "gray90", color = NA),
         plot.background  = element_rect(fill = "gray90", color = NA))

## McFadden
logit0Null <- glm(dsugar ~ 1, data = food, family = "binomial")
mcFadden = 1 - logLik(logit0)/logLik(logit0Null)
mcFadden

## Receiver Operating Characteristic
prob=predict(logit0, type = "response" )
food$prob=prob
h= roc(dsugar~prob, data=food)
auc(h) # 0.6788 Not good
plot(h)

### Should I do cross validation even if the model does not work?
#set.seed(4321)
# Create CV folds
#food_split=initial_split(food, prop = 0.8, strata = dsugar)
#data_train <- training(food_split)
#data_test <- testing(food_split)



