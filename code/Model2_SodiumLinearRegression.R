## This script does the Linear Regression for finding the Determinants of Sodium Intake
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
library(car)

#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("..")

#### Reading the parquet file
food <- read_parquet("proc_data/food_final_morevars.parquet")
str(food)
summary(food)

#Subset with only focus variables
food_model <- subset(food, select = c(gender,age,race,timeus,hhsize,income_pov_cat,sodium))
str(food_model)
xkablesummary(food_model)

#Checking for missing values
sum(is.na(food_model))
food_model <-  food_model[!is.na(food_model$income_pov_cat),]
xkablesummary(food_model)

# EDA
#Histogram of Sodium distribution
hsodium <- ggplot(food_model, aes(sodium)) +
  geom_histogram(
    breaks=seq(0, 21000, by = 1000),
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Daily Sodium Intake",
    x = "Total Sodium (mg)",
    y = "Frequency"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),   # outside area
    panel.background = element_rect(fill = "antiquewhite", color = NA),  # plot area
    panel.grid.major = element_line(color = "white"),             # optional: lighten grid
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # title
    axis.title = element_text(size = 20, face = "bold"),               # axis titles
    axis.text = element_text(size = 20)                                # axis labels
  )

# Display the plot
hsodium
# Save plot
ggsave("output/EDA_hist_sodium.png", plot = hist_sodium, width = 8, height = 6, dpi = 300)

#Boxplot of Sodium Intake by genders
box_sodium_gender <- ggplot(food_model, aes(x = gender, y = sodium, fill = gender)) +
  geom_boxplot(
    alpha = 0.8, 
    color = "chocolate4"
  ) +
  scale_fill_brewer(
    palette = "Set3"
  ) +
  labs(
    title = "Boxplot of Sodium Intake by Genders",
    x = "Genders",
    y = "Sodium Intake (mg)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(), 
    
    #Font sizes
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 20),
    legend.position = "none"
  )
# Display the plot
box_sodium_gender

# Save plot
ggsave("output/EDA_box_sodium_gender.png", plot = box_sodium_gender, width = 8, height = 6, dpi = 300)

#Scatterplot of Sodium Intake by different ages
scatter_sodium_age <- ggplot(food_model, aes(x = age, y = sodium)) +
  geom_point(
    alpha = 0.5, 
    color = "chocolate4"
    ) +
  scale_fill_brewer(
    palette = "Set3"
    ) +
  labs(
    title = "Scatterplot of Sodium Intake by different Ages",
    x = "Age",
    y = "Sodium Intake (mg)"
    ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(), 
    
    #Font sizes
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 20),
    legend.position = "none"
    )
# Display the plot
scatter_sodium_age

# Save plot
ggsave("output/EDA_scatter_sodium_age.png", plot = scatter_sodium_age, width = 8, height = 6, dpi = 300)

#Boxplot of Sodium Intake by different Ethnic Groups
box_sodium_ethnic <- ggplot(food_model, aes(x = race, y = sodium, fill = race)) +
  geom_boxplot(
    alpha = 0.8, 
    color = "chocolate4"
  ) +
  scale_fill_brewer(
    palette = "Set3"
  ) +
  labs(
    title = "Boxplot of Sodium Intake by different Ethnic Groups",
    x = "Ethnic Groups",
    y = "Sodium Intake (mg)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(), 
    
    #Font sizes
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 18),
    legend.position = "none"
  )
# Display the plot
box_sodium_ethnic

# Save plot
ggsave("output/EDA_box_sodium_ethnic.png", plot = box_sodium_ethnic, width = 8, height = 6, dpi = 300)

#Boxplot of Sodium Intake by different Income Groups
box_sodium_income <- ggplot(food_model, aes(x = income_pov_cat, y = sodium, fill = income_pov_cat)) +
  geom_boxplot(
    alpha = 0.8, 
    color = "chocolate4"
  ) +
  scale_fill_brewer(
    palette = "Set3"
  ) +
  labs(
    title = "Boxplot of Sodium Intake by different Income Groups",
    x = "Income Groups",
    y = "Sodium Intake (mg)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(), 
    
    #Font sizes
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 20),
    legend.position = "none"
  )
# Display the plot
box_sodium_income

# Save plot
ggsave("output/EDA_box_sodium_income.png", plot = box_sodium_income, width = 8, height = 6, dpi = 300)

# Display the all pairs plot
png("output/EDA_paris_plot.png", width = 800, height = 600)
pairs_plot <- pairs(food_model,
      labels = colnames(food_model),
      lower.panel = points,
      upper.panel = panel.smooth,
      diag.panel = NULL,
      gap = 0.5,
      cex.labels = 1.2,
      font.labels = 2,
      pch = 19,
      col = "lightblue",
      main = "Pairs Plot of food_model")
dev.off()


#Correlation between variables
food_model_num <- food_model[,c("age","hhsize","sodium")]
str(food_model_num)

#Correlation Matrix and plot
cor_matrix <- cor(food_model_num)
print(cor_matrix)
# Save plot
png("output/EDA_corplot_sodium.png", width = 800, height = 600)
corr_plot <- corrplot(cor_matrix,
                        method = "color",
                        col = COL2('PiYG'),
                        type = "full",
                        addCoef.col = "black",
                        tl.col = "black",
                        tl.srt = 45,
                        diag = TRUE)
dev.off()

#Linear Models

#Model1 with only numerical variables (age and hhsize)
model1 = lm(sodium ~ age+hhsize, data = food_model)
summary(model1)
xkabledply(model1, title = "Model for sodium with age and hhsize as regressors" )
xkablevif(model1)

#Model2 with all focus variables (including categorical) 
model2 = lm(sodium ~ age+gender+race+income_pov_cat+timeus+hhsize, data = food_model)
summary(model2)
xkabledply(model2, title = "Model for sodium with all focus variables")
xkablevif(model2)

#Model3 with age and income as interaction terms
model3 = lm(sodium ~ age+gender+race*income_pov_cat, data = food_model)
summary(model3)
xkabledply(model3, title = "Model for sodium with all focus variables including interaction between race and income")
xkablevif(model3)

#Model4 with variables up to 3 interaction terms
model4 = lm(sodium ~ age + (gender + race + income_pov_cat)^3, data = food_model)
summary(model4)
xkabledply(model4, title = "Model for sodium with  variables upto 3 interactions")
xkablevif(model4)

#Anova Testing
anova = anova(model1,model2,model3,model4)
anova
xkablesummary(anova)

##Best Model Visualizations

#Extract and Prepare Model 2 Data
model2_data <- tidy(model2) %>%
  # Filter out the (Intercept) for cleaner visualization of predictors
  filter(term != "(Intercept)") %>% 
  # Create a significance label
  mutate(
    is_significant = p.value < 0.05,
    short_term = gsub("raceNon-Hispanic |income_pov_cat|gender", "", term) 
  )

#Create the P-value Bar Plot
pplot_model2 = ggplot(model2_data, aes(x = p.value, y = reorder(short_term, p.value), fill = is_significant)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(p.value, 4)), 
            hjust = -0.1, 
            color = "black", 
            size = 7) +
  xlim(0, max(model2_data$p.value) * 1.1) +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "lightcoral"),
                    labels = c("Not Significant (p ≥ 0.05)", "Significant (p < 0.05)")) +
  labs(
    title = "P-values of Predictors in Model 2",
    x = "P-value",
    y = "Predictor Term",
    fill = "Significance"
  ) +
  theme(
    #Font sizes
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text  = element_text(size = 15),
    legend.position = "bottom"
  )

#display the plot 
pplot_model2

#save the plot 
ggsave("output/EDA_pplot_sodiumM2.png", plot = pplot_model2, width = 15, height = 10, dpi = 300)


#Extract and Prepare Model 3 Data
pplott_model3 = model3_data <- tidy(model3) %>%
  # Filter out the (Intercept) for cleaner visualization of predictors
  filter(term != "(Intercept)") %>% 
  # Create a significance label
  mutate(
    is_significant = p.value < 0.05,
    short_term = gsub("raceNon-Hispanic |income_pov_cat|gender", "", term) 
  )

#Create the P-value Bar Plot
pplot_model3 = ggplot(model3_data, aes(x = p.value, y = reorder(short_term, p.value), fill = is_significant)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(p.value, 4)), 
            hjust = -0.1, 
            color = "black", 
            size = 3) +
  xlim(0, max(model3_data$p.value) * 1.1) +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "lightcoral"),
                    labels = c("Not Significant (p ≥ 0.05)", "Significant (p < 0.05)")) +
  labs(
    title = "P-values of Predictors in Model 3",
    x = "P-value",
    y = "Predictor Term",
    fill = "Significance"
  ) +
  theme(
    #Font sizes
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text  = element_text(size = 15),
    legend.position = "bottom"
  )

#display the plot 
pplot_model3

#save the plot 
ggsave("output/EDA_pplot_sodiumM3.png", plot = pplot_model3, width = 15, height = 10, dpi = 300)

# Comprehensive residual plots
par(mfrow = c(2, 2)) 
plot(model3, main = "Diagnostic Plots for Model 3") 


##Transformations
food_model$log_sodium = log(food_model$sodium)

#Histogram of Log-Transformed Sodium Intake
hlog_sodium <- ggplot(food_model, aes(log(sodium))) +
  geom_histogram(
    bins = 40,
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Log-Transformed Sodium Intake",
    x = "log(Sodium)",
    y = "Frequency"
  ) +
  theme(
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 20)
  )
# Display
hlog_sodium
# Save plot
ggsave("output/EDA_hist_sodium_log.png", plot = hlog_sodium, width = 8, height = 6, dpi = 300)

#Linear Regression Model With log(sodium)
model_log <- lm(log_sodium ~ age + gender + race + income_pov_cat + timeus + hhsize, data = food_model)
summary(model_log)
xkablesummary(model_log)
xkablevif(model_log)

# Comprehensive residual plots
par(mfrow = c(2, 2)) 
plot(model_log, main = "Diagnostic Plots for Model log(sodium) ")
