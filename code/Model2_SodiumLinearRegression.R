#Model 1: Linear Regression - Determinants of Sodium Intake

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

#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("..")

#### Reading the parquet file
food <- read_parquet("proc_data/food_final_morevars.parquet")
head(food)
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
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # title
    axis.title = element_text(size = 14, face = "bold"),               # axis titles
    axis.text = element_text(size = 12)                                # axis labels
  )

# Display the plot
hsodium

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

# Display the all pairs plot
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


#Correlation between variables
food_model_num <- food_model[,c("age","hhsize","sodium")]
str(food_model_num)

#Correlation Matrix and plot
cor_matrix <- cor(food_model_num)
print(cor_matrix)
corr_plot <- corrplot(cor_matrix,
                        method = "color",
                        col = COL2('PiYG'),
                        type = "full",
                        addCoef.col = "black",
                        tl.col = "black",
                        tl.srt = 45,
                        diag = TRUE)







