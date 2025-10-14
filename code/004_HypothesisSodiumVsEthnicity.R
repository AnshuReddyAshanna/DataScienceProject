## This script does the Hypothesis testing: Is Sodium Intake different by ethnic groups
rm(list = ls())
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
setwd("..")

#### Reading the parquet file
food <- read_parquet("proc_data/demo_foods.parquet")


## Histograms by Ethnicity
# Make sure gender is a factor with readable labels
food$RIDRETH3 <- factor(food$RIDRETH3,
                        levels = c(1, 2, 3, 4, 6, 7),
                        labels = c("Mexican American",
                                   "Other Hispanic",
                                   "Non-Hispanic White",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic Asian",
                                   "Other Race - Including Multi-Racial"))
## Summary Statistics
sodium_summary <- food %>%
  group_by(RIDRETH3) %>%
  summarise(
    mean_sodium = mean(DR1ISODI_sum, na.rm = TRUE),
    sd_sodium   = sd(DR1ISODI_sum, na.rm = TRUE),
    median_sodium = median(DR1ISODI_sum, na.rm = TRUE),
    N = n()
  )
xkabledply(sodium_summary, title = "Summary Statistics: Sodium Intake by Ethnic Group")


##Boxplot of Sodium intake across different ethnic groups
box_sodium_ethnic <- ggplot(food, aes(x = RIDRETH3, y = DR1ISODI_sum, fill = RIDRETH3)) +
  geom_boxplot(alpha = 0.8, color = "chocolate4") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Boxplot of Sodium Intake by different Ethnic Groups",
    x = "Ethnic Groups",
    y = "Sodium Intake (grams)",
    fill = "Ethnic Groups"
    ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 12)
  )

ggsave("output/EDA_box_sodium_ethnic.png", plot = box_sodium_ethnic, width = 8, height = 6, dpi = 300)

##Barplot of Average Sodium Intake by different Ethnic Groups
bar_sodium_ethnic <- ggplot(sodium_summary, aes(x = RIDRETH3, y = mean_sodium, fill = RIDRETH3)) +
  geom_col(alpha = 0.8, color = "chocolate4") +
  #geom_errorbar(aes(ymin = mean_sodium - sd_sodium, ymax = mean_sodium + sd_sodium), width = 0.2) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Barplot of Average Sodium Intake by different Ethnic Groups",
    x = "Ethnic Group",
    y = "Mean Sodium Intake (grams)",
    fill = "Ethnic Group"
  ) +
  theme_minimal(base_size = 14)

ggsave("output/EDA_bar_sodium_ethnic.png", plot = bar_sodium_ethnic, width = 8, height = 6, dpi = 300)

##Hypothesis
## Null Hypothesis (H0): There is no significant difference in mean sodium intake among ethnic groups.
## Alternative Hypothesis (H1) : There is a significant difference in mean sodium intake among at least one ethnic groups.

### ANOVA TEST
anova_result <- aov(DR1ISODI_sum ~ RIDRETH3, data = food)
xkabledply(anova_result, title = "ANOVA Summary: Sodium Intake by Ethnic Groups")

## Since p-value < 0.05 we reject null hypothesis, concluding that sodium intake significantly differs among ethnic groups.  





