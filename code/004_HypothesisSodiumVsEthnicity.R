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

#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("..")

#### Reading the parquet file
food <- read_parquet("proc_data/demo_foods.parquet")


# Make sure ethnic group is a factor with readable labels
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
    y = "Sodium Intake (grams)"
    ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(), 
    
    #Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text  = element_text(size = 15),
    legend.position = "none"
  )
# Display the plot
box_sodium_ethnic

# Save plot
ggsave("output/EDA_box_sodium_ethnic.png", plot = box_sodium_ethnic, width = 8, height = 6, dpi = 300)

##Barplot of Average Sodium Intake by different Ethnic Groups
bar_sodium_ethnic <- ggplot(sodium_summary, aes(x = RIDRETH3, y = mean_sodium, fill = RIDRETH3)) +
  geom_col(
    alpha = 0.8, 
    color = "chocolate4"
    ) +
  scale_fill_brewer(
    palette = "Set3"
    ) +
  labs(
    title = "Barplot of Average Sodium Intake by different Ethnic Groups",
    x = "Ethnic Group",
    y = "Mean Sodium Intake (grams)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text  = element_text(size = 15),
    legend.position = "none"
  )

# Display the plot
bar_sodium_ethnic

# Save the plot
ggsave("output/EDA_bar_sodium_ethnic.png", plot = bar_sodium_ethnic, width = 8, height = 6, dpi = 300)

##Hypothesis
## Null Hypothesis (H0): There is no significant difference in mean sodium intake among ethnic groups.
## Alternative Hypothesis (H1) : There is a significant difference in mean sodium intake among at least one ethnic groups.

#### ANOVA TEST #####
anova_result <- aov(DR1ISODI_sum ~ RIDRETH3, data = food)

# Display ANOVA test result
anova_result

# Show the summary of the ANOVA test
xkabledply(anova_result, title = "ANOVA Summary: Sodium Intake by Ethnic Groups")

## Since p-value < 0.05 we reject null hypothesis, concluding that sodium intake significantly differs among ethnic groups.
## To know which ethnic groups differ, we are using Post-hoc Tukey HSD.

### Post-hoc Tukey HSD
tukeyAoV <- TukeyHSD(anova_result)
tukeyAoV

# Renaming the column names
tukey_table <- tidy(tukeyAoV) %>%
  rename(
    Comparison = contrast,
    Mean_Difference = estimate,
    Lower_95CI = conf.low,
    Upper_95CI = conf.high,
    Adjusted_p = adj.p.value
  ) %>%
  mutate(Significance = ifelse(Adjusted_p < 0.05, "Significant", "Not Significant"))

# Print clean formatted table
kable(
  tukey_table,
  caption = "Tukey HSD Post-hoc Test for Sodium Intake Across Ethnic Groups",
  digits = 3,
  align = "lccccc"
)

## So Asian Americans have higher sodium intake compared to multiple other groups. The differences between White, Black, and other Hispanic groups are smaller and mostly not significant.



