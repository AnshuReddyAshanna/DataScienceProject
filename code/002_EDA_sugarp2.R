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
survey_design <- svydesign(ids = ~1,weights = ~weight, data = food)

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

##################### OUTLIERS
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


#######################################
##### Scatterplot of Sugar and Number of Snacks
ct <- cor.test(df$numsnacks, df$log_sugar, use = "complete.obs")
r_val <- unname(ct$estimate)   # Pearson r
p_val <- ct$p.value            # p-value
lab_text <- paste0("r = ", round(r_val, 3), "\n p = ", signif(p_val, 3))

g5=ggplot(df, aes(x = numsnacks, y = log_sugar)) +
  geom_point(color = "#4DBBD5", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = "#E64B35") +
  annotate(
    "text", x = Inf, y = 0, hjust  = 1.1, vjust  = 1.5, label  = lab_text, size   = 5
  ) +
  labs(
    title = "Sugar Intake vs. Number of snacks",
    x = "Number of Snacks",
    y = "Log Sugar Intake (gm)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text( hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )
g5


#######################################
##### Scatterplot of Sugar and Number of Meals at Home
ct <- cor.test(df$numhomemeals, df$log_sugar, use = "complete.obs")
r_val <- unname(ct$estimate)   # Pearson r
p_val <- ct$p.value            # p-value
lab_text <- paste0("r = ", round(r_val, 3), "\n p = ", signif(p_val, 3))

g6 <- ggplot(df, aes(x = numhomemeals, y = log_sugar)) +
  geom_point(color = "#4DBBD5", alpha = 0.5, size  = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = "#E64B35") +
  annotate(
    "text", x = Inf, y = 0, hjust  = 1.1, vjust  = 1.5, label  = lab_text, size   = 5
  ) +
  labs(
    title = "Sugar Intake vs. Number of Meals at Home",
    x     = "Number of of Meals at Home", 
    y     = "Log Sugar Intake (gm)") +
  theme_minimal(base_size = 18) +
  theme(
    plot.title   = element_text(hjust = 0.5,face  = "bold",size  = 14),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )
g6
#######################################
##### Scatterplot of Sugar and Age
ct <- cor.test(df$age, df$log_sugar, use = "complete.obs")
r_val <- unname(ct$estimate)   # Pearson r
p_val <- ct$p.value            # p-value
lab_text <- paste0("r = ", round(r_val, 3), "\n p = ", signif(p_val, 3))

g7=ggplot(df, aes(x = age, y = log_sugar)) +
  geom_point(color = "#4DBBD5", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = "#E64B35") +
  annotate(
    "text", x = Inf, y = 0, hjust  = 1.1, vjust  = 1.5, label  = lab_text, size   = 5
  ) +  
  labs(
    title = "Sugar Intake vs. Age",
    x = "Age",
    y = "Log Sugar Intake (gm)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )
g7

#################################################################
############################################
my_colors <- c("<100"    = "#7AEBD1",
               "100-200"      = "#27B291",
               "200-300"      = "#12705A",
               "300-400"      = "#124E70",
               "400-500"      = "#277EB0",
               "500+" = "#6FC2F2",
               "NA" = "gray")


# ---- Prepare data: weighted sugar + within-variable shares ----
df_plot <- df %>%
  group_by(income_pov_cat) %>%
  summarise(
    weighted_n     = sum(weight, na.rm = TRUE),
    weighted_sugar = weighted.mean(log_sugar, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop  = weighted_n / sum(weighted_n),      # share within each variable
    share_label = percent(prop, accuracy = 0.1)
  ) %>%
  ungroup()

# ---- Rescale share so dots can be plotted on sugar scale ----
scale_factor <- max(df_plot$weighted_sugar, na.rm = TRUE) / 
  max(df_plot$prop,           na.rm = TRUE)

df_plot <- df_plot %>%
  mutate(prop_scaled = prop * scale_factor)

# ---- Plot: VERTICAL bars, NOT sorted ----
ginc = ggplot(df_plot, aes(x = income_pov_cat)) +
  geom_col(aes(y = weighted_sugar, fill = income_pov_cat, alpha = 0.4)) +
  geom_point(aes(y = prop_scaled), size = 3, color = "black") +
  geom_text(aes(y = prop_scaled, label = share_label),
            vjust = -0.7, size = 3.5) +
  scale_fill_manual(values = my_colors) +
  
  scale_y_continuous(
    name = "Average sugar consumption (grams)",
    breaks = seq(0, max(df_plot$weighted_sugar) * 1.1, by = 10),
    
    expand = expansion(mult = c(0, 0.1)),
    
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Percentage of people in each category",
      breaks = seq(0, 1, by = 0.05),   # every 5%
      labels = percent_format()
    )
  ) +
  
  labs(
    title = "Average Sugar Consumption and Sample Shares by Income",
    caption = "Source:Author's calculations using WWEIA 2021-2023. \nNote: Calculations using survey weights.",
    x = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 10))
  )
ginc

############################################
my_colors <- c("gender"    = "#4DBBD5",
               "race"      = "#E64B35",
               "education" = "#00A087")


# ---- Prepare data: weighted sugar + within-variable shares ----
df_plot <- food %>%
  pivot_longer(
    cols = c(gender, race, education),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category) %>%
  summarise(
    weighted_n     = sum(weight, na.rm = TRUE),
    weighted_sugar = weighted.mean(sugar, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(variable) %>%
  mutate(
    prop  = weighted_n / sum(weighted_n),      # share within each variable
    share_label = percent(prop, accuracy = 0.1)
  ) %>%
  ungroup() %>%
  mutate(full_cat = paste(variable, category, sep = ": "))

# ---- Rescale share so dots can be plotted on sugar scale ----
scale_factor <- max(df_plot$weighted_sugar, na.rm = TRUE) / 
  max(df_plot$prop,           na.rm = TRUE)

df_plot <- df_plot %>%
  mutate(prop_scaled = prop * scale_factor)

# ---- Plot: VERTICAL bars, NOT sorted ----
g55=ggplot(df_plot, aes(x = category)) +
  geom_col(aes(y = weighted_sugar, fill = variable, alpha = 0.4)) +
  geom_point(aes(y = prop_scaled), size = 3, color = "black") +
  geom_text(aes(y = prop_scaled, label = share_label),
            vjust = -0.7, size = 3.5) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(
    name = "Average sugar consumption",
    expand = expansion(mult = c(0, 0.1)),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name   = "Share of people within each category",
      labels = percent_format()
    )
  ) +
  labs(
    title = "Average Sugar Consumption and Category Shares (Weighted)",
    x = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
g55

############# GRAPH ADD
############################################
my_colors <- c("timeus"    = "#4DBBD5",
               "militar"      = "#E64B35",
               "maritalstatus" = "#00A087")


# ---- Prepare data: weighted sugar + within-variable shares ----
df_plot <- food %>%
  pivot_longer(
    cols = c(timeus, militar, maritalstatus),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category) %>%
  summarise(
    weighted_n     = sum(weight, na.rm = TRUE),
    weighted_sugar = weighted.mean(sugar, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(variable) %>%
  mutate(
    prop  = weighted_n / sum(weighted_n),      # share within each variable
    share_label = percent(prop, accuracy = 0.1)
  ) %>%
  ungroup() %>%
  mutate(full_cat = paste(variable, category, sep = ": "))

# ---- Rescale share so dots can be plotted on sugar scale ----
scale_factor <- max(df_plot$weighted_sugar, na.rm = TRUE) / 
  max(df_plot$prop,           na.rm = TRUE)

df_plot <- df_plot %>%
  mutate(prop_scaled = prop * scale_factor)

# ---- Plot: VERTICAL bars, NOT sorted ----
g56=ggplot(df_plot, aes(x = category)) +
  geom_col(aes(y = weighted_sugar, fill = variable, alpha = 0.4)) +
  geom_point(aes(y = prop_scaled), size = 3, color = "black") +
  geom_text(aes(y = prop_scaled, label = share_label),
            vjust = -0.7, size = 3.5) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(
    name = "Average sugar consumption",
    expand = expansion(mult = c(0, 0.1)),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name   = "Share of people within each category",
      labels = percent_format()
    )
  ) +
  labs(
    title = "Average Sugar Consumption and Category Shares (Weighted)",
    x = "",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
g56

summary(aov(log_sugar ~ incomepovline, data=df))
summary(aov(log_sugar ~ gender, data=df))
summary(aov(log_sugar ~ race, data=df))
summary(aov(log_sugar ~ education, data=df))
summary(aov(log_sugar ~ timeus, data=df))
summary(aov(log_sugar ~ militar, data=df))
summary(aov(log_sugar ~ maritalstatus, data=df))
