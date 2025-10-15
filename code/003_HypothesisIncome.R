## This script does the Hypothesis testing: Do people 
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
library(knitr)
library(broom)


#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("..")

#### Reading the parquet file
food <- read_parquet("proc_data/demo_foods.parquet")

#As percentage
food$INDFMPIR <- food$INDFMPIR * 100


##### Family income as % of poverty level variable
hincpov<- ggplot(food, aes(INDFMPIR)) +
  geom_histogram( na.rm = TRUE, bins = 40,
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Family income % of poverty level",
    x = "Percentage",
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
ggsave("output/EDA_hist_incomepoverty.png", plot = hincpov, width = 8, height = 6, dpi = 300)

##### Family income as % of poverty level variable and Sugar Intake
hsug_income_scatter <- ggplot(food, aes(x = INDFMPIR, y = DR1ISUGR_sum)) +
  geom_point(
    color = "chocolate4",
    fill = "chocolate",
    alpha = 0.6,
    shape = 21,
    size = 3,
    na.rm = TRUE
  ) +
  labs(
    title = "Daily Sugar Intake vs. Family Income (% of Poverty)",
    x = "Family Income as % of Poverty Level",
    y = "Total Sugar Intake (grams)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),   # outside area
    panel.background = element_rect(fill = "antiquewhite", color = NA),  # plot area
    panel.grid.major = element_line(color = "white"),                    # light grid
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12)
  )
ggsave("output/EDA_scatter_sugar_income.png", plot = hsug_income_scatter, width = 8, height = 6, dpi = 300)

hsug_income_scatter
##### CREATE INCOME CATEGORICAL
food <- food %>%
  mutate(
    poverty_cat = if_else(
      INDFMPIR >= 300, 1L,0L),
    poverty_cat = factor(
      poverty_cat,
      levels = c(0, 1),
      labels = c("< 300% poverty", "≥ 300% poverty")
    )
  )

tbl <- food %>%
  select(poverty_cat) %>%
  tbl_summary(type = list(poverty_cat ~ "categorical"),
    missing = "ifany"   # <--- shows N (and %) of missing values
  ) %>%
  bold_labels()
tbl_df <- as_tibble(tbl, col_labels = TRUE)
write.xlsx(tbl_df, "output/EDA_povertycat.xlsx")

food_noNA <- food %>% filter(!is.na(poverty_cat))

## Histograms Sugar by Poverty Category
hsug_income <- ggplot(food_noNA, aes(x = DR1ISUGR_sum, fill = poverty_cat)) +
  geom_histogram(
    color = "black",
    alpha = 0.5,
    position = "identity"   # overlay instead of stacking
  ) +
  scale_fill_manual(
    name = "Family income as % of poverty level variable",
    values = c("< 300% poverty" = "chocolate4", "≥ 300% poverty" = "palegreen3")
  ) +
  labs(
    title = "Distribution of Daily Sugar Intake by Income Category",
    x = "Total Sugar (grams)",
    y = "Frequency"
  ) +
  theme(
    # Background colors
    plot.background  = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12),
    
    # Legend at bottom
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 12)
  )

# Save plot
ggsave("output/EDA_hist_sug_income.png", plot = hsug_income, width = 8, height = 6, dpi = 300)

##### Boxplot by Income
bsug_income <- ggplot(food_noNA, aes(x = poverty_cat, y = DR1ISUGR_sum, fill = poverty_cat)) +
  geom_boxplot(
    color = "black",
    alpha = 0.7,
    outlier.color = "brown3",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  scale_fill_manual(
    values = c("< 300% poverty" = "chocolate4", "≥ 300% poverty" = "palegreen3")
  ) +
  labs(
    title = "Boxplot of Daily Sugar Intake by Income Category",
    x = "Income Category",
    y = "Total Sugar (grams)",
    ) +
  theme(
    # Background colors
    plot.background  = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 12)
  )

# Save plot
ggsave("output/EDA_box_sug_income.png", plot = bsug_income, width = 8, height = 6, dpi = 300)

##### Ttest-sugar means
sugincmean95 <- t.test(food_noNA$DR1ISUGR_sum, conf.level = 0.95)
sugincmean95

foodpoor <- food_noNA[food_noNA$poverty_cat == "< 300% poverty", ]
foodrich <- food_noNA[food_noNA$poverty_cat == "≥ 300% poverty", ]

sugmean95_poor <- t.test(foodpoor$DR1ISUGR_sum, conf.level = 0.95)
sugmean95_poor

sugmean95_rich <- t.test(foodrich$DR1ISUGR_sum, conf.level = 0.95)
sugmean95_rich

prot_summary <- data.frame(
  Group = c("Total", "< 300% poverty", "≥ 300% poverty"),  
  Mean = c(
    sugincmean95$estimate,
    sugmean95_poor$estimate,
    sugmean95_rich$estimate
  ),
  Lower = c(
    sugincmean95$conf.int[1],
    sugmean95_poor$conf.int[1],
    sugmean95_rich$conf.int[1]
  ),
  Upper = c(
    sugincmean95$conf.int[2],
    sugmean95_poor$conf.int[2],
    sugmean95_rich$conf.int[2]
  )
)

group_colors <- c("Total" = "brown3", "< 300% poverty" = "chocolate4", "≥ 300% poverty" = "palegreen3")

sug_mean_ci <- ggplot(prot_summary, aes(x = Group, y = Mean, color = Group)) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 1, show.legend = FALSE) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Mean Daily Sugar Intake with 95% Confidence Intervals",
    x = "",
    y = "Sugar (grams)"
  ) +
  theme(
    plot.background  = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12),
    legend.position = "none"
  )

ggsave("output/EDA_meanCI_sug_inc.png", plot = sug_mean_ci, width = 8, height = 6, dpi = 300)

############T-TEST #################
tres <- t.test(DR1ISUGR_sum ~ poverty_cat,
               data = food_noNA,
               alternative = "greater")
tres
tres_tidy <- tidy(tres) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  rename(
    "Mean (≥300% poverty)" = estimate1,
    "Mean (<300% poverty)" = estimate2,
    "Mean difference (high - low)" = estimate,
    "t statistic" = statistic,
    "p value (one-sided)" = p.value,
    "CI lower" = conf.low,
    "CI upper" = conf.high
  ) %>%
  select(`Mean (≥300% poverty)`, `Mean (<300% poverty)`,
         `Mean difference (high - low)`, `CI lower`, `CI upper`,
         `t statistic`, `p value (one-sided)`)

write.xlsx(tres_tidy, "output/t_test_sugar_income.xlsx", rowNames = FALSE)

############ ANOVA TEST #################
anovares = aov( DR1ISUGR_sum ~ poverty_cat, data=food)
xkabledply(anovares, title = "ANOVA result summary Sugar Intake by Income")
