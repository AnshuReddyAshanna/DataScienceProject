## This script does the Hypothesis testing: Do women on average, eat less protein than men?
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



## Histograms by Gender
# Make sure gender is a factor with readable labels
food$RIAGENDR <- factor(food$RIAGENDR,
                        levels = c(1, 2),
                        labels = c("Male", "Female"))
# Compute group means for labels/points
group_means <- food %>%
  group_by(RIAGENDR) %>%
  summarize(mean_prot = mean(DR1IPROT_sum, na.rm = TRUE), .groups = "drop")

# Optional overall mean (uncomment if you want a horizontal reference line)
overall_mean <- mean(food$DR1IPROT_sum, na.rm = TRUE)

bprot_gender_mean <- ggplot(food, aes(x = RIAGENDR, y = DR1IPROT_sum, fill = RIAGENDR)) +
  geom_boxplot(
    color = "black",
    alpha = 0.7,
    outlier.color = "brown3",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  # Mean per gender as larger points
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 3.5,
    stroke = 1.2,
    fill = "white",
    color = "chocolate4",
    position = position_dodge(width = 0.75)
  ) +
  # Optional: label the mean value above the point
  geom_text(
    data = group_means,
    aes(x = RIAGENDR, y = mean_prot, label = round(mean_prot, 1)),
    vjust = -1.0,
    size = 4,
    color = "chocolate4",
    fontface = "bold"
  ) +
  # Optional: overall mean line across both groups
  geom_hline(yintercept = overall_mean, linetype = "dashed", linewidth = 1, color = "brown3") +
  annotate("text", x = 1.5, y = overall_mean, label = paste0("Overall mean = ", round(overall_mean, 1)),
           vjust = -0.8, color = "brown3", fontface = "bold", size = 4) +
  scale_fill_manual(
    name = "Gender",
    values = c("Male" = "chocolate4", "Female" = "darkorange3")
  ) +
  labs(
    title = "Boxplot of Daily Protein Intake by Gender (with Means)",
    x = "Gender",
    y = "Total Protein (grams)"
  ) +
  theme(
    plot.background  = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 12)
  )

ggsave("output/EDA_box_prot_gender_means.png", plot = bprot_gender_mean, width = 8, height = 6, dpi = 300)

##### Ttest-protein means
protmean95 <- t.test(food$DR1IPROT_sum, conf.level = 0.95)
protmean95

foodmale <- food[food$RIAGENDR == 1, ]
foodfemale <- food[food$RIAGENDR == 2, ]

protmean95_male <- t.test(foodmale$DR1IPROT_sum, conf.level = 0.95)
protmean95_male

protmean95_female <- t.test(foodfemale$DR1IPROT_sum, conf.level = 0.95)
protmean95_female

prot_summary <- data.frame(
  Group = c("Total", "Male", "Female"),
  Mean = c(
    protmean95$estimate,
    protmean95_male$estimate,
    protmean95_female$estimate
  ),
  Lower = c(
    protmean95$conf.int[1],
    protmean95_male$conf.int[1],
    protmean95_female$conf.int[1]
  ),
  Upper = c(
    protmean95$conf.int[2],
    protmean95_male$conf.int[2],
    protmean95_female$conf.int[2]
  )
)

group_colors <- c("Total" = "chocolate3", "Male" = "chocolate4", "Female" = "darkorange3")

prot_mean_ci <- ggplot(prot_summary, aes(x = Group, y = Mean, color = Group)) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 1, show.legend = FALSE) +
  scale_color_manual(values = group_colors) +
  # 🔹 Custom Y-axis breaks and limits
  scale_y_continuous(breaks = seq(60, 90, by = 5), limits = c(60, 90)) +
  labs(
    title = "Mean Daily Protein Intake with 95% Confidence Intervals",
    x = "",
    y = "Protein Intake (grams)"
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

ggsave("output/EDA_meanCI_prot.png", plot = prot_mean_ci, width = 8, height = 6, dpi = 300)

# Boxplot grouped by Gender
bprot_gender <- ggplot(food, aes(x = RIAGENDR, y = DR1IPROT_sum, fill = RIAGENDR)) +
  geom_boxplot(
    color = "black",
    alpha = 0.7,
    outlier.color = "brown3",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  scale_fill_manual(
    name = "Gender",
    values = c("Male" = "chocolate4", "Female" = "darkorange3")
  ) +
  labs(
    title = "Boxplot of Daily Protein Intake by Gender",
    x = "Gender",
    y = "Total Protein (grams)"
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
ggsave("output/EDA_box_prot_gender.png", plot = bprot_gender, width = 8, height = 6, dpi = 300)


############ ANOVA TEST #################
anovares = aov( DR1IPROT_sum ~ RIAGENDR, data=food)
xkabledply(anovares, title = "ANOVA result summary Protein Intake by Gender")


