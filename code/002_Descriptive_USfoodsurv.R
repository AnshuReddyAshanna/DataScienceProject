## This script does the Exploratory Data Analysis of the Food Dataset
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
str(food)

#### Describe Demographics
## Categorical Variables
tbl <- food %>%
  select(RIAGENDR, RIDRETH3, DMDEDUC2, DMDMARTZ) %>%
  tbl_summary(
    type = list(
      RIAGENDR ~ "categorical",
      RIDRETH3 ~ "categorical",
      DMDEDUC2 ~ "categorical",
      DMDMARTZ ~ "categorical"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "ifany"   # <--- shows N (and %) of missing values
  ) %>%
  bold_labels()

tbl_df <- as_tibble(tbl, col_labels = TRUE)
write.xlsx(tbl_df, "output/EDA_categorical_demo.xlsx")

## Continuous Variables
tbl2 <- food %>%
  select(RIDAGEYR, DMDHHSIZ, INDFMPIR) %>%
  summarise(across(
    everything(),
    list(
      mean  = ~ mean(.x, na.rm = TRUE),
      sd    = ~ sd(.x, na.rm = TRUE),
      min   = ~ min(.x, na.rm = TRUE),
      max   = ~ max(.x, na.rm = TRUE),
      n_na  = ~ sum(is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_sep = "__",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(label = sapply(variable, function(v) var_label(food[[v]]) %||% NA_character_)) %>%
  relocate(variable, label) %>%
  mutate(across(c(mean, sd, min, max), ~ round(.x, 2)))
write.xlsx(tbl2, "output/EDA_demogcontinuous_summary.xlsx")

#### Describe Food Nutrients
# Quick look
food_labeled <- food %>%
  rename(
    "Calories (kcal)"      = DR1IKCAL_sum,
    "Protein (g)"          = DR1IPROT_sum,
    "Carbohydrates (g)"    = DR1ICARB_sum,
    "Sugars (g)"           = DR1ISUGR_sum,
    "Fiber (g)"            = DR1IFIBE_sum,
    "Total Fat (g)"        = DR1ITFAT_sum,
    "Sodium (mg)"          = DR1ISODI_sum
  )

foodsum<-xkablesummary(food_labeled[c(
  "Calories (kcal)",
  "Protein (g)",
  "Carbohydrates (g)",
  "Sugars (g)",
  "Fiber (g)",
  "Total Fat (g)",
  "Sodium (mg)"
)])

##### PROTEIN
#Histogram for the distribution of  Protein Intake
hprotg <- ggplot(food, aes(DR1IPROT_sum)) +
  geom_histogram(
    breaks=seq(0, 500, by = 10),
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Daily Protein Intake",
    x = "Total Protein (grams)",
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
ggsave("output/EDA_hist_prot.png", plot = hkcal, width = 8, height = 6, dpi = 300)

# Boxplot for the distribution of Protein Intake
bprotg <- ggplot(food, aes(y = DR1IPROT_sum)) +
  geom_boxplot(
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9,
    outlier.color = "brown3",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  labs(
    title = "Boxplot of Daily Protein Intake",
    x = "",
    y = "Total Protein (grams)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

# Save the boxplot
ggsave("output/EDA_box_prot.png", plot = bprotg, width = 8, height = 6, dpi = 300)

## QQplot
# Set up beige background and font style
par(
  bg = "antiquewhite",   # background color
  col.axis = "black",    # axis color
  col.lab = "black",     # label color
  col.main = "black",    # title color
  font.main = 2,         # bold title
  cex.main = 1.5,        # title size
  cex.lab = 1.3,         # axis title size
  cex.axis = 1.1         # axis label size
)

# Q-Q plot for Protein Intake
qqnorm(
  food$DR1IPROT_sum,
  main = "Q-Q Plot of Daily Protein Intake",
  pch = 19,               # filled points
  col = "chocolate4",     # point color
  cex = 0.8               # point size
)
qqline(
  food$DR1IPROT_sum,
  col = "brown3",         # line color
  lwd = 2                 # line thickness
)
dev.copy(png, "output/EDA_qq_prot_base.png", width = 8, height = 6, units = "in", res = 300)
dev.off()

##### CALORIES
#Histogram for the distribution of  Daily Calorie Intake
hkcal <- ggplot(food, aes(DR1IKCAL_sum)) +
  geom_histogram(
    breaks = seq(0, 10000, by = 500),
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Daily Calorie Intake",
    x = "Total Calories (kcal)",
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
ggsave("output/EDA_hist_kcal.png", plot = hkcal, width = 8, height = 6, dpi = 300)

##### SODIUM
#Histogram for the distribution of Sodium Intake
hsodium <- ggplot(food, aes(DR1ISODI_sum)) +
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

#Display the plot
hsodium

ggsave("output/EDA_hist_sodium.png", plot = hsodium, width = 8, height = 6, dpi = 300)

# Boxplot for the distribution of Sodium Intake
bsodium <- ggplot(food, aes(y = DR1ISODI_sum)) +
  geom_boxplot(
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9,
    outlier.color = "brown3",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  labs(
    title = "Boxplot of Daily Sodium Intake",
    x = "",
    y = "Total Sodium (mg)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

#Display the plot
bsodium

# Save the boxplot
ggsave("output/EDA_box_sodium.png", plot = bsodium, width = 8, height = 6, dpi = 300)

## QQplot
# Set up beige background and font style
par(
  bg = "antiquewhite",   # background color
  col.axis = "black",    # axis color
  col.lab = "black",     # label color
  col.main = "black",    # title color
  font.main = 2,         # bold title
  cex.main = 1.5,        # title size
  cex.lab = 1.3,         # axis title size
  cex.axis = 1.1         # axis label size
)

# Q-Q plot for Sodium Intake
qqnorm(
  food$DR1ISODI_sum,
  main = "Q-Q Plot of Daily Sodium Intake",
  pch = 19,               # filled points
  col = "chocolate4",     # point color
  cex = 0.8               # point size
)
qqline(
  food$DR1ISODI_sum,
  col = "brown3",         # line color
  lwd = 2                 # line thickness
)
dev.copy(png, "output/EDA_qq_sodium_base.png", width = 8, height = 6, units = "in", res = 300)
dev.off()

##### FAT
#Histogram for the distribution of  Daily FAT Intake
hfat <- ggplot(food, aes(DR1ITFAT_sum)) +
  geom_histogram(
    breaks = seq(0, 550, by = 10),
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Daily Fat Intake",
    x = "Total Fat (grams)",
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
ggsave("output/EDA_hist_fat.png", plot = hfat, width = 8, height = 6, dpi = 300)

# Boxplot for the distribution of Fat Intake
bfat <- ggplot(food, aes(y = DR1ITFAT_sum)) +
  geom_boxplot(
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9,
    outlier.color = "brown3",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  labs(
    title = "Boxplot of Daily Fat Intake",
    x = "",
    y = "Total Fat (grams)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

# Save the boxplot
ggsave("output/EDA_box_fat.png", plot = bfat, width = 8, height = 6, dpi = 300)

## QQplot
# Set up beige background and font style
par(
  bg = "antiquewhite",   # background color
  col.axis = "black",    # axis color
  col.lab = "black",     # label color
  col.main = "black",    # title color
  font.main = 2,         # bold title
  cex.main = 1.5,        # title size
  cex.lab = 1.3,         # axis title size
  cex.axis = 1.1         # axis label size
)

# Q-Q plot for Fat Intake
qqnorm(
  food$DR1ITFAT_sum,
  main = "Q-Q Plot of Daily Fat Intake",
  pch = 19,               # filled points
  col = "chocolate4",     # point color
  cex = 0.8               # point size
)
qqline(
  food$DR1ITFAT_sum,
  col = "brown3",         # line color
  lwd = 2                 # line thickness
)
dev.copy(png, "output/EDA_qq_fat_base.png", width = 8, height = 6, units = "in", res = 300)
dev.off()

##### SUGAR
#Histogram for the distribution of Sugar Intake
hsugar <- ggplot(food, aes(DR1ISUGR_sum)) +
  geom_histogram(
    breaks=seq(0, 1000, by = 50),
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9
  ) +
  labs(
    title = "Distribution of Daily Sugar Intake",
    x = "Total Sugar (grams)",
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
ggsave("output/EDA_hist_sugar.png", plot = hsugar, width = 8, height = 6, dpi = 300)

# Boxplot for the distribution of Sugar Intake
bsugar <- ggplot(food, aes(y = DR1ISUGR_sum)) +
  geom_boxplot(
    fill = "chocolate",
    color = "chocolate4",
    alpha = 0.9,
    outlier.color = "brown3",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  labs(
    title = "Boxplot of Daily Sugar Intake",
    x = "",
    y = "Total Sugar (grams)"
  ) +
  theme(
    # Background colors
    plot.background = element_rect(fill = "antiquewhite", color = NA),
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    # Font sizes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

# Save the boxplot
ggsave("output/EDA_box_sugar.png", plot = bsugar, width = 8, height = 6, dpi = 300)

## QQplot
# Set up beige background and font style
par(
  bg = "antiquewhite",   # background color
  col.axis = "black",    # axis color
  col.lab = "black",     # label color
  col.main = "black",    # title color
  font.main = 2,         # bold title
  cex.main = 1.5,        # title size
  cex.lab = 1.3,         # axis title size
  cex.axis = 1.1         # axis label size
)

# Q-Q plot for Sugar Intake
qqnorm(
  food$DR1ISUGR_sum,
  main = "Q-Q Plot of Daily Sugar Intake",
  pch = 19,               # filled points
  col = "chocolate4",     # point color
  cex = 0.8               # point size
)
qqline(
  food$DR1ISUGR_sum,
  col = "brown3",         # line color
  lwd = 2                 # line thickness
)
dev.copy(png, "output/EDA_qq_sugar_base.png", width = 8, height = 6, units = "in", res = 300)
dev.off()

