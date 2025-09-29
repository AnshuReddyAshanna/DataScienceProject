# install if not already
install.packages("haven")
install.packages("tidyverse")
library(dplyr)

library(haven)
library(tidyverse)
library(dplyr)
# path to .xpt file
demographics = read_xpt("../raw_data/DEMO_L.xpt")
day1_foods = read_xpt("../raw_data/DR1IFF_L.xpt")
day1_nutri = read_xpt("../raw_data/DR1TOT_L.xpt")

## Create one merged df
# merge demo + foods (many rows per person, each food reported indv.)
demo_foods <- demographics %>%
  inner_join(day1_foods, by = "SEQN")
#add nutrient totals to demo_foods
demo_foods_full <- demo_foods %>%
  inner_join(day1_nutri, by = "SEQN")

# quick checks of new dfs
dim(demographics)      # participants
dim(day1_foods)        # food records
dim(day1_nutri)        # nutrient totals
dim(demo_foods)        # demo + foods
dim(demo_foods_full)   # demo + foods + nutrient totals
