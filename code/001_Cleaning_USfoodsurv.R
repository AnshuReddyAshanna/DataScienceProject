## This script combines the food variables with the demographic variables
#  And creates the dataset we will use for the analysis
rm(list = ls())
#### Preliminary
# Libraries
library(dplyr)
#Libary to open xpt files
library(haven)
library(tidyverse)
library(ezids)
# To export final dataset as parquet for efficient output
library(arrow)

#Set Working Directory as the DataScienceProject folder
getwd()  
setwd("..")

#### Open Demographics Data
demographics = read_xpt("raw_data/DEMO_L.xpt")
str(demographics)
# Keep only the variables we need
demographics_f=demographics[c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "RIDRETH3", "DMDEDUC2", "DMDMARTZ", "RIDEXPRG", "DMDHHSIZ", "WTINT2YR", "INDFMPIR")]
#Exclude pregnant women and keep only people 20 or older
demographics_f <- demographics_f %>%
  filter(RIDAGEYR >= 20, RIDEXPRG != 1 | is.na(RIDEXPRG)) %>%
  select(-RIDEXPRG)

#Drop large Demographics to save memory
rm(demographics)

#### Open Food Dataset
day1_foods = read_xpt("raw_data/DR1IFF_L.xpt")
str(day1_foods)
# Keep only the variables we need
day1_foods_f=day1_foods[c("SEQN", "WTDRD1", "DR1DAY", "DR1CCMNM", "DR1CCMTX", "DR1_020", "DR1_030Z", "DR1FS", "DR1IFDCD", "DR1IGRMS", "DR1IKCAL", "DR1IPROT", "DR1ICARB", "DR1ISUGR", "DR1IFIBE", "DR1ITFAT", "DR1ISODI")]

#Review food descriptions
food_desc = read_xpt("raw_data/drxfcd_L.xpt")
# We will not use this for now
rm(food_desc)

#Group the food nutritional file from individual-food level to individual level
str(day1_foods_f)

day1_foods_f_g <- day1_foods_f %>%
  group_by(SEQN) %>%
  summarise(
    #Weight
    WTDRD1 = mean(WTDRD1, na.rm = TRUE),
    #Food vars
    across(c("DR1IGRMS", "DR1IKCAL", "DR1IPROT", "DR1ICARB", "DR1ISUGR", "DR1IFIBE", "DR1ITFAT", "DR1ISODI"), 
           list(
             sum  = \(x) sum(x,  na.rm = TRUE),
             mean = \(x) mean(x, na.rm = TRUE),
             sd   = \(x) sd(x,   na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}"
    ),
    # Count of observations
    n_obs = n()
  )

#Free-up space
rm(day1_foods)
rm(day1_foods_f)

## Nutritional Dataset
day1_nutri = read_xpt("raw_data/DR1TOT_L.xpt")
# It does not really give us new information from what we already have
rm(day1_nutri)

## Create one merged df that combines demographics and food information
# merge demo + foods (1-1 individual level)
# Using an Inner Join as We want to apply to the Food Data the filters 
# used in the demographics dataset, and we are not interested in any possible
# observation that could appear in the food dataset but that does not have demographics
demo_foods <- demographics_f %>%
  inner_join(day1_foods_f_g, by = "SEQN") # IN NHANES SEQN is a unique ID for each participant 

#Free-up space
rm(demographics_f)
rm(day1_foods_f_g)

###### Export final datafile
write_parquet(demo_foods, "proc_data/demo_foods.parquet")

#Clean-up
rm(list = ls())
