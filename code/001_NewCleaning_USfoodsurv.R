## This script combines the food variables with the demographic variables
#  And creates the dataset we will use for the analysis
rm(list = ls())
#### Preliminary
# Libraries
library(dplyr)
library(stringr)
library(tm)
library(wordcloud)
#Libary to open xpt files
library(haven)
library(tidyverse)
library(ezids)
# To export final dataset as parquet for efficient output
library(arrow)
library(labelled)
#Set Working Directory as the DataScienceProject folder
getwd()
setwd("..")

#### Open Demographics Data
demographics = read_xpt("raw_data/DEMO_L.xpt")
str(demographics)

## Exclude pregnant women and keep only people 20 or older
# First check their importance
demographics_summary <- demographics %>% 
  mutate(
    under20  = RIDAGEYR < 20,
    pregnant = RIDEXPRG == 1
  ) %>%
  summarise(
    n_total = n(),
    # counts
    n_under20  = sum(under20,  na.rm = TRUE),
    n_pregnant = sum(pregnant, na.rm = TRUE),
    # unweighted shares
    share_under20_unw  = n_under20  / n_total,
    share_pregnant_unw = n_pregnant / n_total,
    # weighted shares
    share_under20_wt =
      sum(WTINT2YR[under20],  na.rm = TRUE) / sum(WTINT2YR, na.rm = TRUE),
    share_pregnant_wt =
      sum(WTINT2YR[pregnant], na.rm = TRUE) / sum(WTINT2YR, na.rm = TRUE)
  )

demographics_summary
rm(demographics_summary)

# Drop
demographics <- demographics %>%
  filter(RIDAGEYR >= 20 | RIDEXPRG != 1) %>%
  select(-RIDEXPRG)

# Demographics ID to keep
demographics_id=demographics$SEQN

#### Open Food Dataset
day1_foods = read_xpt("raw_data/DR1IFF_L.xpt")
str(day1_foods)
# Keep only the variables we need
day1_foods=day1_foods[c("SEQN", "WTDRD1", "DR1DAY", "DR1DBIH", 
                        "DR1ILINE","DR1DRSTZ","DR1LANG",
                        "DR1CCMNM", "DR1CCMTX", "DR1_020", "DR1_030Z",
                        "DR1_040Z","DR1FS", "DR1IFDCD", "DR1IGRMS", 
                        "DR1IKCAL", "DR1IPROT", "DR1ICARB", "DR1ISUGR", 
                        "DR1IFIBE", "DR1ITFAT", "DR1ISODI")]

#### INNER JOIN WITH DEMOGRAPHICS, THIS IS A MANY TO ONE JOIN
demo_foods <- demographics %>% 
  inner_join(day1_foods, by = "SEQN")

#### Add food descriptions
food_desc = read_xpt("raw_data/drxfcd_L.xpt")

#Left join to bring descriptions
food <- demo_foods %>%
  left_join(
    food_desc,
    by = c("DR1IFDCD" = "DRXFDCD")
  )

#Perfect match
rm(day1_foods, food_desc, demographics, demo_foods)


## Filter out some variables I do not think we need
vars_to_drop <- c("SDDSRVYR", #"Data release cycle"
                  "RIDSTATR", #Interview/Examination status"
                  "RIDAGEMN", #"Age in months at screening - 0 to 24 mos"
                  "RIDRETH1", #"Race/Hispanic origin"
                  "RIDEXMON", #"Six-month time period"
                  "RIDEXAGM", #"Age in months at exam - 0 to 19 years"
                  "DMDHRGND", #"HH ref person\x92s gender"
                  "DMDHRAGZ", #"HH ref person\x92s age in years"
                  "DMDHREDZ", #"HH ref person\x92s education level"
                  "DMDHRMAZ", #"HH ref person\x92s marital status"
                  "DMDHSEDZ", #"HH ref person\x92s spouse\x92s education level"
                  "WTMEC2YR", #"Full sample 2-year MEC exam weight"
                  "SDMVSTRA", #"Masked variance pseudo-stratum"
                  "SDMVPSU",  #"Masked variance pseudo-PSU"
                  "WTDRD1", #"Dietary day one sample weight"
                  "DR1ILINE", #"Food/Individual component number"
                  "DR1DRSTZ", #"Dietary recall status"
                  "DR1CCMNM", #"Combination food number"
                  "DR1_020" #"secs"
                  )
food = food %>%
  select(-all_of(vars_to_drop))

# Helper: mode function (returns single most frequent non-NA value)
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

food_individual <- food %>%
  group_by(SEQN) %>%
  summarise(
    # -------------------------------
    # Variables already at individual level
    # (take the first since they should be identical within SEQN)
    # -------------------------------
    RIAGENDR = first(RIAGENDR),
    RIDAGEYR = first(RIDAGEYR),
    RIDRETH3 = first(RIDRETH3),
    DMQMILIZ = first(DMQMILIZ),
    DMDBORN4 = first(DMDBORN4),
    DMDYRUSR = first(DMDYRUSR),
    DMDEDUC2 = first(DMDEDUC2),
    DMDMARTZ = first(DMDMARTZ),
    DMDHHSIZ = first(DMDHHSIZ),
    WTINT2YR = first(WTINT2YR),
    INDFMPIR = first(INDFMPIR),
    
    # -------------------------------
    # Variables where you want the MODE
    # -------------------------------
    DR1DAY  = get_mode(DR1DAY),
    DR1DBIH = get_mode(DR1DBIH),
    
    # -------------------------------
    # Counts with conditions
    # -------------------------------
    # Count DR1_030Z if in the specified set
    DR1_030Z_count = sum(
      DR1_030Z %in% c(6, 16, 17, 18, 9, 13, 15),
      na.rm = TRUE
    ),
    
    # Count DR1_040Z if equal to 1
    DR1_040Z_count = sum(
      DR1_040Z == 1,
      na.rm = TRUE
    ),
    
    # -------------------------------
    # Sum
    # -------------------------------
    DR1ISUGR_sum = sum(DR1ISUGR, na.rm = TRUE),
    DR1IGRMS_sum = sum(DR1IGRMS, na.rm = TRUE),
    DR1IKCAL_sum = sum(DR1IKCAL, na.rm = TRUE),
    DR1IPROT_sum = sum(DR1IPROT, na.rm = TRUE),
    DR1ICARB_sum = sum(DR1ICARB, na.rm = TRUE),
    DR1IFIBE_sum = sum(DR1IFIBE, na.rm = TRUE),
    DR1ITFAT_sum = sum(DR1ITFAT, na.rm = TRUE),
    DR1ISODI_sum = sum(DR1ISODI, na.rm = TRUE),
    
    .groups = "drop"
  )

food_final <- food_individual %>%
  # -----------------------------
# 1. Rename variables
# -----------------------------
rename(
  id            = SEQN,
  gender        = RIAGENDR,
  age           = RIDAGEYR,
  race          = RIDRETH3,
  militar       = DMQMILIZ,
  usborn        = DMDBORN4,
  timeus        = DMDYRUSR,
  education     = DMDEDUC2,
  maritalstatus = DMDMARTZ,
  hhsize        = DMDHHSIZ,
  weight        = WTINT2YR,
  incomepovline = INDFMPIR,
  dayweek       = DR1DAY,
  dayssvy       = DR1DBIH,
  numsnacks     = DR1_030Z_count,
  numhomemeals  = DR1_040Z_count,
  sugar         = DR1ISUGR_sum,
  foodwgtgrams  = DR1IGRMS_sum,
  calories      = DR1IKCAL_sum,
  protein       = DR1IPROT_sum,
  carbs         = DR1ICARB_sum,
  fiber         = DR1IFIBE_sum,
  fats          = DR1ITFAT_sum,
  sodium        = DR1ISODI_sum
) %>%
  # -----------------------------
# 2. Recode to factors
# -----------------------------
mutate(
  # Gender: 1 Male, 2 Female
  gender = factor(
    gender,
    levels = c(1, 2),
    labels = c("Male", "Female")
  ),
  
  # Race: combine 1+2 as Hispanic
  # RIDRETH3 codes (simplified per your instructions)
  # 1 Mexican American, 2 Other Hispanic -> Hispanic
  # 3 Non-Hispanic White
  # 4 Non-Hispanic Black
  # 6 Non-Hispanic Asian
  # all others -> Other Race - Including Multi-Racial
  race = case_when(
    race %in% c(1, 2) ~ 1L,  # Hispanic
    race == 3 ~ 2L,          # NH White
    race == 4 ~ 3L,          # NH Black
    race == 6 ~ 4L,          # NH Asian
    TRUE ~ 5L                # Other / Multi-racial
  ),
  race = factor(
    race,
    levels = 1:5,
    labels = c(
      "Hispanic",
      "Non-Hispanic White",
      "Non-Hispanic Black",
      "Non-Hispanic Asian",
      "Other Race - Including Multi-Racial"
    )
  ),
  
  # Served active duty in US Armed Forces
  # 1 yes; 2,7,9,NA -> no
  militar = case_when(
    militar == 1 ~ 1L,
    militar %in% c(2, 7, 9) | is.na(militar) ~ 2L
  ),
  militar = factor(
    militar,
    levels = c(1, 2),
    labels = c("Yes", "No")
  ),
  
  # Country of birth
  # 1 US, 2/77/99/NA -> Other
  usborn = case_when(
    usborn == 1 ~ 1L,
    usborn %in% c(2, 77, 99) | is.na(usborn) ~ 2L
  ),
  usborn = factor(
    usborn,
    levels = c(1, 2),
    labels = c("US", "Other")
  ),
  
  # Length of time in US
  # 1–2: <4 years; 3–4: 5–15 years; 5–6,77,99,NA: 20+ years
  timeus = case_when(
    timeus %in% c(1, 2) ~ "Less than 4 years",
    timeus %in% c(3, 4) ~ "5 to 15 years",
    timeus %in% c(5, 6, 77, 99) | is.na(timeus) ~ "20 years or more"
  ),
  timeus = factor(
    timeus,
    levels = c("Less than 4 years", "5 to 15 years", "20 years or more")
  ),
  
  # Highest level of education
  # 1–5 -> labeled; 7 & 9 -> missing (NA)
  education = case_when(
    education %in% 1:3 ~ "HS/GED or less",
    education == 4     ~ "Some College / AA",
    education == 5     ~ "College+",
    education %in% c(7, 9) ~ NA_character_
  ),
  education = factor(
    education,
    levels = c("HS/GED or less", "Some College / AA", "College+")
  ),
  
  # Marital status
  # 1 Married/Living with partner
  # 2 Widowed/Divorced/Separated
  # 3,77,99,NA -> Single
  maritalstatus = case_when(
    maritalstatus == 1 ~ "Married/Living with partner",
    maritalstatus == 2 ~ "Widowed/Divorced/Separated",
    maritalstatus %in% c(3, 77, 99) | is.na(maritalstatus) ~ "Single"
  ),
  maritalstatus = factor(
    maritalstatus,
    levels = c(
      "Married/Living with partner",
      "Widowed/Divorced/Separated",
      "Single"
    )
  ),
  
  # Intake day of the week
  dayweek = factor(
    dayweek,
    levels = 1:7,
    labels = c(
      "Sunday", "Monday", "Tuesday", "Wednesday",
      "Thursday", "Friday", "Saturday"
    )
  )
  # dayssvy, numsnacks, numhomemeals, sugar remain numeric
) %>%
  # -----------------------------
# 3. Add variable labels
# -----------------------------
set_variable_labels(
  id            = "Respondent sequence number",
  gender        = "Gender",
  age           = "Age in years",
  race          = "Race/Hispanic origin w/ NH Asian",
  militar       = "Served active duty in US Armed Forces",
  usborn        = "Country of birth",
  timeus        = "Length of time in US",
  education     = "Highest level of education",
  maritalstatus = "Marital status",
  hhsize        = "Total number of people in the Household",
  weight        = "Full sample 2-year interview weight",
  incomepovline = "Ratio of family income to poverty",
  dayweek       = "Intake day of the week",
  dayssvy       = "Number of days between intake day and the day of family questionnaire administered in the household.",
  numsnacks     = "Number of snacks",
  numhomemeals  = "Number of meals ate at home",
  foodwgtgrams  = "Total food weight (gm)",
  sugar         = "Total sugars (gm)",
  calories      = "Total energy (kcal)",
  protein       = "Total protein (gm)",
  carbs         = "Total Carbohydrate  (gm)",
  fiber         = "Total Dietary fiber (gm)",
  fats          = "Total fat (gm)",
  sodium        = "Total Sodium  (mg)"
)

food_final$income_pov_cat <- cut(
  food_final$incomepovline,
  breaks = c(-Inf, 1, 2, 3, 4, 5, Inf),
  labels = c(
    "<100",
    "100-200",
    "200-300",
    "300-400",
    "400-500",
    "500+"
  ),
  right = FALSE
)

food_final$log_sugar = log(food_final$sugar)
###### Export final datafile
write_parquet(food_final, "proc_data/food_final_morevars.parquet")

#Clean-up
rm(list = ls())
