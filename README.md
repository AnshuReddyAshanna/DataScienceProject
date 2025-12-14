# Introduction to Data Science Project (6101-12 – Fall 2025)

## Team Members

Ingri Katherine Quevedo

Anshu Reddy Ashanna

Marlon Demandt

Presentation Slides Project 1: https://www.canva.com/design/DAG0xxTfUy4/opLRMk8t8BcRPFtrzT6HyQ/edit?utm_content=DAG0xxTfUy4&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton
Presentation Slides Project 2: https://www.canva.com/design/DAG6xXGWQ6A/Gq4MxHcmjgg9h7XlNAFyKw/edit
## Project Overview

This project investigates how lifestyle changes brought about by the COVID-19 pandemic may have influenced Americans’ eating habits and whether long-standing nutritional beliefs still hold true. Previous research suggested that nutrient intake in the U.S. varies by gender, ethnicity, and income level. However, given the widespread shifts in behavior and lifestyle during and after the pandemic, these relationships may have changed. Using the most recent national data on dietary intake (“What Do Americans Eat?”, 2021–2023), this study tests whether pre-pandemic findings remain valid or if the pandemic reshaped patterns of nutrition across different demographic groups.

# Project 1
SMART QUESTIONS:
- Do Women on Average, Eat Less Protein Than Men?
- Is Sodium Intake Different Among Ethnic Group?
- Are there differences in Sugar Intake by Income?

Our analysis of a nationally representative sample of U.S. adults aged 20 and older (2021–2023) reveals that several pre-pandemic nutritional patterns continue to hold. On average, men consume more protein than women, consistent with previous findings. However, notable differences across ethnic and income groups also persist. Asian Americans exhibit higher sodium intake compared to several other ethnic groups, while individuals with lower incomes (below 300% of the federal poverty line) tend to consume more sugar than higher-income individuals. These results suggest that although the pandemic may have influenced overall dietary behaviors, some long-standing nutritional disparities across demographic groups remain evident in the post-pandemic period.

# Project 2
Project 2 is structured around three regression-based models, each addressing a specific SMART research question. Together, these models aim to (1) describe patterns of nutrient intake and (2) evaluate whether demographic variables can meaningfully predict healthier versus less healthy diets in the U.S. adult population.
SMART QUESTIONS:
- Do women, on average, consume less protein than men?
- Is sodium intake different across ethnic groups?
- Are there differences in sugar intake by income level?
- Can demographic variables predict whether an individual meets recommended daily nutrition benchmarks?

The pandemic does not appear to have fundamentally altered long-standing demographic differences in diet. At the same time, the results highlight that most Americans do not meet recommended nutritional guidelines and that dietary behavior is influenced by factors beyond basic demographics, such as preferences, culture, and lifestyle.

## Repository Structure
<pre>
├── WriteUp/        # Final project write-up (R Markdown and knitted HTML/PDF)
├── code/           # R scripts for data cleaning, EDA, and statistical modeling
├── docs/           # Project proposal, drafts, and supporting documentation
├── output/         # Generated figures, tables, and model outputs
├── proc_data/      # Cleaned and processed datasets used for analysis
├── raw_data/       # Raw NHANES / WWEIA data files
├── .gitignore      # Git ignore file
├── DataProject12.Rproj  # RStudio project file
└── README.md       # Project overview, data sources, and repository guide
</pre>

---

## Data Source
National Center for Health Statistics (NCHS)
- NHANES (National Health and Nutrition Examination Survey) 2021-2023
- WWEIA (What We Eat in America)
- Collected as part of NHANES to monitor the food and nutrient intakes of the U.S. population.
- National Institute of Health Nutrient Recommendations and Databases

Files 
- Files include DEMO (demographics), DR1IFF (Day 1 foods), DR1TOT (Day 1 totals), and many others.
Link to data: https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/wweia-documentation-and-data-sets/
## Tools & Technologies
- R: Data cleaning, analysis, and visualization
- FAO FAOSTAT Data: Primary source of nutrition data

---
