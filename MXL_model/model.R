choice ~ # Fixed Effects Part
         # --- Individual Characteristics ---
         dummy_location_1 + ... + dummy_location_5 + # Dummies for location (1 ref)
         age + gender + education + income + fast.food.frequency + market_awareness +

         # --- Alternative Specific Constants (ASCs) & Attributes ---
         # (Representing baseline preference relative to a base alternative)
         type_burger_premium + type_bundle_classic + type_bundle_premium + # ASCs for product types (if relevant)
         is_well_known + # Fixed effect for well-known vs less-known brands
         price +
         brand.recall_this +
         brand.recognition_this +
         past.use_this

         # Random Effects Part (| respondent_id)
         (1 + # Random intercept (overall scale/preference heterogeneity)
          is_well_known + # Heterogeneity in preference for well-known brands
          brand.recall_this + # Heterogeneity in sensitivity to recall
          brand.recognition_this + # Heterogeneity in sensitivity to recognition
          past.use_this | respondent_id) # Heterogeneity in sensitivity to past use

setwd("MXL_model")
library(dplyr)
library(gmnl)
library(mlogit)

data <- read.csv("data/filtered_data.csv")

# bin and dummy
data <- data %>%
  mutate(
    # Group location
    location_grouped = case_when(
      location %in% c("miast-50", "miasto-50-150", "miasto-150-500") ~ "city_under_500k",
      location == "miasto-500" ~ "city_over_500k",
      location == "wies" ~ "rural"
    ),
    is_city_under_500k = ifelse(location_grouped == "city_under_500k", 1, 0),
    c = ifelse(location_grouped == "city_over_500k", 1, 0),

    # Gender dummies
    is_female = ifelse(gender == "kobieta", 1, 0),

    # Education dummies
    is_higher_edu = ifelse(education == "wyzsze", 1, 0),
    is_studying = ifelse(education == "w-trakcie-studiow", 1, 0),

    # Income dummies
    income_low = ifelse(income %in% c("ponizej-1000", "1000-2500"), 1, 0),
    income_mid = ifelse(income %in% c("2500-5000", "5000-7500"), 1, 0),
    income_high = ifelse(income %in% c("7500-10000", "powyzej-10000"), 1, 0),

    # Fast food frequency dummies
    eats_fastfood_weekly = ifelse(fast.food.frequency %in% c("raz-w-tygodniu", "czesciej-niz-raz-w-tygodniu"), 1, 0),
    eats_fastfood_rarely = ifelse(fast.food.frequency %in% c("rzadziej-niz-raz-w-miesiacu", "raz-w-miesiacu"), 1, 0)
  ) %>%
  # Drop original columns
  select(
    -location, -location_grouped,
    -gender,
    -education,
    -income,
    -fast.food.frequency
  )



