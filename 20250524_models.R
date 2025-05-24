setwd("MXL_model")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)

## 1. DATA PREPARATION --------------------------------------------------------
data <- read.csv("data/filtered_data.csv")

data <- data %>%
  group_by(respondent_id, question_id) %>%
  mutate(
    alternative_id = row_number(),
    respondent_question_id = paste(respondent_id, question_id, sep = "_")
  ) %>%
  ungroup()

data <- data %>%
  mutate(
    known_factor = case_when(
      brand_mcdonalds == 1 ~ "well_known",
      brand_burger_king == 1 ~ "well_known",
      brand_max_burger == 1 ~ "somewhat_known",
      brand_wendys == 1 ~ "rarely_known",
      no_choice == 1 ~ "no_choice", 
      TRUE ~ NA_character_
    ),
    brand.well_known = ifelse(known_factor == "well_known", 1, 0),
    brand.somewhat_known = ifelse(known_factor == "somewhat_known", 1, 0),
    brand.rarely_known = ifelse(known_factor == "rarely_known", 1, 0)
  )

# bin and dummy
data <- data %>%
  mutate(
    # Group location
    location_grouped = case_when(
      location %in% c("miast-50", "miasto-50-150", "miasto-150-500") ~ "city_under_500k",
      location == "miasto-500" ~ "city_over_500k",
      location == "wies" ~ "rural"
    ),
    city_under_500k = ifelse(location_grouped == "city_under_500k", 1, 0),
    city_over_500k = ifelse(location_grouped == "city_over_500k", 1, 0),
    # rural = ifelse(location_grouped == "rural", 1, 0),

    # Gender dummies
    is_female = ifelse(gender == "kobieta", 1, 0),
    # is_male = ifelse(gender == "mezczyzna", 1, 0),

    # Education dummies
    # is_higher_edu = ifelse(education == "wyzsze", 1, 0),
    # is_studying = ifelse(education == "w-trakcie-studiow", 1, 0),

    # Income dummies
    income_low = ifelse(income %in% c("ponizej-1000", "1000-2500"), 1, 0),
    # income_mid = ifelse(income %in% c("2500-5000", "5000-7500"), 1, 0),
    income_high = ifelse(income %in% c("7500-10000", "powyzej-10000"), 1, 0),

    # Fast food frequency dummies
    # eats_fastfood_weekly = ifelse(fast.food.frequency %in% c("raz-w-tygodniu", "czesciej-niz-raz-w-tygodniu"), 1, 0),
    eats_fastfood_rarely = ifelse(fast.food.frequency %in% c("rzadziej-niz-raz-w-miesiacu", "raz-w-miesiacu"), 1, 0)
  ) %>%
  select(
    -location, -location_grouped,
    -gender,
    -education,
    -income,
    -fast.food.frequency
  )

data <- data %>%
  mutate(
    log_price = log(price + 0.001),
    exp_price = exp(price),
    norm_price = (price - mean(price, na.rm = TRUE)) / sd(price, na.rm = TRUE),

    log_market_awareness = log(market_awareness + 0.001),
    norm_market_awareness = (market_awareness - mean(market_awareness, na.rm = TRUE)) / sd(market_awareness, na.rm = TRUE),

    log_age = log(age + 0.001),
    norm_age = (age - mean(age, na.rm = TRUE)) / sd(age, na.rm = TRUE),

    log_total_price_guess_diff = log(total_price_guess_diff + 0.001),
    norm_total_price_guess_diff = (total_price_guess_diff - mean(total_price_guess_diff, na.rm = TRUE)) / sd(total_price_guess_diff, na.rm = TRUE),

    price_income_high = price * income_high,
    price_income_low = price * income_low,
    price_is_female = price * is_female,
    price_market_awareness = price * market_awareness,
    price_city_under_500k = price * city_under_500k,
    price_city_over_500k = price * city_over_500k,
    price_eats_fastfood_rarely = price * eats_fastfood_rarely,
    price_is_well_known = price * is_well_known,
    price_age = price * age,
    price_total_price_guess_diff_log = price * log_total_price_guess_diff,
    price_brand_well_known = price * brand.well_known,
    price_brand_somewhat_known = price * brand.somewhat_known,
    price_brand_rarely_known = price * brand.rarely_known,

    price_log_income_high = log_price * income_high,
    price_log_income_low = log_price * income_low,
    price_log_is_female = log_price * is_female,
    price_log_market_awareness = log_price * market_awareness,
    price_log_market_awareness_log = log_price * log_market_awareness,
    price_log_city_under_500k = log_price * city_under_500k,
    price_log_city_over_500k = log_price * city_over_500k,
    price_log_eats_fastfood_rarely = log_price * eats_fastfood_rarely,
    price_log_is_well_known = log_price * is_well_known,
    price_log_age = log_price * age,
    price_log_age_log = log_price * log_age,
    price_log_total_price_guess_diff_log = log_price * log_total_price_guess_diff,

    log_brand.recall_score = log(brand.recall_score + 0.001),
    exp_brand.recall_score = exp(brand.recall_score),
    norm_brand.recall_score = (brand.recall_score - mean(brand.recall_score, na.rm = TRUE)) / sd(brand.recall_score, na.rm = TRUE),

    price_log_total_price_guess_diff_norm = log_price * norm_total_price_guess_diff,

    price_norm_income_high = norm_price * income_high,
    price_norm_income_low = norm_price * income_low,
    price_norm_is_female = norm_price * is_female,
    price_norm_market_awareness = norm_price * market_awareness,
    price_norm_market_awareness_log = norm_price * log_market_awareness,
    price_norm_city_under_500k = norm_price * city_under_500k,
    price_norm_city_over_500k = norm_price * city_over_500k,
    price_norm_eats_fastfood_rarely = norm_price * eats_fastfood_rarely,
    price_norm_is_well_known = norm_price * is_well_known,
    price_norm_age = norm_price * age,
    price_norm_age_log = norm_price * log_age,
    price_norm_total_price_guess_diff_log = norm_price * log_total_price_guess_diff

  )

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)

## Primitive model with fixed effects only in WTP space
wtp_model <- gmnl(
  formula = choice ~
    log_brand.recall_score +
    past.use_this + is_well_known + no_choice +
    price_log_income_high + price_log_income_low + price_is_female +
    price_market_awareness + price_log_eats_fastfood_rarely +
    price_log_city_over_500k + price_log_city_under_500k +
    price_log_is_well_known |
    0,
  data = mlogit_data,
  model = "mnl",
  modelType = "wtp",
  base = "price"
)

summary(wtp_model)

