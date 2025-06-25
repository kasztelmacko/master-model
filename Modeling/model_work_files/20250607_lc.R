setwd("MXL_model")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)
library(gtools)

data <- read.csv("data/filtered_data.csv")

data <- data %>%
  group_by(respondent_id, question_id) %>%
  mutate(
    alternative_id = row_number(),
    respondent_question_id = paste(respondent_id, question_id, sep = "_")
  ) %>%
  ungroup()

avg_market_awareness <- mean(data$market_awareness, na.rm = TRUE)

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
    rural = ifelse(location_grouped == "rural", 1, 0),

    # Gender dummies
    is_female = ifelse(gender == "kobieta", 1, 0),
    # is_male = ifelse(gender == "mezczyzna", 1, 0),

    # Education dummies
    is_graduated = ifelse(education == "wyzsze", 1, 0),

    # Income dummies
    income_low = ifelse(income %in% c("ponizej-1000", "1000-2500"), 1, 0),
    income_mid = ifelse(income %in% c("2500-5000", "5000-7500"), 1, 0),
    income_high = ifelse(income %in% c("7500-10000", "powyzej-10000"), 1, 0),

    # Fast food frequency dummies
    eats_fastfood_weekly = ifelse(fast.food.frequency %in% c("raz-w-tygodniu", "czesciej-niz-raz-w-tygodniu"), 1, 0),
    eats_fastfood_rarely = ifelse(fast.food.frequency %in% c("rzadziej-niz-raz-w-miesiacu", "raz-w-miesiacu"), 1, 0),

    # Market awareness dummies
    market_awareness_low = ifelse(market_awareness < avg_market_awareness, 1, 0),
    market_awareness_high = ifelse(market_awareness > avg_market_awareness, 1, 0),

    # type dummies
    is_bundle = ifelse(type_bundle_classic == 1 | type_bundle_premium == 1, 1, 0),
    is_premium = ifelse(type_burger_premium == 1 | type_bundle_premium == 1, 1, 0),

    age_under_23 = ifelse(age < 23, 1, 0),
    age_above_23 = ifelse(age >= 23, 1, 0),
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
    market_awareness_log = log(market_awareness + 0.001),
    age_log = log(age + 0.001),
    price_log = log(price + 0.001),
    market_awareness_exp = I(market_awareness^2),
    age_exp = exp(age),
    market_awareness_sqrt = sqrt(market_awareness),
    age_sqrt = sqrt(age),
    total_recalled_log = log(total_recalled + 0.001),
    total_recognized_log = log(total_recognized + 0.001),
    total_price_guess_diff_log = log(total_price_guess_diff + 0.001)
  )


mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)

##########################################################################
##############################################################
# LCM experiment
experiment_model_LCM_wtp <- gmnl(
  choice ~ brand.recall_this + brand.recognition_this + past.use_this +
      is_well_known + is_bundle + is_premium +
      brand.recall_this:market_awareness_log +
      is_premium:income_high +
      is_well_known:income_high +
      no_choice |
      0 |
      0 |
      0 |
      1 +
      age_log +
      is_female +
      income_high +
      income_low +
      is_graduated +
      city_under_500k + rural +
      market_awareness_low +
      total_price_guess_diff_log +
      # eats_fastfood_weekly:market_awareness_high +
      eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  modelType = "wtp",
  base = "price",
  R = 2000,
  Q = 2
)
summary(experiment_model_LCM_wtp)

# undestand classes
posterior_probs_resp <- experiment_model_LCM_wtp$Wnq[seq(1, nrow(experiment_model_LCM_wtp$Wnq), by = 6), ]
class_assignments <- apply(posterior_probs_resp, 1, which.max)
table(class_assignments)

respondent_data <- mlogit_data %>%
  group_by(respondent_id) %>%
  slice(1) %>%
  ungroup()
respondent_data$class <- class_assignments

class_summary <- respondent_data %>%
  group_by(class) %>%
  summarise(
    n = n(),
    age_mean = mean(age, na.rm = TRUE),
    market_awareness_mean = mean(market_awareness, na.rm = TRUE),
    across(
      c(is_female, income_high, income_low, is_graduated, city_over_500k, rural, eats_fastfood_weekly),
      list(count = ~sum(.)),
      .names = "{.col}_{.fn}"
    )
  )

print(class_summary, n = Inf, width = Inf)

experiment_model_LCM_wtp <- gmnl(
  choice ~ brand.recall_this + brand.recognition_this + past.use_this +
      is_well_known + is_bundle + is_premium +
      no_choice |
      0 |
      0 |
      0 |
      1 +
      age_log +
      is_female +
      income_high +
      income_low +
      is_graduated +
      city_under_500k + rural +
      market_awareness_low +
      total_price_guess_diff_log +
      eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  modelType = "wtp",
  base = "price",
  R = 2000,
  Q = 2
)
summary(experiment_model_LCM_wtp)
