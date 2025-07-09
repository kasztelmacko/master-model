setwd("Modeling")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)

data <- read.csv("data/final_data.csv")

show_latent_class_statistics <- function(model) {
  posterior_probs_resp <- model$Wnq[seq(1, nrow(model$Wnq), by = 6), ]
  class_assignments <- apply(posterior_probs_resp, 1, which.max)

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

  respondent_classes <- respondent_data %>%
    select(respondent_id, class)

  return(respondent_classes)
  }

summary_model <- function(model) {
  cat("Model Summary:\n")
  print(summary(model))
  cat("\nAIC:", AIC(model), "\n")
  cat("BIC:", BIC(model), "\n")
}

# data <- data %>% filter(income_high == 0, age < 28)
data <- data %>%
  filter(respondent_id %in% (respondent_classes %>% filter(class != 2) %>% pull(respondent_id)))

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)

##########################################################################
lc_preference_space <- gmnl(
  choice ~ brand.recall_this + brand.recognition_this + past.use_this +
        is_well_known + 
        is_bundle + 
        is_premium +

        brand.recall_this:city_over_500k +
        brand.recall_this:rural +
        # brand.recall_this:age_log +
        # brand.recall_this:past.use_this +
        # brand.recall_this:eats_fastfood_weekly +
        # brand.recall_this:market_awareness_log +
        # brand.recall_this:eats_fastfood_weekly +
        # brand.recall_this:is_female +

        past.use_this:market_awareness_log +
        past.use_this:city_over_500k +
        past.use_this:age_log +
        # past.use_this:rural +

        is_well_known:income_high +
        is_well_known:income_low +
        # is_well_known:city_over_500k +
        # is_well_known:age_log +
        # is_well_known:eats_fastfood_weekly +
        # is_well_known:market_awareness_low +

        is_premium:income_high +
        # is_bundle:income_high +
        # no_choice:income_high +
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

summary_model(lc_preference_space)
respondent_classes <- show_latent_class_statistics(lc_preference_space)

lc_model <- gmnl(
  choice ~ brand.recall_this + brand.recognition_this + past.use_this +
        is_well_known + 
        is_bundle + 
        is_premium +
        price +
        no_choice |
        0 |
        0 |
        0 |
        1 +
        age_log +
        is_female +
        income_high + income_low +
        is_graduated +
        city_under_500k + rural +
        market_awareness_low +
        eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  modelType = "wtp",
  base = "price",
  R = 2000,
  Q = 2
)
summary_model(lc_model)
respondent_classes <- show_latent_class_statistics(lc_model)



simplest_model <- gmnl(
  choice ~ 
  # brand_mcdonalds
  # brand.recall_this + brand.recognition_this + past.use_this +
  # is_well_known +
  # is_bundle +
  # is_premium +
  brand_burger_king
  + brand_max_burger
  + brand_wendys
  # + type_burger_classic
  + type_burger_premium
  + type_bundle_classic
  + type_bundle_premium
  # + kcal
  # + gram
  + price
  + no_choice |
            0 |
            0 |
            0 |
            0,
  data = mlogit_data,
  model="mnl",
  # R = 2000,
  # ranp = c(
  #   brand_burger_king = "n",
  #   brand_max_burger = "n",
  #   brand_wendys = "n",
  #   type_burger_premium = "n",
  #   type_bundle_classic = "n",
  #   type_bundle_premium = "n",
  #   price = "n"
  # )
)

summary_model(simplest_model)

lc_model <- gmnl(
  choice ~ 
          brand_burger_king
        + brand_max_burger
        + brand_wendys
        # + type_burger_classic
        + type_burger_premium
        + type_bundle_classic
        + type_bundle_premium
        + price
        + no_choice |
        0 |
        0 |
        0 |
        1 +
        age_log +
        is_female +
        income_high + income_low +
        is_graduated +
        city_under_500k + rural +
        market_awareness_low +
        eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  # modelType = "wtp",
  # base = "price",
  R = 2000,
  Q = 2
)
summary_model(lc_model)
show_latent_class_statistics(lc_model)
