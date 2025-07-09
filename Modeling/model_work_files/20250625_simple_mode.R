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

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)

simplest_mnl <- gmnl(
  choice ~ 
  # brand_mcdonalds +
  # brand.recall_this + brand.recognition_this + past.use_this +
  # is_well_known +
  # is_bundle +
  # is_premium +
  brand_burger_king +
  brand_max_burger +
  brand_wendys +
  # type_burger_classic +
  type_burger_premium +
  type_bundle_classic +
  type_bundle_premium +
  # kcal + 
  # gram +
  price +
  no_choice |
            0 |
            0 |
            0 |
            0,
  data = mlogit_data,
  model="mnl",
)
summary_model(simplest_mnl)


simplest_lc <- gmnl(
  choice ~ 
        brand_mcdonalds +
        # brand_burger_king +
        brand_max_burger +
        brand_wendys +
        # + type_burger_classic
        type_burger_premium +
        type_bundle_classic +
        type_bundle_premium +
        price +
        no_choice |
        0 |
        0 |
        0 |
        1 +
        age +
        is_female +
        income_high + income_low +
        is_graduated +
        city_under_500k + rural +
        market_awareness +
        eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  Q = 2
)

summary_model(simplest_lc)
respondent_classes <- show_latent_class_statistics(simplest_lc)

# Coefficients:
#                                Estimate  Std. Error z-value  Pr(>|z|)
# class.1.brand_mcdonalds       -1.174289    0.407291 -2.8832 0.0039370 **
# class.1.brand_max_burger       0.691221    0.237950  2.9049 0.0036737 **
# class.1.brand_wendys           0.691913    0.267914  2.5826 0.0098062 ** 
# class.1.type_burger_premium    1.170276    0.321248  3.6429 0.0002696 ***
# class.1.type_bundle_classic    1.451155    0.402195  3.6081 0.0003085 ***
# class.1.type_bundle_premium    1.078583    0.394882  2.7314 0.0063064 **
# class.1.price                 -0.054349    0.016885 -3.2188 0.0012873 **
# class.1.no_choice             -0.878327    0.379755 -2.3129 0.0207295 *
# class.2.brand_mcdonalds       29.105508  719.442981  0.0405 0.9677299
# class.2.brand_max_burger     -35.859604  839.354843 -0.0427 0.9659225
# class.2.brand_wendys         -33.116116  839.349426 -0.0395 0.9685280
# class.2.type_burger_premium  -43.061465 1079.166006 -0.0399 0.9681708
# class.2.type_bundle_classic -146.497304 3597.220139 -0.0407 0.9675150
# class.2.type_bundle_premium -101.681644 2518.050977 -0.0404 0.9677893
# class.2.price                  4.746469  119.907324  0.0396 0.9684244
# class.2.no_choice            -17.490418  124.639388 -0.1403 0.8884007    
# (class)2                      -0.560927    1.368200 -0.4100 0.6818247
# age:class2                    -0.032194    0.047232 -0.6816 0.4954755
# class2:is_female              -0.631141    0.225014 -2.8049 0.0050332 **
# class2:income_high            -0.529598    0.386468 -1.3704 0.1705756
# class2:income_low             -0.469833    0.248185 -1.8931 0.0583481 .
# class2:is_graduated           -0.191501    0.227269 -0.8426 0.3994405
# class2:city_under_500k         0.389639    0.277117  1.4060 0.1597108
# class2:rural                  -0.461848    0.404031 -1.1431 0.2529970
# class2:market_awareness        0.123643    0.051625  2.3950 0.0166184 *
# class2:eats_fastfood_weekly   -0.141232    0.236695 -0.5967 0.5507200

mixl_preference_space <- gmnl(
  choice ~  
    # brand_mcdonalds +
    brand_burger_king +
    brand_max_burger +
    brand_wendys +
    # + type_burger_classic
    type_burger_premium +
    type_bundle_classic +
    type_bundle_premium +
    price +
    no_choice |
    0 |
    0 |
    0 |
    0,
  data = mlogit_data,
  model = "mixl",
  ranp = c(
    price = "n"
    # is_well_known = "n",
    # is_premium = "n",
    # is_bundle = "n",
    # brand.recall_this = "n",
    # brand.recognition_this = "n",
    # past.use_this = "n"
    ),
  R = 3000
)
summary_model(mixl_preference_space)

lc_preference_space <- gmnl(
  choice ~ 
    brand.recall_this + 
    brand.recognition_this + 
    past.use_this +
    is_well_known + 
    is_bundle + 
    is_premium +
    price +
    price:market_awareness_log +
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
  Q = 2
)
summary_model(lc_preference_space)
show_latent_class_statistics(lc_preference_space)
