setwd("MXL_model")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)
library(stargazer)

data <- read.csv("data/filtered_data.csv")
dce_design <- read.csv("data/DCE_design.csv")
data <- data %>%
  left_join(
    dce_design %>% select(respondent_id, question_id, price, type_burger_classic,type_burger_premium,type_bundle_classic,type_bundle_premium,brand_mcdonalds,brand_burger_king,brand_max_burger,brand_wendys, kcal, gram),
    by = c("respondent_id", "question_id", "price","type_burger_classic","type_burger_premium","type_bundle_classic","type_bundle_premium","brand_mcdonalds","brand_burger_king","brand_max_burger","brand_wendys")
  )

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

    kcal = ifelse(is.na(kcal), 0, kcal),
    gram = ifelse(is.na(gram), 0, gram),

    market_awareness_log = log(market_awareness + 0.001),
    market_awareness_low = ifelse(market_awareness < avg_market_awareness, 1, 0),
    age_log = log(age + 0.001),
    price_log = log(price + 0.001),
    market_awareness_exp = I(market_awareness^2),
    age_exp = exp(age),
    market_awareness_sqrt = sqrt(market_awareness),
    age_sqrt = sqrt(age),
    total_recalled_log = log(total_recalled + 0.001),
    total_recognized_log = log(total_recognized + 0.001),
    total_price_guess_diff_log = log(total_price_guess_diff + 0.001),

    is_bundle = ifelse(type_bundle_classic == 1 | type_bundle_premium == 1, 1, 0),
    is_premium = ifelse(type_burger_premium == 1 | type_bundle_premium == 1, 1, 0)
  ) %>%
  select(
    -location, -location_grouped,
    -gender,
    -education,
    -income,
    -fast.food.frequency
  )


################################################################################
clustering_data <- data %>%
  distinct(respondent_id, .keep_all = TRUE) %>%
  select(respondent_id, 
            eats_fastfood_rarely, eats_fastfood_weekly, 
            income_high, income_mid, income_low,
            is_graduated, 
            city_over_500k, city_under_500k, rural, age
            )
clustering_matrix <- as.matrix(clustering_data %>% select(-respondent_id))
sil_widths <- vector()
for (k in 2:10) {
  km <- kmeans(clustering_matrix, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(clustering_matrix))
  sil_widths[k] <- mean(ss[, 3])
}

plot(2:10, sil_widths[2:10], type = "b", pch = 19,
     xlab = "Number of clusters", ylab = "Average silhouette width",
     main = "Silhouette Method")

set.seed(123)
k <- 3
kmeans_result <- kmeans(clustering_matrix, centers = k, nstart = 25)
clustering_data$cluster <- factor(kmeans_result$cluster)

cluster_summary <- clustering_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    age_mean = mean(age, na.rm = TRUE),
    across(
      c(eats_fastfood_rarely, eats_fastfood_weekly, 
        income_high, income_mid, income_low,
        is_graduated, city_over_500k, city_under_500k, rural),
      list(count = ~sum(.)),
      .names = "{.col}_{.fn}"
    )
  )

print(cluster_summary, n = Inf, width = Inf)
# cluster 1: educated older urbans
# cluster 2: young rare fast food eaters
# cluster 3: urban frequent eaters

data <- data %>%
  left_join(clustering_data %>% select(respondent_id, cluster), by = "respondent_id") %>%
  mutate(
    educated_older_urbans = factor(cluster),
    young_rare_fast_food_eaters = ifelse(cluster == 2, 1, 0),
    urban_frequent_eaters = ifelse(cluster == 3, 1, 0)
  )

write.csv(data, "data/final_data.csv", row.names = FALSE)

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)

##########################################################################
## CLUSTERS INSTEAD OF CONSUMER CHARACTERISTICS WTP SPACE
experiment_model_cluster_wtp <- gmnl(
  choice ~  
            brand.recall_this + 
            brand.recognition_this + 
            past.use_this +
            is_well_known + 
            is_bundle + 
            is_premium +
            # gram + 
            # kcal +
            # price +
          #  is_female:is_well_known +
          #  age:past.use_this +
          #  market_awareness + is_female +
          #  income_high + income_low +
          #  is_graduated + city_under_500k + rural +
          #  eats_fastfood_weekly +
          #  young_rare_fast_food_eaters + urban_frequent_eaters +
           no_choice |
           0 |
           0 |
           0 |
           0,
  data = mlogit_data,
  model = "mixl",
  modelType = "wtp",
  base = "price",
  ranp = c(
    is_well_known = "n",
    is_premium = "n",
    is_bundle = "n",
    brand.recall_this = "n",
    brand.recognition_this = "n",
    past.use_this = "n"
    # gram = "n",
    # kcal = "n"
    ),
  R = 2000
)
summary(experiment_model_cluster_wtp)
