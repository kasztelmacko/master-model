setwd("MXL_model")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)

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
    is_bundle = ifelse(type_bundle_classic == 1 | type_bundle_premium == 1, 1, 0),
    is_premium = ifelse(type_burger_premium == 1 | type_bundle_premium == 1, 1, 0)
  )


################################################################################
clustering_data <- data %>%
  distinct(respondent_id, .keep_all = TRUE) %>%
  select(respondent_id, 
            eats_fastfood_rarely, eats_fastfood_weekly, 
            income_high, income_mid, income_low,
            is_graduated, 
            city_over_500k, city_under_500k, rural,
            age
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
    cluster = factor(cluster),
    cluster_2 = ifelse(cluster == 2, 1, 0),
    cluster_3 = ifelse(cluster == 3, 1, 0)
  )

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)

## FIXED EFFECTS ONLY UTILITY SPACE
experiment_model_fixed_util <- gmnl(
    choice ~ brand.recall_this + brand.recognition_this + past.use_this +
             is_well_known +
             price +
             age:price + is_female:price + eats_fastfood_rarely:price +
             income_high:price + income_low:price + is_graduated:price +
             market_awareness:price + city_over_500k:price + city_under_500k:price +
             no_choice |
             0 |
             0 |
             0,
    data = mlogit_data,
    model = "mnl",
    R = 2000
)
summary(experiment_model_fixed_util)
summary(wtp.gmnl(experiment_model_fixed_util, wrt = "price"))


## FIXED EFFECTS ONLY WTP SPACE
experiment_model_fixed_wtp <- gmnl(
    choice ~ brand.recall_this + brand.recognition_this + past.use_this +
             is_well_known +
             age:price + is_female:price + eats_fastfood_rarely:price +
             income_high:price + income_low:price + is_graduated:price +
             market_awareness:price + city_over_500k:price + city_under_500k:price +
             no_choice |
             0 |
             0 |
             0,
    data = mlogit_data,
    model = "mnl",
    modelType = "wtp",
    base = "price",
    R = 2000
)
summary(experiment_model_fixed_wtp)

## FIXED EFFECTS ONLY CLUSTERS UTIL SPACE SPACE
experiment_model_fixed_clusters_util <- gmnl(
    choice ~ brand.recall_this + brand.recognition_this + past.use_this +
            is_well_known +
            price +
            market_awareness:price + is_female:price +
            cluster_2:price + cluster_3:price +
            no_choice |
            0 |
            0 |
            0,
    data = mlogit_data,
    model = "mnl",
    R = 2000
)
summary(experiment_model_fixed_clusters_util)
summary(wtp.gmnl(experiment_model_fixed_clusters_util, wrt = "price"))

## FIXED EFFECTS ONLY CLUSTERS UTIL WTP SPACE
experiment_model_fixed_clusters_wtp <- gmnl(
    choice ~ brand.recall_score + brand.recognition_this + past.use_this +
            is_well_known +
            market_awareness:price + is_female:price +
            cluster_2:price + cluster_3:price +
            no_choice |
            0 |
            0 |
            0,
    data = mlogit_data,
    model = "mnl",
    modelType = "wtp",
    base = "price",
    R = 2000
)
summary(experiment_model_fixed_clusters_wtp)

experiment_model_fixed_clusters_wtp_is_bundle <- gmnl(
    choice ~ brand.recall_score + brand.recognition_this + past.use_this +
            is_well_known + is_bundle + is_premium +
            market_awareness:price + is_female:price +
            cluster_2:price + cluster_3:price +
            no_choice |
            0 |
            0 |
            0,
    data = mlogit_data,
    model = "mnl",
    modelType = "wtp",
    base = "price",
    R = 2000
)
summary(experiment_model_fixed_clusters_wtp_is_bundle)

## FIXED EFFECTS IN WTP SPACE, WELL-KNOWN PRICE INTERACTION
experiment_model_fixed_clusters_well_known_interact_wtp <- gmnl(
    choice ~ brand.recall_this + brand.recognition_this + past.use_this +
            is_well_known:price +
            market_awareness:price + is_female:price +
            cluster_2:price + cluster_3:price +
            no_choice |
            0 |
            0 |
            0,
    data = mlogit_data,
    model = "mnl",
    modelType = "wtp",
    base = "price",
    R = 2000
)
summary(experiment_model_fixed_clusters_well_known_interact_wtp)

## WEL-KNOWN AS RANDOM UTILITY SPACE
experiment_model_well_known_util <- gmnl(
    choice ~ brand.recall_this + brand.recognition_this + past.use_this +
             is_well_known +
             price +
             age:price + is_female:price + eats_fastfood_rarely:price +
             income_high:price + income_low:price + is_graduated:price +
             market_awareness:price + city_over_500k:price + city_under_500k:price +
             no_choice |
             0 |
             0 |
             0,
    data = mlogit_data,
    model = "mixl",
    ranp = c(price = "n",
             is_well_known = "n"),
    R = 2000
)
summary(experiment_model_well_known_util)
summary(wtp.gmnl(experiment_model_well_known_util, wrt = "price"))

## WEL-KNOWN AS RANDOM WTP SPACE
experiment_model_well_known_wtp <- gmnl(
    choice ~ brand.recall_this + brand.recognition_this + past.use_this +
             is_well_known +
             age:price + is_female:price + eats_fastfood_rarely:price +
             income_high:price + income_low:price + is_graduated:price +
             market_awareness:price + city_over_500k:price + city_under_500k:price +
             no_choice |
             0 |
             0 |
             0,
    data = mlogit_data,
    model = "mixl",
    modelType = "wtp",
    base = "price",
    ranp = c(is_well_known = "n"),
    R = 2000
)
summary(experiment_model_well_known_wtp)


## CLUSTERS INSTEAD OF CONSUMER CHARACTERISTICS UTILITY SPACE
experiment_model_cluster_util <- gmnl(
  choice ~ brand.recall_this + brand.recognition_this + past.use_this +
           is_well_known +
           price +
           market_awareness:price + is_female:price +
           cluster_2:price + cluster_3:price +
           no_choice |
           0 |
           0 |
           0,
  data = mlogit_data,
  model = "mixl",
  ranp = c(price = "n", is_well_known = "n"),
  R = 2000
)
summary(experiment_model_cluster_util)
summary(wtp.gmnl(experiment_model_cluster_util, wrt = "price"))

## CLUSTERS INSTEAD OF CONSUMER CHARACTERISTICS WTP SPACE
experiment_model_cluster_wtp <- gmnl(
  choice ~ brand.recall_this + brand.recognition_this + past.use_this +
           is_well_known + is_bundle +
           market_awareness:price + is_female:price +
           cluster_2:price + cluster_3:price +
           no_choice |
           0 |
           0 |
           0,
  data = mlogit_data,
  model = "mixl",
  modelType = "wtp",
  base = "price",
  ranp = c(
    is_well_known = "n", 
    is_bundle= "n",
    brand.recall_this = "n",
    brand.recognition_this = "n",
    past.use_this = "n"
    ),
  R = 2000
)
summary(experiment_model_cluster_wtp)

# sprawdz jak wyglądają wyniki gdy estymujemy dla każdego klastra osobno
