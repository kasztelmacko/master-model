setwd("Modeling")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# combine all data
data2 <- read.csv("data/2/clean_data.csv")
brand_recall_data2 <- read.csv("data/2/brand_recall.csv")
brand_recognition_data2 <- read.csv("data/2/brand_recognition.csv")

data3 <- read.csv("data/3/clean_data.csv")
brand_recall_data3 <- read.csv("data/3/brand_recall.csv")
brand_recognition_data3 <- read.csv("data/3/brand_recognition.csv")

data <- bind_rows(data2, data3)


original_count <- data %>%
  distinct(respondent_id) %>%
  nrow()


cat("Original number of respondent_id:", original_count, "\n")

# filter out no_choice
excluded_ids <- data %>%
  filter(choice == 1 & no_choice == 1) %>%
  group_by(respondent_id) %>%
  summarise(no_choice_count = n()) %>%
  filter(no_choice_count > 4) %>%
  select(respondent_id)

filtered_data <- data %>%
  anti_join(excluded_ids, by = "respondent_id")


filtered_count <- filtered_data %>%
  distinct(respondent_id) %>%
  nrow()
cat("Number of respondent_id in filtered_data:", filtered_count, "\n")

# filter out age > 35 and age < 18
filtered_data <- filtered_data %>%
  filter(age <= 35 & age >= 18)
filtered_count <- filtered_data %>%
  distinct(respondent_id) %>%
  nrow()
cat("Number of respondent_id in filtered_data:", filtered_count, "\n")

# filter small inne gender group (4 records)
filtered_data <- filtered_data %>%
  filter(gender != "inne")
filtered_count <- filtered_data %>%
  distinct(respondent_id) %>%
  nrow()
cat("Number of respondent_id in filtered_data:", filtered_count, "\n")


# create survey calls
survey_cols <- c(
  "respondent_id",
  "age",
  "gender",
  "income",
  "location",
  "fast.food.frequency",
  "education",
  "total_recalled",
  "total_recognized",
  "avg_price_guess_diff"
)

respondent_data <- filtered_data %>%
  select(all_of(survey_cols)) %>%
  distinct(respondent_id, .keep_all = TRUE)

filtered_data <- filtered_data %>%
  mutate(
    avg_price_guess_diff = ifelse(avg_price_guess_diff > 10, 10, avg_price_guess_diff)
  ) %>%
  mutate(
    norm_total_recalled = (total_recalled - min(total_recalled, na.rm = TRUE)) /
                          (max(total_recalled, na.rm = TRUE) - min(total_recalled, na.rm = TRUE)),
    
    norm_total_recognized = (total_recognized - min(total_recognized, na.rm = TRUE)) /
                            (max(total_recognized, na.rm = TRUE) - min(total_recognized, na.rm = TRUE)),
    
    norm_avg_price_guess_diff = (avg_price_guess_diff - min(avg_price_guess_diff, na.rm = TRUE)) /
                                (max(avg_price_guess_diff, na.rm = TRUE) - min(avg_price_guess_diff, na.rm = TRUE)),
    price_accuracy_score = 1 - norm_avg_price_guess_diff
  ) %>%
  mutate(
    market_awareness = (price_accuracy_score + norm_total_recalled + norm_total_recognized) / 3
  )


# plot age distribution
hist_plot <- ggplot(respondent_data, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

box_plot <- ggplot(respondent_data, aes(x = age, y = 1)) +
  geom_boxplot(fill = "white", color = "black", width = 0.2) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = seq(min(respondent_data$age, na.rm = TRUE), max(respondent_data$age, na.rm = TRUE), by = 2))

ggsave("plots/age_distribution_plot.png", grid.arrange(hist_plot, box_plot, ncol = 1, heights = c(3, 1)), width = 8, height = 6)

# plot recall, recognition, and price guess differences 
survey_long <- filtered_data %>%
  select(total_recalled, total_recognized, avg_price_guess_diff) %>%
  pivot_longer(cols = everything(), names_to = "metric", values_to = "value")

market_awareness_plot <- ggplot(survey_long, aes(x = metric, y = value, fill = "white")) +
  geom_boxplot(fill = "white", color = "black", width = 0.2) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())
ggsave("plots/marekt_awareness_stats.png", market_awareness_plot, width = 8, height = 6)

# show population statistics
calculate_statistics <- function(data, column_name) {
  stats <- data %>%
    group_by(across(all_of(column_name))) %>%
    summarise(
      count = n(),
      percentage = (n() / nrow(data)) * 100,
      .groups = "drop"
    )
  
  cat("\nStatistics for", column_name, ":\n")
  print(stats)
}

cat("Population statistics:\n")
calculate_statistics(respondent_data, "gender")
calculate_statistics(respondent_data, "education")
calculate_statistics(respondent_data, "income")
calculate_statistics(respondent_data, "location")
calculate_statistics(respondent_data, "fast.food.frequency")

# plot recall
generate_plot <- function(data, highlight_brands, brand_column, plot_title) {
  top_brands <- data %>%
    arrange(desc(count)) %>%
    slice_head(n = 8)

  top_brands$color <- ifelse(top_brands[[brand_column]] %in% highlight_brands, top_brands[[brand_column]], "Other")
  
  ggplot(top_brands, aes(x = reorder(!!sym(brand_column), count), y = count)) +
    geom_col(aes(fill = color), color = "black", width = 0.6) +
    geom_text(aes(label = count), hjust = 1.5, color = "black", size = 5) +
    coord_flip() +
    scale_fill_manual(values = c(
      "mcdonalds" = "lightgrey",
      "burgerking" = "lightgrey",
      "maxburgers" = "lightgrey",
      "wendys" = "lightgrey",
      "Other" = "white"
    )) +
    ggtitle(plot_title) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none"
    )
}

highlight_brands <- c("mcdonalds", "burgerking", "maxburgers", "wendys")

plot_recall <- generate_plot(brand_recall_data, highlight_brands, "brand.recall", "Top Recalled Brands")
plot_recognition <- generate_plot(brand_recognition_data, highlight_brands, "brand.recognition", "Top Recognized Brands")

ggsave("plots/recall_recognition_plot.png", 
       grid.arrange(plot_recall, plot_recognition, ncol = 2),
       width = 8, height = 6)

filtered_data <- filtered_data %>%
  mutate(
    # Group location
    city_50_500 = ifelse(location %in% c("miasto-150-500", "miasto-50-150"), 1, 0),
    city_over_500 = ifelse(location == "miasto-500", 1, 0),
    rural_or_small_city = ifelse(location %in% c("miast-50", "wies"), 1, 0),

    # Gender dummies
    is_female = ifelse(gender == "kobieta", 1, 0),

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
    -location,
    -gender,
    -education,
    -income,
    -fast.food.frequency
  )

filtered_data <- filtered_data %>%
  mutate(
    # Log-transformed variables
    log_age = log(age + 1),
    log_total_recalled = log(total_recalled + 1),
    log_total_recognized = log(total_recognized + 1),
    log_avg_price_guess_diff = log(avg_price_guess_diff + 1),
    log_price = log(price + 1),

    norm_age = (age - min(age, na.rm = TRUE)) / (max(age, na.rm = TRUE) - min(age, na.rm = TRUE)),
    norm_total_recalled = (total_recalled - min(total_recalled, na.rm = TRUE)) / (max(total_recalled, na.rm = TRUE) - min(total_recalled, na.rm = TRUE)),
    norm_total_recognized = (total_recognized - min(total_recognized, na.rm = TRUE)) / (max(total_recognized, na.rm = TRUE) - min(total_recognized, na.rm = TRUE)),
    norm_avg_price_guess_diff = (avg_price_guess_diff - min(avg_price_guess_diff, na.rm = TRUE)) / (max(avg_price_guess_diff, na.rm = TRUE) - min(avg_price_guess_diff, na.rm = TRUE)),

    norm_kcal = (kcal - min(kcal, na.rm = TRUE)) / (max(kcal, na.rm = TRUE) - min(kcal, na.rm = TRUE)),
    norm_gram = (gram - min(gram, na.rm = TRUE)) / (max(gram, na.rm = TRUE) - min(gram, na.rm = TRUE)),
    norm_price = (price - min(price, na.rm = TRUE)) / (max(price, na.rm = TRUE) - min(price, na.rm = TRUE))
  )

cluster_vars <- c(
  "age",
  "income_high", "income_low",
  "total_recalled",
  "total_recognized",
  "avg_price_guess_diff",
  "eats_fastfood_weekly"
)

consumer_data <- filtered_data %>%
  group_by(respondent_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(all_of(cluster_vars)) %>%
  na.omit()

consumer_data_scaled <- scale(consumer_data)

sil_width <- c()
for (k in 2:6) {
  km <- kmeans(consumer_data_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(consumer_data_scaled))
  sil_width[k] <- mean(ss[, 3])
}
plot(2:6, sil_width[2:6], type = "b", xlab = "Number of clusters", ylab = "Average silhouette width", main = "Silhouette Analysis for K-means")

set.seed(123)
kmeans_result <- kmeans(consumer_data_scaled, centers = 4, nstart = 25)
consumer_data_with_id <- filtered_data %>%
  group_by(respondent_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(respondent_id, all_of(cluster_vars)) %>%
  na.omit() %>%
  mutate(cluster = factor(kmeans_result$cluster))

cluster_summary <- consumer_data_with_id %>%
  group_by(cluster) %>%
  summarise(
    age_mean = mean(age),
    total_recalled_mean = mean(total_recalled),
    total_recognized_mean = mean(total_recognized),
    avg_price_guess_diff_mean = mean(avg_price_guess_diff),
    
    income_high_count = sum(income_high),
    income_low_count = sum(income_low),
    eats_fastfood_weekly_count = sum(eats_fastfood_weekly),
    
    count = n()
  )
for (i in 1:nrow(cluster_summary)) {
  cat("---- Cluster", cluster_summary$cluster[i], "----\n")
  cat("Respondents:", cluster_summary$count[i], "\n")
  cat("age (mean):", round(cluster_summary$age_mean[i], 2), "\n")
  cat("total_recalled (mean):", round(cluster_summary$total_recalled_mean[i], 2), "\n")
  cat("total_recognized (mean):", round(cluster_summary$total_recognized_mean[i], 2), "\n")
  cat("avg_price_guess_diff (mean):", round(cluster_summary$avg_price_guess_diff_mean[i], 2), "\n")
  cat("income_high (count):", cluster_summary$income_high_count[i], "\n")
  cat("income_low (count):", cluster_summary$income_low_count[i], "\n")
  cat("eats_fastfood_weekly (count):", cluster_summary$eats_fastfood_weekly_count[i], "\n")
  cat("\n")
}

consumer_data_with_id <- consumer_data_with_id %>%
  mutate(
    income_aware_enthusiasts = as.integer(cluster == 1),
    low_awareness_explorers = as.integer(cluster == 2),
    low_income_youngsters = as.integer(cluster == 3),
    average_aware_consumers = as.integer(cluster == 4)
  )
filtered_data <- filtered_data %>%
  left_join(
    consumer_data_with_id %>%
      select(respondent_id, income_aware_enthusiasts, low_awareness_explorers, low_income_youngsters, average_aware_consumers),
    by = "respondent_id"
  )

write.csv(filtered_data, "data/final_data/model_data.csv", row.names = FALSE)
