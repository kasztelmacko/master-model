setwd("Modeling")
library(dplyr)
library(ggplot2)
library(gridExtra)

data <- read.csv("data/clean_data.csv")
brand_recall_data <- read.csv("data/brand_recall.csv")
brand_recognition_data <- read.csv("data/brand_recognition.csv")

# filter out no_choice
excluded_ids <- data %>%
  filter(choice == 1 & no_choice == 1) %>%
  group_by(respondent_id) %>%
  summarise(no_choice_count = n()) %>%
  filter(no_choice_count > 4) %>%
  select(respondent_id)

filtered_data <- data %>%
  anti_join(excluded_ids, by = "respondent_id")

original_count <- data %>%
  distinct(respondent_id) %>%
  nrow()


cat("Original number of respondent_id:", original_count, "\n")

# filter out age > 35
filtered_data <- filtered_data %>%
  filter(age <= 35)

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


# plot age distribution
hist_plot <- ggplot(respondent_data, aes(x = age)) +
  geom_histogram(binwidth = 3, fill = "white", color = "black") +
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
survey_long <- survey %>%
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

plot_recall <- generate_plot(brand_recall_data, highlight_brands, "brand.recall", "Top 8 Recalled Brands")
plot_recognition <- generate_plot(brand_recognition_data, highlight_brands, "brand.recognition", "Recognized Brands")

ggsave("plots/recall_recognition_plot.png", 
       grid.arrange(plot_recall, plot_recognition, ncol = 2),
       width = 8, height = 6)

filtered_data <- filtered_data %>%
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

filtered_data <- filtered_data %>%
  mutate(
    respondent_question_id = paste0(respondent_id, "_", question_id)
  )

write.csv(filtered_data, "data_new/filtered_data.csv", row.names = FALSE)
