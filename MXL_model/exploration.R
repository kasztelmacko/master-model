setwd("MXL_model")
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
write.csv(filtered_data, "data/filtered_data.csv", row.names = FALSE)

# create survey calls
survey_cols <- c(
  "respondent_id",
  "age",
  "gender",
  "income",
  "location",
  "fast.food.frequency",
  "education",
  "market_awareness"
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

median_market_awareness <- respondent_data %>%
  group_by(location) %>%
  summarise(
    avg_market_awareness = mean(market_awareness, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nMedian market_awareness for each location:\n")
print(median_market_awareness)

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
