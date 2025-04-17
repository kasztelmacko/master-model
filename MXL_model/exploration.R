setwd("MXL_model")
library(dplyr)
library(ggplot2)
library(gridExtra)

data <- read.csv("data/clean_data.csv")

# plot age distribution
hist_plot <- ggplot(data, aes(x = age)) +
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

box_plot <- ggplot(data, aes(x = age, y = 1)) +
  geom_boxplot(fill = "white", color = "black", width = 0.2) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_y_continuous(breaks = NULL)

grid.arrange(hist_plot, box_plot, ncol = 1, heights = c(3, 1))
