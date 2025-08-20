setwd("DCE_design")
library(tidyverse)

raw_design <- read.csv("data/newest_raw_design.csv", sep = ";")

# config:
type_conf <- c("burger_classic", "burger_premium", "bundle_classic", "bundle_premium")
brand_conf <- c("wendys", "max_burger", "burger_king", "mcdonalds")

long_design <- raw_design %>%
  pivot_longer(
    cols = starts_with("alt"),
    names_to = c("alt", ".value"),
    names_pattern = "alt(\\d+)\\.(.*)"
  ) %>%
  rename(
    question_id = Choice.situation,
    alternative_id = alt,
    block_id = Block,
    price = tax
  ) %>%
  arrange(question_id, alternative_id)

long_design <- long_design %>%
  mutate(
    type_label = factor(type_conf[type], levels = type_conf),
    brand_label = factor(brand_conf[brand], levels = brand_conf)
  )

dummies <- model.matrix(~ brand_label + type_label - 1, 
                        data = long_design,
                        contrasts.arg = list(
                          brand_label = contrasts(long_design$brand_label, contrasts = FALSE),
                          type_label = contrasts(long_design$type_label, contrasts = FALSE)
                        )) %>%
  as.data.frame()

colnames(dummies) <- colnames(dummies) %>%
  str_replace("^type_label", "type_") %>%
  str_replace("^brand_label", "brand_")

long_design <- long_design %>%
  bind_cols(dummies) %>%
  select(-type, -brand, -type_label, -brand_label)

long_design <- long_design %>%
  mutate(
    kcal = as.numeric(str_replace(kcal, ",", ".")) * 100,
    gram = as.numeric(str_replace(gram, ",", ".")) * 100,
    alternative_id = as.numeric(alternative_id)
  )

long_design <- long_design %>%
  mutate(no_choice = 0)

no_choice_rows <- long_design %>%
  select(block_id, question_id) %>%
  distinct() %>%
  mutate(
    alternative_id = 4,
    no_choice = 1
  )
value_cols <- setdiff(colnames(long_design), c("block_id", "question_id", "alternative_id", "no_choice"))
no_choice_rows[value_cols] <- 0
long_design <- bind_rows(long_design, no_choice_rows) %>%
  arrange(block_id, question_id, alternative_id)

long_design <- long_design %>%
  group_by(block_id) %>%
  mutate(question_id = dense_rank(question_id)) %>%
  ungroup()

respondents <- tibble(
  respondent_id = 1:500,
  block_id = rep(1:10, each = 50)
)

final_design <- respondents %>%
  left_join(long_design, by = "block_id") %>%
  arrange(respondent_id, question_id, alternative_id)

write.csv(final_design, "data/DCE_design_optimized.csv", row.names = FALSE)
