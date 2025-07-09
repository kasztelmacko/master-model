setwd("DCE_design")
#install.packages("remotes")
#remotes::install_github("jhelvy/cbcTools")
library(cbcTools)
library(dplyr)

# create profiles
profiles <- cbc_profiles(
  type = c("burger_classic", "burger_premium", "bundle_classic", "bundle_premium"),
  brand = c("mcdonalds", "burger_king", "max_burger", "wendys"),
  price = c(12, 14, 15, 17, 20, 21, 25, 27, 30, 31, 34),
  kcal = seq(400, 1100, 200),
  gram = seq(220, 420, 65)
)
# create profiles restrictions
restricted_profiles <- cbc_restrict(
  profiles,
  (type == "burger_classic" & price >= 20) |
    (type == "burger_premium" & (price <= 20 | price >= 29)) |
    (type == "bundle_classic" & (price <= 20 | price >= 29)) |
    (type == "bundle_premium" & price <= 26),
  
  (type == "burger_classic" & kcal >= 650) |
    (type == "burger_premium" & (kcal <= 500 | kcal >= 800)) |
    (type == "bundle_classic" & (kcal <= 500 | kcal >= 1000)) |
    (type == "bundle_premium" & kcal <= 800),
  
  (type == "burger_classic" & gram >= 300) |
    (type == "burger_premium" & (gram <= 200 | gram >= 350)) |
    (type == "bundle_classic" & (gram <= 200 | gram >= 400)) |
    (type == "bundle_premium" & gram <= 350)
)
# create D-efficient design
# 200 respondents, 3 alternatives, 6 questions, 3 blocks
design <- cbc_design(
  profiles = restricted_profiles,
  n_resp = 300,
  n_alts = 3,
  n_q = 6,
  # n_blocks = 3,
  method = 'full',
  no_choice = TRUE
)

# inspect design
if ("blockID" %in% names(design)) {
  summary_df <- design %>%
    group_by(blockID) %>%
    summarise(
      prices = toString(sort(unique(price))),
      kcals = toString(sort(unique(kcal))),
      grams = toString(sort(unique(gram))),
      types = {
        type_cols <- select(pick(everything()), starts_with("type_"))
        active_types <- names(type_cols)[as.logical(colSums(type_cols))]
        toString(unique(active_types))
      },
      brands = {
        brand_cols <- select(pick(everything()), starts_with("brand_"))
        active_brands <- names(brand_cols)[as.logical(colSums(brand_cols))]
        toString(unique(active_brands))
      }
    ) %>%
    ungroup()
  invisible(
    Map(function(id, pr, kc, gr, ty, br) {
      cat("\nblockID =", id, "\n")
      cat("prices:", pr, "\n")
      cat("kcals:", kc, "\n")
      cat("grams:", gr, "\n")
      cat("types:", ty, "\n")
      cat("brands:", br, "\n")
    }, summary_df$blockID, summary_df$prices, summary_df$kcals, summary_df$grams, summary_df$types, summary_df$brands)
  )
} else {
  prices <- toString(sort(unique(design$price)))
  kcals <- toString(sort(unique(design$kcal)))
  grams <- toString(sort(unique(design$gram)))
  type_cols <- select(design, starts_with("type_"))
  active_types <- names(type_cols)[as.logical(colSums(type_cols))]
  types <- toString(unique(active_types))
  brand_cols <- select(design, starts_with("brand_"))
  active_brands <- names(brand_cols)[as.logical(colSums(brand_cols))]
  brands <- toString(unique(active_brands))
  cat("prices:", prices, "\n")
  cat("kcals:", kcals, "\n")
  cat("grams:", grams, "\n")
  cat("types:", types, "\n")
  cat("brands:", brands, "\n")
}

# rename columns
rename_dict <- c(
  "profile_id" = "profileID",
  "respondent_id" = "respID",
  "question_id" = "qID",
  "alternative_id" = "altID",
  "observation_id" = "obsID"
  # "block_id" = "blockID"
)
design <- design %>%
  rename(!!!rename_dict)
# save design
design %>%
  mutate(id = row_number(), .before = 1) %>% 
  write.csv("data/DCE_design_full.csv", row.names = FALSE)
  
design %>%
  select(-any_of(c(setdiff(names(rename_dict), "profile_id"), "id"))) %>%
  distinct() %>%
  write.csv("data/DCE_distinct_profiles.csv", row.names = FALSE)