setwd("DCE_design")
#install.packages("remotes")
#remotes::install_github("jhelvy/cbcTools")
library(cbcTools)
library(dplyr)

# create profiles
profiles <- cbc_profiles(
  type = c("burger_classic", "burger_premium", "bundle_classic", "bundle_premium"),
  brand = c("mcdonalds", "burger_king", "max_burger", "wendys"),
  price = seq(12, 37, 1)
)
# create profiles restrictions
restricted_profiles <- cbc_restrict(
  profiles,
  (type == "burger_classic" & price >= 20) |
    (type == "burger_premium" & (price <= 20 | price >= 29)) |
    (type == "bundle_classic" & (price <= 22 | price >= 30)) |
    (type == "bundle_premium" & price <= 26)
)
# create D-efficient design
# 200 respondents, 3 alternatives, 6 questions, 3 blocks
design <- cbc_design(
  profiles = restricted_profiles,
  n_resp = 200,
  n_alts = 3,
  n_q = 6,
  n_blocks = 3,
  method = 'dopt',
  no_choice = TRUE
)
# add random kcal and gram values with restrictions
design$type[design$type_burger_classic == 1] <- "burger_classic"
design$type[design$type_burger_premium == 1] <- "burger_premium"
design$type[design$type_bundle_classic == 1] <- "bundle_classic"
design$type[design$type_bundle_premium == 1] <- "bundle_premium"

design$kcal <- NA

for (i in 1:nrow(design)) {
  if (is.na(design$type[i])) {
    next 
  } else if (design$type[i] == "burger_classic") {
    design$kcal[i] <- sample(600:1100, 1)
  } else if (design$type[i] == "burger_premium") {
    design$kcal[i] <- sample(c(400:500, 800:1100), 1)
  } else if (design$type[i] == "bundle_classic") {
    design$kcal[i] <- sample(c(400:700, 1000:1100), 1)
  } else if (design$type[i] == "bundle_premium") {
    design$kcal[i] <- sample(400:800, 1)
  }
}

design$gram <- NA

for (i in 1:nrow(design)) {
  if (is.na(design$type[i])) {
    next 
  } else if (design$type[i] == "burger_classic") {
    design$gram[i] <- sample(220:420, 1)
  } else if (design$type[i] == "burger_premium") {
    design$gram[i] <- sample(c(180:200, 400:420), 1)
  } else if (design$type[i] == "bundle_classic") {
    design$gram[i] <- sample(c(180:300, 400:420), 1)
  } else if (design$type[i] == "bundle_premium") {
    design$gram[i] <- sample(180:350, 1)
  }
}
design$type <- NULL
# inspect design
design %>%
  group_by(blockID) %>%
  summarise(
    prices = toString(sort(unique(price))),
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
  ungroup() %>%
  {invisible(
    Map(function(id, pr, ty, br) {
      cat("\nblockID =", id, "\n")
      cat("prices:", pr, "\n")
      cat("types:", ty, "\n")
      cat("brands:", br, "\n")
    }, .$blockID, .$prices, .$types, .$brands)
  )}
# rename columns
rename_dict <- c(
  "profile_id" = "profileID",
  "respondent_id" = "respID",
  "question_id" = "qID",
  "alternative_id" = "altID",
  "observation_id" = "obsID",
  "block_id" = "blockID"
)
design <- design %>%
  rename(!!!rename_dict)
# save design
design %>%
  mutate(id = row_number(), .before = 1) %>% 
  write.csv("data/DCE_design.csv", row.names = FALSE)











