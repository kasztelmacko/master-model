setwd("MXL_model")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)

## 1. DATA PREPARATION --------------------------------------------------------
data <- read.csv("data/filtered_data.csv")

data <- data %>%
  group_by(respondent_id, question_id) %>%
  mutate(
    alternative_id = row_number(),
    respondent_question_id = paste(respondent_id, question_id, sep = "_")
  ) %>%
  ungroup()

# bin and dummy
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

    # Gender dummies
    is_female = ifelse(gender == "kobieta", 1, 0),
    # is_male = ifelse(gender == "mezczyzna", 1, 0),

    # Education dummies
    # is_higher_edu = ifelse(education == "wyzsze", 1, 0),
    # is_studying = ifelse(education == "w-trakcie-studiow", 1, 0),

    # Income dummies
    income_low = ifelse(income %in% c("ponizej-1000", "1000-2500"), 1, 0),
    # income_mid = ifelse(income %in% c("2500-5000", "5000-7500"), 1, 0),
    income_high = ifelse(income %in% c("7500-10000", "powyzej-10000"), 1, 0),

    # Fast food frequency dummies
    # eats_fastfood_weekly = ifelse(fast.food.frequency %in% c("raz-w-tygodniu", "czesciej-niz-raz-w-tygodniu"), 1, 0),
    eats_fastfood_rarely = ifelse(fast.food.frequency %in% c("rzadziej-niz-raz-w-miesiacu", "raz-w-miesiacu"), 1, 0)
  ) %>%
  select(
    -location, -location_grouped,
    -gender,
    -education,
    -income,
    -fast.food.frequency
  )

## 2. TESTS ---------------------------------------------------------
data <- as.data.frame(data)
car::vif(lm(price ~ brand.recall_this + brand.recognition_this + brand.past_use_this + 
             income_low + income_high + age + market_awareness + 
             is_well_known + city_under_500k + city_over_500k, data = data))
# VIF < 5, no multicollinearity

X <- model.matrix(~ type_burger_premium + type_bundle_classic + type_bundle_premium +
                  is_well_known + price +
                  brand.recall_this + brand.recognition_this + brand.past_use_this +
                  price:income_low + price:income_high + price:age + price:is_female +
                  price:eats_fastfood_weekly + price:market_awareness +
                  price:city_under_500k + price:city_over_500k,
                  data)
qrX <- qr(X)
cat("Columns =", ncol(X), "Rank =", qrX$rank, "\n")
# no redundant columns

cor_matrix <- cor(data[,c("is_well_known", "brand.recall_this", 
                          "brand.recognition_this", "brand.past_use_this")])
print(cor_matrix)


## 2. DATA FORMATTING ---------------------------------------------------------
data$log_price <- ifelse(data$price > 0, log(data$price), 0)
data$log_age <- ifelse(data$age > 0, log(data$age), 0)
data$log_market_awareness <- ifelse(data$market_awareness > 0, log(data$market_awareness), 0)

normalize_minmax <- function(x) {
  if (all(is.na(x))) return(x)
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

data$price_norm <- normalize_minmax(data$price)
data$age_norm <- normalize_minmax(data$age)
data$market_awareness_norm <- normalize_minmax(data$market_awareness)

data_mlogit_full <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id",
  opposite = "price"  # Ensures negative coefficient for price
)

## 3. MODEL FORMULA -----------------------------------------------------------
# ############################################################################
# MNL with just RQ1 variables
# ############################################################################
rq1_base_model_formula <- choice ~ 
  price +
  brand_burger_king + brand_max_burger + brand_wendys +
  type_burger_premium + type_bundle_classic + type_bundle_premium +
  price:income_low + price:income_high + price:age + price:is_female +
  price:eats_fastfood_weekly + price:market_awareness +
  price:city_under_500k + price:city_over_500k | 0

rq1_base_model <- mlogit(
  formula = rq1_base_model_formula,
  data = data_mlogit_full
)

summary(rq1_base_model)

# ############################################################################
# MNL with just RQ2 variables
# ############################################################################

rq2_base_model_formula <- choice ~ 
  price +
  brand_burger_king + brand_max_burger + brand_wendys +
  type_burger_premium + type_bundle_classic + type_bundle_premium +
  brand.recall_this + brand.recognition_this + brand.past_use_this | 0

rq2_base_model <- mlogit(
  formula = rq2_base_model_formula,
  data = data_mlogit_full
)

summary(rq2_base_model)

# ############################################################################
# MXL ideal
# ############################################################################
# choice ~ alternative_variables | individual_specific_variables | variables_for_random_means
# ############################################################################
mxl_base_model_formula <- choice ~ 
  price + brand_burger_king + brand_max_burger + brand_wendys +
  type_burger_premium + type_bundle_classic + type_bundle_premium +
  brand.recall_this + brand.recognition_this + brand.past_use_this |
  0 |
  income_low + income_high + age + market_awareness + city_over_500k + city_under_500k + eats_fastfood_weekly


ranp = c(
  price = "ln",
  brand_burger_king = "n", 
  brand_max_burger = "n",
  brand_wendys = "n",
  brand.recall_this = "n",
  brand.recognition_this = "n",
  brand.past_use_this = "n"
)

mxl_base_model <- gmnl(
  formula = mxl_base_model_formula,
  data = data_mlogit_full,
  model = "mixl",
  panel = TRUE,
  ranp = ranp,
  R = 500,
  print.level = 2,
  halton = NA,
  correlation = FALSE
)

summary(mxl_base_model)

# ############################################################################
# MXL with log
# ############################################################################
data_mlogit_full_log <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id",
  opposite = "log_price"  # Ensures negative coefficient for price
)

mxl_model_formula_log <- choice ~ 
  no_choice +
  log_price +
  brand.recall_this + brand.recognition_this + brand.past_use_this |
  0 |
  income_low + income_high + log_age + log_market_awareness + city_over_500k + city_under_500k + eats_fastfood_weekly

ranp = c(
  log_price = "n",
  brand.recall_this = "n",
  brand.recognition_this = "n",
  brand.past_use_this = "n"
)

mxl_model_log <- gmnl(
  formula = mxl_model_formula_log,
  data = data_mlogit_full_log,
  model = "mixl",
  panel = TRUE,
  ranp = ranp,
  R = 500,
  print.level = 2,
  halton = NA,
  correlation = FALSE
)

summary(mxl_model_log)

# ############################################################################
# MXL wwith min/max standardization
# ############################################################################
data_mlogit_full_norm <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id",
  opposite = "price_norm"  # Ensures negative coefficient for price
)

mxl_model_formula_norm <- choice ~ 
  no_choice +
  price_norm +
  brand.recall_this + brand.recognition_this + brand.past_use_this |
  0 |
  income_low + income_high + age_norm + market_awareness_norm + city_over_500k + city_under_500k + eats_fastfood_weekly

ranp = c(
  price_norm = "n",
  brand.recall_this = "n",
  brand.recognition_this = "n",
  brand.past_use_this = "n"
)

mxl_model_norm <- gmnl(
  formula = mxl_model_formula_norm,
  data = data_mlogit_full_norm,
  model = "mixl",
  panel = TRUE,
  ranp = ranp,
  R = 500,
  print.level = 2,
  halton = NA,
  correlation = FALSE
)

summary(mxl_model_norm)

# ############################################################################
# MXL with brand_* as random effects
# ############################################################################
mxl_model_formula_price_random <- choice ~
  price + 
  no_choice + 
  brand.recall_this + brand.recognition_this + brand.past_use_this + 
  price:income_low + price:income_high + price:market_awareness_norm + price:eats_fastfood_rarely + price:is_female + price:age_norm + price:city_under_500k + price:city_over_500k +
  price:brand.recall_this + price:brand.recognition_this + price:brand.past_use_this | #alternative_variables
  0 | #individual_specific_variables
  0   #variables_for_random_means

ranp <- c(
    price = "ln"
)

mxl_model_price_random <- gmnl(
  formula = mxl_model_formula_price_random,
  data = data_mlogit_full,
  model = "mixl",
  panel = TRUE,
  ranp = ranp,
  R = 500,
  print.level = 2,
  halton = NA,
  correlation = FALSE
)

summary(mxl_model_price_random)
