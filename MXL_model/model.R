setwd("MXL_model")
library(dplyr)
library(mlogit)
library(car)

## 1. DATA PREPARATION --------------------------------------------------------
data <- read.csv("data/filtered_data.csv")

data <- data %>%
  mutate(
    is_well_known = ifelse(is.na(is_well_known), 0, is_well_known),
  )

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
    eats_fastfood_weekly = ifelse(fast.food.frequency %in% c("raz-w-tygodniu", "czesciej-niz-raz-w-tygodniu"), 1, 0),
    # eats_fastfood_rarely = ifelse(fast.food.frequency %in% c("rzadziej-niz-raz-w-miesiacu", "raz-w-miesiacu"), 1, 0)
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
car::vif(lm(price ~ brand.recall_this + brand.recognition_this + past.use_this + 
             income_low + income_high + age + market_awareness + 
             is_well_known + city_under_500k + city_over_500k, data = data))
# VIF < 5, no multicollinearity

X <- model.matrix(~ type_burger_premium + type_bundle_classic + type_bundle_premium +
                  is_well_known + price +
                  brand.recall_this + brand.recognition_this + past.use_this +
                  price:income_low + price:income_high + price:age + price:is_female +
                  price:eats_fastfood_weekly + price:market_awareness +
                  price:city_under_500k + price:city_over_500k,
                  data)
qrX <- qr(X)
cat("Columns =", ncol(X), "Rank =", qrX$rank, "\n")
# no redundant columns

cor_matrix <- cor(data[,c("is_well_known", "brand.recall_this", 
                          "brand.recognition_this", "past.use_this")])
print(cor_matrix)


## 2. DATA FORMATTING ---------------------------------------------------------
data_mlogit <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id",
  opposite = "price"          # Ensures negative coefficient for price (for WTP)
)

## 3. MODEL FORMULA -----------------------------------------------------------
# ---------------------------------------------------------------------------------
model_formula <- choice ~ 
  type_burger_premium + type_bundle_classic + type_bundle_premium +
  is_well_known + price +
  brand.recall_this + brand.recognition_this + past.use_this +
  price:income_low + price:income_high + price:age + price:is_female +
  price:eats_fastfood_weekly + price:market_awareness +
  price:city_under_500k + price:city_over_500k | 1 | is_well_known + brand.recall_this + brand.recognition_this + past.use_this
# ---------------------------------------------------------------------------------
# model_formula <- choice ~ 
#   # Product attributes
#   type_burger_premium + type_bundle_classic + type_bundle_premium +
#   is_well_known + price +
  
#   # Brand perception effects
#   brand.recall_this + brand.recognition_this + past.use_this +
  
#   # Interactions: consumer traits × price
#   price:income_low + price:income_high + price:age + price:is_female +
#   price:eats_fastfood_weekly + price:market_awareness +
#   price:city_under_500k + price:city_over_500k |
  
#   # Alternative specific constants
#   1 |
  
#   # Random parameters
#   is_well_known + brand.recall_this + brand.recognition_this + past.use_this


# ---------------------------------------------------------------------------------
# model_formula <- choice ~ price + is_well_known + brand.recall_this + brand.recognition_this + past.use_this +
#                  age + is_female + income_low + income_high + eats_fastfood_weekly + market_awareness + 
#                  is_city_under_500k + is_city_over_500k + # Location Dummies
#                  1 | # Intercept (no random effects for fixed variables)
#                  (is_well_known + brand.recall_this + brand.recognition_this + past.use_this | respondent_id) # Random effects for individual-level brand-related variables


## 4. MODEL ESTIMATION --------------------------------------------------------
mixed_logit_model <- mlogit(
  formula = model_formula,
  data = data_mlogit,
  panel = TRUE,
  rpar = c(
    is_well_known = "n",
    brand.recall_this = "n",
    brand.recognition_this = "n",
    past.use_this = "n"
  ),
  R = 100,
  print.level = 2
)

summary(mixed_logit_model)
