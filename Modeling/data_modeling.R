setwd("Modeling")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)
library(gtools)
library(logitr)
library(MASS)

data <- read.csv("data/final_data/model_data.csv")

show_latent_class_statistics <- function(model) {
  posterior_probs_resp <- model$Wnq[seq(1, nrow(model$Wnq), by = 6), ]
  class_assignments <- apply(posterior_probs_resp, 1, which.max)

  respondent_data <- mlogit_data %>%
    group_by(respondent_id) %>%
    slice(1) %>%
    ungroup()
  respondent_data$class <- class_assignments

  class_summary <- respondent_data %>%
    group_by(class) %>%
    summarise(
      n = n(),
      age_mean = mean(age, na.rm = TRUE),
      avg_price_guess_diff = mean(avg_price_guess_diff, na.rm = TRUE),
      total_recalled = mean(total_recalled, na.rm = TRUE),
      total_recognized = mean(total_recognized, na.rm = TRUE),
      across(
        c(is_female, income_high, income_low, is_graduated, city_over_500, city_50_500, rural_or_small_city, eats_fastfood_weekly),
        list(count = ~sum(.)),
        .names = "{.col}_{.fn}"
      )
    )

  print(class_summary, n = Inf, width = Inf)
  }

summary_model <- function(model, null_model = NULL) {
  cat("Model Summary:\n")
  print(summary(model))

  ll_full <- as.numeric(logLik(model))

  # If null_model is provided
  if (!is.null(null_model)) {
    ll_null <- as.numeric(logLik(null_model))
    
    r2_mcfadden <- 1 - (ll_full / ll_null)
    k <- attr(logLik(model), "df")
    adj_r2_mcfadden <- 1 - ((ll_full - k) / ll_null)
    
    cat("\nMcFadden R²:", round(r2_mcfadden, 4), "\n")
    cat("Adjusted McFadden R²:", round(adj_r2_mcfadden, 4), "\n")
  } else {
    cat("\nMcFadden R²: NA (null model not provided)\n")
    cat("Adjusted McFadden R²: NA\n")
  }

  cat("AIC:", AIC(model), "\n")
  cat("BIC:", BIC(model), "\n")
}

simulate_wtp_lc_with_pval <- function(model, price_var = "price", n_sim = 1000) {
  require(MASS)
  
  coef_mean <- model$coefficients
  coef_vcov <- vcov(model)
  
  # Simulate coefficient draws
  draws <- MASS::mvrnorm(n = n_sim, mu = coef_mean, Sigma = coef_vcov)
  colnames(draws) <- names(coef_mean)
  
  # Parse class.X.variable pattern
  coef_names <- colnames(draws)
  matches <- regexec("^class\\.([0-9]+)\\.(.+)$", coef_names)
  parsed <- regmatches(coef_names, matches)
  
  coef_info <- do.call(rbind, lapply(parsed, function(x) {
    if (length(x) == 3) data.frame(class = x[2], var = x[3], stringsAsFactors = FALSE)
    else data.frame(class = NA, var = NA)
  }))
  coef_info$colname <- coef_names
  coef_info <- na.omit(coef_info)
  
  vars <- unique(coef_info$var)
  vars <- vars[vars != price_var]
  classes <- unique(coef_info$class)
  
  # Store results
  wtp_list <- list()
  
  for (cls in classes) {
    class_info <- coef_info[coef_info$class == cls, ]
    price_col <- class_info$colname[class_info$var == price_var]
    price_draws <- draws[, price_col]
    
    for (var in vars) {
      var_col <- class_info$colname[class_info$var == var]
      attr_draws <- draws[, var_col]
      
      wtp_draws <- -attr_draws / price_draws
      
      # Compute empirical p-value
      p_val <- 2 * min(
        mean(wtp_draws < 0, na.rm = TRUE),
        mean(wtp_draws > 0, na.rm = TRUE)
      )
      
      wtp_list[[paste0("class", cls, "_", var)]] <- data.frame(
        mean_wtp = mean(wtp_draws, na.rm = TRUE),
        p_value = p_val
      )
    }
  }
  
  wtp_df <- do.call(rbind, wtp_list)
  return(wtp_df)
}

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)
mlogit_data$obsID <- as.integer(as.factor(mlogit_data$respondent_question_id))

##########################################################################
#                           MULTINOMIAL LOGIT                            # 
##########################################################################

##########################################################################
# MNL PREFERENCE SPACE
##########################################################################
mnl_preference_space <- logitr(
  data     = mlogit_data,
  outcome  = "choice",
  obsID    = "obsID",
  pars     = c("brand.recall_this",
               "brand.recognition_this",
               "past.use_this",
               "is_well_known",
               "is_bundle",
               "is_premium",
               "price",
               "gram",
               "no_choice")
)
summary_model(mnl_preference_space)

# Model Coefficients:
#                           Estimate  Std. Error z-value  Pr(>|z|)    
# brand.recall_this       0.23566323  0.16268182  1.4486 0.1474453
# brand.recognition_this -0.24138941  0.17150562 -1.4075 0.1592874
# past.use_this           0.23841069  0.10411016  2.2900 0.0220222 *
# is_well_known           0.43956974  0.09935003  4.4245 9.669e-06 ***
# is_bundle               1.06409463  0.09813580 10.8431 < 2.2e-16 ***
# is_premium              0.31360782  0.08159047  3.8437 0.0001212 ***
# price                  -0.05356463  0.00604040 -8.8677 < 2.2e-16 ***
# gram                    0.00094774  0.00047413  1.9989 0.0456189 *
# no_choice              -1.46057668  0.24523364 -5.9559 2.587e-09 ***

# Log-Likelihood:          -966.9132006
# Null Log-Likelihood:    -1164.4872633
# AIC:                     1951.8264012
# BIC:                     1994.4270000
# McFadden R2:                0.1696661
# Adj McFadden R2:            0.1619374
# Number of Observations:   840.0000000

##########################################################################
# MNL WTP SPACE
##########################################################################
mnl_wtp_space <- logitr(
  data     = mlogit_data,
  outcome  = "choice",
  obsID    = "obsID",
  pars     = c("brand.recall_this",
               "brand.recognition_this",
               "past.use_this",
               "is_well_known",
               "is_bundle",
               "is_premium",
               "gram",
               "no_choice"),
  scalePar = "price"
)
summary_model(mnl_wtp_space)

# Model Coefficients:
#                           Estimate  Std. Error z-value  Pr(>|z|)
# brand.recall_this        4.4682675   3.0938385  1.4442    0.1487    
# brand.recognition_this  -4.5785797   3.2607438 -1.4042    0.1603
# past.use_this            4.4594017   1.9770679  2.2556    0.0241 *
# is_well_known            8.2036640   1.8911669  4.3379 1.439e-05 ***
# is_bundle               19.8644363   2.1316108  9.3190 < 2.2e-16 ***
# is_premium               5.8631404   1.4905347  3.9336 8.369e-05 ***
# gram                     0.0176506   0.0091146  1.9365    0.0528 .
# no_choice              -27.3157332   4.4747198 -6.1045 1.032e-09 ***

# Log-Likelihood:          -966.9128327
# Null Log-Likelihood:    -1164.4872633
# AIC:                     1951.8256654
# BIC:                     1994.4263000
# McFadden R2:                0.1696665
# Adj McFadden R2:            0.1619377
# Number of Observations:   840.0000000


##########################################################################
#                           LATENT CLASS                                 # 
##########################################################################

lc_null_model <- gmnl(
  choice ~ 0 | 0 | 0 | 0 | 
    1 + norm_age + is_female + income_high + income_low + 
    norm_total_recalled + norm_total_recognized + 
    norm_avg_price_guess_diff + eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  Q = 2,
  R = 2000
)

##########################################################################
# LC PREFERENCE SPACE
##########################################################################
lc_preference_space <- gmnl(
  choice ~ 
    brand.recall_this + 
    brand.recognition_this + 
    past.use_this +
    is_well_known + 
    is_bundle + 
    is_premium +
    price +
    gram +
    no_choice |
    0 |
    0 |
    0 |
    1 +
    norm_age +
    is_female +
    income_high + income_low +
    norm_total_recalled +
    norm_total_recognized +
    norm_avg_price_guess_diff +
    eats_fastfood_weekly, 
  data = mlogit_data,
  model = "lc",
  R = 2000,
  Q = 2
)
summary_model(lc_preference_space, lc_null_model)
show_latent_class_statistics(lc_preference_space)

probs <- lc_preference_space$Qir
class1_probs <- probs[, 1]

# Coefficients:
#                                     Estimate  Std. Error z-value  Pr(>|z|)    
# class.1.brand.recall_this         9.3418e-01  3.9942e-01  2.3389  0.019342 *
# class.1.brand.recognition_this   -1.2289e+00  4.1004e-01 -2.9969  0.002727 **
# class.1.past.use_this             8.7860e-01  2.1273e-01  4.1302 3.625e-05 ***
# class.1.is_well_known             1.0922e-02  1.9939e-01  0.0548  0.956317
# class.1.is_bundle                 1.3650e+00  1.8991e-01  7.1874 6.604e-13 ***
# class.1.is_premium               -3.8975e-01  1.5781e-01 -2.4698  0.013520 *
# class.1.price                    -2.9226e-02  1.1090e-02 -2.6353  0.008406 **
# class.1.gram                     -9.5979e-05  1.0027e-03 -0.0957  0.923741
# class.1.no_choice                -4.9981e-01  4.3796e-01 -1.1412  0.253773
# class.2.brand.recall_this         1.5468e-01  1.8763e-01  0.8244  0.409739    
# class.2.brand.recognition_this    1.0458e-02  1.9720e-01  0.0530  0.957707
# class.2.past.use_this             3.1153e-02  1.2738e-01  0.2446  0.806799
# class.2.is_well_known             6.4288e-01  1.2410e-01  5.1805 2.212e-07 ***
# class.2.is_bundle                 1.0510e+00  1.2628e-01  8.3225 < 2.2e-16 ***
# class.2.is_premium                6.3684e-01  1.0399e-01  6.1239 9.130e-10 ***
# class.2.price                    -6.8372e-02  7.9172e-03 -8.6358 < 2.2e-16 ***
# class.2.gram                      1.2098e-03  5.8149e-04  2.0806  0.037473 *
# class.2.no_choice                -2.6420e+00  3.8436e-01 -6.8737 6.258e-12 ***
# (class)2                          6.6535e+01  3.2311e+01  2.0592  0.039479 *
# norm_age:class2                   7.4210e+01  3.1943e+01  2.3232  0.020168 *
# class2:is_female                 -1.1047e+02  4.9636e+01 -2.2257  0.026037 *  
# class2:income_high               -5.4213e+01  2.5176e+01 -2.1533  0.031293 *
# class2:income_low                 1.7857e+00  2.8175e+00  0.6338  0.526206
# class2:norm_total_recalled       -9.9834e+01  4.7688e+01 -2.0935  0.036306 *
# class2:norm_total_recognized      4.8030e+01  2.3259e+01  2.0650  0.038924 *
# class2:norm_avg_price_guess_diff  2.7331e+01  1.3852e+01  1.9730  0.048493 *
# class2:eats_fastfood_weekly      -2.9489e+01  1.4933e+01 -1.9748  0.048290 *

# Log Likelihood: -910.31
# AIC: 1874.619
# BIC: 2002.421

wtp_sims <- simulate_wtp_lc_with_pval(lc_preference_space, price_var = "price", n_sim = 1000)
print(wtp_sims)

#                                   mean_wtp p_value
# class1_brand.recall_this       65.26890732   0.022 *
# class1_brand.recognition_this -75.19210363   0.010 **
# class1_past.use_this           52.05026160   0.008 **
# class1_is_well_known            1.63888759   0.938 
# class1_is_bundle               83.39343151   0.008 **
# class1_is_premium             -25.59447476   0.020 *
# class1_gram                    -0.03080136   0.952 
# class1_no_choice              -16.97186570   0.292 
# class2_brand.recall_this        2.29025060   0.394 
# class2_brand.recognition_this   0.17575050   0.946
# class2_past.use_this            0.51209187   0.796
# class2_is_well_known            9.46886876   0.000 ***
# class2_is_bundle               15.39353879   0.000 ***
# class2_is_premium               9.45189728   0.000 ***
# class2_gram                     0.01770127   0.036 ***
# class2_no_choice              -39.15966300   0.000 ***


##########################################################################
#                           MIXED LOGIT                                  #
##########################################################################

##########################################################################
# MIXED PREFERENCE SPACE
##########################################################################
null_model <- mlogit(choice ~ 1, data = mlogit_data)
LL0 <- as.numeric(logLik(null_model))

mxl_preference_space <- gmnl(
  choice ~ brand.recall_this + 
           brand.recognition_this + 
           past.use_this +
           is_well_known + 
           is_bundle + 
           is_premium +
           price +
           gram +
           no_choice | 0, 
  data   = mlogit_data,
  model="mixl",
  ranp   = c(
    brand.recall_this      = "n",
    brand.recognition_this = "n",
    past.use_this          = "n",
    is_well_known          = "n",
    is_bundle              = "n",
    is_premium             = "n",
    price                  = "n",
    gram                   = "n",
    no_choice              = "n"
  ),
  R      = 2000,
  halton = NA,
  panel  = TRUE,
  keepDraws = TRUE
)
summary_model(mxl_preference_space)

#                              Estimate  Std. Error z-value  Pr(>|z|)
# brand.recall_this          0.35779591  0.29367509  1.2183 0.2230950
# brand.recognition_this    -0.27162149  0.31515178 -0.8619 0.3887562    
# past.use_this              0.28071655  0.19523623  1.4378 0.1504822
# is_well_known              0.62609482  0.16394698  3.8189 0.0001341 ***
# is_bundle                  1.58649686  0.17440471  9.0966 < 2.2e-16 ***
# is_premium                 0.33917665  0.19123042  1.7737 0.0761203 .
# price                     -0.08570019  0.01109736 -7.7226 1.132e-14 ***
# gram                       0.00114733  0.00070744  1.6218 0.1048437
# no_choice                 -2.98460740  0.53038789 -5.6272 1.831e-08 ***
# sd.brand.recall_this       0.68422405  0.61115177  1.1196 0.2628992
# sd.brand.recognition_this  0.79334569  0.52381034  1.5146 0.1298822
# sd.past.use_this           1.23885560  0.27524140  4.5010 6.764e-06 ***
# sd.is_well_known           0.85015625  0.22783281  3.7315 0.0001903 ***
# sd.is_bundle               0.92230857  0.22101593  4.1730 3.006e-05 ***
# sd.is_premium              1.81440721  0.23204302  7.8193 5.329e-15 ***
# sd.price                   0.05917065  0.01304732  4.5351 5.758e-06 ***
# sd.gram                    0.00244829  0.00148411  1.6497 0.0990105 .
# sd.no_choice               1.65496779  0.61037471  2.7114 0.0067000 **

# Log Likelihood: -880.03
# AIC: 1796.058
# BIC: 1881.26

wtp_results <- wtp.gmnl(mxl_preference_space, wrt = "price")

## (FROM GMNL DOCS) Note that, wtp.gmnl does not include the negative sign.
# so the WTP estimates are just ultiplied by -1

#                              Estimate  Std. Error t-value  Pr(>|t|)
# brand.recall_this           4.1749722   3.4594112 -1.2068 0.2274919
# brand.recognition_this     -3.1694386   3.7025317  0.8560 0.3919871
# past.use_this               3.2755651   2.2910138 -1.4297 0.1527903
# is_well_known               7.3056410   1.9320752 -3.7812 0.0001560 ***
# is_bundle                  18.5121744   2.3075349 -8.0225 1.110e-15 ***
# is_premium                  3.9577118   2.2267851 -1.7773 0.0755154 .  
# gram                        0.0133877   0.0084335 -1.5874 0.1124115
# no_choice                  -34.8261469   6.1624813  5.6513 1.592e-08 ***
# sd.brand.recall_this        7.9839269   7.1688377 -1.1137 0.2654084
# sd.brand.recognition_this   9.2572221   6.1708881 -1.5001 0.1335770
# sd.past.use_this           14.4556926   3.3843992 -4.2713 1.944e-05 ***
# sd.is_well_known            9.9201210   2.6690612 -3.7167 0.0002018 ***
# sd.is_bundle               10.7620365   2.4681484 -4.3604 1.298e-05 ***
# sd.is_premium              21.1715659   3.2813293 -6.4521 1.103e-10 ***
# sd.price                    0.6904378   0.1565343 -4.4108 1.030e-05 ***
# sd.gram                     0.0285681   0.0178267 -1.6025 0.1090352
# sd.no_choice               19.3111333   7.1377196 -2.7055 0.0068201 ** 

LL_full <- as.numeric(logLik(mxl_preference_space))
k <- length(coef(mxl_preference_space))
mcfadden_r2 <- 1 - (LL_full / LL0)
mcfadden_r2_adj <- 1 - ((LL_full - k) / LL0)

mcfadden_r2
mcfadden_r2_adj

##########################################################################
# MIXED WITH CONSUMER CHARACTERISTICS
##########################################################################
mixl_with_price_by_consumer <- gmnl(
  choice ~ 
    brand.recall_this +
    brand.recognition_this +
    past.use_this +
    is_well_known +
    is_bundle +
    is_premium +
    price +
    gram +
    no_choice |
    0 |
    0 |
    is_female +
    income_aware_enthusiasts +
    low_awareness_explorers +
    low_income_youngsters |
    0,
  data = mlogit_data,
  model="mixl",
  ranp = c(
    "price" = "n"
  ),
  mvar = list(
    price = c(
        "is_female", 
        "income_aware_enthusiasts", 
        "low_awareness_explorers", 
        "low_income_youngsters"
        )
    ),
      R = 2000
)
summary_model(mixl_with_price_by_consumer)

# Coefficients:
#                                   Estimate  Std. Error z-value  Pr(>|z|)
# brand.recall_this               0.25400863  0.16701480  1.5209  0.128291
# brand.recognition_this         -0.26885735  0.17743006 -1.5153  0.129700    
# past.use_this                   0.24453315  0.10835151  2.2569  0.024017 *
# is_well_known                   0.44040727  0.10086700  4.3662 1.264e-05 ***
# is_bundle                       1.06758551  0.09939334 10.7410 < 2.2e-16 ***
# is_premium                      0.31810529  0.08349541  3.8099  0.000139 ***
# price                          -0.05280800  0.00943281 -5.5983 2.164e-08 ***
# gram                            0.00096456  0.00049523  1.9477  0.051449 .
# no_choice                      -1.62894269  0.38757362 -4.2029 2.635e-05 ***
# price.is_female                 0.00701063  0.00884876  0.7923  0.428202
# price.income_aware_enthusiasts -0.02342291  0.01076567 -2.1757  0.029577 *
# price.low_awareness_explorers  -0.00028957  0.01546211 -0.0187  0.985058
# price.low_income_youngsters    -0.00842252  0.01170214 -0.7197  0.471684    
# sd.price                        0.02578691  0.03161683  0.8156  0.414725

# Log Likelihood: -963.61
# AIC: 1955.227
# BIC: 2021.495