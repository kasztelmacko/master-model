setwd("Modeling")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)
library(gtools)

data <- read.csv("data/final_data.csv")

data <- data %>%
  mutate(
    price_t = price / 10,
    price_p = price / 5,
    price_3 = price / 3,)

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
      market_awareness_mean = mean(market_awareness, na.rm = TRUE),
      total_recalled_mean = mean(total_recalled, na.rm = TRUE),
      total_recognized_mean = mean(total_recognized, na.rm = TRUE),
      total_price_guess_diff_mean = mean(total_price_guess_diff, na.rm = TRUE),
      across(
        c(is_female, income_high, income_low, is_graduated, city_over_500k, rural, eats_fastfood_weekly),
        list(count = ~sum(.)),
        .names = "{.col}_{.fn}"
      )
    )

  print(class_summary, n = Inf, width = Inf)
  }

summary_model <- function(model) {
  cat("Model Summary:\n")
  print(summary(model))
  cat("\nAIC:", AIC(model), "\n")
  cat("BIC:", BIC(model), "\n")
}

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)

##########################################################################
# SIMPLEST MNL PREFERENCE SPACE
##########################################################################
simplest_mnl <- gmnl(
  choice ~ 
  brand_mcdonalds +
  # brand_burger_king +
  brand_max_burger +
  brand_wendys +
  type_burger_premium +
  type_bundle_classic +
  type_bundle_premium +
  price +
  no_choice |
            0 |
            0 |
            0 |
            0,
  data = mlogit_data,
  model="mnl",
)
summary_model(simplest_mnl)

# Coefficients:
#                       Estimate Std. Error z-value Pr(>|z|)
# brand_burger_king    0.3480986  0.1414434  2.4610  0.01385 *
# brand_max_burger    -0.0110717  0.1393991 -0.0794  0.93670
# brand_wendys         0.0093749  0.1308548  0.0716  0.94289
# type_burger_premium -0.0792737  0.1977744 -0.4008  0.68855
# type_bundle_classic -0.5401654  0.2409937 -2.2414  0.02500 *
# type_bundle_premium -0.4583939  0.2450666 -1.8705  0.06142 .
# price                0.0057989  0.0117071  0.4953  0.62037
# no_choice           -1.3726249  0.2793122 -4.9143 8.91e-07 ***

# Log Likelihood: -888.09
# AIC: 1792.171
# BIC: 1828.534


##########################################################################
# SIMPLEST LC PREFERENCE SPACE
##########################################################################
simplest_lc <- gmnl(
  choice ~ 
        # brand_mcdonalds +
        brand_burger_king +
        brand_max_burger +
        brand_wendys +
        # type_burger_premium +
        # type_bundle_classic +
        # type_bundle_premium +
        # is_well_known +
        is_bundle +
        is_premium +
        price +
        no_choice |
        0 |
        0 |
        0 |
        1 +
        age +
        is_female +
        income_high + income_low +
        is_graduated +
        city_under_500k + rural +
        market_awareness +
        eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  Q = 2
)
summary_model(simplest_lc)
show_latent_class_statistics(simplest_lc)

# Coefficients:
#                               Estimate Std. Error z-value  Pr(>|z|)
# class.1.brand_mcdonalds      -0.719967   0.202555 -3.5544 0.0003788 ***
# class.1.brand_max_burger     -0.395093   0.173517 -2.2770 0.0227878 *  
# class.1.brand_wendys         -0.526176   0.190671 -2.7596 0.0057871 **
# class.1.type_burger_premium   0.094603   0.272949  0.3466 0.7288960
# class.1.type_bundle_classic   0.369453   0.325247  1.1359 0.2559925
# class.1.type_bundle_premium   0.276740   0.339851  0.8143 0.4154741
# class.1.price                -0.041202   0.017202 -2.3952 0.0166134 *
# class.1.no_choice            -2.013646   0.348610 -5.7762 7.640e-09 ***
# class.2.brand_mcdonalds       0.392001   0.290860  1.3477 0.1777443
# class.2.brand_max_burger     -0.293348   0.252030 -1.1639 0.2444487
# class.2.brand_wendys          0.203740   0.281568  0.7236 0.4693165
# class.2.type_burger_premium   0.018094   0.403232  0.0449 0.9642095
# class.2.type_bundle_classic  -2.572701   0.560502 -4.5900 4.433e-06 ***
# class.2.type_bundle_premium  -1.917077   0.490683 -3.9070 9.347e-05 ***
# class.2.price                 0.075055   0.022176  3.3845 0.0007132 ***
# class.2.no_choice           -11.189877 162.778104 -0.0687 0.9451941
# (class)2                     -1.760635   5.262525 -0.3346 0.7379563
# age:class2                    0.512172   0.220184  2.3261 0.0200125 *
# class2:is_female             -6.063969   2.169591 -2.7950 0.0051903 **
# class2:income_high           -4.026725   2.118165 -1.9010 0.0572962 .
# class2:income_low            -6.631108   3.054383 -2.1710 0.0299301 *
# class2:is_graduated          -2.991432   1.397466 -2.1406 0.0323054 *
# class2:city_under_500k        5.772667   2.052742  2.8122 0.0049208 **
# class2:rural                 -3.784642   2.061385 -1.8360 0.0663620 .
# class2:market_awareness      -0.539027   0.323224 -1.6677 0.0953835 .
# class2:eats_fastfood_weekly  -0.414662   0.976082 -0.4248 0.6709659

# Log Likelihood: -855.69
# AIC: 1763.379
# BIC: 1881.558

##########################################################################
# MIXED PREFERENCE SPACE
##########################################################################
mixl_preference_space <- gmnl(
  choice ~  
    brand.recall_this + 
    brand.recognition_this + 
    past.use_this +
    is_well_known + 
    is_bundle + 
    is_premium +
    price +
    no_choice |
    0 |
    0 |
    0 |
    0,
  data = mlogit_data,
  model = "mixl",
  ranp = c(
    price = "ln",
    is_well_known = "n",
    is_premium = "n",
    is_bundle = "n",
    brand.recall_this = "n",
    brand.recognition_this = "n",
    past.use_this = "n"
    ),
  R = 200
)
summary_model(mixl_preference_space)

# Coefficients:
#                            Estimate Std. Error z-value  Pr(>|z|)
# no_choice                 -1.530090   0.347643 -4.4013 1.076e-05 ***
# brand.recall_this          0.226826   0.205685  1.1028 0.2701205
# brand.recognition_this     0.024519   0.220534  0.1112 0.9114741
# past.use_this             -0.532269   0.187462 -2.8393 0.0045207 **
# is_well_known              0.213104   0.219809  0.9695 0.3322969    
# is_bundle                 -0.427812   0.255424 -1.6749 0.0939527 .
# is_premium                -0.152772   0.189262 -0.8072 0.4195538
# price                     -5.826718   2.947888 -1.9766 0.0480899 *
# sd.brand.recall_this       0.067315   0.793346  0.0848 0.9323810
# sd.brand.recognition_this  0.701590   0.780892  0.8984 0.3689476
# sd.past.use_this           0.020741   0.890476  0.0233 0.9814177
# sd.is_well_known           2.781812   0.807982  3.4429 0.0005755 ***
# sd.is_bundle               0.014288   0.703241  0.0203 0.9837904
# sd.is_premium              0.953580   0.554092  1.7210 0.0852550 .
# sd.price                   1.879889   1.702564  1.1042 0.2695272

# Log Likelihood: -876.79
# AIC: 1783.575
# BIC: 1851.756

##########################################################################
# MIXED WTP SPACE
##########################################################################
mixl_wtp_space <- gmnl(
  choice ~  
    brand.recall_this + 
    brand.recognition_this + 
    past.use_this +
    is_well_known + 
    is_bundle + 
    is_premium +
    no_choice |
    0 |
    0 |
    0 |
    0,
  data = mlogit_data,
  model = "mixl",
  modelType = "wtp",
  base = "price",
  ranp = c(
    is_well_known = "n",
    is_premium = "n",
    is_bundle = "n",
    brand.recall_this = "n",
    brand.recognition_this = "n",
    past.use_this = "n"
    ),
  R = 2000
)
summary_model(mixl_wtp_space)

# Coefficients:
#                             Estimate Std. Error z-value  Pr(>|z|)
# no_choice                 -1.6309638  0.3141764 -5.1912 2.089e-07 ***
# brand.recall_this          0.2211028  0.2000209  1.1054 0.2689871
# brand.recognition_this    -0.0062446  0.2099932 -0.0297 0.9762769
# past.use_this             -0.5181656  0.1809879 -2.8630 0.0041967 ** 
# is_well_known              0.2327622  0.2105415  1.1055 0.2689253
# is_bundle                 -0.3463108  0.2254323 -1.5362 0.1244874
# is_premium                -0.0491459  0.1501958 -0.3272 0.7435073
# sd.brand.recall_this       0.0379318  0.7661632  0.0495 0.9605138
# sd.brand.recognition_this  0.5729394  0.8678859  0.6602 0.5091542
# sd.past.use_this           0.0010030  0.8427460  0.0012 0.9990504
# sd.is_well_known           2.6359171  0.7318039  3.6019 0.0003158 ***
# sd.is_bundle               0.0140157  0.6824272  0.0205 0.9836142
# sd.is_premium              0.9195194  0.5275625  1.7430 0.0813409 .

# Log Likelihood: -877.18
# AIC: 1780.361
# BIC: 1839.451

##########################################################################
# LATENT CLASS PREFERENCE SPACE
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
    price:total_price_guess_diff_log +
    no_choice |
    0 |
    0 |
    0 |
    1 +
    age_log +
    is_female +
    income_high + income_low +
    is_graduated +
    city_under_500k + rural +
    market_awareness_log + 
    eats_fastfood_weekly, 
  data = mlogit_data,
  model = "lc",
  R = 2000,
  Q = 2
)
summary_model(lc_preference_space)
show_latent_class_statistics(lc_preference_space)

# Coefficients:
#                                     Estimate Std. Error z-value  Pr(>|z|)
# class.1.brand.recall_this           0.553097   0.330930  1.6713 0.0946544 .
# class.1.brand.recognition_this     -0.371013   0.375602 -0.9878 0.3232591
# class.1.past.use_this              -0.185548   0.202682 -0.9155 0.3599473
# class.1.is_well_known               1.477258   0.333142  4.4343 9.236e-06 ***
# class.1.is_bundle                  -1.603130   0.455263 -3.5213 0.0004294 ***
# class.1.is_premium                 -0.084049   0.285554 -0.2943 0.7684998
# class.1.price                      -0.197152   0.095342 -2.0678 0.0386563 *
# class.1.no_choice                  -1.215058   0.639709 -1.8994 0.0575128 .  
# class.1.price:market_awareness_log  0.087176   0.038866  2.2430 0.0248981 *
# class.2.brand.recall_this          -0.437283   0.447993 -0.9761 0.3290183
# class.2.brand.recognition_this      0.499489   0.476168  1.0490 0.2941889
# class.2.past.use_this              -0.588073   0.656721 -0.8955 0.3705364
# class.2.is_well_known              -9.165253  22.223501 -0.4124 0.6800370
# class.2.is_bundle                   9.745404  22.269086  0.4376 0.6616616
# class.2.is_premium                 -0.317281   0.370298 -0.8568 0.3915399
# class.2.price                      -0.078245   0.236680 -0.3306 0.7409498
# class.2.no_choice                   0.085738   0.795642  0.1078 0.9141868
# class.2.price:market_awareness_log  0.066586   0.100127  0.6650 0.5060368
# (class)2                           -8.268166   4.517871 -1.8301 0.0672347 .  
# age_log:class2                      2.308697   1.411192  1.6360 0.1018416
# class2:is_female                    0.188280   0.235245  0.8004 0.4235021
# class2:income_high                  0.793796   0.389168  2.0397 0.0413778 *
# class2:income_low                   0.168396   0.259722  0.6484 0.5167449
# class2:is_graduated                 0.052702   0.240223  0.2194 0.8263480
# class2:city_under_500k              0.122476   0.281095  0.4357 0.6630476
# class2:rural                        0.219487   0.388400  0.5651 0.5720025
# class2:market_awareness_low         0.045230   0.350044  0.1292 0.8971894
# class2:eats_fastfood_weekly         0.172017   0.226988  0.7578 0.4485567

# Log Likelihood: -849.83
# AIC: 1755.661
# BIC: 1882.931

##########################################################################
# LATENT CLASS WTP SPACE
##########################################################################
lc_wtp_space <- gmnl(
  choice ~ brand.recall_this + brand.recognition_this + past.use_this +
        is_well_known + 
        is_bundle + 
        is_premium +
        no_choice |
        0 |
        0 |
        0 |
        1 +
        age_log +
        is_female +
        income_high + income_low +
        is_graduated +
        city_under_500k + rural +
        # market_awareness_low +
        eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  modelType = "wtp",
  base = "price",
  Q = 2
)
summary_model(lc_wtp_space)
show_latent_class_statistics(lc_wtp_space)

# Coefficients:
#                                   Estimate  Std. Error z-value  Pr(>|z|)
# class.1.brand.recall_this       -0.3869301   0.4193628 -0.9227  0.356183
# class.1.brand.recognition_this   0.3536068   0.4458169  0.7932  0.427681
# class.1.past.use_this           -0.8014758   0.6530133 -1.2273  0.219691
# class.1.is_well_known          -13.1070359 232.6142057 -0.0563  0.955066
# class.1.is_bundle               14.0401274 232.6210971  0.0604  0.951872
# class.1.is_premium               0.0770685   0.3232742  0.2384  0.811571
# class.1.no_choice               -1.2768140   0.4780888 -2.6707  0.007570 **
# class.2.brand.recall_this        0.5776318   0.3207144  1.8011  0.071691 .
# class.2.brand.recognition_this  -0.2497714   0.3714175 -0.6725  0.501277
# class.2.past.use_this           -0.1718243   0.2020237 -0.8505  0.395038
# class.2.is_well_known            1.4169906   0.3111455  4.5541 5.261e-06 ***
# class.2.is_bundle               -1.5494765   0.3666465 -4.2261 2.378e-05 ***
# class.2.is_premium              -0.0069337   0.2300922 -0.0301  0.975960
# class.2.no_choice               -1.2430832   0.4281694 -2.9033  0.003693 **
# (class)2                         8.4160107   4.5777473  1.8385  0.065994 .
# age_log:class2                  -2.3406290   1.4310749 -1.6356  0.101929    
# class2:is_female                -0.2213945   0.2369848 -0.9342  0.350194
# class2:income_high              -0.8161560   0.3943249 -2.0698  0.038475 *
# class2:income_low               -0.2095860   0.2609173 -0.8033  0.421821
# class2:is_graduated             -0.0491590   0.2424045 -0.2028  0.839293
# class2:city_under_500k          -0.0609221   0.2832751 -0.2151  0.829718
# class2:rural                    -0.2659551   0.3869614 -0.6873  0.491899
# class2:eats_fastfood_weekly     -0.1816065   0.2298394 -0.7901  0.429443

# Log Likelihood: -857.31
# AIC: 1760.62
# BIC: 1865.163

calculate_wtp_lc <- function(model, market_awareness_level = NULL, class = NULL, alpha = 0.05) {
  # Check if model is valid
  if (!inherits(model, "gmnl")) stop("Model must be a gmnl object")
 
  # Extract all coefficients and their names
  coefs <- coef(model)
  param_names <- names(coefs)
  
  # Get standard errors and calculate p-values
  std_errors <- sqrt(diag(vcov(model)))
  z_values <- coefs / std_errors
  p_values <- 2 * (1 - pnorm(abs(z_values)))
 
  # Get number of classes from coefficient names
  n_classes <- max(as.numeric(gsub(".*class\\.(\\d+)\\..*", "\\1",
                                 grep("class\\.\\d+", param_names, value = TRUE))))
 
  # If market_awareness_level not provided, use median from data
  if (is.null(market_awareness_level)) {
    market_awareness_level <- median(model$model$market_awareness_log, na.rm = TRUE)
    message(paste("Using median market_awareness_level:", round(market_awareness_level, 3)))
  }
 
  # Initialize results
  wtp_results <- list()
 
  # Loop through classes (use specified classes or all classes)
  classes_to_calculate <- if (is.null(class)) 1:n_classes else class
 
  for (s in classes_to_calculate) {
    # Construct coefficient names for this class
    class_prefix <- paste0("class\\.", s, "\\.")
   
    # Get price coefficient
    price_coef_name <- grep(paste0("^", class_prefix, "price$"), param_names, value = TRUE)
    if (length(price_coef_name) == 0) {
      warning(paste("Price coefficient not found for class", s, "- skipping"))
      next
    }
    price_coef <- coefs[price_coef_name]
    
    # Check if price coefficient is significant
    price_significant <- p_values[price_coef_name] < alpha
    if (!price_significant) {
      warning(paste("Price coefficient not significant for class", s, "- skipping"))
      next
    }
   
    # Get price interaction coefficient
    price_interaction_name <- grep(paste0("^", class_prefix, "price:market_awareness_log$"),
                                 param_names, value = TRUE)
    price_interaction_coef <- if (length(price_interaction_name) > 0) {
      coefs[price_interaction_name]
    } else {
      0
    }
   
    # Calculate adjusted price coefficient
    adjusted_price_coef <- price_coef + price_interaction_coef * market_awareness_level
   
    # Skip if adjusted price coefficient is zero (or near zero)
    if (abs(adjusted_price_coef) < .Machine$double.eps^0.5) {
      warning(paste("Adjusted price coefficient is zero for class", s,
                   "- cannot calculate WTP"))
      next
    }
   
    # Get all non-price attributes for this class
    non_price_attrs <- grep(paste0("^", class_prefix, "(?!price|no_choice)"),
                          param_names, value = TRUE, perl = TRUE)
    
    # Filter for significant attributes only
    significant_attrs <- non_price_attrs[p_values[non_price_attrs] < alpha]
    
    if (length(significant_attrs) == 0) {
      warning(paste("No significant non-price attributes found for class", s))
      wtp_results[[paste0("Class_", s)]] <- list(
        market_awareness_level = market_awareness_level,
        adjusted_price_coef = adjusted_price_coef,
        wtp_values = numeric(0),
        n_significant_vars = 0,
        significance_level = alpha
      )
      next
    }
   
    # Calculate WTP for each significant attribute
    wtp <- sapply(significant_attrs, function(attr) {
      -coefs[attr] / adjusted_price_coef
    })
   
    # Clean up attribute names by removing class prefix
    names(wtp) <- gsub(paste0("^", class_prefix), "", names(wtp))
   
    wtp_results[[paste0("Class_", s)]] <- list(
      market_awareness_level = market_awareness_level,
      adjusted_price_coef = adjusted_price_coef,
      wtp_values = sort(wtp, decreasing = TRUE),
      n_significant_vars = length(significant_attrs),
      significance_level = alpha,
      significant_variables = gsub(paste0("^", class_prefix), "", significant_attrs)
    )
  }
 
  if (length(wtp_results) == 0) {
    warning("No WTP could be calculated for any classes")
    return(NULL)
  }
 
  return(wtp_results)
}

wtp_high_awareness <- calculate_wtp_lc(lc_preference_space, market_awareness_level = quantile(mlogit_data$market_awareness_log, 0.75))
print(wtp_high_awareness)

wtp_low_awareness <- calculate_wtp_lc(lc_preference_space, market_awareness_level = quantile(mlogit_data$market_awareness_log, 0.25))
print(wtp_low_awareness)
