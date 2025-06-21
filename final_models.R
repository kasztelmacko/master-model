setwd("MXL_model")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)
library(gtools)

data <- read.csv("data/final_data.csv")

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
    price = "n",
    is_well_known = "n",
    is_premium = "n",
    is_bundle = "n",
    brand.recall_this = "n",
    brand.recognition_this = "n",
    past.use_this = "n"
    ),
  R = 2000
)
summary_model(mixl_preference_space)

# Coefficients:
#                                Estimate      Std. Error    z-value           Pr(>|z|)
# price                         -0.0028790     0.0110968    -0.2594483         0.795    
# is_well_known                  0.1324477     0.1273240     1.0402410         0.299    
# is_bundle                     -0.2873077     0.2103598    -1.3657918         0.172
# is_premium                     0.0649254     0.1265723     0.5129514         0.608
# no_choice                     -1.7613587     0.2982387    -5.9058682         5e-09 ***
# past.use_this                 -0.2910909     0.1296052    -2.2459822         0.025 *
# brand.recall_this              0.0786449     0.1380627     0.5696316         0.569
# brand.recognition_this        -0.0550320     0.1329816    -0.4138317         0.679
# sd.price                      -0.0438321     0.0079869    -5.4879944      5.31e-08 ***
# sd.is_well_known              -0.4053042     0.2717046    -1.4917090         0.136
# sd.is_bundle                  -0.0004197    19.2653257    -0.0000218             1
# sd.is_premium                  0.0008106    13.3312685     0.0000608             1
# sd.past.use_this              -0.0011654    16.4267239    -0.0000709             1
# sd.brand.recall_this          -0.0382402     2.9890810    -0.0127933          0.99
# sd.brand.recognition_this     -0.0114388     4.9770157    -0.0022983         0.998

# Log-Likelihood= -1126.341
# AIC= 2282.683
# BIC= 2354.416
#NOTE estiamtes from pylogit 

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
#                                Estimate      Std. Error    z-value           Pr(>|z|)
# no_choice                     -1.6253799     0.2745234    -5.9207         3.205e-09 ***
# brand.recall_this              0.1559079     0.1723729     0.9045            0.3657408
# brand.recognition_this         0.0079343     0.1853271     0.0428            0.9658511
# past.use_this                 -0.4125588     0.1612882    -2.5579            0.0105307 *
# is_well_known                  0.0246521     0.1900360     0.1297            0.8967853    
# is_bundle                     -0.3875049     0.2015539    -1.9226            0.0545320 .
# is_premium                     0.0297098     0.1361615     0.2182            0.8272770
# sd.brand.recall_this           0.0039105     0.8390725     0.0047            0.9962815
# sd.brand.recognition_this      0.9979620     0.5176444     1.9279            0.0538687 .
# sd.past.use_this               0.0203283     1.3889323     0.0146            0.9883226
# sd.is_well_known               2.5021341     0.6472551     3.8658            0.0001107 ***
# sd.is_bundle                   0.0145198     0.8435697     0.0172            0.9862672
# sd.is_premium                  1.0241400     0.4746244     2.1578            0.0309441 *

# Log Likelihood: -1132.7
# AIC: 2291.467
# BIC: 2353.635

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
    price:market_awareness_log +
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
    market_awareness_low + 
    eats_fastfood_weekly, 
  data = mlogit_data,
  model = "lc",
  R = 2000,
  Q = 2
)
summary_model(lc_preference_space)
show_latent_class_statistics(lc_preference_space)

# Coefficients:
#                                      Estimate     Std. Error    z-value     Pr(>|z|)
# class.1.brand.recall_this            0.814261     0.258601      3.1487      0.0016399 **
# class.1.brand.recognition_this      -0.203469     0.291689     -0.6976      0.4854570
# class.1.past.use_this               -0.277397     0.174008     -1.5942      0.1108989
# class.1.is_well_known                0.967080     0.260095      3.7182      0.0002007 ***
# class.1.is_bundle                   -1.237689     0.379446     -3.2618      0.0011070 ** 
# class.1.is_premium                  -0.170394     0.224885     -0.7577      0.4486344
# class.1.price                       -0.133900     0.067480     -1.9843      0.0472258 *
# class.1.no_choice                   -0.590670     0.497175     -1.1881      0.2348132
# class.1.price:market_awareness_log   0.063597     0.027915      2.2782      0.0227119 *
# class.2.brand.recall_this           -1.408214     0.807444     -1.7440      0.0811524 .
# class.2.brand.recognition_this       0.370554     0.424494      0.8729      0.3827012
# class.2.past.use_this               -0.211977     0.631794     -0.3355      0.7372362
# class.2.is_well_known               -9.370381    81.052174     -0.1156      0.9079622
# class.2.is_bundle                   10.160359    81.102637      0.1253      0.9003036    
# class.2.is_premium                   0.076797     0.413761      0.1856      0.8527534
# class.2.price                       -0.137713     0.250030     -0.5508      0.5817784
# class.2.no_choice                   -1.075691     0.777712     -1.3831      0.1666194
# class.2.price:market_awareness_log   0.082142     0.113273      0.7252      0.4683505
# (class)2                           -11.135036     3.875710     -2.8730      0.0040655 **
# age_log:class2                       3.087128     1.206634      2.5585      0.0105136 *
# class2:is_female                     0.043645     0.229267      0.1904      0.8490203
# class2:income_high                   0.777961     0.346569      2.2447      0.0247844 *  
# class2:income_low                    0.366205     0.245591      1.4911      0.1359318
# class2:is_graduated                 -0.005391     0.232088     -0.0232      0.9814681
# class2:city_under_500k              -0.034675     0.293422     -0.1182      0.9059285
# class2:rural                         0.375711     0.355556      1.0567      0.2906550
# class2:market_awareness_low          0.400082     0.256336      1.5608      0.1185768
# class2:eats_fastfood_weekly          0.305554     0.234542      1.3028      0.1926525

# Log Likelihood: -1099.9
# AIC: 2255.87
# BIC: 2389.771

##########################################################################
# LATENT CLASS WTP SPACE
##########################################################################
lc_preference_space <- gmnl(
  choice ~ 
    brand.recall_this + brand.recognition_this + past.use_this +
    is_well_known + 
    is_bundle + 
    is_premium +

    brand.recall_this:city_over_500k +
    brand.recall_this:rural +

    past.use_this:market_awareness_log +
    past.use_this:city_over_500k +
    past.use_this:age_log +

    is_well_known:income_high +
    is_well_known:income_low +

    is_premium:income_high +
    no_choice |
    0 |
    0 |
    0 |
    1 +
    age_log +
    is_female +
    income_high +
    income_low +
    is_graduated +
    city_under_500k + rural +
    market_awareness_low +
    total_price_guess_diff_log +
    eats_fastfood_weekly,
  data = mlogit_data,
  model = "lc",
  R = 2000,
  Q = 2
)
summary_model(lc_preference_space)
show_latent_class_statistics(lc_preference_space)

# Coefficients:
#                                               Estimate  Std. Error z-value  Pr(>|z|)
# class.1.brand.recall_this                   -0.0584219   0.7963559 -0.0734  0.9415184
# class.1.brand.recognition_this              -0.1851423   0.3404262 -0.5439  0.5865417
# class.1.past.use_this                       44.3134076  32.1160721  1.3798  0.1676516
# class.1.is_well_known                       -4.0559822   1.2772153 -3.1756  0.0014950 **
# class.1.is_bundle                            2.6306586   1.0126949  2.5977  0.0093856 **
# class.1.is_premium                           0.5864212   0.4115249  1.4250  0.1541585
# class.1.no_choice                           -1.8544441   0.4854954 -3.8197  0.0001336 ***
# class.1.brand.recall_this:city_over_500k    -0.7167986   0.8433484 -0.8499  0.3953565
# class.1.brand.recall_this:rural             -3.1925203   2.0570442 -1.5520  0.1206637
# class.1.past.use_this:market_awareness_log  -5.2104629   3.9182496 -1.3298  0.1835863
# class.1.past.use_this:city_over_500k        -0.7470377   0.9750144 -0.7662  0.4435685
# class.1.past.use_this:age_log               -9.8489032   7.7363627 -1.2731  0.2029945
# class.1.is_well_known:income_high            3.6363203   1.3170039  2.7611  0.0057615 **
# class.1.is_well_known:income_low             1.0108665   1.7415886  0.5804  0.5616261
# class.1.is_premium:income_high              -0.0438644   0.5667439 -0.0774  0.9383076
# class.2.brand.recall_this                    1.7423801   0.5068458  3.4377  0.0005867 ***
# class.2.brand.recognition_this               0.3350679   0.2895141  1.1573  0.2471313
# class.2.past.use_this                      -18.0369899   6.4234383 -2.8080  0.0049851 **
# class.2.is_well_known                        1.1592191   0.2574094  4.5034  6.687e-06 ***
# class.2.is_bundle                           -1.0284645   0.2812309 -3.6570  0.0002552 ***
# class.2.is_premium                          -0.1411369   0.1860655 -0.7585  0.4481315
# class.2.no_choice                           -0.4536070   0.3265759 -1.3890  0.1648392
# class.2.brand.recall_this:city_over_500k    -1.2071265   0.4981976 -2.4230  0.0153935 *
# class.2.brand.recall_this:rural             -0.7844879   0.7754238 -1.0117  0.3116867
# class.2.past.use_this:market_awareness_log   2.3237301   0.8127704  2.8590  0.0042495 **
# class.2.past.use_this:city_over_500k         0.3561146   0.3919840  0.9085  0.3636180
# class.2.past.use_this:age_log                3.7292968   1.7768158  2.0989  0.0358288 *
# class.2.is_well_known:income_high            8.8362772 108.3734032  0.0815  0.9350161
# class.2.is_well_known:income_low            -0.4622107   0.3150121 -1.4673  0.1423001
# class.2.is_premium:income_high             -19.0660233 176.2419127 -0.1082  0.9138521
# (class)2                                    11.9467946   4.3951678  2.7182  0.0065645 **
# age_log:class2                              -3.4235161   1.3728744 -2.4937  0.0126425 *
# class2:is_female                            -0.2049742   0.2559342 -0.8009  0.4231973
# class2:income_high                          -2.0702958   0.4645109 -4.4569  8.314e-06 ***
# class2:income_low                           -0.1973756   0.3323732 -0.5938  0.5526210
# class2:is_graduated                         -0.0077278   0.2553042 -0.0303  0.9758526
# class2:city_under_500k                      -0.2877169   0.2982995 -0.9645  0.3347835
# class2:rural                                -0.2877310   0.4242637 -0.6782  0.4976518
# class2:market_awareness_low                 -0.6606386   0.2950779 -2.2389  0.0251649 *
# class2:total_price_guess_diff_log            0.2795420   0.1315975  2.1242  0.0336518 *
# class2:eats_fastfood_weekly                 -0.4076027   0.2592211 -1.5724  0.1158548

# Log Likelihood: -1086
# AIC: 2254.006
# BIC: 2450.076 