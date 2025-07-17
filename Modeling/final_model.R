setwd("Modeling")
library(dplyr)
library(mlogit)
library(car)
library(gmnl)
library(ggplot2)
library(cluster)
library(gtools)

data <- read.csv("data/2/clean_data.csv")

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
# SIMPLEST MNL PREFERENCE SPACE
##########################################################################
simplest_mnl <- gmnl(
  choice ~ 
  brand_burger_king +
  brand_max_burger +
  brand_wendys +
  type_burger_premium +
  type_bundle_classic +
  type_bundle_premium +
  price +
  kcal +
  gram +
  no_choice |
            0 |
            0 |
            0 |
            0,
  data = mlogit_data,
  model="mnl",
  # modelType = "wtp",
  # base = "price",
)
summary_model(simplest_mnl)

# Coefficients:
#                             Estimate Std. Error z-value Pr(>|z|)
# brand_burger_king           0.325940   0.127383  2.5587 0.010505 *  
# brand_max_burger            0.085357   0.123722  0.6899 0.490250
# brand_wendys                0.117147   0.116091  1.0091 0.312929
# type_burger_premium         0.076455   0.172539  0.4431 0.657680
# type_bundle_classic        -0.379524   0.213107 -1.7809 0.074927 .
# type_bundle_premium        -0.296690   0.213464 -1.3899 0.164565
# price                      -0.116510   0.043432 -2.6826 0.007305 **
# no_choice                  -1.210851   0.241698 -5.0098 5.45e-07 ***
# price:market_awareness_log  0.050493   0.018306  2.7584 0.005809 **

# Log Likelihood: -1140.3
# AIC: 2298.677
# BIC: 2341.717

simplest_mnl <- gmnl(
  choice ~ 
  brand.recall_this +
  brand.recognition_this +
  past.use_this +
  is_well_known +
  is_bundle +
  is_premium +
  price +
  kcal +
  gram +
  no_choice |
            0 |
            0 |
            0 |
            0,
  data = mlogit_data,
  model="mnl",
)
summary_model(simplest_mnl)
wtp.gmnl(simplest_mnl, wrt = "price")

##########################################################################
# SIMPLEST LC PREFERENCE SPACE
##########################################################################
simplest_lc <- gmnl(
  choice ~ 
        brand_mcdonalds +
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
wtp.gmnl(simplest_lc, wrt = "price")
show_latent_class_statistics(simplest_lc)

# Coefficients:
#                                Estimate  Std. Error z-value  Pr(>|z|)    
# class.1.brand_mcdonalds       -1.174289    0.407291 -2.8832 0.0039370 **
# class.1.brand_max_burger       0.691221    0.237950  2.9049 0.0036737 **
# class.1.brand_wendys           0.691913    0.267914  2.5826 0.0098062 **
# class.1.type_burger_premium    1.170276    0.321248  3.6429 0.0002696 ***
# class.1.type_bundle_classic    1.451155    0.402195  3.6081 0.0003085 ***
# class.1.type_bundle_premium    1.078583    0.394882  2.7314 0.0063064 **
# class.1.price                 -0.054349    0.016885 -3.2188 0.0012873 **
# class.1.no_choice             -0.878327    0.379755 -2.3129 0.0207295 *
# class.2.brand_mcdonalds       29.105508  719.442981  0.0405 0.9677299
# class.2.brand_max_burger     -35.859604  839.354843 -0.0427 0.9659225
# class.2.brand_wendys         -33.116116  839.349426 -0.0395 0.9685280    
# class.2.type_burger_premium  -43.061465 1079.166006 -0.0399 0.9681708
# class.2.type_bundle_classic -146.497304 3597.220139 -0.0407 0.9675150
# class.2.type_bundle_premium -101.681644 2518.050977 -0.0404 0.9677893
# class.2.price                  4.746469  119.907324  0.0396 0.9684244
# class.2.no_choice            -17.490418  124.639388 -0.1403 0.8884007
# (class)2                      -0.560927    1.368200 -0.4100 0.6818247
# age:class2                    -0.032194    0.047232 -0.6816 0.4954755
# class2:is_female              -0.631141    0.225014 -2.8049 0.0050332 **
# class2:income_high            -0.529598    0.386468 -1.3704 0.1705756
# class2:income_low             -0.469833    0.248185 -1.8931 0.0583481 .  
# class2:is_graduated           -0.191501    0.227269 -0.8426 0.3994405
# class2:city_under_500k         0.389639    0.277117  1.4060 0.1597108
# class2:rural                  -0.461848    0.404031 -1.1431 0.2529970
# class2:market_awareness        0.123643    0.051625  2.3950 0.0166184 *
# class2:eats_fastfood_weekly   -0.141232    0.236695 -0.5967 0.5507200

# Log Likelihood: -1098.5
# AIC: 2248.961
# BIC: 2373.298

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
    kcal +
    gram +
    no_choice |
    0 |
    0 |
    0 |
    0,
  data = mlogit_data,
  model = "mixl",
  ranp = c(
    # price = "n",
    # kcal = "n",
    # gram = "n",
    is_well_known = "n",
    is_premium = "n",
    is_bundle = "n",
    brand.recall_this = "n",
    brand.recognition_this = "n",
    past.use_this = "n"
    ),
  R = 500
)
summary_model(mixl_preference_space)

# Coefficients:
#                               Estimate  Std. Error z-value  Pr(>|z|)    
# no_choice                  -2.0686e+00  5.9048e-01 -3.5032 0.0004597 ***
# price:market_awareness_log  9.5909e-02  3.4166e-02  2.8071 0.0049984 **
# brand.recall_this           1.2340e-01  1.8715e-01  0.6594 0.5096635
# brand.recognition_this     -1.5755e-02  2.1857e-01 -0.0721 0.9425359
# past.use_this              -4.2956e-01  1.7185e-01 -2.4997 0.0124304 *
# is_well_known               3.5199e-02  2.0642e-01  0.1705 0.8646038
# is_bundle                  -4.0982e-01  2.7892e-01 -1.4693 0.1417571
# is_premium                 -4.0263e-03  1.7597e-01 -0.0229 0.9817457
# price                      -2.1708e-01  7.9795e-02 -2.7205 0.0065182 **
# sd.brand.recall_this        4.2652e-02  1.6774e+00  0.0254 0.9797141    
# sd.brand.recognition_this   1.7345e+00  5.8035e-01  2.9887 0.0028021 **
# sd.past.use_this            7.5261e-05  1.9653e+00  0.0000 0.9999694
# sd.is_well_known            2.7449e+00  7.5456e-01  3.6377 0.0002751 ***
# sd.is_bundle                4.0356e-02  9.3907e-01  0.0430 0.9657221
# sd.is_premium               1.1447e+00  5.6001e-01  2.0441 0.0409422 *
# sd.price                    2.3986e-02  4.2774e-02  0.5608 0.5749598

# Log Likelihood: -1127.1
# AIC: 2286.158
# BIC: 2362.673

# albo bez efektu na price

# Coefficients:
#                              Estimate Std. Error z-value  Pr(>|z|)    
# brand.recall_this           0.1234268  0.1850924  0.6668  0.504875
# brand.recognition_this     -0.0145174  0.2149008 -0.0676  0.946141
# past.use_this              -0.4228207  0.1722180 -2.4551  0.014083 *
# is_well_known               0.0354841  0.2034595  0.1744  0.861548
# is_bundle                  -0.4400048  0.2569197 -1.7126  0.086783 .
# is_premium                  0.0022607  0.1729693  0.0131  0.989572
# price                      -0.2096424  0.0728799 -2.8765  0.004021 **
# price:market_awareness_log  0.0929948  0.0314966  2.9525  0.003152 **
# no_choice                  -1.9438352  0.3945558 -4.9266 8.365e-07 ***
# sd.brand.recall_this        0.0175146  1.5651920  0.0112  0.991072
# sd.brand.recognition_this   1.6760986  0.5261729  3.1855  0.001445 ** 
# sd.past.use_this            0.0561051  1.9108417  0.0294  0.976576
# sd.is_well_known            2.6934220  0.7149279  3.7674  0.000165 ***
# sd.is_bundle                0.0102934  1.1207557  0.0092  0.992672
# sd.is_premium               1.1753202  0.5195738  2.2621  0.023692 *

# Log Likelihood: -1127.1
# AIC: 2284.285
# BIC: 2356.018

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
# no_choice                     -1.6253799     0.2745234    -5.9207            3.205e-09 ***
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
#                                      Estimate Std. Error z-value  Pr(>|z|)
# class.1.brand.recall_this            0.765091   0.265512  2.8816 0.0039571 **
# class.1.brand.recognition_this      -0.249849   0.296199 -0.8435 0.3989382
# class.1.past.use_this               -0.240961   0.175856 -1.3702 0.1706199
# class.1.is_well_known                1.052598   0.255209  4.1245 3.716e-05 ***
# class.1.is_bundle                   -1.343627   0.375318 -3.5800 0.0003436 ***
# class.1.is_premium                  -0.149963   0.231109 -0.6489 0.5164127
# class.1.price                       -0.140715   0.068225 -2.0625 0.0391594 *
# class.1.price:market_awareness_log   0.067725   0.028068  2.4129 0.0158275 *  
# class.1.no_choice                   -0.608523   0.518665 -1.1732 0.2406956
# class.2.brand.recall_this           -1.110934   0.558724 -1.9883 0.0467739 *
# class.2.brand.recognition_this       0.354294   0.406974  0.8706 0.3839967
# class.2.past.use_this               -0.274432   0.607042 -0.4521 0.6512108
# class.2.is_well_known               -7.628400  21.854637 -0.3491 0.7270505
# class.2.is_bundle                    8.360527  21.939569  0.3811 0.7031508
# class.2.is_premium                   0.044679   0.390452  0.1144 0.9088976
# class.2.price                       -0.070208   0.200358 -0.3504 0.7260268
# class.2.price:market_awareness_log   0.050385   0.088605  0.5686 0.5695968
# class.2.no_choice                   -0.972787   0.714421 -1.3616 0.1733099
# (class)2                           -11.251066   3.900388 -2.8846 0.0039191 **
# age_log:class2                       3.215175   1.213515  2.6495 0.0080617 **
# class2:is_female                     0.062317   0.225887  0.2759 0.7826434    
# class2:income_high                   0.815672   0.342169  2.3838 0.0171335 *
# class2:income_low                    0.382459   0.238626  1.6028 0.1089887
# class2:is_graduated                 -0.025375   0.225491 -0.1125 0.9104023
# class2:city_under_500k              -0.108116   0.282841 -0.3823 0.7022757
# class2:rural                         0.441747   0.348626  1.2671 0.2051158
# class2:market_awareness_low          0.161842   0.256815  0.6302 0.5285702
# class2:eats_fastfood_weekly          0.215959   0.220022  0.9815 0.3263291

# Log Likelihood: -1101.1
# AIC: 2258.219 
# BIC: 2392.12

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
# class.1.brand.recall_this      -1.0474e+00  4.8895e-01 -2.1421  0.032186 *
# class.1.brand.recognition_this  2.3043e-01  4.0413e-01  0.5702  0.568544
# class.1.past.use_this          -3.5785e-01  5.9828e-01 -0.5981  0.549758
# class.1.is_well_known          -4.2457e+01  2.3498e+04 -0.0018  0.998558
# class.1.is_bundle               4.3232e+01  2.3498e+04  0.0018  0.998532
# class.1.is_premium              3.1615e-01  3.4958e-01  0.9044  0.365800
# class.1.no_choice              -1.7258e+00  5.2205e-01 -3.3058  0.000947 ***
# class.2.brand.recall_this       7.4850e-01  2.5529e-01  2.9320  0.003368 **
# class.2.brand.recognition_this -1.2742e-01  2.8886e-01 -0.4411  0.659136
# class.2.past.use_this          -2.6329e-01  1.7471e-01 -1.5070  0.131811    
# class.2.is_well_known           1.0262e+00  2.3789e-01  4.3138 1.605e-05 ***
# class.2.is_bundle              -1.1808e+00  2.8992e-01 -4.0729 4.644e-05 ***
# class.2.is_premium             -4.5436e-02  1.8247e-01 -0.2490  0.803356
# class.2.no_choice              -7.7967e-01  3.2938e-01 -2.3671  0.017928 *
# (class)2                        1.2047e+01  3.8781e+00  3.1063  0.001894 **
# age_log:class2                 -3.4585e+00  1.2018e+00 -2.8778  0.004005 **
# class2:is_female               -7.6624e-02  2.2948e-01 -0.3339  0.738452
# class2:income_high             -8.3667e-01  3.4689e-01 -2.4119  0.015869 *
# class2:income_low              -4.1307e-01  2.4299e-01 -1.7000  0.089138 .
# class2:is_graduated             4.2124e-02  2.2962e-01  0.1835  0.854443
# class2:city_under_500k          1.4235e-01  2.8466e-01  0.5001  0.617022
# class2:rural                   -4.8125e-01  3.5381e-01 -1.3602  0.173764    
# class2:eats_fastfood_weekly    -2.2860e-01  2.2351e-01 -1.0228  0.306425

# Log Likelihood: -1104.3
# AIC: 2256.628
# BIC: 2371.401