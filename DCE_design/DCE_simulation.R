setwd("DCE_design")
#install.packages("remotes")
#remotes::install_github("jhelvy/cbcTools")
library(cbcTools)
library(dplyr)
library(gmnl)
library(mlogit)

design <- read.csv("data/DCE_design_full.csv", stringsAsFactors = FALSE)
set.seed(123)

simulation <- cbc_choices(
    design = design,
    obsID = "observation_id",
    priors = list(
        price = randN(mean = -0.5, sd = 0.3),
        type_burger_classic = 0.3,
        type_burger_premium = 0.2,
        type_bundle_classic = 0.6,
        type_bundle_premium = 0.4,
        brand_mcdonalds = randN(mean = 0.5, sd = 0.2),
        brand_burger_king = randN(mean = 0.3, sd = 0.2),
        brand_max_burger = randN(mean = 0.2, sd = 0.2),
        brand_wendys = randN(mean = 0.1, sd = 0.2),
        no_choice = -0.8
    )
)

simulation <- simulation %>%
  mutate(
    respondent_question_id = paste0(respondent_id, "_", question_id),
    is_well_known = ifelse(brand_mcdonalds == 1 | brand_burger_king == 1, 1,
                        ifelse(brand_max_burger == 1 | brand_wendys == 1, 0, 0)),
    is_bundle = ifelse(type_bundle_classic == 1 | type_bundle_premium == 1, 1, 0),
    is_premium = ifelse(type_burger_premium == 1 | type_bundle_premium == 1, 1, 0)
  )

mlogit_data <- mlogit.data(
  simulation,
  choice = "choice",
  shape = "long",
  alt.var = "alternative_id",
  chid.var = "respondent_question_id",
  id.var = "respondent_id"
)
simplest_mnl <- gmnl(
    choice ~ 
    brand_burger_king +
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
summary(simplest_mnl)

# Coefficients:
#                      Estimate Std. Error z-value  Pr(>|z|)
# brand_burger_king   -0.278535   0.200580 -1.3886  0.164941
# brand_max_burger    -0.619013   0.220396 -2.8086  0.004975 ** 
# brand_wendys        -0.305787   0.201642 -1.5165  0.129397
# type_burger_premium  0.222998   0.408089  0.5464  0.584760
# type_bundle_classic  0.322515   0.363739  0.8867  0.375259
# type_bundle_premium  0.709796   0.596894  1.1892  0.234381
# price               -0.056316   0.033337 -1.6893  0.091165 .
# no_choice            2.033531   0.504724  4.0290 5.602e-05 ***

simplest_mixl <- gmnl(
    choice ~ 
    brand_burger_king +
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
    model="mixl",
    R = 200,
    ranp = c(
        price = "n",
        type_burger_premium = "n",
        type_bundle_classic = "n",
        type_bundle_premium = "n",
        brand_burger_king = "n",
        brand_max_burger = "n",
        brand_wendys = "n"
    )
)
summary(simplest_mixl)

# Coefficients:
#                         Estimate Std. Error z-value Pr(>|z|)
# no_choice               1.024138   1.245930  0.8220  0.41108  
# brand_burger_king      -1.070968   1.069227 -1.0016  0.31652
# brand_max_burger       -1.637801   1.417226 -1.1556  0.24783
# brand_wendys           -0.509899   0.599995 -0.8498  0.39542
# type_burger_premium     0.136468   0.828096  0.1648  0.86910
# type_bundle_classic    -2.158676   1.996884 -1.0810  0.27969
# type_bundle_premium    -1.790865   2.585470 -0.6927  0.48852
# price                  -0.174862   0.144688 -1.2085  0.22684
# sd.brand_burger_king    1.599043   1.214170  1.3170  0.18784
# sd.brand_max_burger     1.719156   1.424924  1.2065  0.22763
# sd.brand_wendys         0.631113   1.249491  0.5051  0.61349  
# sd.type_burger_premium  0.730878   1.562946  0.4676  0.64005
# sd.type_bundle_classic  3.461304   1.804026  1.9187  0.05503 .
# sd.type_bundle_premium  3.427291   2.365406  1.4489  0.14736
# sd.price                0.098401   0.078634  1.2514  0.21080

processed_mnl <- gmnl(
    choice ~ 
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
    model = "mnl"
)
summary(processed_mnl)

# Coefficients:
#                Estimate Std. Error z-value  Pr(>|z|)
# is_well_known  0.319217   0.151586  2.1058   0.03522 *
# is_bundle      0.391093   0.322559  1.2125   0.22533
# is_premium     0.326395   0.320278  1.0191   0.30816
# price         -0.060187   0.031773 -1.8943   0.05818 .
# no_choice      2.443364   0.486803  5.0192 5.189e-07 ***

processed_mixl <- gmnl(
    choice ~ 
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
    R = 500,
    ranp = c(
        price = "n",
        is_well_known = "n",
        is_bundle = "n",
        is_premium = "n"
    )
)
summary(processed_mixl)

# Coefficients:
#                   Estimate Std. Error z-value Pr(>|z|)
# no_choice        -1.072350   1.867549 -0.5742  0.56583
# is_well_known     0.448371   0.218587  2.0512  0.04024 *
# is_bundle         0.010008   0.951266  0.0105  0.99161
# is_premium        0.449562   0.508004  0.8850  0.37618
# price            -0.539177   0.319017 -1.6901  0.09100 .
# sd.is_well_known  0.152130   0.943871  0.1612  0.87195
# sd.is_bundle      2.676682   1.545432  1.7320  0.08327 .
# sd.is_premium     0.046175   1.072358  0.0431  0.96565
# sd.price          0.285164   0.172891  1.6494  0.09907 .
