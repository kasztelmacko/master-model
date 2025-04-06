install.packages("remotes")
remotes::install_github("jhelvy/cbcTools")
library(cbcTools)
library(dplyr)

profiles <- cbc_profiles(
  #gram = seq(180, 420, 45),
  #kcal = seq(400, 1100, 85),
  type = c("burger_classic", "burger_premium", "bundle_classic", "bundle_premium"),
  brand = c("mcdonalds", "burger_king", "max_burger", "wendys"),
  price = seq(12, 40, 2)
)

restricted_profiles <- cbc_restrict(
  profiles,
  (type == "burger_classic" & price >= 20) |
    (type == "burger_premium" & (price <= 20 | price >= 29)) |
    (type == "bundle_classic" & (price <= 22 | price >= 30)) |
    (type == "bundle_premium" & price <= 26)
  
#  (type == "burger_classic" & gram >= 220) |
#    (type == "burger_premium" & (gram <= 200 | gram >= 400)) |
#    (type == "bundle_classic" & (gram <= 300 | gram >= 450)) |
#    (type == "bundle_premium" & gram <= 350),
  
#  (type == "burger_classic" & kcal >= 600) |
#    (type == "burger_premium" & (kcal <= 500 | kcal >= 800)) |
#    (type == "bundle_classic" & (kcal <= 700 | kcal >= 1000)) |
#    (type == "bundle_premium" & kcal <= 800)
)

dim(restricted_profiles)

design_dopt <- cbc_design(
  profiles = restricted_profiles,
  n_resp = 200,
  n_alts = 3,
  n_q = 10,
  method = 'dopt',
  no_choice = TRUE
)

design_df <- as.data.frame(design_dopt)

block_summary <- design_df %>%
  group_by(blockID) %>%
  summarise(
    unique_price = n_distinct(price),
    unique_kcal = n_distinct(kcal),
    unique_gram = n_distinct(gram),
    
    burger_classic = sum(type_burger_classic),
    burger_premium = sum(type_burger_premium),
    bundle_classic = sum(type_bundle_classic),
    bundle_premium = sum(type_bundle_premium),
    
    mcdonalds = sum(brand_mcdonalds),
    burger_king = sum(brand_burger_king),
    max_burger = sum(brand_max_burger),
    wendys = sum(brand_wendys)
  )

sim_choices <- cbc_choices(
  design = design_dopt,
  obsID  = "obsID",
  priors = list(
    price = -0.5
  )
)

write.csv(design_dopt, "data/DCE_design.csv")
write.csv(sim_choices, "data/sim_choices_DCE.csv")
