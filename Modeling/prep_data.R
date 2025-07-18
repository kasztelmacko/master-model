setwd("Modeling")
library(dplyr)
library(tidyr)
# load data
design <- read.csv("data/2/DCE_design_new.csv")
survey <- read.csv("data/2/survey_results_new.csv")

# remove unnecessary columns
survey <- survey %>%
    select(
      -id, 
      -created_at, 
      -vigniette
      )
design <- design %>%
    select(
      -block_id, 
      ) %>%
    mutate(choice = 0)

# initial data cleaning
extract_values_to_array <- function(column, output_type = "character") {
    column <- gsub('^\\["*|("*\\]$)', '', column)
    column <- gsub('""', '"', column)
    column <- gsub('\\"', '', column)
    result <- strsplit(column, ",")
    
    if (output_type == "numeric") {
        result <- lapply(result, function(x) {
            suppressWarnings(as.numeric(trimws(x)))
        })
    } else if (output_type == "character") {
        result <- lapply(result, trimws)
    } else {
        stop("output_type must be either 'character' or 'numeric'")
    }
    
    return(result)
}
survey$brand.recall <- extract_values_to_array(survey$brand.recall)
survey$brand.recognition <- extract_values_to_array(survey$brand.recognition)
survey$price.guess <- extract_values_to_array(survey$price.guess, "numeric")
survey$dce <- extract_values_to_array(survey$dce, "numeric")
survey$past.use <- extract_values_to_array(survey$past.use)

# match dce to design
for (i in 1:nrow(survey)) {
    respondent_id <- survey$respondent_id[i]
    dce_choices <- as.numeric(survey$dce[[i]])
    
    for (question_id in 1:length(dce_choices)) {
        chosen_alt <- dce_choices[question_id]
        
        row_idx <- which(
            design$respondent_id == respondent_id &
            design$question_id == question_id &
            design$alternative_id == chosen_alt
        )
        
        if (length(row_idx) == 1) {
            design$choice[row_idx] <- 1
        } else {
            warning(paste("No unique match found for respondent", respondent_id, 
                         "question", question_id, "alternative", chosen_alt))
        }
    }
}

# create _this variables
defaults <- c("mcdonalds", "burgerking", "wendys", "maxburgers", "popeyes","subway", "kfc", "pizzahut", "tacobell", "dunkindonuts", "pandaexpress", "chipotle", "fiveguys", "5guys", "panerabread", "chickfila", "northfish", "shakeshack", "bobbyburger","thaiwok", "pasibus", "papajohns", "telepizza", "dominospizza", "chuckecheese", "saladstory", "innout", "kebabking", "amirkebab", "matsuya", "mosburger", "dagrasso", "raisingcanes", "dairyqueen", "jollibee", "sonic", "arbys", "whataburger", "whitecastle", "littleceaser", "dodopizza", "zahirkebab", "wingstop", "donerkebab", "hesburger", "otacos", "hooters", "wrapme", "pizzadagrasso", "applebees", "silverdragon", "loteria", "wkusnoitoczka")
brand_mapping <- list(
    "mcdonalds" = "brand_mcdonalds",
    "burgerking" = "brand_burger_king",
    "maxburgers" = "brand_max_burger",
    "wendys" = "brand_wendys"
)
clean_brand_names <- function(brand_vec) {
  brand_vec <- lapply(brand_vec, tolower)
  brand_vec <- lapply(brand_vec, function(x) {
    x <- gsub("[^[:alnum:] ]", "", x)
    x <- gsub("\\s+", "", x)
    x <- trimws(x)
    return(x)
  })
  return(brand_vec)
}
add_brand_indicators <- function(design, survey, survey_column_name) {
    new_col_name <- paste0(survey_column_name, "_this")
    design[[new_col_name]] <- 0
    
    for (i in 1:nrow(survey)) {
        respondent_id <- survey$respondent[i]
        brand_list <- tolower(survey[[survey_column_name]][[i]])
        
        for (survey_brand in names(brand_mapping)) {
            design_brand <- brand_mapping[[survey_brand]]
            
            if (survey_brand %in% brand_list) {
                rows_to_update <- which(
                    design$respondent_id == respondent_id & 
                    design[[design_brand]] == 1
                )
                
                if (length(rows_to_update) > 0) {
                    design[rows_to_update, new_col_name] <- 1
                }
            }
        }
    }
    return(design)
}

process_brand_column <- function(data, column_name, defaults) {
  cat("\nProcessing column:", column_name, "\n")
  data[[column_name]] <- clean_brand_names(data[[column_name]])
  all_mentions <- unique(unlist(data[[column_name]]))
  non_defaults <- setdiff(all_mentions, defaults)
  
  if (length(non_defaults) > 0) {
    cat("Found non-standard brand names in", column_name, ":\n")
    print(non_defaults)
    
    correction_map <- list()
    brands_to_remove <- character()
    brands_to_split <- list()
    
    for (brand in non_defaults) {
      cat("\nBrand:", brand, "\n")
      cat("Options:\n")
      cat("1. Map to existing brand\n")
      cat("2. Rename\n")
      cat("3. Ignore and REMOVE from data\n")
      cat("4. Handle as concatenated brands (special case)\n")
      
      choice <- readline(prompt = "Enter choice (1-4): ")
      
      if (choice == "1") {
        cat("Existing brands:\n")
        print(defaults)
        mapped <- readline(prompt = "Map to which existing brand? ")
        if (mapped %in% defaults) {
          correction_map[[brand]] <- mapped
          cat("Mapped", brand, "->", mapped, "\n")
        } else {
          cat("Invalid brand, skipping\n")
        }
      } else if (choice == "2") {
        new_name <- readline(prompt = "New name: ")
        correction_map[[brand]] <- new_name
        defaults <- c(defaults, new_name)
        cat("Renamed", brand, "->", new_name, "\n")
      } else if (choice == "3") {
        brands_to_remove <- c(brands_to_remove, brand)
        cat("Will remove all occurrences of", brand, "\n")
      } else if (choice == "4") {
        cat("Current concatenated brand:", brand, "\n")
        selected <- readline(prompt = "Enter the separate brands (comma separated): ")
        selected_brands <- unlist(strsplit(selected, ",\\s*"))
        selected_brands <- trimws(tolower(selected_brands))
        selected_brands <- gsub("[^[:alnum:]]", "", selected_brands)
        
        if (length(selected_brands) > 0) {
          brands_to_split[[brand]] <- selected_brands
          cat("Will split", brand, "into:", paste(selected_brands, collapse = ", "), "\n")
        } else {
          cat("No brands entered, treating as remove\n")
          brands_to_remove <- c(brands_to_remove, brand)
        }
      }
    }
    if (length(correction_map) > 0) {
      data[[column_name]] <- lapply(data[[column_name]], function(brands) {
        sapply(brands, function(b) {
          ifelse(b %in% names(correction_map), correction_map[[b]], b)
        })
      })
    }
    if (length(brands_to_split) > 0) {
      data[[column_name]] <- lapply(data[[column_name]], function(brands) {
        new_brands <- character(0)
        for (b in brands) {
          if (b %in% names(brands_to_split)) {
            new_brands <- c(new_brands, brands_to_split[[b]])
          } else if (!b %in% brands_to_remove) {
            new_brands <- c(new_brands, b)
          }
        }
        new_brands
      })
    }
    
    if (length(brands_to_remove) > 0) {
      data[[column_name]] <- lapply(data[[column_name]], function(brands) {
        brands[!brands %in% brands_to_remove]
      })
    }
  }
  
  data[[column_name]] <- clean_brand_names(data[[column_name]])
  data[[column_name]] <- lapply(data[[column_name]], unique)
  return(list(data = data, defaults = defaults))
}

result_recall <- process_brand_column(survey, "brand.recall", defaults)
survey <- result_recall$data
defaults <- result_recall$defaults

result_recognition <- process_brand_column(survey, "brand.recognition", defaults)
survey <- result_recognition$data
defaults <- result_recognition$defaults

# create cleaned brand lists csv file
brand_lists_cleaned <- survey %>%
  select(respondent_id, brand.recall, brand.recognition) %>%
  mutate(
    brand.recall = sapply(brand.recall, function(x) paste(x, collapse = "; ")),
    brand.recognition = sapply(brand.recognition, function(x) paste(x, collapse = "; "))
  )
write.csv(brand_lists_cleaned, "data/2/brand_lists_cleaned.csv", row.names = FALSE)

# create brand_recall and brand_recognition csv files
brand_recall_df <- survey %>%
  select(brand.recall) %>%
  unnest(brand.recall) %>%
  group_by(brand.recall) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

write.csv(brand_recall_df, "data/2/brand_recall.csv", row.names = FALSE)

brand_recognition_df <- survey %>%
  select(brand.recognition) %>%
  unnest(brand.recognition) %>%
  group_by(brand.recognition) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

write.csv(brand_recognition_df, "data/2/brand_recognition.csv", row.names = FALSE)

# brand_lists_cleaned <- read.csv("data/brand_lists_cleaned.csv")
# design <- design %>%
#     left_join(brand_lists_cleaned, by = "respondent_id")

design <- add_brand_indicators(design, survey, "past.use")
design <- add_brand_indicators(design, survey, "brand.recall")
design <- add_brand_indicators(design, survey, "brand.recognition")

design <- design %>%
    mutate(
      is_well_known = ifelse(brand_mcdonalds == 1 | brand_burger_king == 1, 1,
                        ifelse(brand_max_burger == 1 | brand_wendys == 1, 0, 0)),
      is_bundle = ifelse(type_bundle_classic == 1 | type_bundle_premium == 1, 1, 0),
      is_premium = ifelse(type_burger_premium == 1 | type_bundle_premium == 1, 1, 0)

    )
# create market awareness variable
real_prices = list(
    22.9,  # WieśMac
    25.99, # Whopper
    32.8  # McZestaw McRoyal
)

survey <- survey %>%
  mutate(
    total_recalled = sapply(brand.recall, function(x) length(x)),
    total_recognized = sapply(brand.recognition, function(x) length(x)),
    avg_price_guess_diff = sapply(price.guess, function(x) {
      guess <- unlist(x)
      real <- unlist(real_prices)
      mean(abs(guess - real))
    }),
  )

design <- design %>%
  mutate(
    respondent_question_id = paste0(respondent_id, "_", question_id)
  )

# create final model dataframe
design_cols <- c(
  "respondent_id",
  "question_id",
  "respondent_question_id",
  "alternative_id",
  "choice",
  "price",
  "gram",
  "kcal",
  "type_burger_classic", 
  "type_burger_premium",
  "type_bundle_classic",
  "type_bundle_premium",
  "brand_mcdonalds",
  "brand_burger_king",
  "brand_max_burger",
  "brand_wendys",
  "is_well_known",
  "is_bundle",
  "is_premium",
  "no_choice",
  "past.use_this",
  "brand.recall_this",
  "brand.recognition_this"
)

survey_cols <- c(
  "respondent_id",
  "age",
  "gender",
  "income",
  "location",
  "fast.food.frequency",
  "education",
  "total_recalled",
  "total_recognized",
  "avg_price_guess_diff"
)

model_data <- design %>%
  select(all_of(design_cols)) %>%
  inner_join(
    survey %>% select(all_of(survey_cols)),
    by = "respondent_id"
  )
str(model_data)

# save data
write.csv(model_data, "data/2/clean_data.csv", row.names = FALSE)
