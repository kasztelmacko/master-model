setwd("MXL_model")
library(dplyr)
# load data
design <- read.csv("data/DCE_design.csv")
survey <- read.csv("data/SurveyResults_rows.csv")

# remove unnecessary columns
survey <- survey %>%
    select(-id, -created_at, -vigniette)
design <- design %>%
    select(-id, -profile_id, -block_id, observation_id) %>%
    mutate(choice = 0)

# initial data cleaning
extract_values_to_array <- function(column) {
    column <- gsub("^\\[|\\]$", "", column)
    column <- gsub('\\"', "", column)
    strsplit(column, ",")
}
survey$brand.recall <- extract_values_to_array(survey$brand.recall)
survey$brand.recognition <- extract_values_to_array(survey$brand.recognition)
survey$price.guess <- extract_values_to_array(survey$price.guess)
survey$dce <- extract_values_to_array(survey$dce)
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

