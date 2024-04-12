## read in survey results spreadsheet
read_survey <- function(x) {
  col_names <- readr::read_csv(x, n_max = 0) |> 
    names()
  
  readr::read_csv(x,
                  col_names = col_names,
                  skip = 3,
                  show_col_types = FALSE) |> 
    janitor::clean_names()
  
}




munge_survey_results <- function(x) {
  df <- tibble(SEX = factor(as.character(x$q2),
                            levels = as.character(1:4),
                            labels = c("Male", "Female", "Other", "No answer")),
               AGEP = factor(as.character(x$q3),
                             levels = as.character(1:7),
                             labels = c("18:24", "25:34", "35:44", "45:54",
                                        "55:64", "65+", "No answer")),
               RACE5 = as.character(x$q4),
               
               HOUSING = factor(x$q22,
                                levels = as.character(1:8),
                                labels = c(
                                  "Single family house",
                                  "Townhouse/duplex",
                                  "Apartment building, 4 or fewer units",
                                  "Apartment building, more than 4 units",
                                  "Manufactured/mobile home",
                                  "Institutional housing or dorm",
                                  "Other",
                                  "Prefer not to answer"
                                )),
               HOWNSHP = factor(x$q23,
                                levels = as.character(1:3),
                                labels = c(
                                  "Own",
                                  "Rent",
                                  "Prefer not to answer"
                                )),
               INCOME = factor(x$q26,
                               levels = as.character(1:7),
                               labels = c(
                                 "< $25,000",
                                 "$25,000 - $49,999",
                                 "$50,000 - $74,999",
                                 "$75,000 - $99,999",
                                 "$100,000 - $200,000",
                                 "> $200,000",
                                 "Prefer not to answer"
                               )),
               HHSIZE = x$q27,
               
               SCHL = factor(as.character(x$q25),
                             levels = as.character(1:8),
                             labels = c("Some high school",
                                        "High school graduate or GED",
                                        "Associate degree",
                                        "Bachelor's degree",
                                        "Master's degree",
                                        "Doctorate or terminal degree",
                                        "Other",
                                        "No answer"))) |> 
    
    mutate(RACE5 = case_when(
      ## create new lvl for two ro more races response
      !(RACE5 %in% c("1","2","3","4","5","6","7","8")) ~ "9",
      .default = as.character(RACE5)
    )) |> 
    mutate(RACE5 = factor(RACE5,
                          levels = as.character(1:9),
                          labels = c(
                            "American Indian/Native American or Alaska Native",
                            "Asian",
                            "Hispanic or Latino or Spanish Origin of any race",
                            "Black or African American",
                            "Native Hawaiian or Other Pacific Islander",
                            "White or Caucasian",
                            "Other",
                            "No answer",
                            "Two or More"))) |> 
    mutate(RACE2 = case_when(
      RACE5 %in% c("American Indian/Native American or Alaska Native",
                   "Asian",
                   "Hispanic or Latino or Spanish Origin of any race",
                   "Black or African American",
                   "Native Hawaiian or Other Pacific Islander",
                   "Other",
                   "Two or More") ~ "2", ## Non-white
      RACE5 == "White or Caucasian" ~ "1", ## White
      RACE5 == "No answer" ~ "3" ## No Answer
    )) |> 
    mutate(RACE2 = factor(RACE2,
                          levels = as.character(1:3),
                          labels = c("White", "Non-white", "No answer"))) |> 
    mutate(Q5 = factor(as.character(x$q5),
                       levels = as.character(1:6),
                       labels = c(
                         "Public supply - municipal",
                         "Public supply - rural water district",
                         "Private supply - well, river, pond",
                         "Private supply - rainwater harvest system",
                         "I don't know",
                         "Other"
                       ))) |> 
    mutate(Q6 = factor(x$q6,
                       levels = c(1,6,2,3),
                       labels = c("Unfiltered tap water",
                                  "Filtered tap water",
                                  "Bottled/prepackaged water",
                                  "Other"))) |>
    mutate(Q7 = case_when(
      x$q7 == 11 ~ FALSE,
      .default = TRUE)) |> 
    mutate(Q7_1 = factor(x$q7_1,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_2 = factor(x$q7_2,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_3 = factor(x$q7_3,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_4 = factor(x$q7_4,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_5 = factor(x$q7_5,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_6 = factor(x$q7_6,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_7 = factor(x$q7_7,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_8 = factor(x$q7_8,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_9 = factor(x$q7_9,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_10 = factor(x$q7_10,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_11 = factor(x$q7_12,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q7_12 = factor(x$q7_12,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q8 = as.integer(x$q8_2)) |> 
    mutate(Q9 = case_when(
      x$q9 == 11 ~ FALSE,
      .default = TRUE
    )) |> 
    mutate(Q9_1 = factor(x$q9_1,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_2 = factor(x$q9_2,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_3 = factor(x$q9_3,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_4 = factor(x$q9_4,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_5 = factor(x$q9_5,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_6 = factor(x$q9_6,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_7 = factor(x$q9_7,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_8 = factor(x$q9_8,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_9 = factor(x$q9_9,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) |> 
    mutate(Q9_10 = factor(x$q9_10,
                          levels = c(0,1),
                          labels = c("No", "Yes"))) |> 
    mutate(Q9_11 = factor(x$q9_11,
                          levels = c(0,1),
                          labels = c("No", "Yes"))) |> 
    mutate(Q9_12 = factor(x$q9_12,
                          levels = c(0,1),
                          labels = c("No", "Yes"))) |> 
    mutate(Q10_1 = factor(x$q10_1,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_2 = factor(x$q10_2,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_3 = factor(x$q10_3,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_4 = factor(x$q10_4,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_5 = factor(x$q10_5,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_6 = factor(x$q10_6,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_7 = factor(x$q10_7,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_8 = factor(x$q10_8,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_9 = factor(x$q10_9,
                          levels = as.character(1:5),
                          labels = c(
                            "Not at all familiar",
                            "Slightly familiar",
                            "Moderately familiar",
                            "Very familiar",
                            "Extremely familiar"
                          ))) |> 
    mutate(Q10_10 = factor(x$q10_10,
                           levels = as.character(1:5),
                           labels = c(
                             "Not at all familiar",
                             "Slightly familiar",
                             "Moderately familiar",
                             "Very familiar",
                             "Extremely familiar"
                           ))) |> 
    mutate(Q11_1 = factor(x$q11_1,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_2 = factor(x$q11_2,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_3 = factor(x$q11_3,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_4 = factor(x$q11_4,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_5 = factor(x$q11_5,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_6 = factor(x$q11_6,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_7 = factor(x$q11_7,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_8 = factor(x$q11_8,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_9 = factor(x$q11_9,
                          levels = as.character(1:5),
                          labels = c(
                            "Not as all concerned",
                            "Slightly concerned",
                            "Moderately concerned",
                            "Very concerned",
                            "Extremely concerned"
                          ))) |> 
    mutate(Q11_10 = factor(x$q11_10,
                           levels = as.character(1:5),
                           labels = c(
                             "Not as all concerned",
                             "Slightly concerned",
                             "Moderately concerned",
                             "Very concerned",
                             "Extremely concerned"
                           ))) |> 
    mutate(Q12_1 = factor(x$q12_1,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_2 = factor(x$q12_2,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_3 = factor(x$q12_3,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_4 = factor(x$q12_4,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_5 = factor(x$q12_5,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_6 = factor(x$q12_6,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_7 = factor(x$q12_7,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_8 = factor(x$q12_8,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_9 = factor(x$q12_9,
                          levels = as.character(1:5),
                          labels = c(
                            "Never",
                            "Occasionally",
                            "Sometimes",
                            "Often",
                            "Always"
                          ))) |> 
    mutate(Q12_10 = factor(x$q12_10,
                           levels = as.character(1:5),
                           labels = c(
                             "Never",
                             "Occasionally",
                             "Sometimes",
                             "Often",
                             "Always"
                           ))) |> 
    mutate(Q12_11 = factor(x$q12_11,
                           levels = as.character(1:5),
                           labels = c(
                             "Never",
                             "Occasionally",
                             "Sometimes",
                             "Often",
                             "Always"
                           ))) |> 
    ## recode  13 afterwards Not sure as 2nd never -> not sure -> might-> planning to change -> have already changed
    mutate(Q13_1 = factor(x$q13_1,
                          levels = as.character(1:5),
                          labels = c(
                            "Will never do this",
                            "Might do this",
                            "Plan to do this",
                            "Currently doing this",
                            "Not sure"
                          ))) |> 
    mutate(Q13_2 = factor(x$q13_2,
                          levels = as.character(1:5),
                          labels = c(
                            "Will never do this",
                            "Might do this",
                            "Plan to do this",
                            "Currently doing this",
                            "Not sure"
                          ))) |> 
    mutate(Q13_3 = factor(x$q13_3,
                          levels = as.character(1:5),
                          labels = c(
                            "Will never do this",
                            "Might do this",
                            "Plan to do this",
                            "Currently doing this",
                            "Not sure"
                          ))) |> 
    mutate(Q13_4 = factor(x$q13_4,
                          levels = as.character(1:5),
                          labels = c(
                            "Will never do this",
                            "Might do this",
                            "Plan to do this",
                            "Currently doing this",
                            "Not sure"
                          ))) |> 
    mutate(Q13_5 = factor(x$q13_5,
                          levels = as.character(1:5),
                          labels = c(
                            "Will never do this",
                            "Might do this",
                            "Plan to do this",
                            "Currently doing this",
                            "Not sure"
                          ))) |> 
    mutate(Q14_1 = factor(x$q14_1,
                          levels = as.character(1:5),
                          labels = c(
                            "No responsibility",
                            "Some responsibility",
                            "Moderate responsibility",
                            "Most responsibility",
                            "Full responsibility"
                          ))) |> 
    mutate(Q14_2 = factor(x$q14_2,
                          levels = as.character(1:5),
                          labels = c(
                            "No responsibility",
                            "Some responsibility",
                            "Moderate responsibility",
                            "Most responsibility",
                            "Full responsibility"
                          ))) |> 
    mutate(Q14_3 = factor(x$q14_3,
                          levels = as.character(1:5),
                          labels = c(
                            "No responsibility",
                            "Some responsibility",
                            "Moderate responsibility",
                            "Most responsibility",
                            "Full responsibility"
                          ))) |> 
    mutate(Q14_4 = factor(x$q14_4,
                          levels = as.character(1:5),
                          labels = c(
                            "No responsibility",
                            "Some responsibility",
                            "Moderate responsibility",
                            "Most responsibility",
                            "Full responsibility"
                          ))) |> 
    mutate(Q14_5 = factor(x$q14_5,
                          levels = as.character(1:5),
                          labels = c(
                            "No responsibility",
                            "Some responsibility",
                            "Moderate responsibility",
                            "Most responsibility",
                            "Full responsibility"
                          ))) |> 
    mutate(Q14_6 = factor(x$q14_6,
                          levels = as.character(1:5),
                          labels = c(
                            "No responsibility",
                            "Some responsibility",
                            "Moderate responsibility",
                            "Most responsibility",
                            "Full responsibility"
                          ))) |> 
    mutate(Q15_1 = factor(x$q15_1,
                          levels = as.character(1:5),
                          labels = c(
                            "Do not trust at all",
                            "Somewhat trust",
                            "Moderately trust",
                            "Mostly trust",
                            "Fully trust"
                          ))) |> 
    mutate(Q15_2 = factor(x$q15_2,
                          levels = as.character(1:5),
                          labels = c(
                            "Do not trust at all",
                            "Somewhat trust",
                            "Moderately trust",
                            "Mostly trust",
                            "Fully trust"
                          ))) |> 
    mutate(Q15_3 = factor(x$q15_3,
                          levels = as.character(1:5),
                          labels = c(
                            "Do not trust at all",
                            "Somewhat trust",
                            "Moderately trust",
                            "Mostly trust",
                            "Fully trust"
                          ))) |> 
    mutate(Q15_4 = factor(x$q15_4,
                          levels = as.character(1:5),
                          labels = c(
                            "Do not trust at all",
                            "Somewhat trust",
                            "Moderately trust",
                            "Mostly trust",
                            "Fully trust"
                          ))) |> 
    mutate(Q15_5 = factor(x$q15_5,
                          levels = as.character(1:5),
                          labels = c(
                            "Do not trust at all",
                            "Somewhat trust",
                            "Moderately trust",
                            "Mostly trust",
                            "Fully trust"
                          ))) |> 
    mutate(Q15_6 = factor(x$q15_6,
                          levels = as.character(1:5),
                          labels = c(
                            "Do not trust at all",
                            "Somewhat trust",
                            "Moderately trust",
                            "Mostly trust",
                            "Fully trust"
                          )))
  
  ## reorder q13 with Not sure as 2nd never -> not sure -> might-> planning to change -> have already changed
  
  df <- df |> 
    mutate(Q13_1 = forcats::fct_relevel(Q13_1, c("Will never do this",
                                                 "Not sure",
                                                 "Might do this",
                                                 "Plan to do this",
                                                 "Currently doing this"))) |> 
    mutate(Q13_2 = forcats::fct_relevel(Q13_2, c("Will never do this",
                                                 "Not sure",
                                                 "Might do this",
                                                 "Plan to do this",
                                                 "Currently doing this"))) |> 
    mutate(Q13_3 = forcats::fct_relevel(Q13_3, c("Will never do this",
                                                 "Not sure",
                                                 "Might do this",
                                                 "Plan to do this",
                                                 "Currently doing this"))) |> 
    mutate(Q13_4 = forcats::fct_relevel(Q13_4, c("Will never do this",
                                                 "Not sure",
                                                 "Might do this",
                                                 "Plan to do this",
                                                 "Currently doing this"))) |> 
    mutate(Q13_5 = forcats::fct_relevel(Q13_5, c("Will never do this",
                                                 "Not sure",
                                                 "Might do this",
                                                 "Plan to do this",
                                                 "Currently doing this"))) 
  
  ## since ACS data only record binary sex
  ## following Gelman's suggestion this will recode sex as "not male"
  df <- df |> 
    mutate(SEX_NM = case_when(
      SEX == "Male" ~ "1",
      SEX == "Female" ~ "2",
      SEX == "Other" ~ "2",
      SEX == "No Answer" ~ NA
    )) |> 
    mutate(SEX_NM = factor(SEX_NM,
                           levels = c("1", "2"),
                           labels = c("Male", "Not Male")
    ))
  
  ## convert No answers to NAs
  df <- df |>
    mutate(AGEP = fct_na_level_to_value(AGEP, "No answer"),
           RACE5 = fct_na_level_to_value(RACE5, "No answer"),
           RACE2 = fct_na_level_to_value(RACE2, "No answer"),
           SCHL = fct_na_level_to_value(SCHL, "No answer"))
  
  ## create variables used in raking. Will need to impute NA values for use in
  ## weighting only
  imputation_data <- df |> 
    select(SEX_NM, AGEP, RACE2, SCHL, HHSIZE, INCOME, HOWNSHP)
  
  ## impute missing values using random forest
  imputed_data <- mice(imputation_data, maxit = 30, seed = 1234,
                       method = "rf", printFlag = FALSE)
  
  ## fill in missing values
  imputed_data <- complete(imputed_data)
  
  ## rename variables with rk_ prefix
  ## these will be used for raking, but the original data is
  ## retain with non prefixed variable names
  imputed_data <- imputed_data |> 
    select(rk_SEX_NM = SEX_NM,
           rk_AGEP = AGEP,
           rk_RACE2 = RACE2,
           rk_SCHL = SCHL)
  
  df |> 
    bind_cols(imputed_data) |> 
    mutate(
           INCOME = fct_na_level_to_value(INCOME, "Prefer not to answer"),
           HOWNSHP = fct_na_level_to_value(HOWNSHP, "Prefer not to answer"),
           SEX = forcats::fct_na_level_to_value(SEX, "No answer"))
  
  
}
