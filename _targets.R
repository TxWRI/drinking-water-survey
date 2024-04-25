
## prior to tar_make(), make sure the census API key is set
## in the .renviron

library(targets)
library(tarchetypes)

targets::tar_option_set(
  packages = c("anesrake",
               "broom",
               "car",
               "GGally",
               "ggforce",
               "gtsummary",
               "janitor",
               "mice",
               "quarto",
               "ragg",
               "ranger",
               "tidyverse",
               "tidycensus",
               "tinytable",
               "scico",
               "srvyr",
               "survey",
               "svyVGAM")
)

files <- fs::dir_ls("R")

for (n in 1:length(files)) {
  source(files[[n]])
}

## prep fancy figure font
font_hoist("Manrope")


list(
  
  ## Get population proportions
  ## from ACS PUMS data
  ## to weight our survey data with
  ## Functions in ACS_Data.R
  tar_target(pums, get_pums_data()),
  tar_target(formated_pums, recode_pums(pums)),
  tar_target(proportions_pums, prop_pums(formated_pums)),
  
  ## return a named list with target marginal proportions
  tar_target(target_prop_list, prop_pums_to_list(proportions_pums)),
  
  ## Read survey data
  tar_target(survey_csv,
             "data/Survey_Data_Coded.csv",
             format = "file"),
  tar_target(survey, read_survey(survey_csv)),
  
  ## Munge survey results
  ## create new raking variables rk_AGEP, rk_SCHL, rk_SEX, rk_RACE5
  ## that have NA values replaced with imputed values
  ## pew uses the mice function in {mice} to impute values
  tar_target(clean_survey, munge_survey_results(survey)),
  
  ## Rake weights
  ## Functions in weights.R
  tar_target(raked_weights,
             rake(target_prop_list,
                  clean_survey)),
  
  
  ## Models
  ## M1 Perceived water quality as a function of demographics, tap water sources, and taste/odor/color issues
  ## fits a ordered logistic regression using {survey}
  
  tar_target(m1,
             fit_m1(clean_survey,
                    raked_weights)),
  tar_target(m1_coefs,
             draw_m1(m1)),

  ## returns the GVIF of each moderator in the model
  tar_target(gvif_m1,
             gvif(m1)),
  
  ## m2 drinking water source as a function of demographics, tap water sources
  ## fits a multinomial regression model using {VGAM} and {svyVGAM} packages
  ## still need to add taste/odor/color issues
  tar_target(m2,
             fit_m2(clean_survey,
                    raked_weights)),
  tar_target(m2_coefs,
             draw_m2(m2)),
  
  ## m3 trust in local utility
  ## fits an ordered logistic regression using {survey}
  tar_target(m3,
             fit_m3(clean_survey, raked_weights)),
  tar_target(m3_coefs,
             draw_m3(m3)),
  
  
  ## model figures
  tar_target(m1_m3_coefs, plot_m1_m3(m1_coefs, m3_coefs)),
  
  ## Summary table
  tar_target(weighted_demo_tbl, write_tbl_weights(clean_survey,
                                                  raked_weights)),
  tar_target(q5_q6_summary, write_tbl_q5_q6(clean_survey, raked_weights)),
  tar_target(q7_summary, write_tbl_q7(clean_survey, raked_weights)),
  tar_target(q11_summary, write_table_q11(clean_survey, raked_weights)),
  tar_target(q12_summary, write_table_q12(clean_survey, raked_weights)),
  
  ## Summary figs
  #tar_target(q8_fig, write_fig_q8(clean_survey, raked_weights)),
  tar_target(q14_summary, write_fig_q14(clean_survey, raked_weights)),
  tar_target(q15_summary, write_fig_q15(clean_survey, raked_weights)),


  ## Report
  tar_quarto(data_analysis_report, "data-analysis/data-analysis.qmd",
             quiet = FALSE),
  tar_quarto(sup_mats, "supplementary-materials/supplementary-materials.qmd",
             quiet = FALSE)

)