
## prior to tar_make(), make sure the census API key is set
## in the .renviron

library(targets)
library(tarchetypes)

targets::tar_option_set(
  packages = c("anesrake",
               "bookdown",
               "broom",
               "car",
               "distributional",
               "fs",
               "GGally",
               "ggdist",
               "ggforce",
               "ggrepel",
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
               "svyVGAM",
               "withr")
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
  tar_target(m2,
             fit_m2(clean_survey,
                    raked_weights)),

  ## m3 trust in local utility
  ## fits an ordered logistic regression using {survey}
  tar_target(m3,
             fit_m3(clean_survey, raked_weights)),
  tar_target(m3_coefs,
             draw_m3(m3)),
  
  ## m4 is level of responsibility by entity
  tar_target(m4,
             fit_m4(clean_survey,
                    raked_weights)),
  ## returns the GVIF of each moderator in the model
  tar_target(gvif_m4,
             gvif(m4)),
  
  ## m5 is level of trust by entity
  tar_target(m5,
             fit_m5(clean_survey,
                    raked_weights)),
  ## returns the GVIF of each moderator in the model
  tar_target(gvif_m5,
             gvif(m5)),
  
  ## Summary table
  tar_target(weighted_demo_tbl, write_tbl_weights(clean_survey,
                                                  raked_weights)),
  tar_target(q5_q6_summary, write_tbl_q5_q6(clean_survey, raked_weights)),
  tar_target(q7_summary, write_tbl_q7(clean_survey, raked_weights)),
  
  ## Figure 1
  tar_target(m4_5_figure, draw_m4_5(m4, m5)),
  tar_target(m4_5_plot_pdf, ggsave("figures/fig1.pdf",
                                   plot = m4_5_figure$p1,
                                   device = cairo_pdf,
                                   width = 6.5,
                                   height = 3.25,
                                   units = "in",
                                   dpi = 300,
                                   scale = 1),
             format = "file"),
  tar_target(m4_5_plot_png, ggsave("figures/fig1.png",
                                   plot = m4_5_figure$p1,
                                   device = agg_png,
                                   width = 6.5,
                                   height = 3.25,
                                   units = "in",
                                   dpi = 300,
                                   scale = 1),
             format = "file"),
  ## Figure 2
  tar_target(q11_summary, write_table_q11(clean_survey, raked_weights)),
  tar_target(q11_file, ggsave("figures/fig2.pdf",
                              plot = q11_summary$p1,
                              device = cairo_pdf,
                              width = 6.5,
                              height = 6.5*0.618,
                              units = "in",
                              dpi = 300,
                              scale = 1),
             format = "file"),  
  tar_target(q11_png_file, ggsave("figures/fig2.png",
                              plot = q11_summary$p1,
                              device = agg_png,
                              width = 6.5,
                              height = 6.5*0.618,
                              units = "in",
                              dpi = 300,
                              scale = 1),
             format = "file"),  
  # Figure 3
  tar_target(q12_summary, write_table_q12(clean_survey, raked_weights)),
  tar_target(q12_pdf_file, ggsave("figures/fig3.pdf",
                              plot = q12_summary$p1,
                              device = cairo_pdf,
                              width = 6.5,
                              height = 6.5*0.618,
                              units = "in",
                              dpi = 300,
                              scale = 1),
             format = "file"), 
  tar_target(q12_png_file, ggsave("figures/fig3.png",
                                  plot = q12_summary$p1,
                                  device = agg_png,
                                  width = 6.5,
                                  height = 6.5*0.618,
                                  units = "in",
                                  dpi = 300,
                                  scale = 1),
             format = "file"), 
    
  ## Summary figs
  tar_target(q12_supplement, plot_q12_supplement(clean_survey, raked_weights)),

  tar_target(q15_by_source, write_fig_q15_bysource(clean_survey, raked_weights))

)