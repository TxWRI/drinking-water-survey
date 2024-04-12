library(targets)
library(svyEffects)
library(survey)
library(ggplot2)
library(tinytable)
library(modelsummary)
library(tidyverse)
x <- targets::tar_read(m1) |>  
  svyAME(varname = "SCHL")

x$diffs |> 
  filter(y %in% c(0)) |> 
  mutate(SCHL = forcats::fct_reorder(SCHL, predicted)) |> 
  ggplot() +
  geom_point(aes(x = predicted, y = SCHL)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = SCHL))
  
# |>  
#   plot("diffs") +
#   coord_flip()
  

tar_read(m1) |>  
  modelsummary::modelsummary(shape = term ~ statistic,
                             fmt = 2,
                             estimate = "estimate",
                             statistic = c(
                               "CI" = "[{conf.low}, {conf.high}]",
                               "t-statistic" = "statistic",
                               "p-value" = "p.value"
                               ),
                             output = "tinytable",
                             exponentiate = TRUE,
                             include_reference = TRUE,
                             coef_omit = c(-1:-28),
                             coef_rename = c(
                              "Male", "Female", "Other", "18:24", "25:34", "35:44", "45:54", "44:64", "65+", "White", "Non-white", "Some high school", "High school or GED", "Associates degree", "Bachelor's degree", "Master's degree", "Doctorate or terminal degree", "Other", "Own", "Rent", "Prefer not to answer", "<\\$25,000", "\\$25,000-\\$49,999", "\\$50,000-\\$74,999", "\\$75,000-\\$99,999", "\\$100,000-\\$200,000", ">\\$200,000", "Prefer not to answer"),
                             gof_map = "none") |> 
  format_tt(j = 5,
            escape = TRUE) |> 
  group_tt(i = list("Sex/gender" = 2,
                    "Age" = 4,
                    "Race/ethnicity" = 10,
                    "Education" = 12,
                    "Home Ownership" = 19,
                    "Income" = 22),
           ) |> 
  style_tt(1, bold = TRUE)
