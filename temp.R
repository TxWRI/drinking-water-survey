

library(targets)
library(survey)
library(srvyr)
library(tidyverse)
library(modelsummary)
library(tinytable)


m3_coef_rename <- function(x) {
  get_estimates(x, include_reference = TRUE) |> 
    slice(1:32) |> 
    pull(term) |> 
    str_replace_all(c("SEX" = "", 
                      "AGEP" = "",
                      "RACE2" = "",
                      "SCHL" = "",
                      "HOWNSHP" = "",
                      "INCOME" = "",
                      "Q5" = "",
                      "Q7" = ""))
}

 
a <- modelsummary(models = list("1"= tar_read(m3)),
                  shape = term ~ model + statistic,
                  estimate = c("Odds-Ratio, [95% CI]" = "{estimate} [{conf.low}, {conf.high}]"),
                  statistic = c("t-stat" = "statistic",
                                "p-value" = "p.value"),
                  exponentiate = TRUE,
                  include_reference = TRUE,
                  coef_omit = 33:36,
                  coef_rename = m3_coef_rename(tar_read(m3)),
                  gof_map = NA,
                  output = "data.frame")


                 
                 
b <- modelsummary(models = list("2"= tar_read(m1)),
             shape = term ~ model + statistic,
             estimate = c("Odds-Ratio, [95% CI]" = "{estimate} [{conf.low}, {conf.high}]"),
             statistic = c("t-stat" = "statistic",
                           "p-value" = "p.value"),
             exponentiate = TRUE,
             include_reference = TRUE,
             coef_omit = 33:42,
             coef_rename = m3_coef_rename(tar_read(m1)),
             gof_map = NA,
             output = "data.frame")




modelsummary(models = list("Water safety rating"= tar_read(m3), "Trust in water utility" = tar_read(m1)),
             shape = term ~ model + statistic,
             estimate = c("Odds-Ratio, [95% CI]" = "{estimate} [{conf.low}, {conf.high}]"),
             statistic = c("t-stat" = "statistic",
                           "p-value" = "p.value"),
             exponentiate = TRUE,
             include_reference = TRUE,
             coef_omit = "\\|",
             coef_rename = m3_coef_rename(tar_read(m3)),
             gof_map = NA,
             output = "tinytable") |> 
  group_tt(i = list("Sex/Gender" = 1,
                "Age" = 4,
                "Race/Ethnicity" = 10,
                "Education" = 12,
                "Homeownership" = 19,
                "Income" = 21,
                "Primary tap water supply" = 27,
                "Taste, odor, color issues"= 31)) |> 
  format_tt(j = 1,
            escape = "latex") |> 
  print("latex")




