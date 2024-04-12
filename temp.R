# tar_read(m2) |>  modelsummary::modelsummary(shape = term ~ statistic,
#                                             fmt = 2,
#                                             estimate = c("Odds-ratio" = "estimate"),
#                                             statistic = c(
#                                               "CI" = "[{conf.low}, {conf.high}]",
#                                               "t-statistic" = "statistic",
#                                               "p-value" = "p.value"
#                                             ),
#                                             exponentiate = TRUE,
#                                             include_reference = TRUE,
#                                             coef_omit = c(34:43),
#                                             gof_map = "none",
#                                             width = 1,
#                                             coef_rename = c(
#                                               "Male", "Female", "Other", "18:24", "25:34", "35:44", "45:54", "44:64", "65+", "White", "Non-white", "Some high school", "High school or GED", "Associates degree", "Bachelor's degree", "Master's degree", "Doctorate/terminal degree", "Other", "Own", "Rent", "<\\$25,000", "\\$25,000-\\$49,999", "\\$50,000-\\$74,999", "\\$75,000-\\$99,999", "\\$100,000-\\$200,000", ">\\$200,000", "Public supply - municipal", "Public supply - rural water district", "Private supply - well, river, pond, rainwater", "I don't know", "Other", "No", "Yes"),
#                                             output = "data.frame"
#                                             ) |> 
#   as_tibble() |> 
#   mutate(group = c("Sex/Gender", rep(" ", 2),
#                    "Age", rep(" ", 5),
#                    "Race", rep(" ", 1),
#                    "Education", rep(" ", 6),
#                    "Home ownership", " ",
#                    "Income", rep(" ", 5),
#                    "Household Tap Water Supply", rep(" ", 4),
#                    "Taste, Odor, Color Issues", rep(" ", 1))) |> 
#   select(" " = group,
#          "Variable" = term,
#          "Odds-ratio" = `(1) / Odds-ratio`,
#          "95% CI" = `(1) / CI`,
#          "t-statistic" = `(1) / t-statistic`,
#          "p-value" = `(1) / p-value`) |> 
#   tinytable::tt(width = 1) |> 
#   format_tt(j =  6, escape = TRUE) |> 
#   style_tt(i = 0,
#            bold = TRUE,
#            fontsize = 0.8) |> 
#   style_tt(j = 1,
#            bold = TRUE,
#            align = "l") |> 
#   style_tt(i = c(3,9,11,18,20,26),
#            j = c(2:6),
#            line = "b",
#            line_width = 0.05) |> 
#   style_tt(fontsize = 0.80)

library(targets)
library(survey)
library(srvyr)
library(svyVGAM)
library(tidyverse)


tidy.svyVGAM <- function(
    x, 
    conf.int = FALSE, 
    conf.level = 0.95,
    exponentiate = FALSE, 
    ...
){
  # Replace `summary(x)$coefficients` with `summary(x)$coeftable`
  ret <- as_tibble(summary(x)$coeftable, rownames = "term")
  
  # All of this stays the same:
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  coefs <- tibble::enframe(stats::coef(x), name = "term", value = "estimate")
  ret <- left_join(coefs, ret, by = c("term", "estimate"))
  if (conf.int){
    ci <- broom:::broom_confint_terms(x, level = conf.level, ...)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  if (exponentiate){ret <- broom:::exponentiate(ret)}

  ret
  
}

df <- tidy.svyVGAM(tar_read(m2), exponentiate = TRUE, conf.int = TRUE)

df |>  tidyr::separate_wider_regex(term, c(var = ".*", ":", level = ".*")) |> 
  mutate(level = forcats::fct(level, levels = c("1", "2"))) |> 
  mutate(level = forcats::lvls_revalue(level, c("Filtered tap water", "Bottled water"))) |> 
  mutate(group = c(rep("Intercept", 2),
                   rep("Sex/Gender", 4),
                   rep("Age", 10),
                   rep("Race", 2),
                   rep("Education", 12),
                   rep("Home Ownership", 2),
                   rep("Income", 10),
                   rep("Primary tap water supply", 6))) |> 
  mutate(group = forcats::fct(group)) |> 
  mutate(var = str_replace_all(var, c("SEX" = "", 
                                      "AGEP" = "",
                                      "RACE2" = "",
                                      "SCHL" = "",
                                      "HOWNSHP" = "",
                                      "INCOME" = "",
                                      "Q5" = ""))) |> 
  ggplot() +
  geom_vline(xintercept = 1) +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = var, color = level),
                  position = position_dodge(width = 0.25)) +

  ggforce::facet_col(vars(group), scales = "free_y", space = "free") +
  scico::scale_color_scico_d(palette = "berlin") +
  scale_x_log10() +
  labs(x = "Log-odds", y = "") +
  theme_mps() +
  theme(axis.text.y = element_text(size = 8, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x.top =  element_text(size = 8, angle = 0,vjust = 0, hjust = 0))

