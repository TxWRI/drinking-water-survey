fit_m3 <- function(df, weights) {
  
  ## M3 trust in water utility
  
  
  df <- df |> 
    mutate(Q5 = forcats::fct_collapse(Q5, "Private supply - well, river, pond, rainwater" = c("Private supply - well, river, pond",
                                                                                              "Private supply - rainwater harvest system"))) |> 
    mutate(Q7 = case_when(
      Q7_1 == "Yes" | Q7_2 == "Yes" | Q7_3 == "Yes" | Q7_4 == "Yes" ~
        "Yes",
      .default = "No"
    )) |> 
    mutate(Q7 = as.factor(Q7)) |> 
    select(SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q5, Q7, Q15_4)
  df$weights <- weights$weightvec
  
  df <- df |> 
    filter(Q5 != "Other") |> 
    mutate(Q5 = forcats::fct_drop(Q5))
  
  survey_design <- df |>
    as_survey_design(weights = weights)
  
  m3 <- svyolr(Q15_4 ~ SEX + AGEP + RACE2 + SCHL + HOWNSHP + INCOME + Q5 + Q7,
               design = survey_design,
               method = "logistic")
  
  m3
}

draw_m3 <- function(m3) {
  
  df <-   modelsummary::modelplot(list("Utility trust" = m3), exponentiate = TRUE, draw = FALSE,
                                  coef_omit = c(25:28)) |> 
    mutate(term = str_replace_all(term, c("SEX" = "", 
                                          "AGEP" = "",
                                          "RACE2" = "",
                                          "SCHL" = "",
                                          "HOWNSHP" = "",
                                          "INCOME" = "",
                                          "Q5" = "",
                                          "Q7" = ""))) |> 
    mutate(group = c(rep("Sex/Gender", 2),
                     rep("Age", 5),
                     rep("Race", 1),
                     rep("Education", 6),
                     rep("Home Ownership", 1),
                     rep("Income", 5),
                     rep("Primary tap water supply", 3),
                     rep("Taste, odor, color issues", 1))) |> 
    mutate(group = forcats::fct(group)) |> 
    mutate(term = forcats::fct_reorder(term, estimate)) 
  
  p1 <- ggplot(df) +
    geom_vline(xintercept = 1) +
    geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term)) +
    ggforce::facet_col(vars(group), scales = "free_y", space = "free") +
    scico::scale_color_scico_d("", palette = "glasgow", begin = 0.25, end = 0.75) +
    scale_x_log10() +
    labs(x = "Odds-ratio", y = "") +
    theme_mps() +
    theme(axis.text.y = element_text(size = 8, hjust = 1),
          axis.text.x = element_text(size = 8),
          legend.text = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", color = "#d9d9d9"),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x.top =  element_text(size = 8, angle = 0,vjust = 0, hjust = 0))

  return(list(df = df,
              p1 = p1))
  }