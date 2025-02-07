## Fit model for level of responsibility for each entity
## This is y ~ factor(entity) + demographics

fit_m4 <- function(df, weights) {
  
  df <- df |> 
    mutate(id = dplyr::row_number()) |> 
    select(id, SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q5, Q14_1, Q14_2, Q14_3, Q14_4, Q14_5, Q14_6, Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6) |> 
    mutate(Q5 = forcats::fct_collapse(Q5, "Private supply - well, river, pond, rainwater" = c("Private supply - well, river, pond",
                                                                                              "Private supply - rainwater harvest system")))
  
  df$weights <- weights$weightvec
  
  df_q14 <- df |> 
    dplyr::rename("SOURCE" = Q5,
                  "Federal" = Q14_1,
                  "State" = Q14_2,
                  "Local" = Q14_3,
                  "Utility" = Q14_4,
                  "Property" = Q14_5,
                  "Resident" = Q14_6) |> 
    tidyr::pivot_longer(cols = Federal:Resident,
                        names_to = "ENTITY",
                        values_to = "RATING")
  
  survey_design_14 <- df_q14 |>
    as_survey_design(weights = weights)
  
  m14 <- svyolr(RATING ~ ENTITY + SEX + AGEP + RACE2 + SCHL + HOWNSHP + INCOME + SOURCE, 
                design = survey_design_14,
                method = "logistic")
  return(m14)
}

fit_m5 <- function(df, weights) {
  df <- df |> 
    mutate(id = dplyr::row_number()) |> 
    select(id, SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q5, Q14_1, Q14_2, Q14_3, Q14_4, Q14_5, Q14_6, Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6) |> 
    mutate(Q5 = forcats::fct_collapse(Q5, "Private supply - well, river, pond, rainwater" = c("Private supply - well, river, pond",
                                                                                              "Private supply - rainwater harvest system")))
  
  
  df$weights <- weights$weightvec
  
  df_q15 <- df |> 
    dplyr::rename("SOURCE" = Q5,
                  "Federal" = Q15_1,
                  "State" = Q15_2,
                  "Local" = Q15_3,
                  "Utility" = Q15_4,
                  "Property" = Q15_5,
                  "Resident" = Q15_6) |> 
    tidyr::pivot_longer(cols = Federal:Resident,
                        names_to = "ENTITY",
                        values_to = "RATING")
  
  survey_design_15 <- df_q15 |>
    as_survey_design(weights = weights)
  
  m15 <- svyolr(RATING ~ ENTITY + SEX + AGEP + RACE2 + SCHL + HOWNSHP + INCOME + SOURCE, 
                design = survey_design_15,
                method = "logistic")
  return(m15)
}


draw_m4_5 <- function(m4, m5) {
  
  p1 <- modelsummary::modelplot(list("Level of Responsibility" = m4, 
                                     "Level of Trust" = m5), exponentiate = TRUE,
                                coef_omit = "^(?!ENTITY)",
                                coef_map = c("ENTITYLocal" = "Local Government",
                                             "ENTITYProperty" = "Property Manager",
                                             "ENTITYResident" = "Resident",
                                             "ENTITYState" = "State Government",
                                             "ENTITYUtility" = "Water Utility")) +
    geom_vline(xintercept = 1) +
    scale_x_continuous(trans = "log10",
                       breaks = c(0.5,1,2,3,4,5)) +
    geom_text_repel(data = tibble(x = 1, y = 5.25,
                                  label = "Baseline Factor: Federal Government"),
                    aes(x = x, y = I(y),
                        label = label),
                    size = 2,
                    nudge_x = 0.1,
                    nudge_y = 0.1,
                    hjust = 0,
                    vjust = 0,
                    segment.curvature = -0.4,
                    segment.ncp = 1,
                    segment.shape = 1,
                    segment.square    = FALSE,
                    segment.inflect   = TRUE,
                    arrow = arrow(length = unit(0.015, "npc")),
                    family = "Manrope Regular",
                    inherit.aes = FALSE) +
    labs(x = "Odds-Ratio", y = "Water Management Entity") +
    scico::scale_color_scico_d(name = "Response Variable:",
                               palette = "tofino", direction = 1,
                               begin = 0.15, end = 0.85,
                               guide = guide_legend(reverse = TRUE)) +
    theme_mps() +
    theme(axis.text.x = element_text(size = 6),
          axis.text.y.left = element_text(hjust = 1,
                                          size = 8),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          plot.subtitle = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", color = "#d9d9d9"),
          strip.text = element_text(size = 9))
  
  m_summary <- modelsummary::modelsummary(list("Level of Responsibility" = m4, 
                                               "Level of Trust" = m5), exponentiate = TRUE,
                                          estimate = "{estimate} [{conf.low}, {conf.high}]",
                                          statistic = c("t-stat" = "statistic", "p" = "p.value"),
                                          output = "data.frame",
                                          gof_map = NA,
                                          coef_omit = "^(?!ENTITY)",
                                          coef_map = c("ENTITYLocal" = "Local Government",
                                                       "ENTITYProperty" = "Property Manager",
                                                       "ENTITYResident" = "Resident",
                                                       "ENTITYState" = "State Government",
                                                       "ENTITYUtility" = "Water Utility"))
  return(list(p1 = p1,
              summary = m_summary))
}