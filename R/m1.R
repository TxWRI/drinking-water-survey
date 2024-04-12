fit_m1 <- function(df, weights) {

  ## M1 perception of drinking water safety

  df <- df |> 
    mutate(Q5 = forcats::fct_collapse(Q5, "Private supply - well, river, pond, rainwater" = c("Private supply - well, river, pond",
                                                                                              "Private supply - rainwater harvest system"))) |> 
    mutate(Q7 = case_when(
      Q7_1 == "Yes" | Q7_2 == "Yes" | Q7_3 == "Yes" | Q7_4 == "Yes" ~
        "Yes",
      .default = "No"
    )) |> 
    mutate(Q7 = as.factor(Q7),
           Q8 = as.factor(Q8)) |> 
    select(SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q5, Q7, Q8)
  df$weights <- weights$weightvec
  
  survey_design <- df |>
    as_survey_design(weights = weights)

  m1 <- svyolr(Q8 ~ SEX + AGEP + RACE2 + SCHL + HOWNSHP + INCOME + Q5 + Q7,
               design = survey_design,
               method = "logistic")
  
  m1
}


## ## get the GVIF for moderators in model

gvif <- function(model) {
  car::vif(model) |> 
    dplyr::as_tibble()
}



draw_m1 <- function(m1) {
  preds_SEX <- m1 |>  svyEffects::svyAME(varname = "SEX")
  preds_AGE <- m1 |> svyEffects::svyAME(varname = "AGEP")
  preds_RACE2 <- m1 |> svyEffects::svyAME(varname = "RACE2")
  preds_SCHL <- m1 |> svyEffects::svyAME(varname = "SCHL")
  preds_HOWNSHP <- m1 |> svyEffects::svyAME(varname = "HOWNSHP")
  preds_INCOME <- m1 |> svyEffects::svyAME(varname = "INCOME")
  preds_Q5 <- m1 |> svyEffects::svyAME(varname = "Q5")
  preds_Q7 <- m1 |> svyEffects::svyAME(varname = "Q7")
  


  diffs <- preds_SEX$diffs |>
    mutate(moderator = "SEX/GENDER") |> 
    rename(x = SEX) |> 
    bind_rows(
      preds_AGE$diffs |>
        mutate(moderator = "AGE") |> 
        rename (x = AGEP),
      preds_SCHL$diffs |>
        mutate(moderator = "EDUCATION") |> 
        rename(x = SCHL),
      preds_HOWNSHP$diffs |>
        mutate(moderator = "HOME OWNERSHIP") |> 
        rename(x = HOWNSHP),
      preds_INCOME$diffs |>
        mutate(moderator = "INCOME") |> 
        rename(x = INCOME),
      preds_Q5$diffs |> 
        mutate(moderator = "HOUSEHOLD TAP WATER SUPPLY") |> 
        rename(x = Q5),
      preds_Q7$diffs |> 
        mutate(moderator = "TASTE/ODOR/COLOR ISSUES") |> 
        rename(x = Q7),
    )
  
  diffs |> 
    filter(y %in% c(0,5,10)) |> ggplot() + 
    geom_vline(xintercept = 0) +
    geom_pointrange(aes(x = predicted, xmin = conf.low, xmax = conf.high, y = x, color = y),
                    position = position_dodge(width = 0.2)) + 
    facet_wrap(~moderator, scales = "free", ncol = 2) + 
    theme_mps() +
    theme(axis.text.x = element_text(size = 6),
          axis.text.y.left = element_text(hjust = 1,
                                          size = 6),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.position = "right",
          legend.direction = "vertical",
          plot.subtitle = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", color = "#d9d9d9"))


}
