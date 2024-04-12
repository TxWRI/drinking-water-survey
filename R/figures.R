
write_fig_q8 <- function(df, weights) {
  ## convert sex No answer to NA
  df <- df |> 
    mutate(SEX = forcats::fct_na_level_to_value(SEX, "No answer")) |> 
    select(SEX, AGEP, RACE2, SCHL, Q8)
  df$weights <- weights$weightvec
  
  survey_design <- df |> 
    as_survey_design(weights = weights)

  
  p1 <- survey_design |>
    group_by(Q8) |>
    survey_tally() |>
    ggplot() +
    geom_col(aes(Q8, n)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = c(0,5,10), labels = c(0,5,10)) +
    theme_mps()
  
  stats <- survey_design |> 
    summarise(mean = survey_mean(Q8),
              median = survey_median(Q8))
  
  
  
  return(list(p1 = p1,
              stats = stats))
}


write_fig_q14 <- function(df, weights) {
  ## convert sex No answer to NA
  df <- df |> 
    mutate(Q8 = as.factor(Q8)) |> 
    select(SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q14_1, Q14_2, Q14_3, Q14_4, Q14_5, Q14_6)
  df$weights <- weights$weightvec
  
  survey_design <- df |> 
    as_survey_design(weights = weights)
  
  
  ## switch to bar plot?
  df_1 <- survey_design |> 
    select(Q14_1) |> 
    group_by(Q14_1) |> 
    summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
    mutate(Question = "Federal Government") |> 
    rename(Response = Q14_1) |> 
    bind_rows(
      survey_design |> 
        select(Q14_2) |> 
        group_by(Q14_2) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "State Government") |> 
        rename(Response = Q14_2),
      survey_design |> 
        select(Q14_3) |> 
        group_by(Q14_3) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Local Government") |> 
        rename(Response = Q14_3),
      survey_design |> 
        select(Q14_4) |> 
        group_by(Q14_4) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Water Utility") |> 
        rename(Response = Q14_4),
      survey_design |> 
        select(Q14_5) |> 
        group_by(Q14_5) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Landlord/property manager") |> 
        rename(Response = Q14_5),
      survey_design |> 
        select(Q14_6) |> 
        group_by(Q14_6) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Residents") |> 
        rename(Response = Q14_6)
    ) |> 
    mutate(Question = forcats::fct_reorder(Question, proportion, .fun = max))
  p1 <- ggplot(df_1) + 
    geom_col(aes(x = proportion, y = Question, fill = Response),
             position = position_dodge()) +
    geom_errorbar(aes(y = Question, xmin = proportion_low, xmax = proportion_upp, 
                      group = Response),
                  position = position_dodge2(padding = 0.8)) +
    scico::scale_fill_scico_d(name = "Level of responsibility",
                              palette = "roma", direction = -1) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = "Proportion", y = "Entity", subtitle = "Question: What level of responsibility should the following entities\nhave for making sure drinking water is safe for consumption?") +
    theme_mps() +
    theme(axis.text.x = element_text(size = 6),
          axis.text.y.left = element_text(hjust = 1,
                                          size = 8),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.position = "right",
          legend.direction = "vertical",
          plot.subtitle = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", color = "#d9d9d9"))
  return(list(p1 = p1,
              df = df_1))
}





write_fig_q15 <- function(df, weights) {
  ## convert sex No answer to NA
  df <- df |> 
    select(SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q15_1, Q15_2, Q15_3, Q15_4, Q15_5, Q15_6)
  df$weights <- weights$weightvec
  
  survey_design <- df |> 
    as_survey_design(weights = weights)
  
  df_1 <- survey_design |> 
    select(Q15_1) |> 
    group_by(Q15_1) |> 
    summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
    mutate(Question = "Federal Government") |> 
    rename(Response = Q15_1) |> 
    bind_rows(
      survey_design |> 
        select(Q15_2) |> 
        group_by(Q15_2) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "State Government") |> 
        rename(Response = Q15_2),
      survey_design |> 
        select(Q15_3) |> 
        group_by(Q15_3) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Local Government") |> 
        rename(Response = Q15_3),
      survey_design |> 
        select(Q15_4) |> 
        group_by(Q15_4) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Water Utility") |> 
        rename(Response = Q15_4),
      survey_design |> 
        select(Q15_5) |> 
        group_by(Q15_5) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Landlord/property manager") |> 
        rename(Response = Q15_5),
      survey_design |> 
        select(Q15_6) |> 
        group_by(Q15_6) |> 
        summarise(proportion = survey_mean(vartype = "ci", prop_method = "logit")) |> 
        mutate(Question = "Residents") |> 
        rename(Response = Q15_6)
    ) |> 
    mutate(Question = forcats::fct_reorder(Question, proportion, .fun = min))
  
  p1 <- ggplot(df_1) + 
    geom_col(aes(x = proportion, y = Question, fill = Response),
             position = position_dodge()) +
    geom_errorbar(aes(y = Question, xmin = proportion_low, xmax = proportion_upp, 
                      group = Response),
                  position = position_dodge2(padding = 0.8)) +
    scico::scale_fill_scico_d(name = "Level of trust",
                               palette = "roma", direction = 1) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = "Proportion", y = "Entity", subtitle = "Question: What is your level of trust in the following entities\nfor making sure drinking water is safe for consumption?") +
    theme_mps() +
    theme(axis.text.x = element_text(size = 6),
          axis.text.y.left = element_text(hjust = 1,
                                          size = 8),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 0),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 8),
          legend.position = "right",
          legend.direction = "vertical",
          plot.subtitle = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", color = "#d9d9d9"))
  return(list(p1 = p1,
              df = df_1))
}
