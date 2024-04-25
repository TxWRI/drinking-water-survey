## create table summarizing unweighted and weighted demographic statistics

write_tbl_weights <- function(unweighted, weighted) {
  
  unweighted <- unweighted |> 
    select(`Sex/Gender` = SEX_NM,
           `Age` = AGEP,
           `Race/Ethnicity` = RACE2,
           `Education` = SCHL) |> 
    mutate(`Sex/Gender` = forcats::fct_na_value_to_level(`Sex/Gender`, level = "No answer"),
           Age = forcats::fct_na_value_to_level(Age, level = "No answer"),
           `Race/Ethnicity` = forcats::fct_na_value_to_level(`Race/Ethnicity`, level = "No answer"),
           Education = forcats::fct_na_value_to_level(Education, level = "No answer")) |>
    tbl_summary(include = c(`Sex/Gender`, `Age`, `Race/Ethnicity`, `Education`),
                statistic = list(all_categorical() ~"{n}")) |>
    as_tibble() |>
    rename(`Unweighted N` = "**N = 1,100**",
           Value = "**Characteristic**") |>
    filter(!is.na(`Unweighted N`)) |>
    mutate(`Variable` = c(rep("Sex/Gender",3),
                          rep("Age", 7),
                          rep("Race/Ethnicity", 3),
                          rep("Education", 8)),
           `Unweighted N` = as.integer(`Unweighted N`)) |>
    group_by(Variable) |>
    mutate(`Unweighted %` = round(`Unweighted N` / sum(`Unweighted N`) * 100, 1)) |>
    select(Variable, Value, `Unweighted N`, `Unweighted %`)
  
  weighted <- weighted |>
    summary()
  
  df <- tibble(
    demographic_indicators = list(`Sex` = as_tibble(weighted$rk_SEX_NM,
                                                    rownames = "value") |>
                                    mutate(`Variable` = "Sex/Gender"),
                                  `Race/Ethnicity` = as_tibble(weighted$rk_RACE2,
                                                               rownames = "value") |>
                                    mutate(`Variable` = "Race/Ethnicity"),
                                  `Age` = as_tibble(weighted$rk_AGEP,
                                                    rownames = "value") |>
                                    mutate(`Variable` = "Age"),
                                  `Education` = as_tibble(weighted$rk_SCHL,
                                                          rownames = "value") |>
                                    mutate(`Variable` = "Education"))
  )
  
  weighted <- df |>
    unnest(cols = c(demographic_indicators)) |>
    select(`Variable`, value, Target, `Wtd N`, `Wtd %`) |>
    filter(value != "Total") |>
    mutate(Target = round(Target * 100, 1),
           `Wtd N` = round(`Wtd N`, 1),
           `Wtd %` = round(`Wtd %` * 100, 1)) |>
    rename(`\\vtop{\\hbox{Target}\\hbox{(\\%)}}` = Target,
           `\\vtop{\\hbox{Weighted}\\hbox{(\\%)}}` = `Wtd %`,
           `\\vtop{\\hbox{Weighted}\\hbox{(n)}}` = `Wtd N`)
  
  
  
  weighted <- unweighted |>
    left_join(weighted, by = c("Variable" = "Variable",
                               "Value" = "value")) |>
    arrange(Variable) |> 
    rename(`\\vtop{\\hbox{Unweighted}\\hbox{(n)}}` = `Unweighted N`,
           `\\vtop{\\hbox{Unweighted}\\hbox{(\\%)}}` = `Unweighted %`,)
  
  weighted |>
    ungroup() |>
    select(-c("Variable")) |>
    tt()
    
  # weighted |>
  #   ungroup() |> 
  #   select(-c("Variable")) |> 
  #   tt() |> 
  #   group_tt(i = list("Age" = 2,
  #                     "Education" = 9,
  #                     "Race/Ethnicity" = 17,
  #                     "Sex/Gender" = 20
  #                     )) |> 
  #   style_tt(i = c(1,9,18,22),
  #            bold = TRUE) |> 
  #   style_tt(j = c(2),
  #            align = "r") |> 
  #   style_tt(j = c(3),
  #            align = "r") |>
  #   style_tt(j = c(4),
  #            align = "r") |> 
  #   style_tt(j = c(5),
  #            align = "r") |> 
  #   style_tt(j = c(6),
  #            align = "r") |> 
  #   style_tt(i = 3, j = 1, 
  #            indent = 5)
}



## response summaries
## Q5 what is the source of your household tap water?
## Q6 What is your main source of drinking water?

write_tbl_q5_q6 <- function(df, weights) {
  ## convert sex No answer to NA
  df <- df |> 
    mutate(SEX = forcats::fct_na_level_to_value(SEX, "No answer")) |> 
    select(SEX, AGEP, RACE2, SCHL, Q5, Q6)
  df$weights <- weights$weightvec
  
  survey_design <- df |> 
    as_survey_design(weights = weights)
  
  q5_results <- survey_design |> 
    select(Q5) |> 
    group_by(Q5) |> 
    summarise(proportion = survey_mean(varttype = "se")) |> 
    mutate(Question = "What is the source of your household tap water?") |> 
    rename(Response = Q5)
  
  q6_results <- survey_design |> 
    select(Q6) |> 
    group_by(Q6) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Question = "What is your main source of drinking water?") |> 
    rename(Response = Q6)
  
  results <- q5_results |> 
    bind_rows(q6_results)
  
  results |> 
    rename("Proportion" = proportion, "SE" = proportion_se) |> 
    mutate(Proportion = paste0(round(Proportion, 3)),
           SE = paste0(round(SE, 3))) |> 
    select(-c(Question)) |> 
    tt() |> 
    group_tt(i = list(
      "What is the source of your household tap water?" = 2,
      "What is your main source of drinking water?" = 8
    )) |> 
    style_tt(i = c(1, 8),
             bold = TRUE) |> 
    style_tt(j = c(2),
             align = "r") |> 
    style_tt(j = c(3),
             align = "r") 
}

write_tbl_q7 <- function(df, weights) {
  ## convert sex No answer to NA
  df <- df |> 
    mutate(SEX = forcats::fct_na_level_to_value(SEX, "No answer")) |> 
    select(SEX, AGEP, RACE2, SCHL, Q7_1, Q7_2, Q7_3, Q7_4, Q7_5, Q7_6, Q7_7, Q7_8, Q7_9, Q7_10, Q7_11, Q7_12)
  df$weights <- weights$weightvec
  
  survey_design <- df |> 
    as_survey_design(weights = weights)
  
  q7_1_results <- survey_design |>
    select(Q7_1) |>
    group_by(Q7_1) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_1)
  q7_2_results <- survey_design |>
    select(Q7_2) |>
    group_by(Q7_2) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_2)
  q7_3_results <- survey_design |>
    select(Q7_3) |>
    group_by(Q7_3) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_3)
  q7_4_results <- survey_design |>
    select(Q7_4) |>
    group_by(Q7_4) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_4)
  q7_5_results <- survey_design |>
    select(Q7_5) |>
    group_by(Q7_5) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_5)
  q7_6_results <- survey_design |>
    select(Q7_6) |>
    group_by(Q7_6) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_6)
  q7_7_results <- survey_design |>
    select(Q7_7) |>
    group_by(Q7_7) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_7)
  q7_8_results <- survey_design |>
    select(Q7_8) |>
    group_by(Q7_8) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_8)
  q7_9_results <- survey_design |>
    select(Q7_9) |>
    group_by(Q7_9) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_9)
  q7_10_results <- survey_design |>
    select(Q7_10) |>
    group_by(Q7_10) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_10)
  q7_11_results <- survey_design |>
    select(Q7_11) |>
    group_by(Q7_11) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_11)
  q7_12_results <- survey_design |>
    select(Q7_12) |>
    group_by(Q7_12) |>
    summarise(proportion = survey_mean(vartype = "se")) |> 
    rename("Response" = Q7_12)
  
  q7_results <- bind_rows(q7_1_results, q7_2_results, q7_3_results, q7_4_results, q7_5_results, q7_6_results, q7_7_results, q7_8_results, q7_9_results, q7_10_results, q7_11_results, q7_12_results) |> 
    filter(Response == "Yes") |> 
    mutate(Issue = c(
      "Odd taste", "Odd smell", "Discolored water", "Cloudy water", "Staining of clothes, teeth, or skin", "High salt content", "Hardness or scale buildup", "Corrosion of pipes or water fixtures", "Boil water notice", "Water shortage", "None of the above", "Other"
    ))
  
  q7_results |> 
    select(Issue, "Proportion" = proportion, "SE" = proportion_se) |> 
    mutate(Proportion = paste0(round(Proportion, 3)),
           SE = paste0(round(SE, 3))) |> 
    tt() |> 
    style_tt(j = c(2),
             align = "r") |> 
    style_tt(j = c(3),
             align = "r") 

}


write_table_q11 <- function(df, weights) {
  ## convert sex No answer to NA
  df <- df |> 
    mutate(SEX = forcats::fct_na_level_to_value(SEX, "No answer")) |> 
    select(Q11_1, Q11_2, Q11_3, Q11_4, Q11_5, Q11_6, Q11_7, Q11_8, Q11_9, Q11_10)
  df$weights <- weights$weightvec
  
  survey_design <- df |> 
    as_survey_design(weights = weights)
  
  q11_1 <- survey_design |> 
    group_by(Q11_1) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Disinfection byproducts") |> 
    rename(Response = Q11_1)
  
  q11_2 <- survey_design |> 
    group_by(Q11_2) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Fertilizers") |> 
    rename(Response = Q11_2)
  
  q11_3 <- survey_design |> 
    group_by(Q11_3) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Heavy metals") |> 
    rename(Response = Q11_3)
  
  q11_4 <- survey_design |> 
    group_by(Q11_4) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Industrial chemicals") |> 
    rename(Response = Q11_4)
  
  q11_5 <- survey_design |> 
    group_by(Q11_5) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Microplastics") |> 
    rename(Response = Q11_5)
  
  q11_6 <- survey_design |> 
    group_by(Q11_6) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "PFAS") |> 
    rename(Response = Q11_6)
  
  q11_7 <- survey_design |> 
    group_by(Q11_7) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Pathogens") |> 
    rename(Response = Q11_7)
  
  q11_8 <- survey_design |> 
    group_by(Q11_8) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Pesticides") |> 
    rename(Response = Q11_8)
  
  q11_9 <- survey_design |> 
    group_by(Q11_9) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Petroleum products") |> 
    rename(Response = Q11_9)
  
  q11_10 <- survey_design |> 
    group_by(Q11_10) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Pharmaceuticals") |> 
    rename(Response = Q11_10)
  
  df <- bind_rows(q11_1, q11_2, q11_3, q11_4, q11_5, q11_6, q11_7, q11_8, q11_9, q11_10) |> 
    mutate(prop = paste0(round(proportion,3), " ±", round(proportion_se,3))) |> 
    select(Response, Source, prop) |> 
    pivot_wider(names_from = Response,
                values_from = prop) |> 
    tt()
  
  
  
  ## make a plot
  
  ## get hcl values for text labels
  hcl <- scico(5, palette = "batlow") |>  farver::decode_colour("rgb", "hcl")
  ## text labels will be white or black
  lab_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  ## make plot
  p1 <- bind_rows(q11_1, q11_2, q11_3, q11_4, q11_5, q11_6, q11_7, q11_8, q11_9, q11_10) |>
    filter(Source != "Other") |>
    mutate(Source = forcats::fct_reorder2(Source, Response, proportion)) |>
    ggplot() +
    geom_col(aes(proportion, Source, fill = Response)) +
    geom_text(aes(proportion, Source, fill = Response,
                  color = Response,
                  label = round(proportion,2)),
              position = position_stack(vjust = 0.5),
              family = "Manrope Medium", size = 2) +
    labs(x = "Proportion", y = "Source") +
    scale_x_continuous(expand = expansion(mult = c(0,0))) +
    scale_fill_scico_d(palette = "batlow") +
    scale_color_manual(values = lab_col, guide = NULL) +
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
          plot.subtitle = element_text(size = 8))
  
  ## return table and plot
  return(list(tbl = df,
              p1 = p1))
  
}



write_table_q12 <- function(df, weights) {
  ## convert sex No answer to NA
  df <- df |> 
    mutate(SEX = forcats::fct_na_level_to_value(SEX, "No answer")) |> 
    select(Q12_1, Q12_2, Q12_3, Q12_4, Q12_5, Q12_6, Q12_7, Q12_8, Q12_9, Q12_10, Q12_11)
  df$weights <- weights$weightvec
  
  survey_design <- df |> 
    as_survey_design(weights = weights)
  
  q12_1 <- survey_design |> 
    group_by(Q12_1) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Local news agency") |> 
    rename(Response = Q12_1)
  
  q12_2 <- survey_design |> 
    group_by(Q12_2) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "National news agency") |> 
    rename(Response = Q12_2)
  
  q12_3 <- survey_design |> 
    group_by(Q12_3) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Local newspaper") |> 
    rename(Response = Q12_3)
  
  q12_4 <- survey_design |> 
    group_by(Q12_4) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "National newspaper") |> 
    rename(Response = Q12_4)
  
  q12_5 <- survey_design |> 
    group_by(Q12_5) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Social media") |> 
    rename(Response = Q12_5)
  
  q12_6 <- survey_design |> 
    group_by(Q12_6) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Radio or podcast") |> 
    rename(Response = Q12_6)
  
  q12_7 <- survey_design |> 
    group_by(Q12_7) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Word of mouth") |> 
    rename(Response = Q12_7)
  
  q12_8 <- survey_design |> 
    group_by(Q12_8) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Email") |> 
    rename(Response = Q12_8)
  
  q12_9 <- survey_design |> 
    group_by(Q12_9) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Government agencies") |> 
    rename(Response = Q12_9)
  
  q12_10 <- survey_design |> 
    group_by(Q12_10) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "My water provider") |> 
    rename(Response = Q12_10)
  
  q12_11 <- survey_design |> 
    group_by(Q12_11) |> 
    summarise(proportion = survey_mean(vartype = "se")) |> 
    mutate(Source = "Other") |> 
    rename(Response = Q12_11)
  
  df <- bind_rows(q12_1, q12_2, q12_3, q12_4, q12_5, q12_6, q12_7, q12_8, q12_9, q12_10, q12_11) |> 
    mutate(prop = paste0(round(proportion,3), " ±", round(proportion_se,3))) |> 
    select(Response, Source, prop) |> 
    pivot_wider(names_from = Response,
                values_from = prop) |> 
    tt()
  
  ## make a plot
  
  ## get hcl values for text labels
  hcl <- scico(5, palette = "batlow") |>  farver::decode_colour("rgb", "hcl")
  ## text labels will be white or black
  lab_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  ## make plot
  p1 <- bind_rows(q12_1, q12_2, q12_3, q12_4, q12_5, q12_6, q12_7, q12_8, q12_9, q12_10, q12_11) |> 
    filter(Source != "Other") |> 
    mutate(Source = forcats::fct_reorder2(Source, Response, proportion)) |> 
    ggplot() +
    geom_col(aes(proportion, Source, fill = Response)) +
    geom_text(aes(proportion, Source, fill = Response,
                  color = Response,
                  label = round(proportion,2)),
              position = position_stack(vjust = 0.5),
              family = "Manrope Medium", size = 2) +
    labs(x = "Proportion", y = "Source") +
    scale_x_continuous(expand = expansion(mult = c(0,0))) +
    scale_fill_scico_d(palette = "batlow") +
    scale_color_manual(values = lab_col, guide = NULL) +
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
          plot.subtitle = element_text(size = 8))
  
  ## return table and plot
  return(list(tbl = df,
              p1 = p1))
  
}