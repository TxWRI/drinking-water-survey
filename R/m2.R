fit_m2 <- function(df, weights) {
  
  ## M2 choice in DW
  
  ## use svyVGAM
  
  df <- df |> 
    mutate(Q5 = forcats::fct_collapse(Q5, "Private supply - well, river, pond, rainwater" = c("Private supply - well, river, pond",
                                                                                              "Private supply - rainwater harvest system"))) |> 
    mutate(Q7 = case_when(
      Q7_1 == "Yes" | Q7_2 == "Yes" | Q7_3 == "Yes" | Q7_4 == "Yes" ~
        "Yes",
      .default = "No"
    )) |> 
    mutate(Q7 = as.factor(Q7)) |> 
    select(SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q5, Q6, Q7)
  df$weights <- weights$weightvec
  
  df <- df |> 
    filter(Q5 != "Other") |> 
    filter(Q6 != "Other") |> 
    mutate(Q5 = forcats::fct_drop(Q5),
           Q6 = forcats::fct_drop(Q6))
  
  survey_design <- df |>
    as_survey_design(weights = weights)
  
  m2 <- svyVGAM::svy_vglm(Q6 ~ SEX + AGEP + RACE2 + SCHL + HOWNSHP + INCOME + Q5 + Q7,
                          design = survey_design,
                          family = multinomial(refLevel = "Unfiltered tap water"))
  
  m2
}


## add broom tidier for svyVGAM to get tibble of coefs, conf.ints, and pvals
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



draw_m2 <- function(model) {
  
  df <- tidy.svyVGAM(model, exponentiate = TRUE, conf.int = TRUE) |> 
    tidyr::separate_wider_regex(term, c(var = ".*", ":", level = ".*")) |> 
    mutate(level = forcats::fct(level, levels = c("1", "2"))) |> 
    mutate(level = forcats::lvls_revalue(level, c("Filtered tap water", "Bottled water"))) |> 
    mutate(group = c(rep("Intercept", 2),
                     rep("Sex/Gender", 4),
                     rep("Age", 10),
                     rep("Race", 2),
                     rep("Education", 12),
                     rep("Home Ownership", 2),
                     rep("Income", 10),
                     rep("Primary tap water supply", 6),
                     rep("Taste, odor, color issues", 2))) |> 
    mutate(group = forcats::fct(group)) |> 
    mutate(var = str_replace_all(var, c("SEX" = "", 
                                        "AGEP" = "",
                                        "RACE2" = "",
                                        "SCHL" = "",
                                        "HOWNSHP" = "",
                                        "INCOME" = "",
                                        "Q5" = "",
                                        "Q7" = ""))) |> 
    mutate(var = forcats::fct_reorder(var, estimate, "max"))
  
  p1 <- ggplot(df) +
    geom_vline(xintercept = 1) +
    geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high, 
                        y = var, color = level),
                    size = 0.25,
                    position = position_dodge(width = 0.5)) +
    
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
  
  return(list( df = df,
               p1 = p1))
}