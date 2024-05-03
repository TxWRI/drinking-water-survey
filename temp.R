

library(targets)
library(survey)
library(srvyr)
library(tidyverse)
library(modelsummary)
library(tinytable)
library(scico)


write_fig_q14 <- function(df, weights) {
  df <- df |>
    mutate(Q8 = as.factor(Q8)) |>
    select(SEX, AGEP, RACE2, SCHL, HOWNSHP, INCOME, Q14_1, Q14_2, Q14_3, Q14_4, Q14_5, Q14_6)
  df$weights <- weights$weightvec

  survey_design <- df |>
    as_survey_design(weights = weights)

  df_1 <- survey_design |>
    select(Q14_1) |>
    group_by(Q14_1) |>
    summarise(proportion = survey_mean(vartype = "se")) |>
    mutate(Question = "Federal Government") |>
    rename(Response = Q14_1) |>
    bind_rows(
      survey_design |>
        select(Q14_2) |>
        group_by(Q14_2) |>
        summarise(proportion = survey_mean(vartype = "se")) |>
        mutate(Question = "State Government") |>
        rename(Response = Q14_2),
      survey_design |>
        select(Q14_3) |>
        group_by(Q14_3) |>
        summarise(proportion = survey_mean(vartype = "se")) |>
        mutate(Question = "Local Government") |>
        rename(Response = Q14_3),
      survey_design |>
        select(Q14_4) |>
        group_by(Q14_4) |>
        summarise(proportion = survey_mean(vartype = "se")) |>
        mutate(Question = "Water Utility") |>
        rename(Response = Q14_4),
      survey_design |>
        select(Q14_5) |>
        group_by(Q14_5) |>
        summarise(proportion = survey_mean(vartype = "se")) |>
        mutate(Question = "Landlord/property manager") |>
        rename(Response = Q14_5),
      survey_design |>
        select(Q14_6) |>
        group_by(Q14_6) |>
        summarise(proportion = survey_mean(vartype = "se")) |>
        mutate(Question = "Residents") |>
        rename(Response = Q14_6)
    ) |>
    mutate(Question = forcats::fct_reorder(Question, proportion, .fun = max))

  ggplot(df_1, aes(y = Question, group = Response, fill = Response, color = Response)) +
    stat_slabinterval(
      aes(xdist = dist_student_t(df = Inf, mu = proportion, sigma = proportion_se),
          fill_ramp = after_stat(level)),
      # stat_gradientinterval(
      #   aes(xdist = dist_student_t(df = Inf, mu = proportion, sigma = proportion_se)),
      point_color = "black",
      point_size = 1,
      interval_color = "grey15",
      stroke = 0.2,
      shape = 21,
      scale = .75,
      .width = c(.50, .80, .95),
      interval_size_range = c(0.1, 0.15),
      position = position_dodgejust(width = 0.5)
    ) +
    scico::scale_fill_scico_d(name = "Level of responsibility",
                              palette = "roma", direction = -1,
                              begin = 0.15, end = 0.85) +
    scico::scale_color_scico_d(name = "Level of responsibility",
                               palette = "roma", direction = -1,
                               begin = 0.15, end = 0.85) +
    scale_fill_ramp_discrete(na.translate = FALSE) +
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
          legend.position = "inside",
          legend.position.inside = c(0.8, 0.2),
          legend.direction = "vertical",
          plot.subtitle = element_text(size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", color = "#d9d9d9"))
}

library(ggdist)
library(distributional)

write_fig_q14(tar_read(clean_survey), tar_read(raked_weights))

  
