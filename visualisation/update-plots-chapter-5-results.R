# ==============================================================================
# Plots chapter 5 results
# ==============================================================================

# setup ------------------------------------------------------------------------
library(ggplot2)
library(RColorBrewer)
library(dplyr)

source(here::here("utils", "settings.R"))

evaluation_scenario <- "baseline"
sensitivity_ensembles <- FALSE

root_folder <- c("visualisation/chapter-5-results")

if (sensitivity_ensembles) {
  settings$model_names_eval <- settings$ensemble_names_all
  root_folder <- paste0(root_folder, "/ensembles")
}

root_folder <- paste0(root_folder, "/scenario-",
                      evaluation_scenario)

if(!dir.exists(root_folder)) {
  dir.create(root_folder)
}


if(evaluation_scenario == 1) {
  settings$evaluation_dates <-
    settings$evaluation_dates[settings$evaluation_dates <= as.Date("2020-07-27")]
}

if(evaluation_scenario == 2) {
  settings$states_included <- settings$states_included[!(settings$states_included %in% "New Jersey")]
}

if(evaluation_scenario == 3) {
  settings$evaluation_dates <-
    settings$evaluation_dates[settings$evaluation_dates <= as.Date("2020-07-20")]

  settings$states_included <-
    settings$states_included[!(settings$states_included %in% c("New Jersey", "US"))]
}

# source function for visualisation
source(here::here("visualisation", "plotting-functions", "visualise-data-functions.R"))
source(here::here("visualisation", "plotting-functions", "evaluation-plots-function.R"))
source(here::here("utils", "load-data-functions.R"))

# get full data set
deaths <- get_us_deaths(data = "weekly") %>%
  dplyr::filter(epiweek < max(epiweek))

forecasts <- load_submission_files(dates = c(settings$evaluation_dates),
                                   models = c(settings$model_names_eval))

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))

full <- prepare_for_scoring(forecasts)



# determine levels of the models for all other forecasts
scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model"))

models <- scores %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE)) %>%
  dplyr::pull(model) %>%
  unique()
model_levels <- levels(models)
settings$model_levels <- model_levels

ordered_manual_colours <- settings$colour_df[match(model_levels,
                                            settings$colour_df$model_names[11:1]), ] %>%
  pull(colours)




## note: if you run this for ensembles, you need to change the
# ordered_manual_colours back to settings$manual_colours
# ordered_manual_colours <- settings$manual_colours










# ==============================================================================
# Forecast visualisation
# ==============================================================================


US_forecast_one_four_weeks <- plot_forecasts(states = "US",
                                         forecasts = forecasts,
                                         facet_formula = model ~ horizon,
                                         ncol_facet = 4,
                                         horizons = c(1, 4),
                                         obs_weeks = 7,
                                         manual_colours = settings$manual_colours) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                      hjust=1),
                 legend.position = "none")  +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here(root_folder,
                           "US-forecast-1-4-wk-ahead.png"),
                US_forecast_one_four_weeks, width = 12, height = 10)



# for Appendix
# several_states_one_week <- plot_forecasts(states = settings$states_included,
#                                        forecasts = forecasts,
#                                        facet_formula = state ~ model,
#                                        ncol_facet = 11,
#                                        horizons = c(1),
#                                        obs_weeks = 7)
#
# ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
#                            "all-states-one-week.png"),
#                 several_states_one_week, width = 18, height = 25)
#

#
#
# all_states_one_week <- plot_forecasts(states = settings$states_included,
#                                       forecasts = forecasts,
#                                       facet_formula = state ~ model,
#                                       ncol_facet = 4,
#                                       horizons = c(1),
#                                       obs_weeks = 7)
#
# ggplot2::ggsave(here::here(root_folder,
#                            "all-states-one-week.png"),
#                 all_states_one_week, width = 18, height = 65,
#                 limitsize = FALSE)
#
# ggplot2::ggsave("~/Deskstop/test.png", test,
#                 width = 10, height = 20)



## save plots for Appendix

for (m in settings$model_names_eval) {
  man_col = settings$manual_colours[settings$model_names_eval == m]

  p <- plot_forecasts(states = settings$states_included,
                                     models = m,
                                     forecasts = forecasts,
                                     facet_formula =  ~ state,
                                     ncol_facet = 4,
                                     horizons = c(1),
                                     obs_weeks = 16,
                                     manual_colours = man_col) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                       hjust=1))

  p <- shift_legend3(p)

  ggplot2::ggsave(here::here(root_folder,
                             paste0("APPENDIX-", m, "-forecasts.png")),
                  p, width = 12, height = 5)

}














# ==============================================================================
# Summarised scores and overall performance
# ==============================================================================

scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model",
                                                        "state", "horizon", "forecast_date"))

models <- scores %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE)) %>%
  dplyr::pull(model) %>%
  unique()
model_levels <- levels(models)

settings$model_levels <- model_levels

summarised_scores <- scores %>%
  dplyr::mutate(log_interval_score = log(interval_score),
                abs_bias = abs(bias),
                penalty = is_underprediction + is_overprediction) %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(log_interval_score = mean(log_interval_score),
                   interval_score = mean(interval_score),
                   coverage_deviation = mean(coverage_deviation),
                   bias = mean(bias),
                   abs_bias = mean(abs_bias),
                   overprediction = mean(is_overprediction),
                   underprediction = mean(is_underprediction),
                   sharpness = mean(sharpness),
                   calibration = mean(calibration),
                   penalty = mean(penalty)) %>%
  dplyr::arrange(interval_score)

tmp <- summarised_scores %>%
  dplyr::select(-calibration) %>%
  dplyr::mutate(log_interval_score_scaled = (log_interval_score - min(log_interval_score)) / sd(log_interval_score),
                interval_score_scaled = (interval_score - min(interval_score)) / sd(interval_score),
                coverage_deviation_scaled = (coverage_deviation) / sd(coverage_deviation),
                bias_scaled = (bias) / sd(bias),
                underprediction_scaled = (underprediction - min(underprediction)) / sd(underprediction),
                overprediction_scaled = (overprediction - min(overprediction) )/ sd(overprediction),
                abs_bias_scaled = (abs_bias - min(abs_bias)) / (sd(abs_bias)),
                sharpness_scaled = (sharpness - min(sharpness)) / sd(sharpness),
                penalty_scaled = (penalty - min(penalty)) / sd(penalty))


scaled <- tmp %>%
  tidyr::pivot_longer(cols = c(log_interval_score_scaled, interval_score_scaled,
                               coverage_deviation_scaled,
                               underprediction_scaled, overprediction_scaled,
                               bias_scaled, abs_bias_scaled, sharpness_scaled,
                               penalty_scaled),
                      values_to = "value_scaled") %>%
  dplyr::select(model, value_scaled, name) %>%
  dplyr::mutate(name = gsub("_scaled", "", name))

normal <- tmp %>%
  tidyr::pivot_longer(cols = c(log_interval_score, interval_score,
                               overprediction, underprediction,
                               coverage_deviation, bias, abs_bias, sharpness,
                               penalty),
                      values_to = "value") %>%
  dplyr::select(model, value, name)


df <- dplyr::inner_join(normal, scaled) %>%
  dplyr::mutate(model = forcats::fct_relevel(model,
                                             settings$model_levels))


coloured_table <- df %>%
  dplyr::mutate(name = factor(name,
                              levels = c("interval_score", "log_interval_score",
                                         "sharpness",
                                         "overprediction", "underprediction",
                                         "penalty",
                                         "bias", "abs_bias",
                                         "coverage_deviation"))) %>%
  dplyr::arrange(name) %>%
  ggplot2::ggplot(ggplot2::aes(y = model, x = name)) +
  #ggplot2::geom_tile(fill = "blue") +
  ggplot2::geom_tile(ggplot2::aes(fill = value_scaled), colour = "white") +
  ggplot2::geom_text(ggplot2::aes(y = model, label = round(value, 2))) +
  ggplot2::scale_fill_gradient2(low = "steelblue", high = "salmon") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::coord_cartesian(expand=FALSE)


ggplot2::ggsave(here::here(root_folder,
                           "coloured-summarised-scores.png"),
                coloured_table, width = 10, height = 5)








# ------------------------------------------------------------------------------
# Random effects model regression

unsummarised_scores <- scoringutils::eval_forecasts(full,
                                                    by = c("forecast_date",
                                                           "target_end_date",
                                                           "model", "state", "horizon"),
                                                    interval_score_arguments = list(weigh = TRUE),
                                                    quantiles = c(0.05, 0.95, 0.25, 0.75, 0.4, 0.6, 0.5),
                                                    summarise_by = c("model", "horizon",
                                                                     "state", "forecast_date"))

library(magrittr)
unsummarised_scores <- unsummarised_scores %>%
  dplyr::mutate(log_scores = log(interval_score),
                state = as.factor(state),
                model = as.factor(model),
                model = forcats::fct_reorder(model, interval_score,
                                             .fun = "mean"),
                model = relevel(model, ref = "COVIDhub-ensemble"),
                forecast_date = as.factor(forecast_date)) %>%
  dplyr::filter(is.finite(log_scores))


fit <- lmerTest::lmer(log_scores ~ model + horizon
                      + (1|state) + (1|forecast_date),
                      data = unsummarised_scores)

saveRDS(fit, paste0(root_folder, "/random-effects-model.RDS"))







# ------------------------------------------------------------------------------
# plot with random locations
random_locations <- lme4::ranef(fit, drop = TRUE)$state %>%
  tibble::enframe(name = "state") %>%
  dplyr::mutate(state = as.factor(state),
                state = forcats::fct_reorder(state,
                                             value,
                                             .fun = "mean")) %>%
  ggplot2::ggplot(ggplot2::aes(x = state, y = value)) +
  ggplot2::geom_point() +
  ggplot2::labs(y = "Random effect", x = "State") +
  cowplot::theme_cowplot() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                     vjust = 1, hjust = 1))

# plot with random forecast dates

random_forecast_date <- lme4::ranef(fit, drop = TRUE)$forecast_date %>%
  tibble::enframe(name = "forecast_date") %>%
  dplyr::mutate(state = as.factor(forecast_date)) %>%
  ggplot2::ggplot(ggplot2::aes(x = forecast_date, y = value)) +
  ggplot2::geom_point() +
  ggplot2::labs(y = "Random effect", x = "Forecast date") +
  cowplot::theme_cowplot() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                     vjust = 1, hjust = 1))


random_effects <- cowplot::plot_grid(random_locations,
                                     random_forecast_date,
                                     rel_widths = c(1.5, 1),
                                     ncol = 2)


ggplot2::ggsave(here::here(root_folder,
                           "random-effects.png"),
                random_effects,
                width = 10, height = 4)















# ==============================================================================
## Examining the relationship between indivudal metrics
# ==============================================================================



# ------------------------------------------------------------------------------
# Correlation between scores ---------------------------


scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state",
                                                        "horizon"))

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

correlation_map <- scores %>%
  dplyr::mutate(penalty = is_underprediction + is_overprediction,
                "absolute bias" = abs(bias),
                log_interval_score = log(interval_score),
                log_sharpness = log(sharpness)) %>%
  dplyr::filter(is.finite(log_interval_score)) %>%
  dplyr::select(-model, -state, -horizon, -bias, -calibration, -log_sharpness) %>%
  cor() %>%
  round(2) %>%
  get_upper_tri() %>%
  reshape2::melt(na.rm = TRUE) %>%
  dplyr::mutate(Var2 = forcats::fct_rev(Var2)) %>%
  ggplot2::ggplot(ggplot2::aes(x=Var1, y=Var2, fill=value)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(ggplot2::aes(y = Var2, label = value)) +
  ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white",
                                high = "salmon",
                                name = "Correlation",
                                breaks = c(-1, -0.5, 0, 0.5, 1)) +
  cowplot::theme_cowplot() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::coord_cartesian(expand = FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "correlation-map.png"),
                correlation_map, width = 10, height = 4.5)


corr_plot <- scores %>%
  dplyr::mutate(penalty = is_underprediction + is_overprediction,
                "absolute bias" = abs(bias),
                log_interval_score = log(interval_score)) %>%
  dplyr::filter(is.finite(log_interval_score)) %>%
  dplyr::select(-model, -state, -horizon, -bias, -calibration) %>%
  GGally::ggpairs() +
  cowplot::theme_cowplot() +
  ggplot2::theme(panel.spacing = ggplot2::unit(3, "mm"),
                 panel.background = ggplot2::element_rect(fill = "aliceblue"))

ggplot2::ggsave(here::here(root_folder,
                           "corr-plot.png"),
                corr_plot, width = 19, height = 18)









# ------------------------------------------------------------------------------
# Regression of the metrics on WIS ---------------------------

unsum_scores <- scoringutils::eval_forecasts(full,
                                             by = c("model", "state",
                                                    "target_end_date",
                                                    "horizon"),
                                             interval_score_arguments = list(weigh = TRUE),
                                             summarise_by = c("model", "state",
                                                              "horizon"))

saveRDS(unsum_scores, paste0(root_folder,"/unsummarised_scores.rds"))

unsum_scores <- readRDS(paste0(root_folder, "/unsummarised_scores.rds")) %>%
  dplyr::mutate(penalty = is_overprediction + is_underpredictio8n,
                log_scores = log(interval_score),
                abs_bias_std = (abs(bias) - mean(abs(bias))) / sd(abs(bias)),
                coverage_deviation_std = (coverage_deviation -
                                            mean(coverage_deviation)) /sd(coverage_deviation),
                sharpness_std = (sharpness - mean(sharpness))/sd(sharpness),
                penalty_std = (penalty - mean(penalty)) / sd(penalty)) %>%
  dplyr::filter(is.finite(log_scores))


fit <- lm(log_scores ~ abs_bias_std + coverage_deviation_std + sharpness_std + penalty_std,
   data = unsum_scores)

saveRDS(fit, here::here(root_folder, "regression-wis.rds"))









# ------------------------------------------------------------------------------
# contributions from sharpness, over, underprediction

scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon"))

wis_contribution <- scores %>%
  dplyr::mutate(model = forcats::fct_relevel(model,
                                             settings$model_levels),
                model = forcats::fct_rev(model)) %>%
  ggplot2::ggplot(ggplot2::aes(x = horizon, group = model)) +
  ggplot2::geom_linerange(ggplot2::aes(ymax = is_underprediction,
                                       ymin = 0, colour = "Underprediction"),
                          size = 3) +
  ggplot2::geom_linerange(ggplot2::aes(ymax = is_overprediction + is_underprediction,
                                       ymin = is_underprediction,
                                       colour = "Overprediction"),
                          size = 3)  +
  ggplot2::geom_linerange(ggplot2::aes(ymax = sharpness + is_overprediction + is_underprediction,
                                       ymin = is_overprediction + is_underprediction,
                                       colour = "Sharpness"),
                          size = 3) +
  ggplot2::facet_wrap(~ model, nrow = 2) +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "Horizon", y = "Contribution to Weighted Interval Score") +
  ggplot2::theme(legend.position = "bottom",
                 panel.spacing = ggplot2::unit(4, "mm"))

ggplot2::ggsave(here::here(root_folder,
                           "wis-contributions.png"),
                wis_contribution, width = 13, height = 6.5)









# ------------------------------------------------------------------------------
# relative contributions from sharpness, over, underprediction

# scores <- scoringutils::eval_forecasts(full,
#                                        by = c("model", "state",
#                                               "target_end_date",
#                                               "horizon"),
#                                        interval_score_arguments = list(weigh = TRUE),
#                                        summarise_by = c("model", "horizon"))
#
# man_colours <- c(RColorBrewer::brewer.pal(3, name = "Set1")[2],
#                  RColorBrewer::brewer.pal(3, name = "Set1")[1],
#                  RColorBrewer::brewer.pal(3, name = "Set1")[3])
#
# wis_contribution_rel_horizon <- scores %>%
#   dplyr::mutate(model = forcats::fct_relevel(model,
#                                              settings$model_levels),
#                 model = forcats::fct_rev(model)) %>%
#   dplyr::select(model,horizon, is_underprediction, is_overprediction, sharpness) %>%
#   tidyr::pivot_longer(cols = c(sharpness, is_underprediction, is_overprediction)) %>%
#     dplyr::mutate(name = forcats::fct_relevel(name,
#                                               c("is_underprediction",
#                                                 "is_overprediction",
#                                                 "sharpness"))) %>%
#     dplyr::arrange(name) %>%
#   ggplot2::ggplot(ggplot2::aes(x = horizon, y = value, group = model,
#                                fill = name)) +
#   ggplot2::geom_bar(position = "fill", stat = "identity") +
#   ggplot2::facet_wrap(~ model, nrow = 2) +
#   ggplot2::scale_fill_manual(values = man_colours,
#                              name = "Colour") +
#   cowplot::theme_cowplot() +
#   ggplot2::labs(x = "Horizon", y = "Contribution to Weighted Interval Score") +
#   ggplot2::theme(legend.position = "bottom",
#                  panel.spacing = ggplot2::unit(4, "mm"))
#
# ggplot2::ggsave(here::here(root_folder,
#                            "wis-contributions-relative.png"),
#                 wis_contribution_rel_horizon, width = 13, height = 6.5)


#
#
# scores <- scoringutils::eval_forecasts(full,
#                                        by = c("model", "state",
#                                               "target_end_date",
#                                               "horizon"),
#                                        interval_score_arguments = list(weigh = TRUE),
#                                        summarise_by = c("model", "state"))
#
#
#
# wis_contribution_rel_state <- scores %>%
#   dplyr::mutate(model = forcats::fct_relevel(model,
#                                              settings$model_levels),
#                 model = forcats::fct_rev(model)) %>%
#   dplyr::select(model,state, is_underprediction, is_overprediction, sharpness) %>%
#   tidyr::pivot_longer(cols = c(sharpness, is_underprediction, is_overprediction)) %>%
#   dplyr::mutate(name = forcats::fct_relevel(name,
#                                             c("is_underprediction",
#                                               "is_overprediction",
#                                               "sharpness"))) %>%
#   dplyr::arrange(name) %>%
#   ggplot2::ggplot(ggplot2::aes(x = state, y = value, group = model,
#                                fill = name)) +
#   ggplot2::geom_bar(position = "fill", stat = "identity") +
#   ggplot2::facet_wrap(~ model, nrow = 2) +
#   ggplot2::scale_fill_manual(values = man_colours,
#                              name = "Colour") +
#   cowplot::theme_cowplot() +
#   ggplot2::labs(x = "Horizon", y = "Contribution to Weighted Interval Score") +
#   ggplot2::theme(legend.position = "bottom",
#                  panel.spacing = ggplot2::unit(4, "mm"),
#                  axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
#                                                      hjust=1))
#
# ggplot2::ggsave(here::here(root_folder,
#                            "wis-contributions-rel-state.png"),
#                 wis_contribution_rel_state, width = 13, height = 6.5)
#
#
#
#
#
# wis_contribution_rel_state2 <- scores %>%
#   dplyr::mutate(model = forcats::fct_relevel(model,
#                                              settings$model_levels),
#                 model = forcats::fct_rev(model),
#                 state = forcats::fct_reorder(state,
#                                              interval_score,
#                                              .fun='mean',
#                                              .desc = TRUE)) %>%
#   dplyr::select(model,state, is_underprediction, is_overprediction, sharpness) %>%
#   tidyr::pivot_longer(cols = c(sharpness, is_underprediction, is_overprediction)) %>%
#   dplyr::mutate(name = forcats::fct_relevel(name,
#                                             c("is_underprediction",
#                                               "is_overprediction",
#                                               "sharpness"))) %>%
#   dplyr::arrange(name) %>%
#   ggplot2::ggplot(ggplot2::aes(x = model, y = value, group = model,
#                                fill = name)) +
#   ggplot2::geom_bar(position = "fill", stat = "identity") +
#   ggplot2::facet_wrap(~ state, nrow = 2) +
#   ggplot2::scale_fill_manual(values = man_colours,
#                              name = "Colour") +
#   cowplot::theme_cowplot() +
#   ggplot2::coord_flip() +
#   ggplot2::labs(x = "Model", y = "Contribution to Weighted Interval Score") +
#   ggplot2::theme(legend.position = "bottom",
#                  panel.spacing = ggplot2::unit(4, "mm"),
#                  axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
#                                                      hjust=1))
#
# ggplot2::ggsave(here::here(root_folder,
#                            "wis-contributions-rel-state2.png"),
#                 wis_contribution_rel_state2, width = 13, height = 6.5)











# WIS contributions by states
#
# scores <- scoringutils::eval_forecasts(full,
#                                        by = c("model", "state",
#                                               "target_end_date",
#                                               "horizon"),
#                                        interval_score_arguments = list(weigh = TRUE),
#                                        summarise_by = c("model", "state"))
#
# wis_contribution_state <- scores %>%
#   dplyr::mutate(model = forcats::fct_relevel(model,
#                                              settings$model_levels),
#                 model = forcats::fct_rev(model)) %>%
#   ggplot2::ggplot(ggplot2::aes(x = state, group = model)) +
#   ggplot2::geom_linerange(ggplot2::aes(ymax = is_underprediction,
#                                        ymin = 0, colour = "Underprediction"),
#                           size = 3) +
#   ggplot2::geom_linerange(ggplot2::aes(ymax = is_overprediction + is_underprediction,
#                                        ymin = is_underprediction,
#                                        colour = "Overprediction"),
#                           size = 3)  +
#   ggplot2::geom_linerange(ggplot2::aes(ymax = sharpness + is_overprediction + is_underprediction,
#                                        ymin = is_overprediction + is_underprediction,
#                                        colour = "Sharpness"),
#                           size = 3) +
#   ggplot2::facet_wrap(~ model, nrow = 2) +
#   cowplot::theme_cowplot() +
#   ggplot2::labs(x = "Horizon", y = "Contribution to Weighted Interval Score") +
#   ggplot2::theme(legend.position = "bottom",
#                  panel.spacing = ggplot2::unit(4, "mm"))
#
# ggplot2::ggsave(here::here(root_folder,
#                            "wis-contributions.png"),
#                 wis_contribution, width = 13, height = 6.5)










# ------------------------------------------------------------------------------
# range plots -----------------------------------


# WIS by range
summarised_scores <- scoringutils::eval_forecasts(full,
                                                  by = c("forecast_date",
                                                         "target_end_date",
                                                         "model", "state", "horizon"),
                                                  interval_score_arguments = list(weigh = TRUE),
                                                  summarise_by = c("model", "range"))


wis_overview_plot <- score_overview_plot(summarised_scores)

ggplot2::ggsave(here::here(root_folder,
                           "scores-by-range.png"),
                wis_overview_plot, width = 10, height = 5)




## Penalty by range plot
scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "range"))


scores <- scores %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean'),
                penalty = is_overprediction + is_underprediction) %>%
  dplyr::arrange(model)

penalty_by_range <- scores %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               y = penalty,
                               colour = range)) +
  ggplot2::geom_hline(yintercept = 0,
                      linetype = "dashed",
                      alpha = 0.5,
                      size = 0.5) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(ggplot2::aes(group = range),
                     colour = "black",
                     size = 0.01) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_continuous(low = "steelblue", high = "salmon") +
  ggplot2::theme(legend.position = "right",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::labs(y = "Penalty",
                x = "Model")

ggplot2::ggsave(here::here(root_folder,
                           "penalty-by-range.png"),
                penalty_by_range, width = 10, height = 5)






## coverage deviation by range plot
coverage_deviation_by_range <- scores %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               y = coverage_deviation,
                               colour = range)) +
  ggplot2::geom_hline(yintercept = 0,
                      linetype = "dashed",
                      alpha = 0.5,
                      size = 0.5) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(ggplot2::aes(group = range),
                     colour = "black",
                     size = 0.01) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_continuous(low = "steelblue", high = "salmon") +
  ggplot2::theme(legend.position = "right",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::labs(y = "Coverage Deviation",
                x = "")

ggplot2::ggsave(here::here(root_folder,
                           "coverage-deviation-by-range.png"),
                coverage_deviation_by_range, width = 10, height = 5)





## sharpness by range plot
summarised_scores <- summarised_scores %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean')) %>%
  dplyr::arrange(model)

sharpness_by_range <- summarised_scores %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               y = sharpness,
                               colour = range)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(ggplot2::aes(group = range),
                     colour = "black",
                     size = 0.01) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_continuous(low = "steelblue", high = "salmon") +
  ggplot2::theme(legend.position = "right",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::labs(y = "Sharpness",
                x = "")

ggplot2::ggsave(here::here(root_folder,
                           "sharpness-by-range.png"),
                sharpness_by_range, width = 10, height = 4.6)





# combine to one range plot

range_plot <- cowplot::plot_grid(wis_overview_plot +
                                   ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
                                   ggplot2::labs(x = ""),
                                 penalty_by_range +
                                   ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
                                   ggplot2::labs(x = ""),
                                 sharpness_by_range +
                                   ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
                                   ggplot2::labs(x = ""),
                                 coverage_deviation_by_range,
                                 ncol = 1,
                                 rel_heights = c(1, 1, 1, 1.8))


ggplot2::ggsave(here::here(root_folder,
                           "all-range-plots.png"),
                range_plot, width = 10, height = 14)




# ==============================================================================
## Identifying External drviers in WIS
# ==============================================================================

# WIS by the number of deaths in a state

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE))

df <-
  scores %>%
  dplyr::group_by(state, model) %>%
  dplyr::summarise(wis = median(interval_score),
                   mean_wis = mean(interval_score),
                   deaths = mean(true_values))

wis_vs_deaths <- df %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(median_wis = median(wis)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(state = forcats::fct_reorder(state,
                                             deaths,
                                             "mean")) %>%
  ggplot2::ggplot(ggplot2::aes(x = deaths, y = wis, colour = state)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(ggplot2::aes(y = median_wis),
                      colour = "black",
                      shape = 18,
                      size = 3) +
  ggplot2::scale_y_log10() +
  ggplot2::scale_x_log10() +
  ggplot2::geom_smooth(method='lm', colour = "steelblue", alpha = 0.2) +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "Average Death Number", y = "Weighted Interval Score")

ggplot2::ggsave(here::here(root_folder,
                           "wis-vs-deaths.png"),
                wis_vs_deaths,
                width = 10, height = 4)


# linear model fit
fit <- lm(log(wis) ~ log(deaths), data = df)
summary(fit)

fit_without_baseline <- lm(log(wis) ~ log(deaths),
                           data = df %>%
                             dplyr::filter(model != "COVIDhub-baseline"))
summary(fit_without_baseline)




# difficulty plot
# difficulty_plot <- df %>%
#   dplyr::mutate(difficulty = log(wis) -
#                   (fit$coefficients[1] + log(deaths) * fit$coefficients[2])) %>%
#   dplyr::group_by(state) %>%
#   dplyr::mutate(median_difficulty = median(difficulty),
#                 mean_difficulty = mean(difficulty)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(state = factor(state),
#                 state = forcats::fct_reorder(state,
#                                              median_difficulty,
#                                              .fun='median')) %>%
#   ggplot2::ggplot(ggplot2::aes(x = state, y = difficulty, colour = state)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_point(ggplot2::aes(y = median_difficulty),
#                       colour = "black",
#                       shape = 18,
#                       size = 3) +
#   ggplot2::geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
#   cowplot::theme_cowplot() +
#   ggplot2::labs(x = "State", y = "Difficulty") +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
#                                                      hjust=1),
#                  legend.position = "none")
#
# ggplot2::ggsave(here::here(root_folder,
#                            "difficulty-states.png"),
#                 difficulty_plot,
#                 width = 10, height = 4)


# difficulty heatmap
difficulty_states_heatmap <- df %>%
  dplyr::mutate(difficulty = log(wis) -
                  (fit$coefficients[1] + log(deaths) * fit$coefficients[2])) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(median_difficulty = median(difficulty),
                mean_difficulty = mean(difficulty),
                diff_scaled = difficulty / sd(difficulty)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(state = factor(state),
                state = forcats::fct_reorder(state,
                                             median_difficulty,
                                             .fun='median'),
                model = forcats::fct_relevel(model,
                                             settings$model_levels)) %>%
  ggplot2::ggplot(ggplot2::aes(y = model,
                               x = state,
                               fill = difficulty)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = round(difficulty, 2)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red") +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "", x = "") +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::coord_cartesian(expand = FALSE)


ggplot2::ggsave(here::here(root_folder,
                           "difficulty-states-heatmap.png"),
                difficulty_states_heatmap,
                width = 10, height = 4.8)


# # Examining difficulty in a regression framework - failed
# scores <- scoringutils::eval_forecasts(full,
#                                        by = c("forecast_date",
#                                               "target_end_date",
#                                               "model", "state", "horizon"),
#                                        interval_score_arguments = list(weigh = TRUE))
#
#
#
# df <-
#   scores %>%
#   dplyr::mutate(state = as.factor(state),
#                 state = relevel(state, ref = "US")) %>%
#   dplyr::group_by(state, model, horizon) %>%
#   dplyr::summarise(wis = median(interval_score),
#                    mean_wis = mean(interval_score),
#                    deaths = mean(true_values))
#
#
# fit <- lm(log(wis) ~ log(deaths) + state + horizon, data = df)
# summary(fit)
# fit$coefficients


# calibration per state
scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state"))

scores <- scores %>%
  dplyr::arrange(state, model, interval_score) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(cov_scaled = (coverage_deviation) / sd(coverage_deviation)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE),
                state = forcats::fct_reorder(state,
                                             coverage_deviation,
                                             .fun='mean',
                                             .desc = TRUE))

heatmap_plot_coverage <- ggplot2::ggplot(scores,
                                         ggplot2::aes(y = model,
                                                      x = state,
                                                      fill = cov_scaled)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = round(coverage_deviation, 1)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red") +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "", x = "") +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::coord_cartesian(expand = FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "heatmap-model-coverage.png"),
                heatmap_plot_coverage, width = 10, height = 4.8)





Texas_forecast_one_weeks <- plot_forecasts(states = "Texas",
                                           forecasts = forecasts,
                                           facet_formula = model ~ horizon,
                                           ncol_facet = 4,
                                           horizons = c(1),
                                           obs_weeks = 13) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1),
                 legend.position = "none") +
  ggplot2::labs(x = "")

ggplot2::ggsave(here::here(root_folder,
                           "Texas-one-week.png"),
                Texas_forecast_one_weeks, width = 11, height = 4.8)
#
#
# NY_forecast_one_weeks <- plot_forecasts(states = "New York",
#                                         forecasts = forecasts,
#                                         facet_formula = model ~ horizon,
#                                         ncol_facet = 4,
#                                         horizons = c(1),
#                                         obs_weeks = 13) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
#                                                      hjust=1),
#                  legend.position = "none")
#
# ggplot2::ggsave(here::here(root_folder,
#                            "NY-one-week.png"),
#                 N>_forecast_one_weeks, width = 10, height = 4.8)





# heatmap model bias per state for the APPENDIX

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state"))

scores <- scores %>%
  dplyr::arrange(state, model, interval_score) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(bias_scaled = (bias) / sd(bias)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE),
                state = forcats::fct_reorder(state,
                                             bias,
                                             .fun='mean',
                                             .desc = TRUE))

heatmap_plot_bias <- ggplot2::ggplot(scores,
                                     ggplot2::aes(y = model,
                                                  x = state,
                                                  fill = bias_scaled)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = round(bias, 2)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red") +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "", x = "") +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::coord_cartesian(expand = FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "heatmap-model-bias.png"),
                heatmap_plot_bias, width = 10, height = 4.8)





#
# # penalty plot
# scores <- scores %>%
#   dplyr::arrange(state, model, interval_score) %>%
#   dplyr::group_by(state) %>%
#   dplyr::mutate(penalty = is_overprediction + is_underprediction,
#                 penalty_scaled = (penalty - min(penalty)) / sd(penalty)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(model = forcats::fct_reorder(model,
#                                              interval_score,
#                                              .fun='mean',
#                                              .desc = TRUE),
#                 state = forcats::fct_reorder(state,
#                                              penalty,
#                                              .fun='mean',
#                                              .desc = TRUE))
#
# heatmap_plot_penalty <- ggplot2::ggplot(scores,
#                                      ggplot2::aes(y = model,
#                                                   x = state,
#                                                   fill = penalty_scaled)) +
#   ggplot2::geom_tile() +
#   ggplot2::geom_text(ggplot2::aes(label = round(penalty, 2))) +
#   ggplot2::scale_fill_gradient2(low = "skyblue", high = "red") +
#   cowplot::theme_cowplot() +
#   ggplot2::labs(y = "", x = "Location") +
#   ggplot2::theme(legend.position = "none",
#                  axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
#                                                      hjust=1)) +
#   ggplot2::coord_cartesian(expand = FALSE)
#
# ggplot2::ggsave(here::here(root_folder,
#                            "heatmap-model-bias.png"),
#                 heatmap_plot_bias, width = 10, height = 4.8)













# ==============================================================================
## Understanding model characteristics that drive differences in WIS
# ==============================================================================



# ------------------------------------------------------------------------------
# heatmap plot with all scores in all states -----------------------------------

forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                   models = settings$model_names_eval)

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))

full <- prepare_for_scoring(forecasts)


scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state"))

scores <- scores %>%
  dplyr::arrange(state, model, interval_score) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(rank = rank(interval_score, ties.method = "average"),
                multiple = pmin(interval_score / min(interval_score), 5) -1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE),
                state = forcats::fct_reorder(state,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE))

heatmap_plot <- ggplot2::ggplot(scores, ggplot2::aes(y = model, x = state, fill = multiple)) +
  ggplot2::geom_tile(colour = "white", size = 0.3) +
  ggplot2::geom_text(ggplot2::aes(label = round(interval_score, 1)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red",
                                name = "",
                                breaks = seq(1, 15, 3)) +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "", x = "") +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::coord_cartesian(expand = FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "heatmap-model-scores.png"),
                heatmap_plot, width = 10, height = 5)







# ------------------------------------------------------------------------------
# bias over horizons

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon"),
                                       quantiles = c(0.05, 0.15, 0.25, 0.4, 0.5,
                                                     0.6, 0.75, 0.85, 0.95))

bias_horizons <- scores %>%
  dplyr::mutate(model = forcats::fct_relevel(model,
                                             settings$model_levels[11:1]),
                horizon = as.factor(horizon)) %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               colour = model)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.05, ymax = bias_0.95),
                          size = 2,
                          alpha = 0.1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.25, ymax = bias_0.75),
                          size = 1,
                          alpha = 0.4,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.4, ymax = bias_0.6),
                          size = 2,
                          alpha = 1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::scale_color_manual(values = ordered_manual_colours) +
  ggplot2::geom_point(ggplot2::aes(y = bias),
                      size = 2,
                      colour = "black",
                      fill = "white",
                      position = ggplot2::position_dodge2(width = 0.5,
                                                          padding = 0)) +
  ggplot2::geom_point(ggplot2::aes(y = bias_0.5),
                      size = 2.2,
                      shape = 23,
                      fill = "white",
                      colour = "black",
                      position = ggplot2::position_dodge2(width = 0.5,
                                                          padding = 0)) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed", alpha = 0.5) +
  ggplot2::expand_limits(y = 0) +
  ggplot2::facet_grid(NULL) +
  ggplot2::labs(x = "", y = "Bias") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "none",
                 panel.background = element_rect(fill = "aliceblue"),
                 axis.text.x = ggplot2::element_blank())

ggplot2::ggsave(here::here(root_folder,
                           "bias-horizons.png"),
                bias_horizons,
                width = 10, height = 3.5)



# ------------------------------------------------------------------------------
# Looking at over- and underprediction

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon"),
                                       quantiles = c(0.05, 0.15, 0.25, 0.4, 0.5,
                                                     0.6, 0.75, 0.85, 0.95))


prediction_penalties <- scores %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean'),
                horizon = as.factor(horizon)) %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               colour = model)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour ="dark grey") +
  ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = (is_overprediction)),
                          position = ggplot2::position_dodge2(width = 0.5,
                                                          padding = 0),
                          colour = "salmon") +
  ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = -(is_underprediction)),
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0),
                          colour = "steelblue") +
  ggplot2::coord_cartesian(ylim = c(-200, 200)) +
  ggplot2::labs(x = "", y = "Over-/underprediction penalties") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))

ggplot2::ggsave(here::here(root_folder,
                           "prediction-penalties.png"),
                prediction_penalties,
                width = 10, height = 5)


bias_together <- cowplot::plot_grid(bias_horizons,
                                    prediction_penalties,
                                    ncol = 1,
                                    rel_heights = c(1, 1.5))

ggplot2::ggsave(here::here(root_folder,
                           "prediction-penalties-and-bias.png"),
                bias_together,
                width = 10, height = 6)

# ------------------------------------------------------------------------------
# Bias example for the ensemble

forecasts_ensemble <- load_submission_files(dates = settings$evaluation_dates,
                                            models = c("COVIDhub-ensemble", "crps-ensemble",
                                                       "mean-ensemble", "qra-ensemble"))

forecasts_ensemble <- filter_forecasts(forecasts_ensemble,
                                       locations = settings$locations_included,
                                       horizons = c(1, 2, 3, 4))

full_ensemble <- prepare_for_scoring(forecasts_ensemble)

scores_ensemble <- scoringutils::eval_forecasts(full_ensemble,
                                                by = c("model", "state",
                                                       "target_end_date",
                                                       "horizon"),
                                                interval_score_arguments = list(weigh = TRUE),
                                                summarise_by = c("model", "state",
                                                                 "target_end_date", "horizon")) %>%
  dplyr::filter(horizon == 1)


high_wis_states <- scores_ensemble %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(interval_score = sum(interval_score)) %>%
  dplyr::arrange(-interval_score) %>%
  dplyr::pull(state)


plot_ensemble <- plot_forecasts(forecasts = forecasts_ensemble,
                                states = high_wis_states[1:6],
                                models = c("COVIDhub-ensemble", "crps-ensemble",
                                           "mean-ensemble", "qra-ensemble"),
                                ncol_facet = 4,
                                grid = TRUE,
                                facet_formula = state ~ model,
                                obs_weeks = 13) +
  ggplot2::theme(legend.position = "none",
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0))) +
  ggplot2::labs(x = "")

bias_ensemble <- ggplot2::ggplot() +
  ggplot2::geom_point(data = scores_ensemble %>%
                        dplyr::filter(state %in% high_wis_states[1:6]),
                      ggplot2::aes(y = bias,
                                   x = target_end_date),
                      colour = "black") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(x = "", y = "Bias",
                #caption = paste0("Mean bias is ", round(mean(scores$bias), 3)),
                col = "Model", fill = "Model") +
  ggplot2::facet_grid(state ~ model) +
  ggplot2::expand_limits(x = c(as.Date("2020-05-23"), as.Date("2020-08-22"))) +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 panel.background = ggplot2::element_rect(fill = "aliceblue"),
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0)))

bias_plot_combined_ensemble <- cowplot::plot_grid(plot_ensemble,
                                                  bias_ensemble, ncol = 1,
                                                  rel_heights = c(2, 1.7),
                                                  scale = c(1, 1),
                                                  align = 'v')

ggplot2::ggsave(here::here(root_folder,
                           "bias_ensemble.png"),
                bias_plot_combined_ensemble,
                width = 10, height = 14)







# ------------------------------------------------------------------------------
# empirical coverage for all -----------------------------

# combined interval and quantile coverage
scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "range"))


combined <- combine_with_deaths(forecasts)



models <- as.character(settings$model_levels)[11:1]

plot_list <- list()
i <- 1
for (m in models) {

  df <- scores %>%
    dplyr::filter(model %in% m)

  man_col <- settings$manual_colours[m == settings$model_names_eval]

  ## overall model calibration - empirical interval coverage
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = range, colour = model)) +
    ggplot2::geom_polygon(data = data.frame(x = c(0, 0, 100),
                                            y = c(0, 100, 100),
                                            g = c("o", "o", "o")),
                          ggplot2::aes(x = x, y = y, group = g,
                                       fill = g),
                          alpha = 0.05,
                          colour = "white",
                          fill = "olivedrab3") +
    ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                       linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
    cowplot::theme_cowplot() +
    ggplot2::scale_color_manual(values = man_col) +
    ggplot2::facet_wrap(~ model, ncol = 3) +
    ggplot2::theme(legend.position = "none",
                   panel.spacing = unit(5, "mm"),
                   plot.margin = ggplot2::margin(t = 6, r = 5,
                                                 b = 6, l = 4, unit = "mm")) +
    ggplot2::ylab("% Obs inside interval") +
    ggplot2::xlab("Interval range") +
    ggplot2::coord_cartesian(expand = FALSE)


  df2 <- combined %>%
    dplyr::filter(model %in% m)

  p2 <- df2  %>%
    dplyr::group_by(model, quantile) %>%
    dplyr::summarise(coverage = mean(deaths <= value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = quantile, colour = model)) +
    ggplot2::geom_polygon(data = data.frame(x = c(0, 0.5, 0.5,
                                                  0.5, 0.5, 1),
                                            y = c(0, 0, 0.5,
                                                  0.5, 1, 1),
                                            g = c("o", "o", "o")),
                          ggplot2::aes(x = x, y = y, group = g,
                                       fill = g),
                          alpha = 0.05,
                          colour = "white",
                          fill = "olivedrab3") +
    ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                       linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = coverage)) +
    ggplot2::scale_color_discrete(man_col) +
    ggplot2::facet_wrap(~ model, ncol = 3) +
    cowplot::theme_cowplot() +
    ggplot2::theme(panel.spacing = unit(5, "mm"),
                   legend.position = "none",
                   plot.margin = ggplot2::margin(t = 6, r = 9,
                                                 b = 6, l = 0, unit = "mm")) +
    ggplot2::xlab("Quantile") +
    ggplot2::ylab("% obs below quantile") +
    ggplot2::coord_cartesian(expand = FALSE)

  plot_list[[i]] <- cowplot::plot_grid(p1, p2,
                                       ncol = 2)
  i <- i + 1
}

coverage_plots <- cowplot::plot_grid(plotlist = plot_list,
                                     ncol = 2)

ggplot2::ggsave(here::here(root_folder,
                           "coverage-plots.png"),
                coverage_plots,
                width = 10, height = 15)








# ------------------------------------------------------------------------------
# interval coverage and quantile coverage over horizons
scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon", "range"))

plot_list <- list()

models <- as.character(settings$model_levels)[11:1]
i <- 1
for (m in models) {

  df <- scores %>%
    dplyr::filter(model %in% m)

  man_col <- settings$manual_colours[m == settings$model_names_eval]

  ## overall model calibration - empirical interval coverage
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = horizon,
                                             y = calibration,
                                             colour = range,
                                             group = range)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = range / 100, colour = range),
                        linetype = "dashed",
                        alpha = 0.5,
                        size = 0.25) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = horizon),
                        colour = "grey",
                        linetype = "dashed",
                        alpha = 0.3,
                        size = 0.25) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    ggplot2::facet_wrap(~ model, ncol = 3) +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(t = 4, r = 5,
                                                 b = 4, l = 4, unit = "mm")) +
    ggplot2::xlab("Forecast horizon") +
    ggplot2::ylab("Interval coverage")


  df2 <- combined %>%
    dplyr::filter(model %in% m)

  p2 <- df2 %>%
    dplyr::group_by(model, quantile, horizon) %>%
    dplyr::summarise(coverage = mean(deaths <= value)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = horizon,
                                 y = coverage,
                                 colour = quantile,
                                 group = quantile)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = quantile, colour = quantile),
                        linetype = "dashed",
                        alpha = 0.5,
                        size = 0.25) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = horizon),
                        colour = "grey",
                        linetype = "dashed",
                        alpha = 0.3,
                        size = 0.25) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    ggplot2::facet_wrap(~ model, ncol = 3) +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(t = 4, r = 9,
                                                 b = 4, l = 0, unit = "mm")) +
    ggplot2::xlab("Forecast horizon") +
    ggplot2::ylab("Quantle coverage")

  plot_list[[i]] <- cowplot::plot_grid(p1, p2,
                                       ncol = 2)
  i <- i + 1
}

coverage_plots_horizon <- cowplot::plot_grid(plotlist = plot_list,
                                     ncol = 2)

ggplot2::ggsave(here::here(root_folder,
                           "APPENDIX-coverage-plots-horizons.png"),
                coverage_plots_horizon,
                width = 14.5, height = 17.5)




# ------------------------------------------------------------------------------
# Ensemble - Coverage example

man_col <- settings$manual_colours[settings$model_names_eval %in% c("COVIDhub-ensemble", "crps-ensemble",
                                                                    "mean-ensemble", "qra-ensemble")]

full_ensemble <- full %>%
  dplyr::filter(model %in% c("COVIDhub-ensemble", "crps-ensemble",
                             "mean-ensemble", "qra-ensemble"))

scores_ensemble <- scoringutils::eval_forecasts(full_ensemble,
                                                by = c("forecast_date",
                                                       "target_end_date",
                                                       "model", "state", "horizon"),
                                                interval_score_arguments = list(weigh = TRUE),
                                                summarise_by = c("model", "range"))


## overall model calibration - empirical interval coverage
interval_coverage_ensemble <- ggplot2::ggplot(scores_ensemble,
                                              ggplot2::aes(x = range,
                                                           colour = model)) +
  ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_manual(values = man_col) +
  # ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylab("Percent observations inside interval range") +
  ggplot2::xlab("Interval range")


# ------------------------------------------------------------------------------
# plot with quantile coverage

forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                   models = settings$model_names_eval)

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))

combined <- combine_with_deaths(forecasts)

combined_ensemble <- combined %>%
  dplyr::filter(model %in% c("COVIDhub-ensemble", "crps-ensemble",
                             "mean-ensemble", "qra-ensemble"))

quantile_coverage_plot_ensemble <- combined_ensemble  %>%
  dplyr::group_by(model, quantile) %>%
  dplyr::summarise(coverage = mean(deaths <= value)) %>%
  ggplot2::ggplot(ggplot2::aes(x = quantile, colour = model)) +
  ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coverage)) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_manual(values = man_col) +
  #ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(legend.position = "right") +
  ggplot2::xlab("Quantile") +
  ggplot2::ylab("Proportion of observations below quantile")


coverage_ensemble <- cowplot::plot_grid(interval_coverage_ensemble,
                                        quantile_coverage_plot_ensemble,
                                        rel_widths = c(0.65,1),
                                        ncol = 2)

ggplot2::ggsave(here::here(root_folder,
                           "coverage_ensemble.png"),
                coverage_ensemble,
                width = 10, height = 4)











# ------------------------------------------------------------------------------
# PIT plots

source(here::here("ensembling", "crps-ensemble-metalog", "fit-distribution-functions-metalog.R"))
combined <- combine_with_deaths(forecasts)
fc <- data.table::as.data.table(combined)
n_samples = settings$n_samples
library(rmetalog)
samples <- fc[, .(y_pred = get_samples_metalog(value, quantile, n_samples = n_samples),
                  sample_nr = 1:n_samples,
                  state = unique(state),
                  y_obs = unique(deaths)),
              by = c("model", "forecast_date",
                     "target_end_date",
                     "target", "location")]

df <- samples %>%
  dplyr::rename(predictions = y_pred,
                true_values = y_obs,
                id = target_end_date,
                sample = sample_nr)

pit_plot <- scoringutils::eval_forecasts(df,
                                         by = c("model", "state", "target", "id",
                                                "forecast_date"),
                                         summarise_by = c("model"),
                                         pit_arguments = list(num_bins = 23,
                                                              plot = TRUE),
                                         pit_plots = TRUE)$pit_plots

pit_plot$overall_pit <- NULL

pit_plot2 <- purrr::map2(pit_plot, names(pit_plot),
                         .f = function(.x, .y) {
                           x <- .x +
                             ggplot2::labs(title = .y)
                           return(x)
                         })

all_pit_plots <- cowplot::plot_grid(plotlist = pit_plot2,
                                    ncol = 3)

ggplot2::ggsave(here::here(root_folder,
                           "all-pit-plots.png"),
                all_pit_plots,
                width = 11, height = 9)




# ------------------------------------------------------------------------------
# Sharpness over horizons


scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       quantiles = c(0.05, 0.95, 0.25, 0.75, 0.4, 0.6, 0.5),
                                       summarise_by = c("model", "horizon"))

sharpness_horizons <- scores %>%
  dplyr::mutate(model = forcats::fct_relevel(model,
                                             settings$model_levels[11:1])) %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               colour = model)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = sharpness_0.05,
                                       ymax = sharpness_0.95),
                          size = 2,
                          alpha = 0.1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = sharpness_0.25,
                                       ymax = sharpness_0.75),
                          size = 2,
                          alpha = 0.4,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = sharpness_0.4,
                                       ymax = sharpness_0.6),
                          size = 2,
                          alpha = 1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_point(ggplot2::aes(y = sharpness_0.5),
                      size = 2,
                      shape = 23,
                      fill = "white",
                      colour = "black",
                      position = ggplot2::position_dodge2(width = 0.5,
                                                          padding = 0)) +
  ggplot2::geom_point(ggplot2::aes(y = sharpness),
                      size = 2,
                      shape = 18,
                      colour = "black",
                      position = ggplot2::position_dodge2(width = 0.5,
                                                          padding = 0)) +
  ggplot2::facet_grid(NULL) +
  ggplot2::coord_cartesian(ylim = c(0, 100)) +
  ggplot2::scale_color_manual(values = ordered_manual_colours) +
  # ggplot2::scale_y_continuous(trans = scales::log10_trans()) +
  ggplot2::labs(x = "", y = "Sharpness") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "none",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))

# maybe: make y = horizon, facet_grid ~ model and change the colour such
# that the models are coloured, not the horizons
# log scale on y axis or not?
ggplot2::ggsave(here::here(root_folder,
                           "sharpness-horizons.png"),
                sharpness_horizons,
                width = 12, height = 4.8)





# ------------------------------------------------------------------------------
# Detailed sharpness for crps-ensemble models example

forecasts_ensemble <- load_submission_files(dates = settings$evaluation_dates,
                                            models = c("crps-ensemble"))

forecasts_ensemble <- filter_forecasts(forecasts_ensemble,
                                       locations = settings$locations_included,
                                       horizons = c(1, 2, 3, 4))

full_ensemble <- prepare_for_scoring(forecasts_ensemble)

scores_ensemble <- scoringutils::eval_forecasts(full_ensemble,
                                                by = c("model", "state",
                                                       "target_end_date",
                                                       "forecast_date",
                                                       "horizon"),
                                                interval_score_arguments = list(weigh = TRUE),
                                                summarise_by = c("model", "state",
                                                                 "target_end_date", "horizon")) %>%
  dplyr::filter(horizon == 1)

high_wis_states <- c("US", "Texas", "Florida", "California", "New Jersey", "Arizona",
            "Georgia", "Illinois")

man_col <- settings$manual_colours[settings$model_names_eval == "crps-ensemble"]

plot_ensemble <- plot_forecasts(forecasts = forecasts_ensemble,
                                   states = high_wis_states[1:8],
                                   models = c("crps-ensemble"),
                                   obs_weeks = 13,
                                facet_formula = ~ state,
                                ncol_facet = 4,
                                manual_colours = man_col) +
  ggplot2::theme(legend.position = "none",
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0))) +
  ggplot2::labs(x = "")

sharpness_ensemble_plot <- ggplot2::ggplot() +
  ggplot2::geom_point(data = scores_ensemble %>%
                        dplyr::filter(state %in% high_wis_states[1:8]),
                      ggplot2::aes(y = sharpness,
                                   x = target_end_date),
                      colour = "black") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(x = "", y = "Sharpness",
                col = "Model", fill = "Model") +
  ggplot2::facet_wrap(~ state,
                      ncol = 4,
                      scales = "free_y") +
  ggplot2::expand_limits(x = c(as.Date("2020-05-23"), as.Date("2020-08-22"))) +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0)))

sharpness_plot_combined <- cowplot::plot_grid(plot_ensemble,
                                              sharpness_ensemble_plot,
                                              ncol = 1,
                                              rel_heights = c(2, 1.5),
                                              scale = c(1, 1),
                                              align = 'v')

ggplot2::ggsave(here::here(root_folder,
                           "sharpness-predictions-ensemble.png"),
                sharpness_plot_combined,
                width = 10, height = 7)







# ==============================================================================
# Analysis of the ensemble models
# ==============================================================================


# ------------------------------------------------------------------------------
# Plots with weights over time

if (evaluation_scenario != "ensembles") {
  crps_weights_files <- list.files(here::here("ensembling", "crps-ensemble", "crps-weights"))
  crps_weights_files <- crps_weights_files[grepl(".csv", crps_weights_files)]
  crps_weights_paths <- here::here("ensembling", "crps-ensemble", "crps-weights",
                                   crps_weights_files)
  qra_weights_files <- list.files(here::here("ensembling", "qra-ensemble", "qra-weights"))
  qra_weights_files <- qra_weights_files[grepl(".csv", qra_weights_files)]
  qra_weights_paths <- here::here("ensembling", "qra-ensemble", "qra-weights",
                                  qra_weights_files)

  crps_weights <- purrr::map_dfr(crps_weights_paths, read.csv) %>%
    dplyr::mutate(method = "crps")

  qra_weights <- purrr::map_dfr(qra_weights_paths, read.csv) %>%
    dplyr::mutate(method = "qra")

  weights <- dplyr::bind_rows(crps_weights, qra_weights)

  weights_time <- weights %>%
    dplyr::mutate(model = forcats::fct_reorder(model,
                                               weights,
                                               .fun='mean',
                                               .desc = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(fill = model,
                                 y = weights,
                                 x = forecast_date)) +
    ggplot2::facet_grid(method ~ .) +
    ggplot2::labs(x = "Forecast date", y = "Model weights") +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::theme(#legend.position = "bottom",
      text = ggplot2::element_text(family = "Sans Serif"))

  ggplot2::ggsave(here::here(root_folder,
                             "weights-time.png"),
                  weights_time,
                  width = 10, height = 4)


scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "forecast_date", "horizon"))

weights_and_scores <- dplyr::inner_join(weights, scores) %>%
  dplyr::mutate(model = forcats::fct_reorder(model, interval_score, .fun = "mean"))




crps_score <- weights_and_scores %>%
  dplyr::filter(method == "crps") %>%
  ggplot2::ggplot(ggplot2::aes(x = forecast_date, y = weights,
                               group = model)) +
  ggplot2::geom_point(ggplot2::aes(y = weights),
                      colour = "steelblue") +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ model, ncol = 11) +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "", y = "CRPS weights") +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

qra_score <- weights_and_scores %>%
  dplyr::filter(method == "qra") %>%
  ggplot2::ggplot(ggplot2::aes(x = forecast_date, y = weights,
                               group = model)) +
  ggplot2::geom_point(colour = "dark green") +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ model, ncol = 11) +
  ggplot2::labs(x = "Forecast date", y = "QRA weights") +
  cowplot::theme_cowplot() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))

wis_score1 <- weights_and_scores %>%
  dplyr::filter(horizon == 1,
                method == "crps") %>%
  dplyr::group_by(forecast_date) %>%
  dplyr::mutate(rank = rank(interval_score, ties.method = "first")) %>%
  dplyr::ungroup() %>%

  ggplot2::ggplot(ggplot2::aes(x = forecast_date, y = interval_score,
                               group = model)) +
  ggplot2::geom_point(ggplot2::aes(colour = rank),
                      size = 4) +
  ggplot2::scale_color_continuous(low = "seagreen4", high = "orange1",
                                  name = "Model Rank per forecast date") +
  ggplot2::geom_text(ggplot2::aes(y = interval_score + 60,
                                  label = rank)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ model, ncol = 11) +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "", y = "WIS 1 week ahead") +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 legend.position = "none")


wis_score2 <- weights_and_scores %>%
  dplyr::filter(horizon == 2,
                method == "crps") %>%
  dplyr::group_by(forecast_date) %>%
  dplyr::mutate(rank = rank(interval_score, ties.method = "first")) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x = forecast_date, y = interval_score,
                               group = model)) +
  ggplot2::geom_point(ggplot2::aes(colour = rank),
                      size = 4) +
  ggplot2::scale_color_continuous(low = "seagreen4", high = "orange1",
                                  name = "Model Rank per forecast date") +
  ggplot2::geom_text(ggplot2::aes(y = interval_score + 80,
                                  label = rank)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ model, ncol = 11) +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "", y = "WIS 2 weeks ahead") +
  ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 legend.position = "none")

weights_vs_scores <- cowplot::plot_grid(wis_score1, wis_score2,
                                        crps_score, qra_score,
                                        ncol = 1,
                                        rel_heights = c(1,1, 0.6, 0.85))

ggplot2::ggsave(here::here(root_folder,
                           "weights-vs-wis.png"),
                weights_vs_scores,
                width = 13, height = 10)

}


if (evaluation_scenario == "ensemble") {
  crps_weights_dirs <- list.dirs(here::here("ensembling",
                                            "crps-ensemble",
                                            "crps-weights"))[-1]
  crps_weights_files <- list.files(crps_weights_dirs)
}






# make a regression of ensemble models

if (sensitivity_ensembles) {

  forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                     models = c(settings$model_names_eval))

  forecasts <- filter_forecasts(forecasts,
                                locations = settings$locations_included,
                                horizons = c(1, 2, 3, 4))

  full <- prepare_for_scoring(forecasts)

  scores <- scoringutils::eval_forecasts(full,
                                         by = c("forecast_date",
                                                "target_end_date",
                                                "model", "state", "horizon"),
                                         interval_score_arguments = list(weigh = TRUE),
                                         summarise_by = c("model", "forecast_date",
                                                          "horizon", "state"))


  unsummarised_scores <- scores %>%
    dplyr::mutate(log_scores = log(interval_score),
                  state = as.factor(state),
                  model = as.factor(model),
                  model = forcats::fct_reorder(model, interval_score,
                                               .fun = "mean"),
                  model = relevel(model, ref = "COVIDhub-ensemble"),
                  forecast_date = as.factor(forecast_date)) %>%
    dplyr::filter(is.finite(log_scores))


  unsummarised_scores %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(mean = mean(interval_score),
                     mean_log = mean(log_scores))

  ensemble_fit <- lmerTest::lmer(log_scores ~ model
                        + (1|state) + (1|forecast_date),
                        data = unsummarised_scores)

  summary(ensemble_fit) %>%
    coef() %>%
    as.data.frame() %>%
    dplyr::arrange(Estimate)

  saveRDS(ensemble_fit, paste0(root_folder, "/random-effects-model-ensemble.RDS"))


}




## ensemble table
model_names <- settings$ensemble_names_all[settings$ensemble_names_all != "COVIDhub-ensemble"]

d <- tibble::tibble("Model name" = model_names,
                    "Type" = c("CRPS", "CRPS", "CRPS", "CRPS", "CRPS",
                               "CRPS", "CRPS", "CRPS", "CRPS", "CRPS",
                               "QRA", "QRA", "QRA", "QRA",
                               "CRPS / metalog"),
                    "Weeks of past data included" = c(1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 1, 2, 3, 4, 2),
                    "Horizon that was optimised for" = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4, NA, NA, NA, NA, 2)) %>%
  dplyr::arrange(`Model name`)


saveRDS(d, paste0(root_folder, "/ensemble-variants-table.RDS"))
