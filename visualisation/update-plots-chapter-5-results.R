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
  settings$evaluation_dates <-
    settings$evaluation_dates[settings$evaluation_dates <= as.Date("2020-07-20")]
}

if(evaluation_scenario == 3) {
  settings$evaluation_dates <-
    settings$evaluation_dates[settings$evaluation_dates <= as.Date("2020-07-13")]
}


# source function for visualisation
source(here::here("visualisation", "plotting-functions", "visualise-data-functions.R"))
source(here::here("visualisation", "plotting-functions", "evaluation-plots-function.R"))
source(here::here("utils", "load-data-functions.R"))

# scenario <- settings$scenario

# get full data set
deaths <- get_us_deaths(data = "weekly") %>%
  dplyr::filter(epiweek < max(epiweek))

forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                   models = settings$model_names_eval)

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))

full <- prepare_for_scoring(forecasts)















# ==============================================================================
# Forecast visualisation
# ==============================================================================


# ------------------------------------------------------------------------------
# plot with forecasts for US ---------------------------

# over different horizons maybe appendix?
# US_forecast_plots <- plot_forecasts(states = "US",
#                                    forecasts = forecasts,
#                                    facet_formula = model ~ horizon,
#                                    ncol_facet = 4,
#                                    horizons = c(1, 2, 3, 4),
#                                    obs_weeks = 7)
#
# US_forecast_one_week <- plot_forecasts(states = "US",
#                                     forecasts = forecasts,
#                                     facet_formula = ~ model,
#                                     ncol_facet = 4,
#                                     horizons = c(1),
#                                     obs_weeks = 7)
#
# US_forecast_four_weeks <- plot_forecasts(states = "US",
#                                          forecasts = forecasts,
#                                          facet_formula = ~ model,
#                                          ncol_facet = 4,
#                                          horizons = c(4),
#                                          obs_weeks = 7)

US_forecast_one_four_weeks <- plot_forecasts(states = "US",
                                         forecasts = forecasts,
                                         facet_formula = model ~ horizon,
                                         ncol_facet = 4,
                                         horizons = c(1, 4),
                                         obs_weeks = 7) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                      hjust=1))


# ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
#                            "US-forecast-1-wk-ahead.png"),
#                 US_forecast_one_week, width = 10, height = 5)

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







# ==============================================================================
# Look at summarised scores
# ==============================================================================


# ------------------------------------------------------------------------------
# scores table ---------------------------

# summarised_scores <- scoringutils::eval_forecasts(full,
#                                                   by = c("model", "state",
#                                                          "target_end_date",
#                                                          "horizon"),
#                                                   interval_score_arguments = list(weigh = TRUE),
#                                                   summarise_by = c("model")) %>%
#   dplyr::arrange(interval_score)
#
# summarised_scores %>%
#   dplyr::select(-calibration) %>%
#   saveRDS(here::here("summarised_scores.rds"))













# ------------------------------------------------------------------------------
# Coloured scores table ---------------------------

scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "forecast_date",
                                                        "horizon", "state"))

summarised_scores <- scores %>%
  dplyr::mutate(log_interval_score = log(interval_score),
                abs_bias = abs(bias)) %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(log_interval_score = mean(log_interval_score),
                   interval_score = mean(interval_score),
                   coverage_deviation = mean(coverage_deviation),
                   bias = mean(bias),
                   abs_bias = mean(abs_bias),
                   sharpness = mean(sharpness),
                   calibration = mean(calibration)) %>%
  dplyr::arrange(interval_score)

tmp <- summarised_scores %>%
  dplyr::select(-calibration) %>%
  dplyr::mutate(log_wis_scaled = (log_interval_score - min(log_interval_score)) / sd(log_interval_score),
                wis_scaled = (interval_score - min(interval_score)) / sd(interval_score),
                cov_scaled = (coverage_deviation) / sd(coverage_deviation),
                bias_scaled = (bias) / sd(bias),
                abs_bias_scaled = (abs_bias - min(abs_bias)) / (sd(abs_bias)),
                sharpness_scaled = (sharpness - min(sharpness)) / sd(sharpness))

df <- dplyr::bind_cols(tmp %>%
                         tidyr::pivot_longer(cols = c(log_wis_scaled, wis_scaled, cov_scaled,
                                                      bias_scaled, abs_bias_scaled, sharpness_scaled),
                                             values_to = "value_scaled"),
                       tmp %>%
                         tidyr::pivot_longer(cols = c(log_interval_score, interval_score,
                                                      coverage_deviation, bias, abs_bias, sharpness),
                                             values_to = "value"),
                       by = c("model")) %>%
  dplyr::rename(model = model...1,
                name = name...17) %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE))

coloured_table <- df %>%
  dplyr::mutate(name = factor(name,
                              levels = c("interval_score", "log_interval_score", "coverage_deviation",
                                         "bias", "abs_bias", "sharpness"))) %>%
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
  ggplot2::labs(x = "Metric", y = "Model") +
  ggplot2::coord_cartesian(expand=FALSE)


ggplot2::ggsave(here::here(root_folder,
                           "coloured-summarised-scores.png"),
                coloured_table, width = 10, height = 5)











# ------------------------------------------------------------------------------
# Correlation between scores ---------------------------


scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state",
                                                        "forecast_date", "horizon"))

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

correlation_map <- scores %>%
  dplyr::mutate("absolute bias" = abs(bias),
                log_interval_score = log(interval_score),
                log_sharpness = log(sharpness)) %>%
  dplyr::filter(is.finite(log_interval_score)) %>%
  dplyr::select(-model, -state, -horizon, -bias, -forecast_date, -calibration) %>%
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
                                name = "Correlation") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "", y = "") +
  ggplot2::coord_cartesian(expand = FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "correlation-map.png"),
                correlation_map, width = 13, height = 8)


corr_plot <- scores %>%
  dplyr::mutate("absolute bias" = abs(bias),
                log_interval_score = log(interval_score),
                log_sharpness = log(sharpness)) %>%
  dplyr::select(-model, -state, -horizon, -bias, -forecast_date, -calibration) %>%
  dplyr::filter(is.finite(log_interval_score)) %>%
  GGally::ggpairs() +
  cowplot::theme_cowplot() +
  ggplot2::theme(panel.spacing = unit(3, "mm"),
                 panel.background = element_rect(fill = "aliceblue"))

ggplot2::ggsave(here::here(root_folder,
                           "corr-plot.png"),
                corr_plot, width = 14, height = 14)











# ------------------------------------------------------------------------------
# Regression metrics on WIS ---------------------------

unsum_scores <- scoringutils::eval_forecasts(full,
                                             by = c("model", "state",
                                                    "target_end_date",
                                                    "horizon"),
                                             interval_score_arguments = list(weigh = TRUE),
                                             summarise_by = c("model", "state",
                                                              "forecast_date", "horizon"))

saveRDS(unsum_scores, paste0(root_folder,"/unsummarised_scores.rds"))

unsum_scores <- readRDS(paste0(root_folder, "/unsummarised_scores.rds")) %>%
  dplyr::mutate(log_scores = log(interval_score),
                abs_bias_std = (abs(bias) - mean(abs(bias))) / sd(abs(bias)),
                coverage_deviation_std = (coverage_deviation -
                                            mean(coverage_deviation)) /sd(coverage_deviation),
                sharpness_std = (sharpness - mean(sharpness))/sd(sharpness)) %>%
  dplyr::filter(is.finite(log_scores))


lm(log_scores ~ abs_bias_std + coverage_deviation_std + sharpness_std, data = unsum_scores) %>%
  summary()

















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
  dplyr::mutate(rank = rank(interval_score, ties.method = "average")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE),
                state = forcats::fct_reorder(state,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE))

heatmap_plot <- ggplot2::ggplot(scores, ggplot2::aes(y = model, x = state, fill = rank)) +
  ggplot2::geom_tile(colour = "white", size = 0.3) +
  ggplot2::geom_text(ggplot2::aes(label = round(interval_score, 1)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red",
                                name = "Model rank per state",
                                breaks = seq(1,11,2)) +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "") +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::coord_cartesian(expand = FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "heatmap-model-scores.png"),
                heatmap_plot, width = 10, height = 5.5)







# heatmap with horizons --------------------------------------------------------

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon"))

scores <- scores %>%
  dplyr::arrange(horizon, model, interval_score) %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(multiple = interval_score / min(interval_score)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE))


heatmap_plot2 <- ggplot2::ggplot(scores, ggplot2::aes(y = model,
                                                      x = horizon, fill = multiple)) +
  ggplot2::geom_tile(colour = "white", size = 0.3) +
  ggplot2::geom_text(ggplot2::aes(label = round(interval_score, 1)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red",
                                name = "Colour: Multiple of minimum score per model") +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "", x = "Forecast horizon in weeks") +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif")) +
  ggplot2::coord_cartesian(expand=FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "heatmap-model-scores-horizon.png"),
                heatmap_plot2, width = 10, height = 4.5)














# ------------------------------------------------------------------------------
## WIS score contributions by range plot
# maybe get rid of lines in the plot and also get rid of the numbers
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




























# ==============================================================================
# Calibration - BIAS
# ==============================================================================


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

scores %>%
  filter(model == "crps-ensemble-4-1")


bias_horizons <- scores %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean'),
                horizon = as.factor(horizon)) %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               colour = model)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.05, ymax = bias_0.95),
                          size = 2,
                          alpha = 0.1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.25, ymax = bias_0.75),
                          source(here::here("ensembling", "crps-ensemble", "fit-distribution-functions.R"))
                          combined <- combine_with_deaths(forecasts)
                          fc <- data.table::as.data.table(combined)
                          n_samples = settings$n_samples

                          samples <- fc[, .(y_pred = get_samples(value, quantile, n_samples = n_samples),
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
                                                                   pit_arguments = list(num_bins = 30,
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
                          size = 2,
                          alpha = 0.4,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.4, ymax = bias_0.6),
                          size = 2,
                          alpha = 1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::scale_color_manual(values = settings$manual_colours) +
  ggplot2::geom_point(ggplot2::aes(y = bias_0.5),
                      size = 2,
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
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))

# maybe: make y = horizon, facet_grid ~ model and change the colour such
# that the models are coloured, not the horizons

ggplot2::ggsave(here::here(root_folder,
                           "bias-horizons.png"),
                bias_horizons,
                width = 10, height = 5)







# ------------------------------------------------------------------------------
# Detailed bias for YYG

forecasts_YYG <- load_submission_files(dates = settings$evaluation_dates,
                                       models = "YYG-ParamSearch")

forecasts_YYG <- filter_forecasts(forecasts_YYG,
                                  locations = settings$locations_included,
                                  horizons = c(1, 2, 3, 4))

full_YYG <- prepare_for_scoring(forecasts_YYG)

scores_YYG <- scoringutils::eval_forecasts(full_YYG,
                                       by = c("model", "state",
                                              "target_end_date",
                                              "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state",
                                                        "target_end_date", "horizon")) %>%
  dplyr::filter(horizon == 1)


high_bias_states <- scores_YYG %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(bias = mean(abs(bias))) %>%
  dplyr::arrange(-bias) %>%
  dplyr::pull(state)


plot_YYG <- plot_forecasts(forecasts = forecasts_YYG,
                                   states = high_bias_states[1:6],
                                   models = "YYG-ParamSearch",
                                   obs_weeks = 13) +
  ggplot2::theme(legend.position = "none",
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0))) +
  ggplot2::labs(x = "")

bias_YYG <- ggplot2::ggplot() +
  ggplot2::geom_point(data = scores_YYG %>%
                        dplyr::filter(state %in% high_bias_states[1:6]),
                      ggplot2::aes(y = bias,
                                                  x = target_end_date),
                      colour = "black") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(x = "Week", y = "Bias",
                col = "Model", fill = "Model") +
  ggplot2::facet_wrap(~ state) +
  ggplot2::expand_limits(x = c(as.Date("2020-05-23"), as.Date("2020-08-22"))) +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0)))

bias_plot_combined <- cowplot::plot_grid(plot_YYG,
                                         bias_YYG, ncol = 1,
                                         rel_heights = c(2, 1.5),
                                         scale = c(1, 1),
                                         align = 'v')

ggplot2::ggsave(here::here(root_folder,
                           "bias-YYG.png"),
                bias_plot_combined,
                width = 10, height = 7)




















l









# ==============================================================================
# Calibration - Coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# empirical coverage for all -----------------------------

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "range"))


## overall model calibration - empirical interval coverage
interval_coverage_all <- ggplot2::ggplot(scores, ggplot2::aes(x = range, colour = model)) +
  ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_manual(values = settings$manual_colours) +
  ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(legend.position = "none",
                 panel.spacing = unit(5, "mm")) +
  ggplot2::ylab("Percent observations inside interval range") +
  ggplot2::xlab("Interval range")

ggplot2::ggsave(here::here(root_folder,
                           "interval-coverage-all.png"),
                interval_coverage_all,
                width = 10, height = 7)



# ------------------------------------------------------------------------------
# plot with quantile coverage

combined <- combine_with_deaths(forecasts)

quantile_coverage_plot_all <- combined  %>%
  dplyr::group_by(model, quantile) %>%
  dplyr::summarise(coverage = mean(deaths <= value)) %>%
  ggplot2::ggplot(ggplot2::aes(x = quantile, colour = model)) +
  ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coverage)) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_manual(values = settings$manual_colours) +
  ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(panel.spacing = unit(5, "mm"),
                 legend.position = "none") +
  ggplot2::xlab("Quantile") +
  #ggplot2::scale_y_continuous(breaks=c(0,0.25,0.5,0.75, 1)) +
  ggplot2::ylab("Proportion of observations below quantile")

ggplot2::ggsave(here::here(root_folder,
                           "quantile-coverage-all.png"),
                quantile_coverage_plot_all,
                width = 10, height = 7)











# ------------------------------------------------------------------------------
# interval coverage and quantile coverage over horizons

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon", "range"))

coverage_over_horizons <- ggplot2::ggplot(scores, ggplot2::aes(x = horizon,
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
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::xlab("Forecast horizon in weeks") +
  ggplot2::ylab("Empirical interval coverage")

coverage_over_horizons <- shift_legend3(coverage_over_horizons)

ggplot2::ggsave(here::here(root_folder,
                           "interval-coverage-horizons.png"),
                coverage_over_horizons,
                width = 10, height = 7)










quantile_coverage_over_horizons <- combined  %>%
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
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("Forecast horizon in weeks") +
  ggplot2::ylab("Empirical quantile coverage")


quantile_coverage_over_horizons <- shift_legend3(quantile_coverage_over_horizons)

ggplot2::ggsave(here::here(root_folder,
                           "quantile-coverage-horizons.png"),
                quantile_coverage_over_horizons,
                width = 12, height = 7)








# ------------------------------------------------------------------------------
## coverage deviation by range plot

summarised_scores <- summarised_scores %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean')) %>%
  dplyr::arrange(model)

coverage_deviation_by_range <- summarised_scores %>%
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
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::labs(y = "Coverage deviation",
                x = "Model")

ggplot2::ggsave(here::here(root_folder,
                           "coverage-deviation-by-range.png"),
                coverage_deviation_by_range, width = 10, height = 5)











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
  ggplot2::labs(y = "", x = "Location") +
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
                 legend.position = "none")

ggplot2::ggsave(here::here(root_folder,
                           "Texas-one-week.png"),
                Texas_forecast_one_weeks, width = 10, height = 4.8)


NY_forecast_one_weeks <- plot_forecasts(states = "New York",
                                           forecasts = forecasts,
                                           facet_formula = model ~ horizon,
                                           ncol_facet = 4,
                                           horizons = c(1),
                                           obs_weeks = 13) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1),
                 legend.position = "none")

ggplot2::ggsave(here::here(root_folder,
                           "NY-one-week.png"),
                N>_forecast_one_weeks, width = 10, height = 4.8)


















# ==============================================================================
# Calibration - PIT
# ==============================================================================





# ------------------------------------------------------------------------------
# PIT plots

source(here::here("ensembling", "crps-ensemble", "fit-distribution-functions.R"))
combined <- combine_with_deaths(forecasts)
fc <- data.table::as.data.table(combined)
n_samples = settings$n_samples

samples <- fc[, .(y_pred = get_samples(value, quantile, n_samples = n_samples),
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
                                         pit_arguments = list(num_bins = 30,
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


















# ==============================================================================
# Sharpness
# ==============================================================================

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
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean')) %>%
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
  ggplot2::coord_cartesian(ylim = c(0, 55)) +
  ggplot2::scale_color_manual(values = settings$manual_colours) +
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
                x = "Model")

ggplot2::ggsave(here::here(root_folder,
                           "sharpness-by-range.png"),
                sharpness_by_range, width = 10, height = 4.6)










#
# # ------------------------------------------------------------------------------
# # WIS over horizons
#
# wis_horizons <- scores %>%
#   dplyr::mutate(model = forcats::fct_reorder(model,
#                                              interval_score,
#                                              .fun='mean'),
#                 horizon = as.factor(horizon)) %>%
#   ggplot2::ggplot(ggplot2::aes(x = model,
#                                colour = horizon)) +
#   ggplot2::geom_linerange(ggplot2::aes(ymin = interval_score_0.05,
#                                        ymax = interval_score_0.95),
#                           size = 2,
#                           alpha = 0.1,
#                           position = ggplot2::position_dodge2(width = 0.5,
#                                                               padding = 0)) +
#   ggplot2::geom_linerange(ggplot2::aes(ymin = interval_score_0.25,
#                                        ymax = interval_score_0.75),
#                           size = 2,
#                           alpha = 0.4,
#                           position = ggplot2::position_dodge2(width = 0.5,
#                                                               padding = 0)) +
#   ggplot2::geom_linerange(ggplot2::aes(ymin = interval_score_0.4,
#                                        ymax = interval_score_0.6),
#                           size = 2,
#                           alpha = 1,
#                           position = ggplot2::position_dodge2(width = 0.5,
#                                                               padding = 0)) +
#   ggplot2::geom_point(ggplot2::aes(y = interval_score_0.5),
#                       size = 2,
#                       colour = "black",
#                       position = ggplot2::position_dodge2(width = 0.5,
#                                                           padding = 0)) +
#   ggplot2::geom_point(ggplot2::aes(y = interval_score),
#                       size = 2,
#                       shape = 18,
#                       colour = "black",
#                       position = ggplot2::position_dodge2(width = 0.5,
#                                                           padding = 0)) +
#   ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
#                       linetype = "dashed", alpha = 0.5) +
#   ggplot2::expand_limits(y = 0) +
#   ggplot2::facet_grid(NULL) +
#   ggplot2::scale_y_continuous(trans = scales::log10_trans()) +
#   ggplot2::labs(x = "", y = "Weighted interval score") +
#   cowplot::theme_cowplot() +
#   ggplot2::theme(legend.position = "bottom",
#                  text = ggplot2::element_text(family = "Sans Serif"),
#                  axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
#                                                      hjust=1))
#
# # maybe: make y = horizon, facet_grid ~ model and change the colour such
# # that the models are coloured, not the horizons
#
# ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
#                            "bias-horizons.png"),
#                 wis_horizons,
#                 width = 14, height = 9)























# ==============================================================================
# Regression analysis
# ==============================================================================

# ------------------------------------------------------------------------------
# Random effects model on WIS

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


fit <- lmerTest::lmer(log_scores ~ model * horizon
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

# ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
#                            "random-location-effect.png"),
#                 random_locations,
#                 width = 10, height = 4)


# ------------------------------------------------------------------------------
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


# ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
#                            "random-forecast-date-effect.png"),
#                 random_forecast_date,
#                 width = 10, height = 4)


random_effects <- cowplot::plot_grid(random_locations,
                                     random_forecast_date,
                                     rel_widths = c(1.5, 1),
                                     ncol = 2)


ggplot2::ggsave(here::here(root_folder,
                           "random-effects.png"),
                random_effects,
                width = 10, height = 4)














# ------------------------------------------------------------------------------
# In-depth look at the ensemble models
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Ensemble - Coverage

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
  ggplot2::scale_color_manual(values = settings$manual_colours) +
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
  ggplot2::scale_color_manual(values = settings$manual_colours) +
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
# Ensemble - Bias

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
                           facet_formula = ~ state + model,
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
  ggplot2::labs(x = "Week", y = "Bias",
                #caption = paste0("Mean bias is ", round(mean(scores$bias), 3)),
                col = "Model", fill = "Model") +
  ggplot2::facet_wrap(state ~ model,
                      ncol = 4) +
  ggplot2::expand_limits(x = c(as.Date("2020-05-23"), as.Date("2020-08-22"))) +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",,
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0)))

bias_plot_combined_ensemble <- cowplot::plot_grid(plot_ensemble,
                                                  bias_ensemble, ncol = 1,
                                                  rel_heights = c(2, 1.5),
                                                  scale = c(1, 1),
                                                  align = 'v')

ggplot2::ggsave(here::here(root_folder,
                           "bias_ensemble.png"),
                bias_plot_combined_ensemble,
                width = 10, height = 16)









# ------------------------------------------------------------------------------
# Ensemble - Sharpnes

scores_ensemble <- scoringutils::eval_forecasts(full_ensemble,
                                                by = c("model", "state",
                                                       "target_end_date",
                                                       "horizon"),
                                                interval_score_arguments = list(weigh = TRUE),
                                                summarise_by = c("model", "state"))

scores_ensemble <- scores_ensemble  %>%
  dplyr::arrange(state, model, interval_score) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(rank = rank(sharpness, ties.method = "average")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             sharpness,
                                             .fun='mean',
                                             .desc = TRUE),
                state = forcats::fct_reorder(state,
                                             sharpness,
                                             .fun='mean',
                                             .desc = TRUE)) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(multiple = sharpness / min(sharpness))

heatmap_plot_sharpness <- ggplot2::ggplot(scores_ensemble,
                                ggplot2::aes(y = model, x = state, fill = multiple)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = round(sharpness, 1)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red",
                                name = "Multiple of minimal sharpness per state",
                                breaks = seq(1:3)) +
  cowplot::theme_cowplot() +
  ggplot2::labs(y = "", x = "Location") +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1)) +
  ggplot2::coord_cartesian(expand = FALSE)

ggplot2::ggsave(here::here(root_folder,
                           "heatmap-sharpness-ensemble.png"),
                heatmap_plot_sharpness, width = 10, height = 3.5)










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
  dplyr::filter(horizon == 1) %>%
  dplyr::group_by(forecast_date) %>%
  dplyr::mutate(rank = rank(interval_score, ties.method = "first")) %>%
  dplyr::ungroup() %>%
  dplyr::filter(method == "crps") %>%
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
  dplyr::filter(horizon == 2) %>%
  dplyr::group_by(forecast_date) %>%
  dplyr::mutate(rank = rank(interval_score, ties.method = "first")) %>%
  dplyr::ungroup() %>%
  dplyr::filter(method == "crps") %>%
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
                                         summarise_by = c("model", "forecast_date",
                                                          "horizon", "state"))


  unsummarised_scores <- scores %>%
    dplyr::mutate(log_scores = log(interval_score),
                  state = as.factor(state),
                  model = as.factor(model),
                  model = forcats::fct_reorder(model, interval_score,
                                               .fun = "mean"),
                  model = relevel(model, ref = "qra-ensemble-1"),
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












#
# suppressWarnings(ggsave(here::here("evaluation", "plots",
#                                    forecast_date, "submission-national.png"),
#        plot = national_plot,
#        width =
