# ==============================================================================
# Plots chapter 5 results
# ==============================================================================

# setup ------------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(dplyr)

# source function for visualisation
source(here::here("evaluation", "visualise-data-functions.R"))
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))



# get full data set
deaths <- get_us_deaths(data = "weekly") %>%
  dplyr::filter(epiweek < max(epiweek))

forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                   models = settings$model_names_eval)

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))

full <- prepare_for_scoring(forecasts)

# ------------------------------------------------------------------------------
# plot with forecasts for US ---------------------------

# over different horizons maybe appendix?
US_forecast_plots <- plot_forecasts(states = "US",
                                   forecasts = forecasts,
                                   facet_formula = model ~ horizon,
                                   ncol_facet = 4,
                                   horizons = c(1, 2, 3, 4),
                                   obs_weeks = 7)

US_forecast_one_week <- plot_forecasts(states = "US",
                                    forecasts = forecasts,
                                    facet_formula = ~ model,
                                    ncol_facet = 4,
                                    horizons = c(1),
                                    obs_weeks = 7)

US_forecast_four_weeks <- plot_forecasts(states = "US",
                                         forecasts = forecasts,
                                         facet_formula = ~ model,
                                         ncol_facet = 4,
                                         horizons = c(4),
                                         obs_weeks = 7)

US_forecast_one_four_weeks <- plot_forecasts(states = "US",
                                         forecasts = forecasts,
                                         facet_formula = model ~ horizon,
                                         ncol_facet = 6,
                                         horizons = c(1, 4),
                                         obs_weeks = 7)


ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "US-forecast-1-wk-ahead.png"),
                US_forecast_one_week, width = 10, height = 5)

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "US-forecast-1-4-wk-ahead.png"),
                US_forecast_one_four_weeks, width = 25, height = 12)

# for Appendix
several_states_one_week <- plot_forecasts(states = settings$states_included,
                                       forecasts = forecasts,
                                       facet_formula = state ~ model,
                                       ncol_facet = 11,
                                       horizons = c(1),
                                       obs_weeks = 7)

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "all-states-one-week.png"),
                several_states_one_week, width = 18, height = 25)












# ------------------------------------------------------------------------------
## scores overall by range plot
summarised_scores <- scoringutils::eval_forecasts(full, summarised = TRUE,
                                                  by = c("model", "state",
                                                         "target_end_date",
                                                         "horizon"),
                                                  interval_score_arguments = list(weigh = TRUE),
                                                  summarise_by = c("model"))


# maybe get rid of lines in the plot and also get rid of the numbers
wis_overview_plot <- score_overview_plot(summarised_scores)

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "scores-by-range.png"),
                wis_overview_plot, width = 7, height = 12)






















# ------------------------------------------------------------------------------
# heatmap plot with all scores in all states -----------------------------------

forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                   models = settings$model_names_eval)

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))



scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state"))

scores <- scores %>%
  # change this once we have the WIS
  dplyr::filter(range == 60) %>%
  dplyr::arrange(state, model, interval_score) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(rank = rank(interval_score, ties.method = "average")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean'),
                state = forcats::fct_reorder(state,
                                             interval_score,
                                             .fun='mean'))

heatmap_plot <- ggplot2::ggplot(scores, ggplot2::aes(x = model, y = state, fill = rank)) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = round(interval_score, 1)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red",
                                name = "Model rank per state") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 panel.background = element_rect(fill = "aliceblue"))

# maybe tilt model names?

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "heatmap-model-scores.png"),
                heatmap_plot, width = 10, height = 10)



# heatmap with horizons --------------------------------------------------------

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon"))


scores <- scores %>%
  # change this once we have the WIS
  dplyr::filter(range == 60) %>%
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
  ggplot2::geom_tile() +
  ggplot2::geom_text(ggplot2::aes(label = round(interval_score, 1)),
                     family = "Sans Serif") +
  ggplot2::scale_fill_gradient2(low = "skyblue", high = "red",
                                name = "Colour: Multiple of minimum score per model") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 panel.background = element_rect(fill = "aliceblue"))

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "heatmap-model-scores-horizon.png"),
                heatmap_plot2, width = 10, height = 10)




# ------------------------------------------------------------------------------
# table with overal scores -----------------------------------------------------

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model"))

scores %>%
  dplyr::filter(range %in% c(0, 50, 90))










# ------------------------------------------------------------------------------
# plot with empirical coverage from baseline model -----------------------------

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model"))


## overall model calibration - empirical interval coverage
interval_coverage_all <- ggplot2::ggplot(scores, ggplot2::aes(x = range, colour = model)) +
  ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_manual(values = settings$manual_colours) +
  ggplot2::facet_wrap(~ model, ncol = 4) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "none") +
  ggplot2::ylab("Percent observations inside interval range") +
  ggplot2::xlab("Interval range")

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "interval-coverage-all.png"),
                interval_coverage_all)



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
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "none") +
  ggplot2::xlab("Quantile") +
  ggplot2::ylab("Proportion of observations below quantile")

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "quantile-coverage-all.png"),
                quantile_coverage_plot_all)







# ------------------------------------------------------------------------------
# interval coverage over time

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "horizon"))

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
  ggplot2::facet_wrap(~ model, ncol = 6) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("Forecast week") +
  ggplot2::ylab("Coverage rate")


ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "interval-coverage-horizons.png"),
                coverage_over_horizons)


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
  ggplot2::facet_wrap(~ model, ncol = 6) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("Forecast week") +
  ggplot2::ylab("Coverage rate")

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "quantile-coverage-horizons.png"),
                quantile_coverage_over_horizons,
                width = 10, height = 10)








# ------------------------------------------------------------------------------
# bias over horizons


scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise = TRUE,
                                       summarise_by = c("model", "horizon"),
                                       quantiles = c(0.05, 0.15, 0.25, 0.4, 0.5,
                                                     0.6, 0.75, 0.85, 0.95))


bias_horizons <- scores %>%
  dplyr::filter(range == 0) %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             bias,
                                             .fun='mean'),
                horizon = as.factor(horizon)) %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               colour = horizon)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.05, ymax = bias_0.95),
                          size = 2,
                          alpha = 0.1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.25, ymax = bias_0.75),
                          size = 2,
                          alpha = 0.4,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = bias_0.4, ymax = bias_0.6),
                          size = 2,
                          alpha = 1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
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
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))

# maybe: make y = horizon, facet_grid ~ model and change the colour such
# that the models are coloured, not the horizons

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "bias-horizons.png"),
                bias_horizons,
                width = 14, height = 9)




# ------------------------------------------------------------------------------
# Sharpness over horizons

sharpness_horizons <- scores %>%
  dplyr::filter(range == 0) %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             sharpness,
                                             .fun='mean')) %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               colour = factor(horizon))) +
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
  ggplot2::facet_grid(NULL) +
  ggplot2::coord_cartesian(ylim = c(0, 1000)) +
  # ggplot2::scale_y_continuous(trans = scales::log10_trans()) +
  ggplot2::labs(x = "", y = "Sharpness") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))

# maybe: make y = horizon, facet_grid ~ model and change the colour such
# that the models are coloured, not the horizons
# log scale on y axis or not?
ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "sharpness-horizons.png"),
                sharpness_horizons,
                width = 14, height = 9)





wis_horizons <- scores %>%
  dplyr::filter(range == 0) %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean'),
                horizon = as.factor(horizon)) %>%
  ggplot2::ggplot(ggplot2::aes(x = model,
                               colour = horizon)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = interval_score_0.05,
                                       ymax = interval_score_0.95),
                          size = 2,
                          alpha = 0.1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = interval_score_0.25,
                                       ymax = interval_score_0.75),
                          size = 2,
                          alpha = 0.4,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = interval_score_0.4,
                                       ymax = interval_score_0.6),
                          size = 2,
                          alpha = 1,
                          position = ggplot2::position_dodge2(width = 0.5,
                                                              padding = 0)) +
  ggplot2::geom_point(ggplot2::aes(y = interval_score_0.5),
                      size = 2,
                      colour = "black",
                      position = ggplot2::position_dodge2(width = 0.5,
                                                          padding = 0)) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed", alpha = 0.5) +
  ggplot2::expand_limits(y = 0) +
  ggplot2::facet_grid(NULL) +
  ggplot2::scale_y_continuous(trans = scales::log10_trans()) +
  ggplot2::labs(x = "", y = "Weighted interval score") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))

# maybe: make y = horizon, facet_grid ~ model and change the colour such
# that the models are coloured, not the horizons

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "bias-horizons.png"),
                wis_horizons,
                width = 14, height = 9)



















# ------------------------------------------------------------------------------
# PIT plots

source(here::here("ensembling", "crps-ensemble", "fit-distribution-functions.R"))
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

pit_plot2 <- lapply(pit_plot,
                    FUN = function(x) {
                      x <- x +
                        ggplot2::theme(legend.position = "bottom",
                                       text = ggplot2::element_text(family = "Sans Serif"))
                      return(x)
                    })

cowplot::plot_grid(plotlist = pit_plot2)

















# ------------------------------------------------------------------------------
# Plots with weights over time













#
# suppressWarnings(ggsave(here::here("evaluation", "plots",
#                                    forecast_date, "submission-national.png"),
#        plot = national_plot,
#        width =
