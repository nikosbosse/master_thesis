
# PCA analysis

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       quantiles = c(0.05, 0.95, 0.25, 0.75, 0.4, 0.6, 0.5),
                                       summarise_by = c("model", "horizon", "state"))



m <- scores %>%
  dplyr::select(sharpness, is_overprediction, is_underprediction) %>%
  as.matrix()


a <- prcomp(m, scale = TRUE)
a

plot(a)





# # heatmap with horizons --------------------------------------------------------
#
# scores <- scoringutils::eval_forecasts(full,
#                                        by = c("forecast_date",
#                                               "target_end_date",
#                                               "model", "state", "horizon"),
#                                        interval_score_arguments = list(weigh = TRUE),
#                                        summarise_by = c("model", "horizon"))
#
# scores <- scores %>%
#   dplyr::arrange(horizon, model, interval_score) %>%
#   dplyr::group_by(model) %>%
#   dplyr::mutate(multiple = interval_score / min(interval_score)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(model = forcats::fct_reorder(model,
#                                              interval_score,
#                                              .fun='mean',
#                                              .desc = TRUE))
#
#
# heatmap_plot2 <- ggplot2::ggplot(scores, ggplot2::aes(y = model,
#                                                       x = horizon, fill = multiple)) +
#   ggplot2::geom_tile(colour = "white", size = 0.3) +
#   ggplot2::geom_text(ggplot2::aes(label = round(interval_score, 1)),
#                      family = "Sans Serif") +
#   ggplot2::scale_fill_gradient2(low = "skyblue", high = "red",
#                                 name = "Colour: Multiple of minimum score per model") +
#   cowplot::theme_cowplot() +
#   ggplot2::labs(y = "", x = "Forecast horizon in weeks") +
#   ggplot2::theme(legend.position = "bottom",
#                  text = ggplot2::element_text(family = "Sans Serif")) +
#   ggplot2::coord_cartesian(expand=FALSE)
#
# ggplot2::ggsave(here::here(root_folder,
#                            "heatmap-model-scores-horizon.png"),
#                 heatmap_plot2, width = 10, height = 4.5)


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
## sharpness by state

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
  dplyr::mutate(rank = rank(sharpness, ties.method = "average")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(model = forcats::fct_reorder(model,
                                             interval_score,
                                             .fun='mean',
                                             .desc = TRUE),
                state = forcats::fct_reorder(state,
                                             sharpness,
                                             .fun='mean',
                                             .desc = TRUE))

heatmap_plot_sharpness <- ggplot2::ggplot(scores,
                                          ggplot2::aes(y = model,
                                                       x = state,
                                                       fill = rank)) +
  ggplot2::geom_tile(colour = "white", size = 0.3) +
  ggplot2::geom_text(ggplot2::aes(label = round(sharpness, 1)),
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
                           "heatmap-model-sharpness.png"),
                heatmap_plot_sharpness, width = 10, height = 5.5)












# ------------------------------------------------------------------------------
# WIS over horizons

wis_horizons <- scores %>%
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
  ggplot2::geom_point(ggplot2::aes(y = interval_score),
                      size = 2,
                      shape = 18,
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
