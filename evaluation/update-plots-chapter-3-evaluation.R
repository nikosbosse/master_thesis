# ==============================================================================
# Plots chapter 3 evaluation
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

forecasts <- load_submission_files(dates = "all",
                                   models = "COVIDhub-baseline")

forecasts <- filter_forecasts(forecasts,
                              locations = "US",
                              horizons = 1,
                              target_end_dates = "auto")

full <- prepare_for_scoring(forecasts)







# ------------------------------------------------------------------------------
# plot with baseline model and US and bias of that plot ------------------------

US_plot_baseline <- plot_forecasts(states = "US",
                                   models = "COVIDhub-baseline",
                                   obs_weeks = 13)


full %>%
  dplyr::filter(target_end_date == "2020-08-01",
                boundary == "upper")%>%
  pull(predictions)


scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "target_end_date",
                                              "state", "horizon"))

plot_bias <- ggplot2::ggplot() +
  ggplot2::geom_point(data = scores, ggplot2::aes(y = bias,
                                                  x = target_end_date),
                      colour = "black") +
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(x = "Week", y = "Bias",
                caption = paste0("Mean bias is ", round(mean(scores$bias), 3)),
                col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"))

bias_plot_combined <- cowplot::plot_grid(US_plot_baseline,
                                         plot_bias, ncol = 1,
                                         rel_heights = c(2, 1),
                                         scale = c(1, 1),
                                         align = 'hv')

ggplot2::ggsave(here::here("evaluation", "plots", "bias_example.png"),
                bias_plot_combined)





# ------------------------------------------------------------------------------
# calibration example plots ----------------------------------------------------
true_values <- rnorm(1000, 0, 1)
predictions1 <- replicate(10000, rnorm(1000))
predictions2 <- replicate(10000, rnorm(1000, mean = 0.5))
predictions3 <- replicate(10000, rnorm(1000, sd = 1.4))
predictions4 <- replicate(10000, rnorm(1000, sd =  0.7))
predictions5 <- replicate(10000, rnorm(1000, mean = 0.2, sd =  0.7))

# plot with observations
true_value_plot <- ggplot2::ggplot(data = data.frame(x = true_values),
                ggplot2::aes(x = x)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                          fill = "grey",
                          colour = "dark grey") +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "True values",
                y = "Density") +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"))

# plot with standard normal distribution
standard_normal <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black")

pit_standard_normal <- scoringutils::pit(true_values, predictions1)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"))

# plot with shifted mean
shifted_mean <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(mean = 0.5))

pit_shifted_mean <- scoringutils::pit(true_values, predictions2)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"))

# plot with overdispersion
overdispersion <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 1.4))

pit_overdispersion <- scoringutils::pit(true_values, predictions3)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"))

# plot with underdispesion
underdispersion <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 0.7))

pit_underdispersion <- scoringutils::pit(true_values, predictions4)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"))

# # plot with underdispesion + mean shift
# underdispesion_mean_shift <- true_value_plot +
#   ggplot2::geom_function(fun = dnorm, colour = "black", args = list(mean = 0.2,
#                                                                     sd = 0.7))
#
# pit_underdispesion_mean_shift <- scoringutils::pit(true_values, predictions5)$hist_PIT +
#   cowplot::theme_cowplot() +
#   ggplot2::theme(legend.position = "bottom",
#                  text = ggplot2::element_text(family = "Sans Serif"))


calibration_examples <- cowplot::plot_grid(standard_normal, shifted_mean,
                   pit_standard_normal, pit_shifted_mean,
                   underdispersion, overdispersion,
                   pit_underdispersion, pit_overdispersion,
                   ncol = 2,
                   rel_heights = c(1, 1),
                   scale = c(1, 1),
                   align = 'hv')



ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
                           "calibration-examples.png"),
                calibration_examples,
                height = 15, width = 8)









# ------------------------------------------------------------------------------
# pit calibration plot of US forecasts from baseline model ---------------------

forecasts <- load_submission_files(dates = "all",
                                   models = "COVIDhub-baseline")

forecasts <- filter_forecasts(forecasts,
                              locations = NULL,
                              horizons = 1,
                              target_end_dates = "auto")

n_samples <- 1000

combined <- combine_with_deaths(forecasts) %>%
  data.table::as.data.table()

samples <- combined[, .(y_pred = get_samples(value, quantile, n_samples = n_samples),
                        sample_nr = 1:n_samples,
                        deaths = unique(deaths)),
                    by = c("model", "forecast_date", "target",
                           "target_end_date", "state")]

df <- samples %>%
  dplyr::rename(predictions = y_pred,
                true_values = deaths,
                id = target_end_date,
                sample = sample_nr)

pit_plot <- scoringutils::eval_forecasts(df,
                                         by = c("model", "state", "target"),
                                         pit_arguments = list(num_bins = 30,
                                                              plot = TRUE),
                                         pit_plots = TRUE)$pit_plots


plot <- pit_plot$overall_pit +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"))

ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
                           "pit-baseline-model.png"),
                plot)








# ------------------------------------------------------------------------------
# plot with empirical coverage from baseline model -----------------------------

forecasts <- load_submission_files(dates = "all",
                                   models = "COVIDhub-baseline")

forecasts <- filter_forecasts(forecasts,
                              locations = NULL,
                              horizons = 1,
                              target_end_dates = "auto")

n_samples <- 1000

combined <- combine_with_deaths(forecasts) %>%
  data.table::as.data.table()

scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "forecast_date",
                                              "target_end_date"),
                                       summarise_by = c("model"))

## overall model calibration - empirical interval coverage
interval_coverage <- ggplot2::ggplot(scores, ggplot2::aes(x = range)) +
  ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
  cowplot::theme_cowplot() +
  ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("Percent observations inside interval range") +
  ggplot2::ylab("Interval range")

ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
                           "interval-coverage.png"),
                interval_coverage)



# ------------------------------------------------------------------------------
# plot with quantile coverage

forecasts <- load_submission_files(dates = "all",
                                   models = "COVIDhub-baseline")

forecasts <- filter_forecasts(forecasts,
                              locations = "US",
                              horizons = 1,
                              target_end_dates = "auto")

forecasts <- combine_with_deaths(forecasts)

quantile_coverage_plot <- forecasts %>%
  dplyr::group_by(quantile) %>%
  dplyr::summarise(coverage = mean(deaths <= value)) %>%
  ggplot2::ggplot(ggplot2::aes(x = quantile)) +
  ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coverage)) +
  cowplot::theme_cowplot() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("Quantile") +
  ggplot2::ylab("Percent observations below quantile")

ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
                           "quantile-coverage.png"),
                quantile_coverage_plot)








# ------------------------------------------------------------------------------
# plot with interval coverage over time
forecasts <- load_submission_files(dates = "all",
                                   models = "COVIDhub-baseline")

forecasts <- filter_forecasts(forecasts,
                              locations = NULL,
                              horizons = 1,
                              target_end_dates = "auto")

full <- prepare_for_scoring(forecasts)

scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "forecast_date", "state",
                                              "target_end_date", "horizon"),
                                       summarise_by = c("model", "target_end_date"))

coverage_time <- ggplot2::ggplot(scores, ggplot2::aes(x = target_end_date,
                                     y = calibration,
                                     colour = range,
                                     group = range)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = range / 100, colour = range),
                      linetype = "dashed") +
  ggplot2::geom_vline(ggplot2::aes(xintercept = target_end_date),
                      colour = "grey",
                      linetype = "dashed",
                      alpha = 0.4) +
  ggplot2::geom_label(data = scores %>%
                        dplyr::filter(target_end_date == max(target_end_date)),
                      ggplot2::aes(y = calibration,
                                   label = range,
                                   family = "Sans Serif")) +
  ggplot2::geom_line() +
  cowplot::theme_cowplot() +
  ggplot2::facet_grid(NULL) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("Forecast week") +
  ggplot2::ylab("Coverage rate")

ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
                           "coverage-time.png"),
                coverage_time)








# ------------------------------------------------------------------------------
# plot with points for explaining CRPS

df <- data.frame(pred = rnorm(9),
                 true_value = 0,
                 group = 1:9)

mae_plot <- df %>%
  ggplot2::ggplot(ggplot2::aes(x = group)) +
  ggplot2::geom_point(ggplot2::aes(y = pred)) +
  ggplot2::geom_segment(ggplot2::aes(x = group, y = pred,
                                     yend = true_value,
                                     xend = group),
                        colour = "dark red",
                        alpha = 0.3) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed") +
  cowplot::theme_cowplot() +
  ggplot2::facet_grid(NULL) +
  ggplot2::labs(x = "Point predictions",
                y = "Predicted values and mean absolute error") +
  ggplot2::expand_limits(y = c(0,1)) +
  ggplot2::coord_flip() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.position = "bottom")

quantiles <- seq(0.005, 0.999, 0.043)

df2 <- data.frame(true_value = 0,
                 quantiles = quantiles,
                 quant_pred = qnorm(quantiles, -0.6, 1.2))


quantile_mae <- df2 %>%
  ggplot2::ggplot(ggplot2::aes(x = quantiles)) +
  ggplot2::geom_point(ggplot2::aes(y = quant_pred)) +
  ggplot2::geom_segment(ggplot2::aes(x = quantiles, y = quant_pred,
                                     yend = true_value,
                                     xend = quantiles),
                        colour = "dark red",
                        alpha = 0.3) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed") +
  cowplot::theme_cowplot() +
  ggplot2::expand_limits(x = c(0,1)) +
  ggplot2::labs(x = "Quantiles of the predictive distribution",
                y = "Predicted value") +
  ggplot2::coord_flip() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom")


quantiles <- seq(0.005, 0.999, 0.001)

df3 <- data.frame(true_value = 0,
                  quantiles = quantiles,
                  quant_pred = qnorm(quantiles, -0.6, 1.2))

crps_mae <- df3 %>%
  ggplot2::ggplot(ggplot2::aes(x = quantiles)) +
  ggplot2::geom_line(ggplot2::aes(y = quant_pred)) +
  ggplot2::geom_area(ggplot2::aes(y = quant_pred),
                     fill = "dark red", alpha = 0.1) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed") +
  cowplot::theme_cowplot() +
  ggplot2::expand_limits(x = c(0,1)) +
  ggplot2::labs(x = "Predictive cumulative distribution",
                y = "Value") +
  ggplot2::coord_flip() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom")

mae_plots <- cowplot::plot_grid(mae_plot, quantile_mae, crps_mae,
                                ncol = 1,
                                rel_heights = c(1, 1),
                                scale = c(1, 1),
                                align = 'hv')


ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
                           "mae-plots.png"),
                mae_plots)







# if(!dir.exists(here::here("evaluation", "plots",
#                           forecast_date))) {
#   dir.create(here::here("evaluation", "plots",
#                         forecast_date))
# }


# national_plot <- plot_forecasts(national = TRUE, obs_weeks = 8, exclude_new_epiweek = FALSE)
#
#
# suppressWarnings(ggsave(here::here("evaluation", "plots",
#                                    forecast_date, "submission-national.png"),
#        plot = national_plot,
#        width = 10, height = 10, dpi = 300))
#
#
#
# subnational_plot <- plot_forecasts(national = FALSE, state_min_cutoff = 50, obs_weeks = 8,
#                                    exclude_new_epiweek = FALSE)
#
#
# suppressWarnings(ggsave(here::here("evaluation", "plots",
#                                    forecast_date, "submission-subnational.png"),
#        plot = subnational_plot,
#        width = 20, height = 25))
