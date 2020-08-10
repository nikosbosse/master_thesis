library(ggplot2)
library(RColorBrewer)
library(dplyr)


# ==============================================================================
# Plots chapter 2
# ==============================================================================

# source function for visualisation
source(here::here("evaluation", "visualise-data-functions.R"))
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))

forecast_date <- settings$forecast_date

# plot true data for selected states
plot <- plot_true_data(states = settings$locations_to_plot,
                       facet_formula = ~ state)

ggplot2::ggsave(here::here("evaluation", "plots", "plot-observations.png"),
                plot,
                width = 12, height = 8)

# plot forecasts made for the US

plot <- plot_forecasts(states = "US")

ggplot2::ggsave(here::here("evaluation", "plots", "overall-national.png"),
                plot,
                width = 14, height = 6)











# ==============================================================================
# Plots chapter 3 evaluation
# ==============================================================================

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


# plot with one model and US and bias of that plot -----------------------------
US_plot_baseline <- plot_forecasts(states = "US",
                                   models = "COVIDhub-baseline",
                                   obs_weeks = 13)



scores <- scoringutils::eval_forecasts(full,
                             by = c("model", "target_end_date"))

plot_bias <- ggplot2::ggplot() +
ggplot2::geom_point(data = scores, ggplot2::aes(y = bias,
                                                x = target_end_date),
                      colour = "black") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(x = "Week", y = "Bias",
                caption = NULL,
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
# calibration plot of US forecasts from baseline model -------------------------

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
scores <- scoringutils::eval_forecasts(full,
                                       by = c("model"))

## overall model calibration - empirical interval coverage
interval_coverage <- ggplot2::ggplot(scores, ggplot2::aes(x = range)) +
  ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
  cowplot::theme_cowplot() +
  ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("nominal coverage") +
  ggplot2::ylab("empirical coverage")

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
  dplyr::summarise(coverage = mean(deaths < value)) %>%
  ggplot2::ggplot(ggplot2::aes(x = quantile)) +
  ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coverage)) +
  cowplot::theme_cowplot() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("nominal quantile coverage") +
  ggplot2::ylab("empirical quantile coverage")

ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
                           "quantile-coverage.png"),
                quantile_coverage_plot)



# ------------------------------------------------------------------------------
# plot with interval coverage over time
#
# forecasts <- load_submission_files(dates = "all",
#                                    models = "COVIDhub-baseline")
#
# forecasts <- filter_forecasts(forecasts,
#                               locations = "auto",
#                               horizons = 1,
#                               target_end_dates = "auto")
#
# full <- prepare_for_scoring(forecasts)
#
# full %>%
#   dplyr::select(-epiweek, -target_end_date, -forecast_date) %>%
#   scoringutils::eval_forecasts()
#
# scores <- scoringutils::eval_forecasts(full,
#                                        by = c("model"))




























## calibration plot for different models
## overall model performance
ggplot2::ggplot(scores, ggplot2::aes(x = target_end_date,
                                     y = calibration,
                                     colour = range,
                                     group = range)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = range / 100, colour = range),
                      linetype = "dashed") +
  ggplot2::geom_vline(ggplot2::aes(xintercept = target_end_date),
                      colour = "grey",
                      linetype = "dashed") +
  ggplot2::geom_line() +
  cowplot::theme_cowplot() +
  ggplot2::facet_grid(NULL) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("horizon") +
  ggplot2::ylab("coverage rate")










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
