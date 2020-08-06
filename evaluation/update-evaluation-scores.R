# ============================================================================ #
# load data
# ============================================================================ #
library(magrittr)
library(dplyr)

source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))

# load past forecasts
# Get observed data ------------------------------------------------------------------

deaths <- get_us_deaths(data = "weekly") %>%
  dplyr::filter(epiweek < max(epiweek))

# Get forecasts -----------------------------------------------------------
forecasts <- load_submission_files(dates = "all",
                                   models = settings$model_names_eval)

forecasts <- filter_forecasts(forecasts,
                              locations = NULL,
                              horizons = "auto",
                              target_end_dates = "auto")

combined <- combine_with_deaths(forecasts)

# reformat for scoring
combined <- combined %>%
  dplyr::rename(true_values = deaths,
                predictions = value) %>%
  dplyr::mutate(boundary = ifelse(quantile <= 0.5, "lower", "upper"),
                range = abs(1 - 2 * quantile) * 100)

full <- dplyr::bind_rows(combined,
                         combined %>%
                           dplyr::filter(quantile == 0.5) %>%
                           dplyr::mutate(boundary = "upper")) %>%
  # weird unexplicable rounding error?
  dplyr::mutate(range = round(range, digits = 0),
                horizon = as.numeric(substring(target, 1, 2))) %>%
  dplyr::select(-target, -quantile, -location) %>%
  unique() %>%
  # filter away duplicate forecasts if there are any (if there aren't, this has no effect)
  dplyr::group_by(forecast_date, model, state, epiweek, boundary, range, horizon) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()


# ============================================================================ #
# evaluate forecasts and plot
# ============================================================================ #

# source plotting function
source(here::here("evaluation", "evaluation-plots-function.R"))


# regular plots
scores <- scoringutils::eval_forecasts(full, summarised = TRUE,
                                       by = c("model", "state", "target_end_date",
                                              "horizon"),
                                       quantiles = c(0.05, 0.25, 0.75, 0.95),
                                       summarise_by = c("model", "horizon"))

unsummarised_scores <- scoringutils::eval_forecasts(full, summarised = FALSE,
                                                    by = c("model", "state",
                                                           "target_end_date",
                                                           "horizon"))


# weighted interval score overview plot
summarised_scores <- scoringutils::eval_forecasts(full, summarised = TRUE,
                                                  by = c("model", "state",
                                                         "target_end_date",
                                                         "horizon"),
                                                  interval_score_arguments = list(weigh = TRUE),
                                                  summarise_by = c("model"))


wis_overview_plot <- score_overview_plot(summarised_scores)















plots <- plot_scores(scores %>%
                       dplyr::filter(range %in% c(0, 20, 50, 90)))

# state plots
scores_state <- scoringutils::eval_forecasts(full, summarised = TRUE,
                                       by = c("model", "state", "submission_date",
                                              "horizon"),
                                       quantiles = c(0.05, 0.25, 0.75, 0.95),
                                       summarise_by = c("model", "state", "horizon"))



plots_state <- plot_scores(scores_state,
                           y = "state",
                           dodge_width = 0.75)

current_date <- Sys.Date()

if(!dir.exists(here::here("evaluation", "plots",
                          current_date, "scoring"))) {
  dir.create(here::here("evaluation", "plots",
                        current_date, "scoring"))
}

# regular plots
suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                  current_date, "scoring", "interval_scores.png"),
       plot = plots$interval_score_plot,
       width = 10, height = 10, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                           current_date, "scoring", "calibration.png"),
                plot = plots$calibration_plot,
                width = 10, height = 10, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                           current_date,"scoring",  "bias.png"),
                plot = plots$bias_plot,
                width = 10, height = 10, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                           current_date, "scoring", "sharpness.png"),
                plot = plots$sharpness_plot,
                width = 10, height = 10, dpi = 300))


# state plots
suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                           current_date, "scoring", "state_interval_scores.png"),
                plot = plots_state$interval_score_plot,
                width = 10, height = 35, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                           current_date, "scoring", "state_calibration.png"),
                plot = plots_state$calibration_plot,
                width = 10, height = 35, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                           current_date, "scoring", "state_bias.png"),
                plot = plots_state$bias_plot,
                width = 10, height = 35, dpi = 300))

suppressWarnings(ggplot2::ggsave(here::here("evaluation", "plots",
                           current_date, "scoring", "state_sharpness.png"),
                plot = plots_state$sharpness_plot,
                width = 10, height = 35, dpi = 300))

