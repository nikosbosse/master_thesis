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

forecasts <- load_submission_files(dates = "all",
                                   models = settings$model_names_eval)

forecasts <- filter_forecasts(forecasts,
                              locations = NULL,
                              horizons = c(1, 2, 3, 4),
                              target_end_dates = "auto")


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

ggplot2::ggsave(here::here("visualisation", "chapter-5-results",
                           "US-forecast-1-wk-ahead.png"),
                US_forecast_one_week, width = 10, height = 5)


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

full <- prepare_for_scoring(forecasts)

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

