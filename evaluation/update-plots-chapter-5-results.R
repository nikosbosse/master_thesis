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


several_states_one_week <- plot_forecasts(states = c("California", "New York",
                                                   "Texas", "Washington"),
                                       forecasts = forecasts,
                                       facet_formula = model ~ state,
                                       ncol_facet = 4,
                                       horizons = c(1),
                                       obs_weeks = 7)


several_states_one_week













# ------------------------------------------------------------------------------
# plot with all scores in all states -------------------------------------------

full <- prepare_for_scoring(forecasts)

scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "state")) %>%
  dplyr::filter(range == 60) %>%
  dplyr::arrange(state, model, interval_score) %>%
  data.table::dcast(state ~ model, value.var = "interval_score")
# ISSUE: SUMMARISE INTERVAL SCORE CORRECTLY

# Plot I want:
#         Model1 Model2
# State1
# State2
# + colour coding










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

