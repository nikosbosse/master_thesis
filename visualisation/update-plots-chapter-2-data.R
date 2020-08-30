library(ggplot2)
library(RColorBrewer)
library(dplyr)


# ==============================================================================
# Plots chapter 2
# ==============================================================================

# source function for visualisation
source(here::here("visualisation", "plotting-functions", "visualise-data-functions.R"))
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))

forecast_date <- settings$forecast_date

# plot true data for selected states
plot <- plot_true_data(states = settings$states_included,
                       facet_formula = ~ state)

plot <- plot +
  ggplot2::labs(x = "") +
  ggplot2::geom_rect(ggplot2::aes(xmin = min(settings$evaluation_dates),
                                  xmax = max(settings$evaluation_dates),
                                  ymin = -Inf,
                                  ymax = Inf),
                     fill = "green",
                     alpha = 0.005)

ggplot2::ggsave(here::here("visualisation", "chapter-2-background-data",
                           "plot-observations.png"),
                plot,
                width = 10, height = 7)

# plot forecasts made for the US

plot <- plot_forecasts(states = "US")

ggplot2::ggsave(here::here("visualisation", "chapter-2-background-data", "overall-national.png"),
                plot,
                width = 14, height = 6)







# ------------------------------------------------------------------------------
# table with overview of the different models and ensembles

table <- tibble::tibble(locations = settings$states_included,
                        dates = c(sort(settings$evaluation_dates), rep(NA, 7)),
                        models = c(settings$model_names,
                                   c(NA, "mean-ensemble",
                                     "qra-ensemble",
                                     "crps-ensemble", NA)),
                        `model summary` = c("Baseline prediction model",
                                            "Official quantile average ensemble",
                                            "time series / growth rate model",
                                            "Bayesian SEIR model",
                                            "Machine Learning / SEIR model",
                                            "SEIR / human selection model",
                                            "Regression model",
                                            "Growth rate model",
                                            NA,
                                            "Quantile average ensemble",
                                            "QRA ensemble",
                                            "CRPS ensemble", NA))

saveRDS(table, here::here("visualisation", "chapter-2-background-data",
                          "table-overview.RDS"))



