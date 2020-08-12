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






