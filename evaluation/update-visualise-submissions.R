library(ggplot2)
library(RColorBrewer)
library(dplyr)

# source function for visualisation
source(here::here("evaluation", "visualise-submission-functions.R"))
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))

forecast_date <- settings$forecast_date

plot <- plot_forecasts()

ggplot2::ggsave(here::here("evaluation", "plots", "overall-national.png"),
                plot,
                width = 14, height = 6)



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
