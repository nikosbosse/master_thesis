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
                                   models = "COVIDhub-baseline")

forecasts <- filter_forecasts(forecasts,
                              locations = "US",
                              horizons = 1,
                              target_end_dates = "auto")

full <- prepare_for_scoring(forecasts)



