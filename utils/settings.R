# determin a full set to be used -----------------------------------------------

# load in all data
# past_forecasts <- load_submission_files(dates = "all",
#                                         num_last = NULL,
#                                         models = settings$model_names)
#
# # get locations
# locations <- past_forecasts %>%
#   split(as.factor(past_forecasts$model)) %>%
#   purrr::map_dfr(.f = function(x) {
#     df <- data.frame(location = unique(x$location),
#                      model = unique(x$model))
#     return(df)
#   }) %>%
#   dplyr::group_by(location) %>%
#   dplyr::add_count() %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(n == max(n)) %>%
#   dplyr::pull(location) %>%
#   unique()
#
# # get all horizons
# horizons <- past_forecasts %>%
#   dplyr::mutate(horizon = as.numeric(substr(target, 1, 1))) %>%
#   split(as.factor(past_forecasts$model)) %>%
#   purrr::map_dfr(.f = function(x) {
#     df <- data.frame(horizon = unique(x$horizon),
#                      model = unique(x$model))
#     return(df)
#   }) %>%
#   dplyr::group_by(horizon) %>%
#   dplyr::add_count() %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(n == max(n)) %>%
#   dplyr::pull(horizon) %>%
#   unique()

# get all dates

model_names <- c("COVIDhub-baseline",
                 "COVIDhub-ensemble",
                 "epiforecasts-ensemble1",
                 #"IHME-CurveFit",
                 "UMass-MechBayes",
                 "YYG-ParamSearch")


model_names_eval <- c(model_names,
                      "crps-ensemble", "mean-ensemble", "qra-ensemble")

submission_dates <- lubridate::ymd("2020-07-27") - seq(0, 13 * 7, 7)
submission_dates <- submission_dates[1:7]

# check at some point that models all have the correct target_end_dates
target_end_dates <- submission_dates + 5

if (!exists("forecast_date")) {
  forecast_date <- Sys.Date()
}

n_samples <- 1000

num_last <- 2

locations_to_plot <-c("US", "New York", "California",
                      "Texas", "Virginia", "Florida")

settings <- list(submission_dates = submission_dates,
                 locations = locations,
                 forecast_date = forecast_date,
                 target_end_dates = target_end_dates,
                 model_names = model_names,
                 model_names_eval = model_names_eval,
                 n_samples = n_samples,
                 num_last = num_last,
                 horizons = horizons,
                 locations_to_plot = locations_to_plot)
