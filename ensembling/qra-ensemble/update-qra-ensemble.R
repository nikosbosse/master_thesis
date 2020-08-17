
library(magrittr)
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))


# get weights ------------------------------------------------------------------

# load past forecasts
forecast_date <- settings$forecast_date

# manual
if (forecast_date == "2020-06-29") {
  settings$num_last <-  1
}

past_forecasts <- load_submission_files(dates = as.Date(settings$forecast_date),
                                        num_last = settings$num_last,
                                        models = settings$model_names,
                                        drop_latest_forecast = TRUE)

full_set <- filter_forecasts(past_forecasts,
                             locations = settings$locations_included,
                             horizons = settings$horizons)

# store quantiles available
tau <- full_set$quantile %>%
  round(digits = 3) %>%
  unique()

# combine with deaths
combined <- combine_with_deaths(full_set)

# reformat
combined <- combined %>%
  dplyr::mutate(quantile = round(quantile, digits = 3)) %>%
  tidyr::pivot_wider(values_from = value, names_from = quantile,
                     names_prefix="quantile_") %>%
  dplyr::arrange(target, target_end_date, location, model, epiweek) %>%
  dplyr::select(-c(forecast_date, target, target_end_date, location, epiweek, state))


# extract true values and check if they have the correct length
models <- unique(combined$model)

true_values <- combined %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(n = 1:dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(n) %>%
  dplyr::summarise(deaths = unique(deaths), .groups = "drop_last") %>%
  .$deaths

# this should be TRUE
if(!(length(true_values) == (nrow(combined))  / length(models))){
  warning("QRA: check that true values and models align")
  message(paste("number of true values: ", length(true_values)))
  message(paste("number of rows in df: ", nrow(combined)))
  message(paste("number of models: ", length(models)))
}

# extract forecasts as matrices and store as quantgen array
qarr <- combined %>%
  dplyr::select(-deaths, -horizon) %>%
  dplyr::group_split(model, .keep = FALSE) %>%
  setNames(models) %>%
  purrr::map(.f = as.matrix) %>%
  quantgen::combine_into_array()

model_weights <- quantgen::quantile_ensemble(qarr = qarr,
                                             y = true_values,
                                             verbose = FALSE,
                                             tau = tau)$alpha

message("QRA weights:")
message(paste0("\n", models, "\n", model_weights, "\n"))

weights_df <- data.frame(model = models,
                         weights = model_weights,
                         forecast_date = forecast_date,
                         num_last = settings$num_last)
filename <- here::here("ensembling", "qra-ensemble", "qra-weights",
                       paste0(forecast_date, "-crps-weights.csv"))
data.table::fwrite(weights_df, file = filename)



# ensembling -------------------------------------------------------------------
forecasts <- load_submission_files(dates = settings$forecast_date,
                                   num_last = NULL,
                                   models = settings$model_names)

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = settings$horizons,
                              types = c("quantile", "point"))

# pivot_wider
forecasts_wide <- forecasts %>%
  dplyr::mutate(quantile = round(quantile, digits = 3)) %>%
  tidyr::pivot_wider(names_from = model,
                     values_from = value)

qra_ensemble <- forecasts_wide %>%
  dplyr::mutate(ensemble = forecasts_wide %>%
                dplyr::select(dplyr::all_of(models)) %>%
                as.matrix() %>%
                matrixStats::rowWeightedMeans(w = model_weights,
                                              na.rm = TRUE)) %>%
  dplyr::rename(value = ensemble) %>%
  dplyr::select(-dplyr::all_of(models)) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)
  # round values after ensembling
  # dplyr::mutate(value = round(value))


# write dated file
if (!dir.exists(here::here("data", "processed-data", "qra-ensemble"))) {
  dir.create(here::here("data", "processed-data", "qra-ensemble"))
}

filename <- here::here("data", "processed-data", "qra-ensemble",
                       paste0(forecast_date, "-epiforecasts-ensemble1-qra.csv"))
data.table::fwrite(qra_ensemble, filename)
