
# Load required packages and functions -----------------------------------------

library(magrittr)

# source settings
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))


# Load Forecasts ---------------------------------------------------------------
forecasts <- load_submission_files(dates = settings$forecast_date,
                                   models = settings$model_names) %>%
  dplyr::filter(grepl("wk", target),
                grepl("death", target))

# get forecast_date
forecast_date <- settings$forecast_date


# average quantiles ------------------------------------------------------------

# store models as strings
models <- unique(forecasts$model)

# pivot_wider
forecasts_wide <- forecasts %>%
  dplyr::mutate(quantile = round(quantile, digits = 3)) %>%
  tidyr::pivot_wider(names_from = model,
                     values_from = value)

# add average; rename ensemble to value
mean_ensemble <- forecasts_wide %>%
  dplyr::mutate(ensemble = forecasts_wide %>%
                  dplyr::select(dplyr::all_of(models)) %>%
                  rowMeans(na.rm = TRUE)) %>%
  dplyr::rename(value = ensemble) %>%
  dplyr::select(-dplyr::all_of(models)) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value) %>%
  # round values after ensembling
  dplyr::mutate(value = round(value))


# store as csv submission ------------------------------------------------------
if(!dir.exists(here::here("data", "processed-data", "mean-ensemble"))) {
  dir.create(here::here("data", "processed-data", "mean-ensemble"))
}

filename <- here::here("data", "processed-data", "mean-ensemble",
                       paste0(as.character(forecast_date), "-mean-ensemble.csv"))
data.table::fwrite(mean_ensemble, filename)
