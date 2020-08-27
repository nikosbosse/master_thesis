# ============================================================================ #
# do ensembling with predictive samples
# ============================================================================ #

library(dplyr)
library(magrittr)
library(data.table)

# source settings
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))
source(here::here("ensembling", "crps-ensemble", "fit-distribution-functions.R"))

forecast_date <- settings$forecast_date

# ---------------------------------------------------------------------------- #
# ------------------ get weights based on past observations ------------------ #
# ---------------------------------------------------------------------------- #

# load past forecasts ----------------------------------------------------------
# filter out current forecast in order to optimise only on previous forecasts
# also filter only horizon == 1 for stackr optimisation
today <- forecast_date

num_last <- settings$ensemble_past_included

# manual corection if not enough past observations available
if (forecast_date == "2020-06-29") {
  num_last <-  1
}
if (forecast_date == "2020-07-06") {
  if (num_last > 2)
    num_last <-  2
}
if (forecast_date == "2020-07-13") {
  if (num_last > 3) {
    num_last <-  1
  }
}

optimisation_horizon <- settings$crps_optimisation_horizon

# Load Forecasts ---------------------------------------------------------------
past_forecasts <- load_submission_files(dates = forecast_date,
                                        num_last = num_last,
                                        models = settings$model_names,
                                        drop_latest_forecast = TRUE)

full_set <- filter_forecasts(past_forecasts,
                             locations = settings$locations_included,
                             horizons = optimisation_horizon)

while (nrow(full_set) == 0) {
  optimisation_horizon <- optimisation_horizon - 1
  full_set <- filter_forecasts(past_forecasts,
                               locations = settings$locations_included,
                               horizons = optimisation_horizon)

}

combined <- combine_with_deaths(full_set)


# convert to samples -----------------------------------------------------------
forecasts <- data.table::as.data.table(combined)
n_samples = settings$n_samples

samples <- forecasts[, .(y_pred = get_samples(value, quantile, n_samples = n_samples),
                         sample_nr = 1:n_samples,
                         state = unique(state),
                         y_obs = unique(deaths)),
                     by = c("model", "forecast_date",
                            "target_end_date",
                            "target", "location")]

samples <- samples %>%
  dplyr::rename(geography = location,
                date = target_end_date)


w <- stackr::crps_weights(data = samples,
                          lambda = "equal")


message("CRPS weights:")
message(paste0("\n", names(w), "\n", w, "\n"))

# save weights
weights_df <- data.frame(model = names(w),
                         weights = w,
                         forecast_date = forecast_date,
                         num_last = settings$num_last)


if (!dir.exists(here::here("ensembling", "crps-ensemble", "crps-weights",
                           paste(settings$ensemble_past_included,
                                 settings$crps_optimisation_horizon, sep = "-")))) {
  dir.create(here::here("ensembling", "crps-ensemble", "crps-weights",
                        paste(settings$ensemble_past_included,
                              settings$crps_optimisation_horizon, sep = "-")))
}

filename <- here::here("ensembling", "crps-ensemble", "crps-weights",
                       paste(settings$ensemble_past_included,
                             settings$crps_optimisation_horizon, sep = "-"),
                       paste0(forecast_date,
                              "-crps-weights.csv"))
data.table::fwrite(weights_df, file = filename)



# ---------------------------------------------------------------------------- #
# ------------------ create ensemble and make submission  -------------------- #
# ---------------------------------------------------------------------------- #

# Load Forecasts ---------------------------------------------------------------
past_forecasts <- load_submission_files(dates = forecast_date,
                                        num_last = NULL,
                                        models = settings$model_names)

full_set <- filter_forecasts(past_forecasts,
                             locations = settings$locations_included,
                             horizons = settings$horizons) %>%
  dplyr::mutate(date = target_end_date,
                y_pred = value,
                geography = state)

forecasts <- data.table::as.data.table(full_set)

samples <- forecasts[, .(y_pred = get_samples(value, quantile, n_samples = n_samples),
                         sample_nr = 1:n_samples,
                         state = unique(state)),
                     by = c("model", "forecast_date",
                            "target_end_date",
                            "target", "location")]

# unsure what do do with warning In qgamma(quantiles, shape = par[1], rate = par[2]) : NaNs produced

samples <- samples %>%
  dplyr::mutate(geography = state,
                date = target_end_date)

# make ensemble ----------------------------------------------------------------
ensemble <- stackr::mixture_from_samples(samples, weights = w)

# make submission --------------------------------------------------------------
state_codes <- tigris::fips_codes %>%
  dplyr::select(state_code, state_name) %>%
  unique() %>%
  rbind(c("US", "US"))

incidences <- ensemble %>%
  dplyr::rename(state_name = geography) %>%
  dplyr::inner_join(state_codes, by = "state_name") %>%
  dplyr::rename(location = state_code,
                target_end_date = date,
                value = y_pred) %>%
  dplyr::mutate(forecast_date = as.Date(forecast_date),
                target_end_date = as.Date(target_end_date),
                horizon = round(as.numeric(target_end_date - forecast_date) / 7),
                inc_or_cum = "inc")


cumulative <- incidences %>%
  dplyr::group_by(location, target_end_date, sample_nr) %>%
  dplyr::summarise(value = sum(value),
                   horizon = unique(horizon), .groups = "drop") %>%
  dplyr::mutate(inc_or_cum = "cum")

combined <- incidences %>%
  dplyr::bind_rows(cumulative) %>%
  dplyr::mutate(value = round(value)) %>%
  dplyr::group_by(target_end_date, location, inc_or_cum, horizon) %>%
  dplyr::group_modify( ~ {
    quantile(.x$value, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
      tibble::enframe(name = "quantile", value = "value") %>%
      dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)
  })  %>%
  dplyr::ungroup() %>%
  dplyr::mutate(target = paste(horizon,
                               "wk ahead",
                               inc_or_cum,
                               "death",
                               sep = " "),
                type = "quantile")

combined <- combined %>%
  dplyr::bind_rows(combined %>%
                     dplyr::filter(quantile == 0.5) %>%
                     dplyr::mutate(type = "point",
                                   quantile = NA)) %>%
  dplyr::mutate(forecast_date = forecast_date) %>%
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)



# write file

if (!dir.exists(here::here("data", "processed-data",
                           paste("crps-ensemble",
                                 settings$ensemble_past_included,
                                 settings$crps_optimisation_horizon, sep = "-")))) {
  dir.create(here::here("data", "processed-data",
                        paste("crps-ensemble",
                              settings$ensemble_past_included,
                              settings$crps_optimisation_horizon, sep = "-")))
}
filename <- here::here("data", "processed-data",
                       paste("crps-ensemble",
                             settings$ensemble_past_included,
                             settings$crps_optimisation_horizon, sep = "-"),
                       paste0(forecast_date, "-crps-ensemble.csv"))

data.table::fwrite(combined, filename)

