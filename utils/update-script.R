source("utils/settings.R")

submission_dates <- as.character(settings$submission_dates)

# filter out the last two dates - these are needed to obtain the QRA weights
submission_dates <- submission_dates[submission_dates > as.Date("2020-06-22")]

ensemble_past_included <- 4
crps_optimisation_horizon <- 4

for (forecast_date in submission_dates) {

  forecast_date <- as.Date(forecast_date)
  print(forecast_date)

  forecast_date <- as.Date(forecast_date)
  # update mean ensemble
  # source("ensembling/mean-ensemble/update-equal-quantile-average.R")
  # print("mean ensemble done")

  # update qra ensemble
  # source("ensembling/qra-ensemble/update-qra-ensemble.R")
  # print("QRA ensemble done")

  # update CRPS ensemble
  source("ensembling/crps-ensemble/update-crps-ensemble.R")
  print("CRPS ensemble done")
}

