source("utils/settings.R")

submission_dates <- as.character(settings$submission_dates)

submission_dates <- submission_dates[1:5]

for (forecast_date in submission_dates) {

  print(forecast_date)

  forecast_date <- as.Date(forecast_date)
  # update mean ensemble
  source("ensembling/mean-ensemble/update-equal-quantile-average.R")

  # update qra ensemble
  source("ensembling/qra-ensemble/update-qra-ensemble.R")

  # update qra ensemble
  source("ensembling/crps-ensemble/update-crps-ensemble.R")
}

debugonce(load_submission_files)

# ändern:
# - soll immer das aktuelle Datum einlesen



# update qra ensemble
# check script to load in data --> something isn't right

# update crps ensemble

# update evaluation
