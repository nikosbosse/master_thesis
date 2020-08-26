# determin a full set to be used -----------------------------------------------

model_names <- c("COVIDhub-baseline",
                 "COVIDhub-ensemble",
                 "epiforecasts-ensemble1",
                 #"IHME-CurveFit",
                 "UMass-MechBayes",
                 "YYG-ParamSearch",
                 "CU-select",
                 "UT-Mobility",
                 "LANL-GrowthRate")

horizons <- 1:4

model_names_eval <- c(model_names,
                      "crps-ensemble", "mean-ensemble", "qra-ensemble")

submission_dates <- lubridate::ymd("2020-08-03") - seq(0, 7 * 7, 7)
#submission_dates <- submission_dates[1:7]

# check at some point that models all have the correct target_end_dates
target_end_dates <- submission_dates + 5

if (!exists("forecast_date")) {
  forecast_date <- Sys.Date()
}

n_samples <- 1000

num_last <- 2

locations_included <- c("04", "06", "12", "13", "17", "24", "25", "34",
                        "36", "39", "42", "48", "US")

states_included <- c("Arizona", "California", "Florida", "Georgia", "Illinois",
                     "Maryland", "Massachusetts", "New Jersey", "New York",
                     "Ohio", "Pennsylvania", "Texas", "US")


locations_to_plot <-c("US", "New York", "California",
                      "Texas", "Virginia", "Florida")

# cut the last two dates as these are needed to form the ensemble weights
evaluation_dates <- submission_dates[submission_dates >= as.Date("2020-06-29")]


manual_colours <- c(RColorBrewer::brewer.pal(8, name = "Set2")[-6],
                    RColorBrewer::brewer.pal(7, name = "Set1")[c(1, 2, 4, 7)])


settings <- list(submission_dates = submission_dates,
                 forecast_date = forecast_date,
                 target_end_dates = target_end_dates,
                 model_names = model_names,
                 model_names_eval = model_names_eval,
                 n_samples = n_samples,
                 num_last = num_last,
                 horizons = horizons,
                 locations_to_plot = locations_to_plot,
                 locations_included = locations_included,
                 states_included = states_included,
                 evaluation_dates = evaluation_dates,
                 manual_colours = manual_colours)
