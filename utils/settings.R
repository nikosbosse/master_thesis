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

model_names_eval <- sort(c(model_names,
                      "crps-ensemble", "mean-ensemble", "qra-ensemble"))

ensemble_names_all <- c("crps-ensemble-1-1",
                        "crps-ensemble-2-1",
                        "crps-ensemble-2-2",
                        "crps-ensemble-3-1",
                        "crps-ensemble-3-2",
                        "crps-ensemble-3-3",
                        "crps-ensemble-4-1",
                        "crps-ensemble-4-2",
                        "crps-ensemble-4-3",
                        "crps-ensemble-4-4",
                        "qra-ensemble-1",
                        "qra-ensemble-2",
                        "qra-ensemble-3",
                        "qra-ensemble-4",
                        "COVIDhub-ensemble",
                        "crps-ensemble-metalog-2-2")

submission_dates <- lubridate::ymd("2020-08-03") - seq(0, 6 * 7, 7)
#submission_dates <- submission_dates[1:7]

# check at some point that models all have the correct target_end_dates
target_end_dates <- submission_dates + 5

if (!exists("forecast_date")) {
  forecast_date <- Sys.Date()
}

n_samples <- 1000

if (!exists("ensemble_past_included")) {
  ensemble_past_included <- 2
}

if (!exists("crps_optimisation_horizon")) {
  crps_optimisation_horizon <- 2
}





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
                    RColorBrewer::brewer.pal(7, name = "Set1")[c(1, 2, 4, 7, 5)],
                    RColorBrewer::brewer.pal(8, name = "Dark2")[c(1, 4, 8, 6)])

colour_df <- data.frame(model_names = c(model_names_eval, rep(NA, 5)),
                        colours = manual_colours,
                        ensemble_names = c(ensemble_names_all))

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
                 manual_colours = manual_colours,
                 ensemble_past_included = ensemble_past_included,
                 crps_optimisation_horizon = crps_optimisation_horizon,
                 ensemble_names_all = ensemble_names_all,
                 colour_df = colour_df)
