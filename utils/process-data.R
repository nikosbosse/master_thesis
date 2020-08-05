# file that takes in the files from the relevant models, compares which dates are
# available and changes the file name and the forecast date to the submission date


source(here::here("utils", "settings.R"))

submission_dates <- settings$submission_dates
models <- settings$model_names


results <- list()

for (model in models) {
  msg <- paste("--------- processing model", model, "---------")
  message(msg)

  if(!dir.exists(here::here("data", "processed-data", model))) {
    dir.create(here::here("data", "processed-data", model))
  }

  files <- list.files(here::here("data", model))
  files <- files[grepl(".csv", files)]
  available_dates <- as.Date(substr(files, 1, 10))

  # all submission dates are available
  if(all(submission_dates %in% available_dates)) {
    msg <- paste("all dates immediately available for model", model)
    message(msg)

    #filter out submisisons made in addition to the ones required
    files <- files[available_dates %in% submission_dates]

    file.copy(here::here("data", model, files), here::here("data", "processed-data", model, files))

    results[[model]] <- data.frame(model = model,
                                   not_recovered = "none")

    # not all submission dates are available
  } else {
    missing_dates <- submission_dates[!(submission_dates %in% available_dates)]
    missing_dates <- as.Date(missing_dates)
    msg <- paste0("the following dates are not immediately available for model ",
                  model, ": ",
                  paste(as.character(missing_dates), collapse = ", "))
    message(msg)

    # copy files that are immediately correct
    correct_dates <- submission_dates[submission_dates %in% available_dates]
    correct_files <- files[available_dates %in% submission_dates]
    file.copy(here::here("data", model, correct_files),
              here::here("data", "processed-data", model, correct_files))

    not_recovered <- c()
    for (date in as.character(missing_dates)) {
      prior_date <- as.Date(date) - 1:5

      # recover, if prior date is available
      if (any(prior_date %in% available_dates)) {
        prior_date <- prior_date[prior_date %in% available_dates]
        file <- here::here("data", model, paste0(prior_date, "-", model, ".csv"))
        df <- data.table::fread(file)
        # change forecast date to submission date
        df[, forecast_date := as.character(date)]

        #save data.frame
        file_out <- here::here("data", "processed-data", model,
                               paste0(date, "-", model, ".csv"))
        data.table::fwrite(df, file = file_out)

        correct_dates <- c(correct_dates, date)
      } else {
        # else collect information about which dates were not recoverable
        not_recovered <- c(not_recovered, date)
      }

      # print out diagnostics
    }
    if (length(not_recovered > 0)) {
      msg <- paste("the following dates remain missing:", paste(not_recovered, collapse = ", "))
      message(msg)

      results[[model]] <- data.frame(model = model,
                                     not_recovered = as.character(not_recovered))

    } else {
      msg <- paste("all dates successfully recovered for model", model)
      message(msg)

      results[[model]] <- data.frame(model = model,
                                     not_recovered = "none")
    }
  }
}

results <- data.table::rbindlist(results)
