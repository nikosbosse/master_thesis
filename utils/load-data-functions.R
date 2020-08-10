# Get & reshape JHU data
# Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data

library(magrittr)

# Arguments
# data = c("cumulative", "daily")
# anomaly_threshold = minimum death count above which to adjust anomalies (e.g 100)
# check_adjustmente = boolean, if TRUE prints which states have had data adjusted

# Deaths data -------------------------------------------------------------
get_us_deaths <- function(data = "daily", anomaly_threshold = 100,
                          check_adjustment = FALSE, load_from_server = FALSE){

  if(!load_from_server) {
    if(data == "daily"){
      daily <- readRDS(here::here("data", "processed-data", "deaths-data-daily.rds"))
      return(daily)
    }

    if(data == "cumulative"){
      cumulative_adj <- readRDS(here::here("data", "processed-data",
                                           "deaths-data-cumulative.rds"))
      return(cumulative_adj)
    }
    if(data == "weekly"){
      weekly <- readRDS(here::here("data", "processed-data", "deaths-data-weekly.rds"))
      return(weekly)
    }
  }

  # Get & reshape data
  cumulative <- suppressMessages(readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")) %>%
    dplyr::select(Province_State, dplyr::matches("^\\d")) %>%
    tidyr::pivot_longer(cols = -Province_State, names_to = "date", values_to = "deaths") %>%
    dplyr::mutate(date = lubridate::mdy(date)) %>%
    dplyr::group_by(Province_State, date) %>%
    dplyr::summarise(deaths = sum(deaths), .groups = "drop_last") %>%
    dplyr::rename(state = Province_State) %>%
    dplyr::mutate(epiweek = lubridate::epiweek(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!state %in% c("Diamond Princess", "Grand Princess"))

  # Import known data issues
  # The source for this is NYT: https://github.com/nytimes/covid-19-data
  state_data_issues <- suppressMessages(readr::read_csv(here::here("utils", "state_data_issues.csv"))) %>%
    dplyr::mutate(date = lubridate::dmy(date)) %>%
    dplyr::filter(problematic == TRUE)

  # Convert to daily data
  daily <- cumulative %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(deaths = c(0, diff(deaths)),
                  deaths = replace(deaths, deaths < 0 , 0)) %>%
    dplyr::ungroup() %>%
    # Adjust known data issues
    dplyr::left_join(state_data_issues %>%
                       dplyr::select(state, date, lag, lag_days),
                     by = c("state", "date")) %>%
    dplyr::mutate(raw_deaths = deaths,
                  lag = ifelse(is.na(lag), FALSE, lag),
                  deaths = ifelse(lag == TRUE & lag_days == 1,
                                  dplyr::lag(raw_deaths, n = 1),
                                  ifelse(lag == TRUE & lag_days == 7,
                                         dplyr::lag(raw_deaths, n = 7),
                                         raw_deaths))) %>%
    dplyr::group_by(state) %>%
    # Detect & adjust anomalies (>1000% change)
    dplyr::mutate(p_diff = deaths / dplyr::lag(deaths),
                  p_diff = ifelse(p_diff == "Inf", 0, p_diff),
                  extreme_diff = ifelse(abs(p_diff) > 10, TRUE, FALSE),
                  anomaly_adjusted = ifelse(extreme_diff == TRUE & deaths > anomaly_threshold, TRUE, FALSE),
                  deaths = ifelse(anomaly_adjusted == TRUE,
                                  dplyr::lag(deaths, n = 1),
                                  deaths)) %>%
    dplyr::ungroup() %>%
    dplyr::select(state, date, epiweek, deaths, raw_deaths,
                  -extreme_diff, -p_diff, -lag_days,
                  known_issue_adjusted = lag, anomaly_adjusted)


  # Print adjusted states
  if(check_adjustment){
    # Get states with known data issues
    known_issues <- dplyr::filter(daily, known_issue_adjusted == TRUE) %>%
      dplyr::pull(state) %>%
      unique()

    # Get states with new detected anomalies
    anomaly_adjusted <- dplyr::filter(daily, anomaly_adjusted == TRUE) %>%
      dplyr::pull(state) %>%
      unique()

    message(writeLines(text = c("* Known data issues adjusted in:",
                                known_issues,
                                "* New anomalies detected, data adjusted in:",
                                anomaly_adjusted
    )))
  }

  # Re-accumulate over adjusted data
  cumulative_adj <- daily %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(deaths = cumsum(deaths),
                  raw_deaths = cumsum(raw_deaths)
    )

  # Return data
  if(data == "daily"){
    # Save daily deaths in all states
    # saveRDS(daily, here::here("data", "processed-data", "deaths-data-daily.rds"))
    return(daily)
  }

  if(data == "cumulative"){
    # saveRDS(cumulative_adj, here::here("data", "processed-data", "deaths-data-cumulative.rds"))
    return(cumulative_adj)
  }

  if (data == "weekly") {
    # load deaths
    weekly <- daily %>%
      dplyr::group_by(epiweek, state) %>%
      dplyr::summarise(deaths = sum(deaths),
                       target_end_date = max(date),
                       .groups = "drop_last") %>%
      dplyr::ungroup()

    weekly <- dplyr::bind_rows(weekly,
                               weekly %>%
                                 dplyr::group_by(epiweek) %>%
                                 dplyr::summarise(state = "US",
                                                  deaths = sum(deaths),
                                                  target_end_date = unique(target_end_date),
                                                  .groups = "drop_last"))
    # saveRDS(weekly, here::here("data", "processed-data", "deaths-data-weekly.rds"))
    return(weekly)
  }

}


load_submission_files <- function(dates = c("latest", "all"),
                                  num_last = NULL,
                                  models = c("all"),
                                  drop_latest_forecast = FALSE) {

  if (models[1] == "all") {
    model_names <- list.files("data", "processed-data")
  } else {
    model_names <- models
  }

  load_model_files <- function(model_name) {

    files <- sort(list.files(here::here("data", "processed-data", model_name)))
    files <- files[grepl(".csv", files)]

    if (as.character(dates[1]) == "all") {
      # if num_last is specified, only the fetch the last num_last
      if (!is.null(num_last)) {
        files <- files[1:num_last]
      }
    # dates == "latest"
    } else if (as.character(dates[1]) == "latest") {

      # if num_last is specified, also fetch the last num_last
      if (is.null(num_last)) {
        num_last = 0
      }
      n <- length(files)
      files <- files[(n - num_last):n]
    # dates are specifically given
    } else {
      dates_available <- as.Date(substr(files, 1, 10))
      index <- 1:length(dates_available)
      index <- index[as.character(dates_available) %in% as.character(dates)]

      # only makes sense if only a single file was given
      if (!is.null(num_last)) {
        if (length(index) > 1) {
          stop("if you use num_last, you should only provide on date or 'latest'")
        }

        files <- files[(index - num_last):index]
      } else {
        files <- files[index]
      }
    }

    if (drop_latest_forecast) {
      files <- files[-length(files)]
    }

    file_paths <- here::here("data", "processed-data", model_name, files)

    forecasts <- purrr::map_dfr(file_paths,
                                .f = function(x) {
                                  df <- data.table::fread(x) %>%
                                    dplyr::mutate(location = as.character(location))
                                  return(df)}) %>%
      dplyr::mutate(model = model_name)

    return(forecasts)
  }

  forecasts <- purrr::map_dfr(model_names, load_model_files)

  # Join forecasts ----------------------------------------------------------
  # and add state names
  forecasts <- dplyr::bind_rows(forecasts) %>%
    dplyr::left_join(tigris::fips_codes %>%
                       dplyr::select(state_code, state = state_name) %>%
                       unique() %>%
                       rbind(c("US", "US")),
                     by = c("location" = "state_code")) %>%
    # unclear bug where there seems to be a numerical error somewhere
    dplyr::mutate(quantile = round(quantile, 3)) %>%
    dplyr::mutate(horizon = as.numeric(stringi::stri_extract_first_regex(target,
                                                                         "[0-9]+")))

  return(forecasts)
}


combine_with_deaths <- function(forecasts,
                                inner_join = TRUE) {

  deaths <- get_us_deaths(data = "weekly")

  forecasts <- dplyr::mutate(forecasts,
                             epiweek = lubridate::epiweek(target_end_date))

  # join deaths with past forecasts and reformat
  if (inner_join) {
    combined <- dplyr::inner_join(forecasts,
                                  deaths, by = c("state", "epiweek", "target_end_date"))

  } else {
    combined <- dplyr::left_join(forecasts,
                                 deaths, by = c("state", "epiweek", "target_end_date"))

  }

  return(combined)
}


filter_forecasts <- function(forecasts, dates = NULL,
                             locations = "auto",
                             horizons = NULL,
                             target_end_dates = NULL) {
  forecasts <- forecasts %>%
    dplyr::filter(type == "quantile",
                  grepl("inc", target),
                  grepl("death", target)) %>%
    dplyr::select(-type) %>%
    dplyr::mutate(target_end_date = as.Date(target_end_date))


  if(!is.null(horizons)) {

    if (horizons[1] == "auto") {
      forecasts <- forecasts %>%
        dplyr::group_by(model) %>%
        dplyr::mutate(max_horizon = max(horizon)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(horizon <= min(max_horizon)) %>%
        dplyr::select(-max_horizon)
    } else {
      forecasts <- forecasts %>%
        dplyr::filter(horizon %in% horizons)
    }
  }

  if (!is.null(target_end_dates)) {
    if (target_end_dates[1] == "auto") {
      forecasts <- forecasts %>%
        dplyr::group_by(model) %>%
        dplyr::mutate(max_date = max(target_end_date),
                      min_date = min(target_end_date)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(target_end_date <= min(max_date),
                      target_end_date >= max(min_date)) %>%
        dplyr::select(-max_date, -min_date)
    } else {
      forecasts <- forecasts %>%
        dplyr::filter(target_end_date %in% target_end_dates)
    }
  }

  if (!is.null(locations)) {

    if (locations[1] == "auto") {
      locations_to_remove <- forecasts %>%
        dplyr::group_by(location, target, target_end_date) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n < max(n)) %>%
        dplyr::pull(location) %>%
        unique()

      forecasts <- forecasts %>%
        dplyr::filter(!(location %in% locations_to_remove))
    } else {
      forecasts <- forecasts %>%
        dplyr::filter(location %in% locations)
    }
  }

  return(forecasts)
}



# function to prepare data for scoring
# adds deaths and reformats for scoringutils
prepare_for_scoring <- function(forecasts) {
  combined <- combine_with_deaths(forecasts)

  combined <- combined %>%
    dplyr::rename(true_values = deaths,
                  predictions = value) %>%
    dplyr::mutate(boundary = ifelse(quantile <= 0.5, "lower", "upper"),
                  range = abs(1 - 2 * quantile) * 100)

  full <- dplyr::bind_rows(combined,
                           combined %>%
                             dplyr::filter(quantile == 0.5) %>%
                             dplyr::mutate(boundary = "upper")) %>%
    # weird unexplicable rounding error?
    dplyr::mutate(range = round(range, digits = 0),
                  horizon = as.numeric(substring(target, 1, 2))) %>%
    dplyr::select(-target, -quantile, -location) %>%
    unique() %>%
    # filter away duplicate forecasts if there are any (if there aren't, this has no effect)
    dplyr::group_by(forecast_date, model, state, epiweek, boundary, range, horizon) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  return(combined)
}
