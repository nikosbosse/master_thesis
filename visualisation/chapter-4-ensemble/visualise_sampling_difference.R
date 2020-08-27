source(here::here("ensembling", "crps-ensemble", "fit-distribution-functions.R"))

source(here::here("utils", "settings.R"))


# get full data set
deaths <- get_us_deaths(data = "weekly") %>%
  dplyr::filter(epiweek < max(epiweek))

forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                   models = settings$model_names)

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))

full <- prepare_for_scoring(forecasts)

combined <- combine_with_deaths(forecasts)
fc <- data.table::as.data.table(combined)
n_samples = settings$n_samples

samples <- fc[, .(y_pred = get_samples(value, quantile, n_samples = n_samples),
                  sample_nr = 1:n_samples,
                  state = unique(state),
                  y_obs = unique(deaths),
                  horizon = unique(horizon)),
              by = c("model", "forecast_date",
                     "target_end_date",
                     "target", "location")]


sampled_quantiles <- samples %>%
  dplyr::group_by(target_end_date, state, horizon, model) %>%
  dplyr::group_modify( ~ {
    quantile(.x$y_pred, probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)) %>%
      tibble::enframe(name = "quantile", value = "value") %>%
      dplyr::mutate(quantile = as.numeric(stringr::str_remove(quantile, "%"))/100)
  })


both <- dplyr::bind_rows(sampled_quantiles %>%
                           dplyr::mutate(type = "sampled"),
                         combined %>%
                           dplyr::mutate(type = "original"))

#
# both %>%
#   ggplot2::ggplot(ggplot2::aes(x = value, fill = type)) +
#   ggplot2::geom_histogram(binwidth = 50,
#                           alpha = 0.5) +
#   ggplot2::facet_wrap(~ model) +
#   ggplot2::coord_cartesian(xlim = c(0, 2000))

df <- both %>%
  dplyr::group_by(type, model) %>%
  dplyr::summarise(mean = mean(value),
                   q0.01 = quantile(value, 0.01),
                   q0.05 = quantile(value, 0.05),
                   q0.25 = quantile(value, 0.25),
                   q0.5 = quantile(value, 0.5),
                   q0.75 = quantile(value, 0.75),
                   q0.95 = quantile(value, 0.95),
                   q99 = quantile(value, 0.99),
                   q995 = quantile(value, 0.995))

names <- names(df)[grepl("q", names(df))]


difference_plot <- df %>%
  tidyr::pivot_longer(cols = all_of(names),
                      values_to = "value") %>%
  dplyr::select(-mean) %>%
  tidyr::pivot_wider(names_from = type, values_from = value) %>%
  dplyr::mutate(difference = sampled - original) %>%
  ggplot2::ggplot(ggplot2::aes(x = name,
                               y = difference,
                               group = model)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                      colour = "grey") +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ model) +
  ggplot2::labs(x = "Quantile",
                y = "Difference between sampled minus original values") +
  cowplot::theme_cowplot() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                     hjust=1))



ggplot2::ggsave(here::here("visualisation", "chapter-4-ensemble",
                           "difference-true-sampled.png"),
                difference_plot,
                width = 10, height = 6)
