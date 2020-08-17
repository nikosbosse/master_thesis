plot_forecasts <- function(forecasts = NULL,
                          states = "US",
                          facet_formula = model ~ state,
                          models = settings$model_names,
                          obs_weeks = 12,
                          horizons = 1,
                          ncol_facet = NULL,
                          exclude_new_epiweek = TRUE,
                          observations_only = FALSE) {

  # Get observed data ------------------------------------------------------------------

  deaths <- get_us_deaths(data = "weekly") %>%
    dplyr::filter(epiweek < max(epiweek))

  if (is.null(forecasts)) {
    # Get forecasts -----------------------------------------------------------
    forecasts <- load_submission_files(dates = "all",
                                       models = models)

    forecasts <- filter_forecasts(forecasts,
                                  locations = NULL,
                                  horizons = horizons,
                                  target_end_dates = "auto")
  }


  # Reshape forecasts and add observed data ------------------------------------
  # Filter to incidence forecasts and pivot forecasts for plotting

  forecasts_wide <- forecasts %>%
    dplyr::group_by(state, target_end_date, model, horizon) %>%
    dplyr::mutate(quantile = stringr::str_c("c", quantile)) %>%
    dplyr::filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
    tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
    dplyr::ungroup()

  if(is.null(states) | states == "all") {
    states <- unique(deaths$state)
  }

  plot_fc <- forecasts_wide %>%
    dplyr::filter(state %in% states,
                  horizon %in% horizons)

  plot_deaths <- deaths %>%
    dplyr::filter(epiweek >= (max(epiweek) - obs_weeks)) %>%
    dplyr::select(-epiweek, c0.5 = deaths) %>%
      dplyr::filter(state %in% states)


  manual_colours <- c(RColorBrewer::brewer.pal(8, name = "Set2")[-6],
                      RColorBrewer::brewer.pal(7, name = "Set1")[c(1, 2, 4, 7)])

  plot <- plot_fc %>%
    ggplot2::ggplot(ggplot2::aes(x = target_end_date, col = model, fill = model)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = c0.05, ymax = c0.95), color = NA, alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = c0.25, ymax = c0.75), color = NA, alpha = 0.2) +
    ggplot2::geom_point(data = plot_deaths, ggplot2::aes(y = c0.5), size = 1,
                        colour = "black") +
    ggplot2::geom_line(data = plot_deaths, ggplot2::aes(y = c0.5), lwd = 0.5,
                       colour = "black") +
    ggplot2::geom_line(ggplot2::aes(y = c0.5), lwd = 0.2) +
    ggplot2::geom_point(ggplot2::aes(y = c0.5), size = 2,
                        fill = "white",
                        shape = 21) +
    ggplot2::scale_fill_manual(values = manual_colours) +
    ggplot2::scale_color_manual(values = manual_colours,) +
    ggplot2::facet_wrap(facet_formula, scales = "free_y",
                        ncol = ncol_facet) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "Week", y = "Weekly incident deaths",
                  caption = NULL,
         col = "Model", fill = "Model") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Sans Serif"),
                   panel.background = element_rect(fill = "aliceblue"))

  return(plot)
}
















plot_true_data <- function(states = "US",
                           facet_formula = ~ state,
                           obs_weeks = 20,
                           exclude_new_epiweek = TRUE){

  # Get observed data ------------------------------------------------------------------
  deaths <- get_us_deaths(data = "weekly") %>%
    dplyr::filter(epiweek < max(epiweek))

  plot_deaths <- deaths %>%
    dplyr::filter(epiweek >= (max(epiweek) - obs_weeks)) %>%
    dplyr::select(-epiweek, c0.5 = deaths) %>%
    dplyr::filter(state %in% states)

  plot <- plot_deaths %>%
    ggplot2::ggplot(ggplot2::aes(x = target_end_date)) +
    ggplot2::geom_point(ggplot2::aes(y = c0.5), size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = c0.5), lwd = 0.2) +
    ggplot2::facet_wrap(facet_formula, scales = "free_y") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "Week", y = "Weekly incident deaths",
                  caption = NULL,
                  col = "Model", fill = "Model") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom",
                   text = ggplot2::element_text(family = "Sans Serif"))

  return(plot)

}
