plot_forecasts = function(national = TRUE,
                          state_min_cutoff = NA,
                          obs_weeks = 12,
                          exclude_new_epiweek = TRUE){


  # Get observed data ------------------------------------------------------------------

  deaths <- get_us_deaths(data = "weekly") %>%
    dplyr::filter(epiweek < max(epiweek))

  # Get forecasts -----------------------------------------------------------
  forecasts <- load_submission_files(dates = "all",
                                     models = settings$model_names)

  forecasts <- filter_forecasts(forecasts,
                                locations = NULL,
                                horizons = "auto",
                                target_end_dates = "auto")

  # Reshape forecasts and add observed data ------------------------------------
  # Filter to incidence forecasts and pivot forecasts for plotting

  forecasts_wide <- forecasts %>%
    dplyr::group_by(state, target_end_date, model, horizon) %>%
    dplyr::mutate(quantile = stringr::str_c("c", quantile)) %>%
    dplyr::filter(quantile %in% c("c0.05", "c0.25", "c0.5", "c0.75", "c0.95")) %>%
    tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
    dplyr::ungroup()

  plot_deaths <- deaths %>%
    dplyr::mutate(model = "Observed") %>%
    dplyr::filter(epiweek >= (max(epiweek) - obs_weeks)) %>%
    dplyr::select(-epiweek, c0.5 = deaths)

  # Identify and filter which states to keep -------------------------------------------
  if (national) {

    plot_deaths <- plot_deaths %>%
      dplyr::filter(state == "US") %>%
      dplyr::select(-model)

    plot_df <- forecasts_wide %>%
      dplyr::filter(state == "US",
                    horizon == 1)

  } else {
    plot_df <- dplyr::bind_rows(plot_deaths %>%
                                  dplyr::filter(!(state == "US")),
                                forecasts_wide %>%
                                  dplyr::filter(!(state == "US")))
  }



  manual_colours <- RColorBrewer::brewer.pal(5, name = "Set2")
  # manual_colours[1] <- "#000000"




  plot <- plot_df %>%
    ggplot2::ggplot(ggplot2::aes(x = target_end_date, col = model, fill = model)) +
    ggplot2::geom_point(ggplot2::aes(y = c0.5), size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = c0.5), lwd = 0.2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = c0.05, ymax = c0.95), color = NA, alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = c0.25, ymax = c0.75), color = NA, alpha = 0.2) +
    ggplot2::geom_point(data = plot_deaths, ggplot2::aes(y = c0.5), size = 1,
                        colour = "black") +
    ggplot2::geom_line(data = plot_deaths, ggplot2::aes(y = c0.5), lwd = 0.5,
                       colour = "black") +
    ##
    ggplot2::scale_fill_manual(values = manual_colours) +
    ggplot2::scale_color_manual(values = manual_colours) +
    ggplot2::facet_wrap(model ~ state, scales = "free_y") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "Week", y = "Weekly incident deaths",
                  caption = NULL,
         col = "Model", fill = "Model") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom",
          text = ggplot2::element_text(family = "Sans Serif"))

  return(plot)
}

