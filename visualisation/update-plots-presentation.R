
# introduction to forecasting paradigm plot

p1 <- ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.01)),
                ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black") +
  ggplot2::ggtitle("Decent") +
  cowplot::theme_cowplot()


p2 <- ggplot2::ggplot(data.frame(x = seq(-8, 8, 0.01)),
                ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(sd = 5)) +
  ggplot2::ggtitle("We can work with this") +
  cowplot::theme_cowplot()


p3 <- ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.01)),
                ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(sd = 0.4)) +
  ggplot2::ggtitle("This is awesome!") +
  cowplot::theme_cowplot()


p4 <- ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.01)),
                ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(mean = 1.2, sd = 0.7)) +
  ggplot2::ggtitle("Hmmm....") +
  cowplot::theme_cowplot()

p5 <- ggplot2::ggplot(data.frame(x = seq(-4, 6.5, 0.01)),
                      ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(mean = 1.2, sd = 2.7)) +
  ggplot2::ggtitle("At least you tried") +
  cowplot::theme_cowplot()

p6 <- ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.01)),
                      ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(mean = 2.6, sd = 0.2)) +
  ggplot2::ggtitle("Wow, this is not helping") +
  cowplot::theme_cowplot()


presentation <- cowplot::plot_grid(p6, p5, p4, p2, p1, p3,
                   ncol = 3)

ggplot2::ggsave("visualisation/thesis-presentation/forecast-paradigm.png",
                presentation,
                width = 10, height = 8)















# coverage plots

source("utils/settings.R")

forecasts <- load_submission_files(dates = settings$evaluation_dates,
                                   models = "YYG-ParamSearch")

forecasts <- filter_forecasts(forecasts,
                              locations = settings$locations_included,
                              horizons = c(1, 2, 3, 4))

full <- prepare_for_scoring(forecasts)


scores <- scoringutils::eval_forecasts(full,
                                       by = c("forecast_date",
                                              "target_end_date",
                                              "model", "state", "horizon"),
                                       interval_score_arguments = list(weigh = TRUE),
                                       summarise_by = c("model", "range"))


## overall model calibration - empirical interval coverage
interval_coverage <- ggplot2::ggplot(scores,
                                              ggplot2::aes(x = range,
                                                           colour = model)) +
  ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_manual(values = settings$manual_colours) +
  # ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylab("Percent observations inside interval range") +
  ggplot2::xlab("Interval range")


# ------------------------------------------------------------------------------
# plot with quantile coverage

combined <- combine_with_deaths(forecasts)

quantile_coverage <- combined  %>%
  dplyr::group_by(model, quantile) %>%
  dplyr::summarise(coverage = mean(deaths <= value)) %>%
  ggplot2::ggplot(ggplot2::aes(x = quantile, colour = model)) +
  ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coverage)) +
  cowplot::theme_cowplot() +
  ggplot2::scale_color_manual(values = settings$manual_colours) +
  #ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(legend.position = "right") +
  ggplot2::xlab("Quantile") +
  ggplot2::ylab("Proportion of observations below quantile")


coverage_ensemble <- cowplot::plot_grid(interval_coverage,
                                        quantile_coverage,
                                        rel_widths = c(0.65,1),
                                        ncol = 2)

ggplot2::ggsave("visualisation/thesis-presentation/coverage-YYG.png",
                coverage_ensemble,
                width = 10, height = 5)

















ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.01)),
                      ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 1, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black") +
  ggplot2::scale_x_continuous(name = "quantiles", breaks = c(-4, -2, 0, 1, 2, 4),
                            labels = c("0.05", "0.25", "0.5", "0.65", "0.75", "0.95")) +
  ggplot2::labs(y = "",
                caption = "Bias = 1 - 2 * 0.65 = -0.3") +
  ggplot2::coord_flip() +
  cowplot::theme_cowplot()






