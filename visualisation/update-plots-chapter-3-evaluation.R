# ==============================================================================
# Plots chapter 3 evaluation
# ==============================================================================

# setup ------------------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(dplyr)

# source function for visualisation
source(here::here("visualisation", "plotting-functions", "visualise-data-functions.R"))
source(here::here("utils", "settings.R"))
source(here::here("utils", "load-data-functions.R"))



# get full data set
deaths <- get_us_deaths(data = "weekly") %>%
  dplyr::filter(epiweek < max(epiweek))

forecasts <- load_submission_files(dates = "all",
                                   models = "COVIDhub-baseline")

forecasts <- filter_forecasts(forecasts,
                              locations = "US",
                              horizons = 1,
                              target_end_dates = "auto")

full <- prepare_for_scoring(forecasts)







# illustration of calibration and sharpness


# introduction to forecasting paradigm plot

p1 <- ggplot2::ggplot(data.frame(x = seq(-4, 6.5, 0.01)),
                      ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(mean = 1.2, sd = 2.7)) +
  ggplot2::ggtitle("Neither sharp nor calibrated") +
  cowplot::theme_cowplot()

p2 <- ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.01)),
                      ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(mean = 2.6, sd = 0.2)) +
  ggplot2::ggtitle("Sharp, but not well calibrated") +
  cowplot::theme_cowplot()


p3 <- ggplot2::ggplot(data.frame(x = seq(-8, 8, 0.01)),
                      ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(sd = 5)) +
  ggplot2::ggtitle("Well calibrated, but not sharp") +
  cowplot::theme_cowplot()


p4 <- ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.01)),
                      ggplot2::aes(x = x)) +
  ggplot2::geom_vline(xintercept = 0, colour = "red") +
  ggplot2::geom_function(fun = dnorm, colour = "black",
                         args = list(sd = 0.4)) +
  ggplot2::ggtitle("Well calibrated and sharp") +
  cowplot::theme_cowplot()


calibration_sharpness <- cowplot::plot_grid(p1, p2, p3, p4,
                                            ncol = 2)

ggplot2::ggsave("visualisation/chapter-3-evaluation/forecast-paradigm.png",
                calibration_sharpness,
                width = 10, height = 4)
























# ------------------------------------------------------------------------------
# BIAS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# plot with baseline model and US and bias of that plot ------------------------

US_plot_baseline <- plot_forecasts(forecasts = forecasts,
                                   states = "US",
                                   models = "COVIDhub-baseline",
                                   obs_weeks = 16) +
  ggplot2::theme(legend.position = "none",
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0))) +
  ggplot2::labs(x = "")

scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "target_end_date",
                                              "state", "horizon"))

plot_bias <- ggplot2::ggplot() +
  ggplot2::geom_point(data = scores, ggplot2::aes(y = bias,
                                                  x = target_end_date),
                      colour = "black") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::labs(x = "Week", y = "Bias",
                caption = paste0("Mean bias is ", round(mean(scores$bias), 3)),
                col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom",
                 text = ggplot2::element_text(family = "Sans Serif"),
                 axis.title.y = ggplot2::element_text(margin = margin(t = 0,
                                                                      r = 20,
                                                                      b = 0,
                                                                      l = 0)))

bias_plot_combined <- cowplot::plot_grid(US_plot_baseline,
                                         plot_bias, ncol = 1,
                                         rel_heights = c(2, 1),
                                         scale = c(1, 1),
                                         align = 'v')

ggplot2::ggsave(here::here("visualisation", "chapter-3-evaluation", "bias_example.png"),
                bias_plot_combined,
                width = 10,
                height = 4.5)






# ------------------------------------------------------------------------------
# plot with empirical coverage from baseline model -----------------------------

# forecasts <- load_submission_files(dates = "all",
#                                    models = "COVIDhub-baseline")
#
# forecasts <- filter_forecasts(forecasts,
#                               locations = NULL,
#                               horizons = 1,
#                               target_end_dates = "auto")
#
# n_samples <- 1000
#
# combined <- combine_with_deaths(forecasts) %>%
#   data.table::as.data.table()

full2 <- prepare_for_scoring(forecasts_all)
full <- prepare_for_scoring(forecasts)


scores <- scoringutils::eval_forecasts(full,
                                       by = c("model", "forecast_date",
                                              "target_end_date", "state"),
                                       summarise_by = c("model", "range"))

## overall model calibration - empirical interval coverage
interval_coverage <- ggplot2::ggplot(scores, ggplot2::aes(x = range)) +
  ggplot2::geom_line(ggplot2::aes(y = range), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = calibration * 100)) +
  cowplot::theme_cowplot() +
  #ggplot2::facet_wrap(~ model, ncol = 3) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::ylab("Percent observations inside interval range") +
  ggplot2::xlab("Interval range")

ggplot2::ggsave(here::here("visualisation", "chapter-3-evaluation",
                           "interval-coverage.png"),
                interval_coverage,
                width = 10,
                height = 4.5)











# ------------------------------------------------------------------------------
# Coverage
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# plot with quantile coverage

# forecasts <- load_submission_files(dates = "all",
#                                    models = "COVIDhub-baseline")
#
# forecasts <- filter_forecasts(forecasts,
#                               locations = "US",
#                               horizons = 1,
#                               target_end_dates = "auto")
#
# forecasts <- combine_with_deaths(forecasts)

combined <- combine_with_deaths(forecasts)

quantile_coverage_plot <- combined %>%
  dplyr::group_by(quantile) %>%
  dplyr::summarise(coverage = mean(deaths <= value)) %>%
  ggplot2::ggplot(ggplot2::aes(x = quantile)) +
  ggplot2::geom_line(ggplot2::aes(y = quantile), colour = "grey",
                     linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = coverage)) +
  cowplot::theme_cowplot() +
  #ggplot2::facet_wrap(~ model) +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::xlab("Quantile") +
  ggplot2::ylab("Percent observations below quantile")

ggplot2::ggsave(here::here("visualisation", "chapter-3-evaluation",
                           "quantile-coverage.png"),
                quantile_coverage_plot,
                width = 10,
                height = 4.5)












# ------------------------------------------------------------------------------
# PIT calibration
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# calibration example plots ----------------------------------------------------
true_values <- rnorm(1000, 0, 1)
predictions1 <- replicate(10000, rnorm(1000))
predictions2 <- replicate(10000, rnorm(1000, mean = 0.5))
predictions3 <- replicate(10000, rnorm(1000, sd = 1.4))
predictions4 <- replicate(10000, rnorm(1000, sd =  0.7))
predictions5 <- replicate(10000, rnorm(1000, mean = 0.2, sd =  0.7))

# plot with observations
true_value_plot <- ggplot2::ggplot(data = data.frame(x = true_values),
                ggplot2::aes(x = x)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                          fill = "grey",
                          colour = "dark grey") +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "True values",
                y = "Density") +
  ggplot2::theme(legend.position = "bottom")

# plot with standard normal distribution
standard_normal <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black") +
  ggplot2::ggtitle("Normal(0, 1)")

pit_standard_normal <- scoringutils::pit(true_values, predictions1)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom")

# plot with shifted mean
shifted_mean <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(mean = 0.5)) +
  ggplot2::ggtitle("Normal(0.5, 1)")


pit_shifted_mean <- scoringutils::pit(true_values, predictions2)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom")

# plot with overdispersion
overdispersion <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 1.4)) +
  ggplot2::ggtitle("Normal(0, 1.4)")

pit_overdispersion <- scoringutils::pit(true_values, predictions3)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom")

# plot with underdispersion
underdispersion <- true_value_plot +
  ggplot2::geom_function(fun = dnorm, colour = "black", args = list(sd = 0.7)) +
  ggplot2::ggtitle("Normal(0, 0.7)")


pit_underdispersion <- scoringutils::pit(true_values, predictions4)$hist_PIT +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom")

# # plot with underdispesion + mean shift
# underdispesion_mean_shift <- true_value_plot +
#   ggplot2::geom_function(fun = dnorm, colour = "black", args = list(mean = 0.2,
#                                                                     sd = 0.7))
#
# pit_underdispesion_mean_shift <- scoringutils::pit(true_values, predictions5)$hist_PIT +
#   cowplot::theme_cowplot() +
#   ggplot2::theme(legend.position = "bottom",
#                  text = ggplot2::element_text(family = "Sans Serif"))


calibration_examples1 <- cowplot::plot_grid(standard_normal, NULL,
                                           pit_standard_normal,
                                           ncol = 1,
                                           rel_heights = c(1, -0.2, 1),
                                           align = 'hv')

calibration_examples2 <- cowplot::plot_grid(shifted_mean, NULL,
                                            pit_shifted_mean,
                                            ncol = 1,
                                            rel_heights = c(1, -0.2, 1),
                                            align = 'hv')

calibration_examples3 <- cowplot::plot_grid(underdispersion, NULL,
                                            pit_underdispersion,
                                            ncol = 1,
                                            rel_heights = c(1, -0.2, 1),
                                            align = 'hv')

calibration_examples4 <- cowplot::plot_grid(overdispersion, NULL,
                                            pit_overdispersion,
                                            ncol = 1,
                                            rel_heights = c(1, -0.2, 1),
                                            align = 'hv')

calibration_examples <- cowplot::plot_grid(calibration_examples1,
                                           calibration_examples2,
                                           NULL, NULL,
                                           calibration_examples3,
                                           calibration_examples4,
                                           ncol = 2,
                                           rel_heights = c(1, 0.2, 1))

ggplot2::ggsave(here::here("visualisation", "chapter-3-evaluation",
                           "calibration-examples.png"),
                calibration_examples,
                height = 8, width = 10)



# test AD test
results <- list()
for (i in 1:1000) {
  true_values <- rnorm(1000, 0, 1)
  predictions1 <- replicate(10000, rnorm(1000))

  results[[i]] <- scoringutils::pit(true_values,
                                    predictions1, plot = FALSE)$p_value
}
saveRDS(results, "visualisation/chapter-3-evaluation/ADtest_1000_10000.RDS")

r <- unlist(results)
sum(r < 0.01)
sum(r >= 0.01 & r < 0.1)
sum(r >= 0.1)

results2 <- list()
for (i in 1:1000) {
  true_values <- rnorm(100, 0, 1)
  predictions1 <- replicate(2000, rnorm(100, 0.05, 1.01))

  results2[[i]] <- scoringutils::pit(true_values,
                                    predictions1, plot = FALSE)$p_value
}

saveRDS(results2, "visualisation/chapter-3-evaluation/ADtest_100_2000.RDS")

r2 <- unlist(results2)
sum(r2 < 0.01)
sum(r2 >= 0.01 & r2 < 0.1)
sum(r2 >= 0.1)


results3 <- list()
for (i in 1:1000) {
  true_values <- rnorm(50, 0, 1)
  predictions1 <- replicate(1000, rnorm(50))

  results3[[i]] <- scoringutils::pit(true_values,
                                     predictions1, plot = FALSE)$p_value
}

saveRDS(results3, "visualisation/chapter-3-evaluation/ADtest_50_1000.RDS")

r3 <- unlist(results3)
sum(r3 < 0.01)
sum(r3 >= 0.01 & r3 < 0.1)
sum(r3 >= 0.1)







# ------------------------------------------------------------------------------
# pit calibration plot of US forecasts from baseline model ---------------------

forecasts_all <- load_submission_files(dates = "all",
                                       models = "COVIDhub-baseline")

forecasts_all <- filter_forecasts(forecasts_all,
                                  locations = NULL,
                                  horizons = 1,
                                  target_end_dates = "auto")

source("ensembling/crps-ensemble/fit-distribution-functions.R")

n_samples <- 1000

combined <- combine_with_deaths(forecasts_all) %>%
  data.table::as.data.table()

samples <- combined[, .(y_pred = get_samples(value, quantile, n_samples = n_samples),
                        sample_nr = 1:n_samples,
                        deaths = unique(deaths)),
                    by = c("model", "forecast_date", "target",
                           "target_end_date", "state")]

df <- samples %>%
  dplyr::rename(predictions = y_pred,
                true_values = deaths,
                id = target_end_date,
                sample = sample_nr)

pit_plot <- scoringutils::eval_forecasts(df,
                                         by = c("model", "state",
                                                "target", "forecast_date"),
                                         summarise_by = c("forecast_date", "model"),
                                         pit_arguments = list(num_bins = 30,
                                                              plot = TRUE),
                                         pit_plots = TRUE)$pit_plots


plot <- pit_plot$overall_pit +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(here::here("visualisation","chapter-3-evaluation",
                           "pit-baseline-model.png"),
                plot,
                width = 10, height = 3.3)












# ------------------------------------------------------------------------------
# plot with points for explaining CRPS

df <- data.frame(pred = rnorm(9),
                 true_value = 0,
                 group = 1:9)

mae_plot <- df %>%
  ggplot2::ggplot(ggplot2::aes(x = group)) +
  ggplot2::geom_point(ggplot2::aes(y = pred)) +
  ggplot2::geom_segment(ggplot2::aes(x = group, y = pred,
                                     yend = true_value,
                                     xend = group),
                        colour = "dark red",
                        alpha = 0.3) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed") +
  cowplot::theme_cowplot() +
  ggplot2::facet_grid(NULL) +
  ggplot2::labs(x = "Point predictions",
                y = "Predicted values and mean absolute error") +
  ggplot2::expand_limits(y = c(0,1)) +
  ggplot2::coord_flip() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.position = "bottom")

quantiles <- seq(0.005, 0.999, 0.043)

df2 <- data.frame(true_value = 0,
                 quantiles = quantiles,
                 quant_pred = qnorm(quantiles, -0.6, 1.2))


quantile_mae <- df2 %>%
  ggplot2::ggplot(ggplot2::aes(x = quantiles)) +
  ggplot2::geom_point(ggplot2::aes(y = quant_pred)) +
  ggplot2::geom_segment(ggplot2::aes(x = quantiles, y = quant_pred,
                                     yend = true_value,
                                     xend = quantiles),
                        colour = "dark red",
                        alpha = 0.3) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed") +
  cowplot::theme_cowplot() +
  ggplot2::expand_limits(x = c(0,1)) +
  ggplot2::labs(x = "Quantiles of the predictive distribution",
                y = "Predicted value") +
  ggplot2::coord_flip() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom")


quantiles <- seq(0.005, 0.999, 0.001)

df3 <- data.frame(true_value = 0,
                  quantiles = quantiles,
                  quant_pred = qnorm(quantiles, -0.6, 1.2))

crps_mae <- df3 %>%
  ggplot2::ggplot(ggplot2::aes(x = quantiles)) +
  ggplot2::geom_line(ggplot2::aes(y = quant_pred)) +
  ggplot2::geom_area(ggplot2::aes(y = quant_pred),
                     fill = "dark red", alpha = 0.1) +
  ggplot2::geom_hline(yintercept = 0, colour = "dark grey",
                      linetype = "dashed") +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "Cumulative distribution",
                y = "Value") +
  ggplot2::coord_flip() +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom")



df4 <- data.frame(true_value = 0,
                  value = seq(-3.690995164,
                              3.108278767,
                              length.out = 995),
                  Fx = c(rep(0, 700), rep(1, 295)))

point_cdf <- df4 %>%
  ggplot2::ggplot(ggplot2::aes(x = value)) +
  ggplot2::geom_line(ggplot2::aes(y = Fx)) +
  ggplot2::annotate("rect", xmin = 0, xmax = 1.090386,
                    ymin = 0, ymax = 1,
                    fill = "red",
                    alpha = 0.1) +
  ggplot2::geom_vline(xintercept = 0, colour = "dark grey",
                      linetype = "dashed") +
  cowplot::theme_cowplot() +
  #ggplot2::expand_limits(x = c(0,1)) +
  ggplot2::labs(y = "Cumulative distribution",
                x = "") +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom")


# mae_plots <- cowplot::plot_grid(mae_plot, quantile_mae, crps_mae,
#                                 ncol = 1,
#                                 rel_heights = c(1, 1),
#                                 scale = c(1, 1),
#                                 align = 'hv')
#
#
# ggplot2::ggsave(here::here("evaluation", "plots", "chapter-3-evaluation",
#                            "mae-plots.png"),
#                 mae_plots)


crps_explanation <- cowplot::plot_grid(point_cdf, crps_mae,
                                       ncol = 1)

ggplot2::ggsave(here::here("visualisation", "chapter-3-evaluation",
                           "crps-explanation.png"),
                crps_explanation,
                width = 10, height = 5)

















# ------------------------------------------------------------------------------
# plot for explaining log score

seq <- seq(-3, 3, length.out = 10000)

df <- data.frame(x = seq,
                 y = log(dnorm(seq)))

log_score_example <- df %>%
  ggplot2::ggplot(ggplot2::aes(x = x)) +
  ggplot2::geom_line(ggplot2::aes(y = y)) +
  ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = -0.8,
                                     y = log(dnorm(-0.8)),
                                     yend = log(dnorm(-0.8))),
                        colour = "dark grey",
                        alpha = 0.3,
                        linetype = "dashed") +
  ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf,
                                     x = -0.8, xend = -0.8),
                        colour = "dark grey",
                        linetype = "dashed") +
  ggplot2::geom_point(ggplot2::aes(y = log(dnorm(-0.8)), x = -0.8),
                        colour = "black") +
  cowplot::theme_cowplot() +
  ggplot2::labs(x = "Value",
                y = "Log predictive density") +
  ggplot2::theme(text = ggplot2::element_text(family = "Sans Serif"),
                 legend.position = "bottom") +
  ggplot2::scale_x_continuous(breaks = c(-2, -0.8, 0, 2),
                              labels = c("-2","observed","0", "2")) +
  ggplot2::scale_y_continuous(breaks = c(-5, -3, log(dnorm(-0.8)), -1),
                                         labels = c("-5","-3","log score", "-1")) +
  ggplot2::theme(axis.text.y = element_text(color = c("black", "black", "dark grey", "black")),
                 axis.ticks.y = element_line(color = c("black", "black", "dark grey", "black"))) +
  ggplot2::theme(axis.text.x = element_text(color = c("black", "dark grey", "black", "black")),
                 axis.ticks.x = element_line(color = c("black", "dark grey", "black", "black")))


ggplot2::ggsave(here::here("visualisation", "chapter-3-evaluation",
                           "log-score-example.png"),
                log_score_example,
                width = 10, height = 3.5)



# national_plot <- plot_forecasts(national = TRUE, obs_weeks = 8, exclude_new_epiweek = FALSE)
#
#
# suppressWarnings(ggsave(here::here("evaluation", "plots",
#                                    forecast_date, "submission-national.png"),
#        plot = national_plot,
#        width = 10, height = 10, dpi = 300))
#
#
#
# subnational_plot <- plot_forecasts(national = FALSE, state_min_cutoff = 50, obs_weeks = 8,
#                                    exclude_new_epiweek = FALSE)
#
#
# suppressWarnings(ggsave(here::here("evaluation", "plots",
#                                    forecast_date, "submission-subnational.png"),
#        plot = subnational_plot,
#        width = 20, height = 25))
