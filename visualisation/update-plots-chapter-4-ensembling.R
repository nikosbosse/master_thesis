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





# ------------------------------------------------------------------------------
# plot mixture and average of two distributions ------- ------------------------

x <- seq(-2,7,length=1000)
y <- seq(0,1,length=1000)
m1 = 0; s1 = 1
m2 = 3.5; s2 = 1.4
f1 = function(x) dnorm(x,m1,s1)
F1 = function(x) pnorm(x,m1,s1)
Q1 = function(q) qnorm(q,m1,s1)
f2 = function(x) dnorm(x,m2,s2)
F2 = function(x) pnorm(x,m2,s2)
Q2 = function(q) qnorm(q,m2,s2)

d <- data.frame(x = x,
                y = y,
           f1 = f1(x),
           f2 = f2(x),
           F1 = F1(x),
           F2 = F2(x),
           Fmix = (F1(x) + F2(x)) / 2,
           Q1 = Q1(y),
           Q2 = Q2(y),
           Qavg = (Q1(y) + Q2(y)) / 2)

mixture_cdf <- d %>%
ggplot2::ggplot(ggplot2::aes(x = x)) +
  ggplot2::geom_line(ggplot2::aes(y = F1, colour = "Distribution 1")) +
  ggplot2::geom_line(ggplot2::aes(y = F2, colour = "Distribution 2")) +
  ggplot2::geom_line(ggplot2::aes(y = Fmix, colour = "Combination")) +
  ggplot2::geom_segment(ggplot2::aes(x = 1.3, xend = 1.3,
                        y = F1(1.3), yend = F2(1.3)),
                        colour = "dark grey",
                        linetype = "dashed") +
  ggplot2::geom_segment(ggplot2::aes(x = 2.8, xend = 2.8,
                                     y = F1(2.8), yend = F2(2.8)),
                        colour = "dark grey",
                        linetype = "dashed") +
  ggplot2::geom_segment(ggplot2::aes(x = 4.3, xend = 4.3,
                                     y = F1(4.3), yend = F2(4.3)),
                        colour = "dark grey",
                        linetype = "dashed") +
  ggplot2::labs(x = "", y = "F(x)",
                #title = "Mixture",
                caption = paste0(""),
                col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "none",
                 text = ggplot2::element_text(family = "Sans Serif"))


qa_average <- d %>%
  ggplot2::ggplot(ggplot2::aes(x = x,
                               fill = "Distribution 1")) +
  ggplot2::geom_line(ggplot2::aes(y = F1, colour = "Distribution 1")) +
  ggplot2::geom_line(ggplot2::aes(y = F2, colour = "Distribution 2")) +
  ggplot2::geom_line(ggplot2::aes(x = Qavg, y = y, colour = "Combination")) +
  ggplot2::geom_segment(ggplot2::aes(y = 0.2, yend = 0.2,
                                     x = Q1(0.2), xend = Q2(0.2)),
                        colour = "dark grey",
                        linetype = "dashed") +
  ggplot2::geom_segment(ggplot2::aes(y = 0.5, yend = 0.5,
                                     x = Q1(0.5), xend = Q2(0.5)),
                        colour = "dark grey",
                        linetype = "dashed") +
  ggplot2::geom_segment(ggplot2::aes(y = 0.8, yend = 0.8,
                                     x = Q1(0.8), xend = Q2(0.8)),
                        colour = "dark grey",
                        linetype = "dashed") +
  ggplot2::labs(x = "", y = "",
                #title = "Quantile Average",
                caption = paste0(""),
                col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "none",
                 text = ggplot2::element_text(family = "Sans Serif"))



# d %>%
#   ggplot2::ggplot(ggplot2::aes(fill = "Distribution 1")) +
#   #ggplot2::geom_density(ggplot2::aes(y = F1, colour = "Distribution 1")) +
#   ggplot2::geom_density(ggplot2::aes(y = f2,
#                                      x = x,
#                                      fill = "Distribution 2"),
#                         stat = "identity") +
#   ggplot2::geom_density(ggplot2::aes(x = Fmix,
#                                      fill = "Combination"),
#                         alpha = 0.3,
#                         stat = "density") +
#   ggplot2::geom_density(ggplot2::aes(y = f1,
#                                      x = x,
#                                      fill = "Distribution 1"),
#                         alpha = 0.3,
#                         stat = "identity") +
#   #ggplot2::geom_line(ggplot2::aes(x = Qavg, y = y, colour = "Combination")) +
#   ggplot2::labs(x = "", y = "Density",
#                 title = "Quantile Average",
#                 caption = paste0(""),
#                 col = "Model", fill = "Model") +
#   cowplot::theme_cowplot() +
#   ggplot2::theme(legend.position = "none",
#                  text = ggplot2::element_text(family = "Sans Serif"))


x1 <- rnorm(10000)
x2 <- rnorm(10000, 3.5, 1.4)

mean <- (x1 + x2) / 2
mean2 <- (sort(x1) + sort(x2)) / 2
index <- sample(c(TRUE, FALSE), 10000, replace = TRUE)
mixture <- ifelse(index, x1, x2)

# check that taking a quantile average is indeed equivalent to just averaging
# the sorted vector of
quantiles1 <- quantile(x1, seq(0.01, 0.99, by = 0.01))
quantiles2 <- quantile(x2, seq(0.01, 0.99, by = 0.01))
quantiles3 <- quantile(mean2, seq(0.01, 0.99, by = 0.01))

quantiles4 <- (quantiles1 + quantiles2) / 2


df <- data.frame(x1 = x1,
                 x2 = x2,
                 mean2 = mean2,
                 mean = mean,
                 mixture = mixture)

mixture_plot <- df %>%
ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = x1, fill = "Distribution 1"),
                          #binwidth = bw,
                          alpha = 0.5) +
  ggplot2::geom_density(ggplot2::aes(x = x2, fill = "Distribution 2"),
                          #binwidth = bw,
                          alpha = 0.5) +
  ggplot2::geom_density(ggplot2::aes(x = mixture,
                                       fill = "Combination: Mixture"),
                          #binwidth = bw,
                          alpha = 0.8) +
  ggplot2::labs(x = "", y = "Density",
                title = "Mixture",
                caption = paste0(""),
                col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "none",
                 text = ggplot2::element_text(family = "Sans Serif"))



average_plot <- df %>%
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = x1, fill = "Distribution 1"),
                        #binwidth = bw,
                        alpha = 0.5) +
  ggplot2::geom_density(ggplot2::aes(x = x2, fill = "Distribution 2"),
                        #binwidth = bw,
                        alpha = 0.5) +
  ggplot2::geom_density(ggplot2::aes(x = mean2, fill = "Combination"),
                        #binwidth = bw,
                        alpha = 0.8) +
  ggplot2::labs(x = "", y = "",
                caption = paste0(""),
                title = "Quantile average",
                col = "Model", fill = "Model") +
  cowplot::theme_cowplot() +
  ggplot2::theme(legend.position = "none",
                 text = ggplot2::element_text(family = "Sans Serif"))

# extract the legend from one of the plots
legend <- cowplot::get_legend(
  average_plot + ggplot2::theme(legend.box.margin = margin(0, 0, 0, 12))
)

average_mixture <- cowplot::plot_grid(mixture_plot,
                                      average_plot,
                                      mixture_cdf,
                                      qa_average,
                                      ncol = 2)


ggplot2::ggsave(here::here("visualisation", "chapter-4-ensemble",
                           "average-mixture-example.png"),
                average_mixture)












