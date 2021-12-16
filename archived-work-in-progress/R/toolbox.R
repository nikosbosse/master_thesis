
# =============================================================== # 
### See whether the fit looks good
inc_fitted <- my_fitted_incidences(stanfit2)

my_pred_vs_true_inc_plot(inc, my_fitted_incidences(stanfit1))
my_pred_vs_true_inc_plot(inc, my_fitted_incidences(stanfit2))
# _______________________________________________________________ # 



# =============================================================== #
### See whether there is a mismatch between prior and posterior
y <- rnorm(10000, 0, 1)
sigma_eta_post <- extract(stanfit2)$sigma_eta
sigma_epsilon_post <- extract(stanfit2)$sigma_epsilon

my_plot_two_histograms(y, sigma_eta_post, upper_limit = 100, breaks = 1000)
# _______________________________________________________________ # 



# =============================================================== #
### look at evolution of delta suggested by the random walk / trend
delta_02 <- my_evolution_delta(delta0 = 0, D = -2, phi = 0.85, n = 100, random = F, sigma = 0.04)

delta_06 <- my_evolution_delta(delta0 = 0, D = -0.02, phi = 0.6, n = 100, random = F, sigma = 0.5)

delta_09 <- my_evolution_delta(delta0 = 0, D = -0.02, phi = 0.9, n = 100, random = F, sigma = 0.5)
delta_neg04 <- my_evolution_delta(delta0 = 0, D = -0.02, phi = -0.4, n = 100, random = F, sigma = 0.5)

plot(delta_02, type = "l", family = "Serif", col = "green")
plot(delta_neg04, type = "l", family = "Serif", col = "green")
lines(delta, type = "l", family = "Serif", col = "red")
plot(delta, type = "l", family = "Serif", col = "blue")

R <- my_evolution_R(n = 100, sigma_epsilon = 0.08, delta0 = 0, D = -0.2, phi = 0.6, random = F, sigma = 0.02)
plot(R, type = "l", family = "Serif", col = "blue")
# _______________________________________________________________ # 



# =============================================================== #
### extract iterative forecasts
list_of_fits <- my_iterative_fit
predictions <- sapply(list_of_fits, var = "I_pred")
# _______________________________________________________________ # 