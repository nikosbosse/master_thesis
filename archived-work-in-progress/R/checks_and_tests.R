# ======================================================== #
# 				 Leave this part unchanged                 #
# ======================================================== #
source("~/Rconfig.R")
source("functions_master_thesis.R")

my_setup()





# ======================================================== #
# 				       Check Model Fit                     #
# ======================================================== #

inc

inc2 <- my_load_data()$inc
inc <- inc2[1:350]
infectiousness <- my_infectiousness(inc)$infectiousness_weekly
inc <- my_make_weekly(inc)
T <- length(inc)
#infectiousness[1:10] <- 1

l <- list(T = T, past_incidences = inc, tau = 1, n_pred = 10, infectiousness = infectiousness)

stanfit2 <- rstan::stan(file = "../stan/combined_EpiEstim_bsts_only_sigma_eta_r_transformed.stan",
                        data = l, chains = 4,
                        iter = 4000, warmup = 2000, thin = 1, control = list(adapt_delta = 0.99, max_treedepth = 10))

stanfit2 <- rstan::stan(file = "../stan/combined_EpiEstim_bsts_only_sigma_eta_r_transformed_experiment.stan",
                        data = l, chains = 4,
                        iter = 4000, warmup = 2000, thin = 1, control = list(adapt_delta = 0.99, max_treedepth = 10))

launch_shinystan(stanfit2)



pairs(stanfit2, pars = c("sigma_eta", "disp", "phi", "D", "lp__", "delta[15]", "r[15]")) #, "energy__"))


## check all posteriors vs. priors
## sigma_eta
library(invgamma)
posterior_draws <- rstan::extract(stanfit2)$sigma_eta
#y <- rgamma(10000, 0.1, 0.1)
y <- invgamma::rinvgamma(5000, 4, 0.4)
#y <- invgamma::rinvgamma(1000, 1, 1)
my_plot_two_histograms(y, posterior_draws, breaks = 100, upper_limit = 1)


##phi
posterior_draws <- rstan::extract(stanfit2)$phi
y <- rnorm(10000, 0, 0.1)
my_plot_two_histograms(y, posterior_draws, upper_limit = 3)

##disp
posterior_draws <- rstan::extract(stanfit2)$disp
y <- rnorm(10000, 0, 0.4)
#y <- invgamma::rinvgamma(5000, 4, 20)
#y <- rgamma(5000, 0.05, 0.05)
y <- (1/y^2)
my_plot_two_histograms(y, posterior_draws, upper_limit = 100)

##D
posterior_draws <- rstan::extract(stanfit2)$D
y <- rnorm(10000, 0, 0.5)
my_plot_two_histograms(y, posterior_draws, upper_limit = 2)



## check posterior draws
posterior_draws <- my_extract(stanfit2, "Inc_post")
my_pred_vs_true_inc_plot(inc, posterior_draws)


launch_shinystan(stanfit2)


library(bayesplot)


posterior <- as.array(stanfit2)
lp <- log_posterior(stanfit2)
np <- nuts_params(stanfit2)

color_scheme_set("darkgray")
mcmc_parcoord(posterior, pars = c("disp", "sigma_eta", "phi", "R[15]"))



mcmc_nuts_energy(np)#, pars = c("disp", "sigma_eta", "phi", "R[15]"))



## ====================================================== ##
# check for cointegration between incidences and conflicts #
## ====================================================== ##

library("tseries")

## load data 
incidences <- my_load_data()
nkivu <- my_load_conflict_data()

## plot data
plot(incidences, type = "l", lwd = 2)
lines(nkivu, col = "red", lwd = 2)

inc <- incidences$inc
conf <- nkivu$counts[1:length(inc)]

## check correlation
cor.test(conf, inc)


## test for cointegration with augmented dickey fuller
comb <- lm(inc~conf)
adf.test(comb$residuals, k=1)

ccf(conf, inc)
# highest correlation with incidence and conflict 15 days later. 
# seems spurious

astsa::lag2.plot(conf, inc, 15)


