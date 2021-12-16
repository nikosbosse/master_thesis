setwd("/home/nikos/Desktop/data/Google Drive/Uni/Statistik/Masterarbeit Statistik/R/")

source("functions_master_thesis.R")

my_setup()

# ================================= #
# Load and prepare Data
# ================================= #

inc <- my_load_data()


# ================================= #
# combined EpiEstim and BSTS in Stan
# ================================= #o

inc2 <- inc
inc <- inc2[1:30]
t <- length(inc)
l <- list(t = t, past_incidences = inc, tau = 7, n_pred = 10)

stanfit2 <- rstan::stan(file = "../stan/combined_EpiEstim_bsts.stan",
                        data = l,
                        iter = 4000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.99))



pairs(stanfit2, pars = c("sigma_epsilon", "sigma_eta", "lp__"))

stanfit2