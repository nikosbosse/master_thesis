setwd("/home/nikos/Desktop/data/Google Drive/Uni/Statistik/Masterarbeit Statistik/R/")

a <- 

source("functions_master_thesis.R")

my_setup()
library(EpiEstim)
library(dplyr)
library(tidyverse)

# ================================= #
# Load and prepare Data
# ================================= #

inc <- my_load_data()$inc

# ================================= #
# Rebuild EpiEstim in Stan
# ================================= #

inc <- inc[1:100]
t <- length(inc)
l <- list(t = t, past_incidences = inc, tau = 7, n_pred = 10)

stanfit2 <- rstan::stan(file = "./../stan/archive/estimate_R_EpiEstim_rebuild.stan",
                        data = l,
                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))
launch_shinystan(stanfit2)


# ================================= #
# Run EpiEstim in Stan
# ================================= #

# EpiEstim analysis for comparison
r <- estimate_R(inc, config = make_config(list(
                        cv_posterior=2,
                        t_start=2:(length(inc)-7),
                        t_end=9:length(inc),
                        mean_si= 15.3,
                        std_si= 9.3)),
                      method="parametric_si")

# ================================= #
# compare EpiEstim with Stan estimates
# ================================= #

s1 <- summary(stanfit2)$summary %>%
	as.data.frame() %>%
	rownames_to_column("var") %>%
	filter(grepl("^R", var)) %>%
	dplyr::select(-var) %>%
	mutate(estimate="stan") %>%
	dplyr::select(c(1,4,8,11)) %>%
	mutate(id=1:n())

s1 <- s1[16:nrow(s1),] %>%
	mutate(id=1:n())
colnames(s1) <- c("mean", "low", "high", "estimate", "id")

s2 <- r$R %>%
    dplyr::select(c(3,5,11)) %>%
    mutate(estimate="epiestim") %>%
    mutate(id=1:n())

s2 <- s2[8:nrow(s2),]%>%
	mutate(id=1:n())

colnames(s2) <- c("mean", "low", "high", "estimate", "id")

df <- bind_rows(s1, s2)

ggplot(df, aes(x=id, y=mean, ymin=low, ymax=high, color=estimate, fill=estimate)) +
	geom_line() +
	geom_ribbon(alpha=0.5) +
	coord_cartesian(ylim=c(0, 7.5))



# ================================= #
# Run BSTS in Stan
# ================================= #

past_r <- s2$mean
log_r <- log(past_r / (15 - past_r))
t <- length(log_r)
l <- list(t = t, past_r = past_r, n_pred = 10)


res2 <- my_stan_bsts(past_r, n_pred = 40)




## BSTS with one step ahead forecasting
past_r2 <- past_r
past_r <- past_r2#[1:25]
t <- length(past_r)
l <- list(t = t, past_r = past_r, n_pred = 5)


model <- stan_model(file = "../stan/bsts_r_one_step_ahead.stan")

stanfit_vb <- vb(model, data = l, iter = 4000)



s <- summary(stanfit_vb)$summary

s[1900:nrow(s),]

stanfit2 <- rstan::stan(file = "../stan/bsts_r_one_step_ahead.stan" ,
	                        data = l,
	                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))



# r <- res$predicted
# r <- r[,c(1,4,8)]
# colnames(r) <- c("mean", "low", "high")

# ggplot(r, aes(x = 1:10, y = mean, ymin = low, ymax = high)) + geom_line() + geom_ribbon(alpha = 0.5)






delta <- sum %>% as.data.frame() %>% rownames_to_column("var") %>% filter(grepl("^d", var))
delta <- delta$mean
plot(delta, type = "l")




# ================================= #
# Run BSTS in R
# ================================= #


## now try BSTS modeling for the time series of R_t
# extract Rs and transform them 
log_transform_r <- function(){}
log_r <- r$R$"Mean(R)"
log_r <- log_r[!is.na(log_r)]
log_r <- log(log_r / (15 - log_r))

# first a state space must be specified
ss <- AddSemilocalLinearTrend(list(), log_r, slope.ar1.prior = NormalPrior(0,0.1))

ss2 <- AddSemilocalLinearTrend(list(), past_r, slope.ar1.prior = NormalPrior(0,0.1))

model <- bsts(past_r, state.specification = ss, niter = 2000)


predicted <- predict(model, horizon=40, burn=400)$distribution
b = 15
return_value <- b*exp(predicted) / (1 + exp(predicted))

apply(predicted, 2, mean)

res2$predicted$mean



plot_r_pred <- function(predicted){
  mean_R <- rowMeans(predicted)
  quantiles <- rowQuantiles(predicted, probs=c(0.05, 0.95))
  days <- 1:nrow(predicted)
  q <- ggplot() +  geom_line(aes(x=days, y=mean_R)) +
    geom_ribbon(aes(x=days, ymin=quantiles[,1],
                    ymax=quantiles[,2]),alpha=0.3)

}

plot(plot_r_pred(t(return_value)))

past_r

res2$params



# ================================= #
# Assess goodness of forecast
# ================================= #

## extract all the posterior samples drawn
sam <- rstan::extract(stanfit_vb)
r_true <- l$past_r
r_pred <- sam$r_pred[,1:403]


## check calibration
u <- my_PIT(r_true, r_pred)
hist(u, breaks = 50)

## check centrality
my_centrality(u)

## check sharpness
sharp <- my_sharpness(r_pred)
mean(sharp[-1])

## check bias
bias <- my_bias(r_true, r_pred)
mean(bias[-1])

## check ranked probability score RPS



# ================================= #
# combined EpiEstim and BSTS in Stan
# ================================= #o

inc2 <- inc
inc <- inc2[1:30]
t <- length(inc)
l <- list(t = t, past_incidences = inc[1:30], tau = 7, n_pred = 10)

stanfit <- rstan::stan(file = "../stan/combined_EpiEstim_bsts.stan",
                        data = l,
                        iter = 4000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.99))

stanfit

inc[8] <- 4


a <- (extract(stanfit2)$r_pred[,3])
min(a)
max(a)
which(a < 0)
extract(stanfit2)$I_pred[,1]



pnorm(0, mean=-3.127164, sd = sigma_eta)

qnorm(0.999) * 0.73


ex <- extract(stanfit2)$delta_t

hist(ex, breaks = 100)

which(ex == Inf)

sort(ex, decreasing = T)