library('rstan')
library('loo')
library('bayesplot')
setwd("/home/nikos/Desktop/Masterarbeit Statistik/R/")
theme_set(theme_get() + theme(text = element_text(family = 'Serif')))

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(width=as.integer(160))
par(family = "Serif")
library(shinystan)

Nt <- 50 ## number of timesteps
Ns <- 20 ## number of areas

## true beta (fluctuating around mean, forced to be >= 0)
beta_mean <- 1.5
beta_true <- matrix(pmax(0, rnorm(n=Nt*Ns, mean=beta_mean, sd=1)), ncol=Nt)

head(beta_true)

## spatial interaction matrix - random
space <- matrix(runif(n=Ns*Ns), ncol=Ns)
space <- space - diag(diag(space))
## column-normalise
space <- t(t(space)/colSums(space))

## simulate infectious process
process <- matrix(0, nrow=Ns, ncol=Nt) ## initialise to 0
process[sample(1:Ns, 1), 1] <- 5 ## start with 1 case somewhere

## spatiotemporal Poisson process
overdisp <- 0.5

## spatial interaction parameter
gamma <- beta_mean / (2 * Ns)
delta <- 0.8
epsilon <- beta_mean / 10

for (t in 1:(Nt-1)) {
  mean <- (diag(beta_true[, t]) + gamma * space) %*% process[, t]**delta + epsilon
  process[, t+1] <- rnbinom(Ns, mu=mean, size=1/overdisp)
}

sm <- stan_model("../stan/sim.stan")

## NUTS sampling
chain <- sampling(
    sm,
    data=list(n_areas=Ns, n_timesteps=Nt, space=space, cases=process),
    chains=1, iter=5000
)
# sometimes the model is misspecified? 

## plot Rhat values
cs <- as.data.frame(summary(chain)$summary)
ggplot(cs, aes(x=Rhat)) + geom_histogram()

## VB sampling
veb <- vb(sm, data=list(n_areas=Ns, n_timesteps=Nt, space=space, cases=process))

## compare NUTS and VB
log_lik_nuts <- extract_log_lik(chain, merge_chains=FALSE)
log_lik_vb <- extract_log_lik(veb, merge_chains=FALSE)

r_eff_nuts <- relative_eff(exp(log_lik_nuts))
r_eff_vb <- relative_eff(exp(log_lik_vb))

loo_nuts <- loo(log_lik_nuts, r_eff = r_eff_nuts, save_psis=TRUE)
loo_vb <- loo(log_lik_vb, r_eff = r_eff_vb, save_psis=TRUE)

## predictions
pred_nuts <- rstan::extract(chain, pars=c("pred_cases"))[["pred_cases"]]
pred_vb <- rstan::extract(veb, pars=c("pred_cases"))[["pred_cases"]]

## flatten
yrep_nuts <- t(apply(pred_nuts, 1, function(x) c(t(x[, -ncol(x)]))))
yrep_vb <- t(apply(pred_vb, 1, function(x) c(t(x[, -ncol(x)]))))
flat_data <- c(t(process[, -1]))

## check loo intervals
ppc_loo_intervals(
    y = flat_data,
    yrep = yrep_nuts,
    psis_object=loo_nuts$psis_object,
    order="median"
)

## randomised quantiles
intquant <- function(x, data) {
    dimx <- dim(x)
    if (is.null(dimx)) dimx <- c(length(x), 1)
    xdata <- array(rep(data, each=dimx[1]), dim=dimx)

    upper <- 1-apply(x>xdata, seq_along(dimx)[-1], mean)
    lower <- apply(x<xdata, seq_along(dimx)[-1], mean)

    upper[upper < lower] <- lower[upper < lower]
    rand <- runif(prod(dimx[-1]), lower, upper)

    return(array(rand, dim=dimx[-1]))
}

dimx <- dim(yrep_nuts)
## PIT histogram
pit.values <- intquant(yrep_nuts, flat_data)
hist(pit.values)

launch_shinystan(chain)
