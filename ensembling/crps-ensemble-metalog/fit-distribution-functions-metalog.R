get_samples_SHIFT <- function(values, quantiles, n_samples) {
  # SHELF cannot handle quantiles that are exactly the same.
  # We therefore check whether there are any duplicated values
  # if so, a small number is added to it to allow for fitting

  if (any(duplicated(values))) {
    values <- values + rnorm(n = length(values), mean = 0.01, sd = 0.01)
  }

  fit <- SHELF::fitdist(vals = sort(values),
                        probs = sort(quantiles))

  best_dist <- fit$best.fitting

  possibilities <- c("normal", "t", "gamma", "lognormal", "logt", "beta", "hist")
  index <- (1:7)[possibilities %in% best_dist]

  samples <- SHELF::sampleFit(fit, n_samples)[, index]

  return(samples)
}

# sebs code --> takes in a named list of quantiles
fn_gamma <- function(par, x) {
  quantiles <- as.numeric(names(x))
  quantiles <- quantiles[!is.na(x)]
  x <- x[!is.na(x)]
  return(sum((qgamma(quantiles, shape = par[1], rate = par[2]) - x)**2))
}

fit_gamma <- function(values, quantiles, init) {

  x <- values
  names(x) <- quantiles

  if (missing(init)) {
    init <- c(shape = 1, rate = 1)
  }

  res <- nloptr::sbplx(x0 = init, fn = fn_gamma, x = x,
                       lower = c(shape = 0, rate = 0),
                       control = list(xtol_rel = 1.0e-6, ftol_rel = 1.0e-6))
  sol <- res$par
  names(sol) <- names(init)

  return(as.list(sol))
}




fit_metalog <- function(values, quantiles, init) {

  x <- values
  names(x) <- quantiles

  if (missing(init)) {
    init <- c(shape = 1, rate = 1)
  }

  res <- nloptr::sbplx(x0 = init, fn = fn_gamma, x = x,
                       lower = c(shape = 0, rate = 0),
                       control = list(xtol_rel = 1.0e-6, ftol_rel = 1.0e-6))
  sol <- res$par
  names(sol) <- names(init)

  return(as.list(sol))
}


get_samples_metalog <- function(values, quantiles, term_limit = 13, n_samples = 1000) {
  fit <- metalog(x = values,
                 probs = quantiles,
                 term_limit = term_limit,
                 term_lower_bound = term_limit - 1)

  samples <- rmetalog(fit, n = n_samples, term = term_limit)
  return(samples)
}

# rmetalog(a, term = 7)
#
#
# library(rmetalog)
#
# quantile_levels <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
# values <- qnorm(quantile_levels)
# names(values) <- quantile_levels
#
# a <-
#
# plot(a)
#
# a <- metalog(x = values,
#              probs = quantiles,
#              term_limit = 13,
#              term_lower_bound = 12)
#
# plot(a)
# rmetalog(a, term = 13)
#
# class(a)
