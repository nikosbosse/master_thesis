my_setup <- function(){
	options(max.print = 2000)
	par(family = "Serif")
	options(width=as.integer(160))
	options(max.print = 36000)
	#library(tidyverse)
	library(ggplot2)
	theme_set(theme_get() + theme(text = element_text(family = 'Serif')))

	library(rstan)
	#options(mc.cores = parallel::detectCores())
	options(mc.cores = 4)
	rstan_options(auto_write = TRUE)
	library(extraDistr)
	#library(EpiEstim)
	library(shinystan)
	library(bsts)
	library(matrixStats)
}


# ================================================= # 
# ================================================= # 

my_load_data <- function(){
	ebola <- read.csv("../data/Data_ DRC Ebola Outbreak, North Kivu and Ituri - MOH-By-Health-Zone.csv", header = T, stringsAsFactors = F)

	# remove row with explanations
	ebola <- ebola[-1,]

	## only look at one province
	nkivu <- ebola[ebola$province == "North Kivu",]
	nkivu$total_cases_change <- as.integer(nkivu$total_cases_change)

	## sum up all cases for that province
	nkivu_inc <- aggregate(total_cases_change ~ report_date, data=nkivu, FUN="sum")

	nkivu_inc$report_date <- as.Date(nkivu_inc$report_date)
	colnames(nkivu_inc) <- c("date", "inc")

	nkivu_inc$inc[nkivu_inc$inc < 0] = 0
	nkivu_inc <- nkivu_inc[-1,] #remove first entry that is zero
	return(nkivu_inc)
}


# ================================================= # 
# ================================================= # 

my_EpiEstim_stan <- function(past_incidences){	
	inc <- past_incidences
	t <- length(inc)
	l <- list(t = t, past_incidences = inc, tau = 7)

	stanfit2 <- rstan::stan(file = "../stan/estimate_R_EpiEstim_rebuild.stan",
	                        data = l,
	                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))

	s1 <- summary(stanfit2)$summary %>%
		as.data.frame() %>%
		rownames_to_column("var") %>%
		filter(grepl("^R", var)) %>%
		select(-var) %>%
		mutate(estimate="stan") %>%
		select(c(1,4,8,11)) %>%
		mutate(id=1:n())

	s1 <- s1[17:nrow(s1),] %>%
		mutate(id=1:n())
	colnames(s1) <- c("mean", "low", "high", "estimate", "id")

	result <- list(R = s1, stanfit = stanfit2)	
}

# ================================================= # 
# ================================================= # 

my_stan_bsts <- function(past_r, n_pred = 10){
	t <- length(past_r)
	l <- list(t = t, past_r = past_r, n_pred = n_pred)

	stanfit2 <- rstan::stan(file = "../stan/bayesian_structural_time_series_model_r.stan" ,
	                        data = l,
	                        iter = 4000, warmup = 800, thin = 1, control = list(adapt_delta = 0.97))

	sum <- summary(stanfit2)$summary
	sum <- sum %>% as.data.frame(rownames(sum)) %>% mutate(var = rownames(sum)) 

	params <- sum %>% filter(sum$var %in% c("sigma_epsilon", "sigma_eta", "phi", "D"))
	rownames(params) <- params$var

	predicted <- sum %>% filter(grepl("^r_pred", var))
	rownames(predicted) <- predicted$var

	res <- list(params = params, predicted = predicted, stanfit = stanfit2)

	r <- res$predicted
	r <- r[,c(1,4,8)]
	colnames(r) <- c("mean", "low", "high")


	ggplot(r, aes(x = 1:n_pred, y = mean, ymin = low, ymax = high)) + geom_line() + geom_ribbon(alpha = 0.5)

	return(res)
}


# ================================================= # 
# ================================================= # 
## empirical cumulative distribution function
my_F <- function(predictions, k){
	return(sum(predictions <= k) / length(predictions))
}

## ECDF for integers
my_F_int <- function(predictions, k){
	return(sum(predictions == k) / length(predictions))
}


# ================================================= # 
# ================================================= # 

## PIT transformation
my_PIT <- function(true_values, samples){
	# true_values is a vector of length n
	# samples is a matrix with n columns where
	# every column [,i] has all the draws that correspond 
	# to one true_value[i]

	n <- length(true_values)
	u <- numeric(n)
	#u[1] <- NA

	for (i in 1:n){
		r <- runif(1, min = 0, max = 1) 
		F_k <- my_F(samples[,i], true_values[i])
		F_k_1 <- my_F(samples[,i], true_values[i] - 1)
		u[i] <- F_k #for continuous variables
		#u[i] <- F_k + r * (F_k - F_k_1)
	}
	return(u[!is.na(u)])
}


# ================================================= # 
# ================================================= # 

my_centrality <- function(u){
	sum(u > 0.25 & u < 0.75)/length(u) - 0.5
}


# ================================================= # 
# ================================================= # 

my_sharpness <- function(forecasts){
	#forecasts is a matrix with n columns where
	# every column [,i] has all the draws that correspond 
	# to one true_value[i]

	S <- function(predictions){
		1/0.675 * median(abs(predictions - median(predictions)))
	}
	res <- apply(forecasts, MARGIN = 2, FUN = S)
	return(res)
}


# ================================================= # 
# ================================================= # 

my_bias <- function(true_values, samples){
	res <- numeric(length(true_values))
	#res[1] <- NA
	for (j in 1:length(res)){
		F_k <- my_F(samples[,j], true_values[j])
		F_k_1 <- my_F(samples[,j], (true_values[j] - 1))
		res[j] <- 1 - (F_k + F_k_1) 
		#res[j] <- 1 - (F_k - F_k_1) 
	}
	return(res[!is.na(res)])
}


# ================================================= # 
# ================================================= # 
## ranked probability score

my_RPS <- function(true_values, samples){
	t <- length(true_values)
	rps <- numeric(t)	
	for (j in 1:t){
		m <- max(samples[,j])
		#i <- 1:m
		#rps[t] <- 0
		for (i in 1:m){
			rps[j] <- rps[j] + (my_F(samples[,j], i) - (i >= true_values[j]))^2 
		}
	}
	return(rps)
}

# ================================================= # 
# ================================================= # 
## David-Sebastiani-Score

my_DSS <- function(true_values, samples){
	t <- length(true_values)
	dss <- numeric(t)	
	for (j in 1:t){
		mu_sample <- mean(samples[, j])
		sd_sample <- sd(samples[, j]) 

		dss[j] <- ((true_values[j] - mu_sample) / sd_sample)^2 + 2 * log(sd_sample) 
	}
	return(dss)
}

# ================================================= # 
# ================================================= # 
## Function to get the infectiousness. Already 
## implemented in stan, but I want to double
## check the results

my_infectiousness <- function(past_incidences, n_pred = 0){
	## inputs: past_incidences
	## number of timesteps to forecast n_pred
	
	t <- length(past_incidences)

	w <- rep(0, times = t + n_pred)
	for (i in 1:(t + n_pred)){
	    if (i > 40){
	      w[i] = 0;
	    } else {
	      w[i] = pgamma(i + 0.5, 2.706556, 0.1768991) - pgamma(i  - 0.5, 2.706556, 0.1768991); 
	    }
	}

	infectiousness <- rep(0.000001, times = t)
    for (s in 2:t){
    infectiousness[s] = 0;
    for (i in 1:(s - 1)){
      infectiousness[s] = infectiousness[s] + past_incidences[i] * w[s - i];
    }
    #infectiousness[1:10] <- 1

    infectiousness_weekly <- rep(0, times = t/7)
    for (i in 1:length(infectiousness_weekly)){
    	infectiousness_weekly[i] <- sum(infectiousness[(7*(i-1)+1):(7*i)])
    }

    infectiousness_pred = 0
    for (i in 1:(t)){
      infectiousness_pred = infectiousness_pred + past_incidences[i] * w[t + 1 - i]
    }
    
  }
  l <- list(weights = w, infectiousness = infectiousness, infectiousness_one_ahead = infectiousness_pred, infectiousness_weekly = infectiousness_weekly)
  return(l)

  ## output: 
  ## weights (cut after 40 periods)
  ## calculated infectiousness
  ## one step ahead calculation of infectiousness
}


# ================================================= # 
# ================================================= # 
## get next expected incidence
## my_infectiousness(inc) = my_next_expected_incidence(inc[-length(inc)])
## now also implemented in my_infectiousness)()


my_next_expected_incidence <- function(past_incidences){
	t <- length(past_incidences) 
	# note that we have t instead of t-1 here as in the master thesis
	adjusted_affected <- 0	
	for (s in 1:t){
		## implementation for continuous numbers
		# weight <- ddgamma(t + 1 - s, 2.706556, 0.1768991) 

		## implementation for integers
		## not that here there is no cut-off value that sets every weight for periods > 40 to 0. 
		weight <- pgamma(t + 1 - s + 0.5, 2.706556, 0.1768991) - pgamma(t + 1 - s  - 0.5, 2.706556, 0.1768991)

		adjusted_affected <- adjusted_affected + past_incidences[s] * weight
	}	
	return(adjusted_affected)
}


# ================================================= # 
# ================================================= # 
my_infection_overview <- function(past_incidences){
	tmp <- my_infectiousness(past_incidences)

	data.frame(incidences = past_incidences, 
			   infectiousness = tmp$infectiousness,
			   #weights_rel = rev(tmp$weights)
			   weights = tmp$weights
			   )

	## output weights: outputs the weights for the 
	## corresponding number of periods. w[1] = 1 period
	## away. The weights do not at all correspond to the 
	## columns next to it! 

	## weights_rel: weights relative to the one after the last period
}


# ================================================= # 
# ================================================= # 
## unsure what this does. delete?
my_plot_r_pred <- function(predicted){
  mean_R <- rowMeans(predicted)
  quantiles <- rowQuantiles(predicted, probs=c(0.05, 0.95))
  days <- 1:nrow(predicted)
  q <- ggplot() +  geom_line(aes(x=days, y=mean_R)) +
    geom_ribbon(aes(x=days, ymin=quantiles[,1],
                    ymax=quantiles[,2]),alpha=0.3)

}

# ================================================= # 
# ================================================= # 
## plot 2 histograms. useful for comparing prior and posterior
my_plot_two_histograms <- function(vector1, vector2, 
								   breaks = 100, 
								   upper_limit = NULL){
	if(!is.null(upper_limit)){
		vector1 <- vector1[vector1 < upper_limit]
		vector2 <- vector2[vector2 < upper_limit]
	}

	## set breakpoints and define minimum 
	## breakpoint a and maximum breakpoint b
	a <- min(c(vector1, vector2)) 
	b <- max(c(vector1, vector2)) 

	## define axis
	ax <- pretty(a:b, n = breaks)

	while(min(ax) > a | max(ax) < b){
		if (min(ax) > a){
		a <- a - (ax[2] - ax[1])
		}
		if (max(ax) < b){
		b <- b + (ax[2] - ax[1])
		}
		ax <- pretty(a:b, n = breaks)
	}

	

	## make histogram A and B
	plot1 <- hist(vector1, breaks = ax, plot = FALSE)
	plot1$density = plot1$counts/sum(plot1$counts)
	plot2 <- hist(vector2, breaks = ax, plot = FALSE)
	plot2$density = plot2$counts/sum(plot2$counts)

	## set correct font
	par(family = "Serif")

	## define two colors
	col1 <- rgb(168,209,225,max = 255, alpha = 75)
	col2 <- rgb(248,183,193, max = 255, alpha = 75)

	## plot and add 2nd plot to first
	plot(plot1, col = col1, xlab = "vec1 is blue, vec2 is pink", xlim = c(a, b)) 
	plot(plot2, col = col2, add = TRUE) 
}


# ================================================= # 
# ================================================= # 
## diagnostic functions to visualize the evolution of
## delta and R under different circumstances

my_evolution_delta <- function(delta0 = 0, D = -0.02, phi = 0, n = 100, random = F, sigma = 0.5){
	deltas = rep(0, n)
	for (i in 2:n){
		deltas[i] <- D + phi * (deltas[i-1] - D)
		if (random){
			deltas[i] <- deltas[i] + rnorm(1, 0, sigma)
		}
	}
	return(deltas)
}


my_evolution_R <- function(n = 100, sigma_epsilon = 0.5, ...){
	R <- rep(0, n)
	R[1] <- 1
	delta <- my_evolution_delta(n = n, ...) 
	for (i in 2:n){
		mean <- R[i-1] + delta[i]
		while (R[i] <= 0){
			R[i] <- rnorm(1, mean, sigma_epsilon)	
		}
		
	}
	return(R)
}

## results: 
## the phi determines how quickly the thing will revert to a constant
## trend. See this paper: http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html



# ================================================= # 
# ================================================= # 
## functions to extract and to plot the posterior predictive
## values against the values that were actually observed
my_extract <- function(stanfitobject, var = "Inc_post"){
	tmp <- extract(stanfitobject)
	tmp <- getElement(tmp, var)
	apply(tmp, MARGIN = 2, FUN = mean)
}


my_pred_vs_true_inc_plot <- function(y_true, 
									 y_pred, 
									 vert_lines = NULL){
	ymin <- min(c(y_true, y_pred))
	ymax <- max(c(y_true, y_pred))

	plot(y_true, type = "l", col = "grey", family = "Serif", ylim = c(ymin, ymax))
	lines(y_pred, col = "red", lwd = 3)

	if (!is.null(vert_lines) && vert_lines > 0){
		abline(v = vert_lines, col = "blue", lty = 2)
	}
}


# ================================================= # 
# ================================================= # 
## fit the stan model iteratively 

my_iterative_fit <- function(past_incidences, 
								  n_pred = 14, 
								  interval = 0,
								  start_period = 30,
								  tau = 7,
								  stanfile = "../stan/combined_EpiEstim_bsts_only_sigma_eta.stan"){
	## inputs: 
	## vector past incidences
	## number of periods to forecast into the future
	## interval between predictions
	time <- Sys.time()
	if (interval == 0) interval <- n_pred

	total_n <- (length(past_incidences))
	current_n <- start_period

	## calculate how many fits you need. 
	runs <- ceiling((total_n - start_period) / interval)

	# predictions <- numeric(0)
	res <- list()

	i <- 0
	while (current_n < total_n){
		print("run ", as.character(i), "of ", as.character(runs))
		index <- 1:current_n
		if ((length(index)) > total_n) {index <- i:total_n}

		inc <- past_incidences[index]
		T <- length(inc)
		l <- list(T = T, past_incidences = inc, tau = tau, n_pred = n_pred)
		stanfit <- rstan::stan(file = stanfile,
                        data = l,
                        iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.99))	

		i <- i + 1
		res[[i]] <- stanfit
		current_n <- start_period + i * interval
	}

	print(time - Sys.time())
	return(res)
}


# ================================================= # 
# ================================================= # 
## use iterative fits of the stan model to plot
## predicted vs. actual cases of Ebola

my_iter_pred_vs_true <- function(inc, 
								 n_pred = 14, 
								 interval = 0,
								 start_period = 30,
								 tau = 7,
								 stanfile = "../stan/combined_EpiEstim_bsts_only_sigma_eta.stan"){

	if (interval == 0) interval <- n_pred

	l <- my_iterative_fit(past_incidences = inc,
						  n_pred = n_pred, 
						  interval = interval,
						  start_period = start_period,
						  tau = tau,
						  stanfile = stanfile)

	n_total <- length(inc) 
	predictions <- lapply(l, my_extract, var = "I_pred")
	predictions <- unlist(predictions, use.names = FALSE)

	if ((n_total - start_period) > interval){
		vert_lines = seq(interval, n_total - start_period, interval)
	} else {
		vert_lines = -1
	}
	
	try(my_pred_vs_true_inc_plot(y_true = inc[(start_period + 1):n_total], 
								 y_pred = predictions, 
								 vert_lines = vert_lines))

	return(list(predictions = predictions, 
				stanfitobjects = l, 
				vert_lines = vert_lines))
}

vert_lines <- function(interval = 14, y, start_period = 30) {
	n_total <- length(y)
	vert_lines = seq(interval, n_total - start_period, interval)
	return(vert_lines)
}


# ================================================= # 
# ================================================= # 

my_load_conflict_data <- function(){
	conflicts <- read.csv("../data/2018-08-03-2020-01-19-Democratic_Republic_of_Congo.csv", stringsAsFactors = F)

	cols_to_keep <- c("event_date", 
					  "event_type", 
					  "sub_event_type", 
					  "admin1", 
					  "admin2")

	conflicts <- conflicts[, colnames(conflicts) %in% cols_to_keep]
	
	Sys.setlocale("LC_TIME", "C")
	conflicts$event_date <- as.Date(conflicts$event_date, format = "%d %B %Y")

	nkivu <- conflicts[conflicts$admin1 == "Nord-Kivu", ]

	nkivu$counts <- 1
	confl_inc <- aggregate(counts ~ event_date, data=nkivu, FUN="sum")

	return(confl_inc)
}


# ================================================= # 
# ================================================= # 
## aggregate data by week

my_make_weekly <- function(vector){
	t = length(vector)
	weekly <- rep(0, times = t/7)
    for (i in 1:length(weekly)){
    	weekly[i] <- sum(vector[(7*(i-1)+1):(7*i)])
    }
    return(weekly)
}