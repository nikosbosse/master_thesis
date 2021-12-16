data {
  int<lower = 0> n_timesteps;         // Number of time steps
  int<lower = 1> n_pred_timesteps;    // Number of predictive time steps
  real population;                    // population size
  int cases[n_timesteps];             // Number of cases
  int cum_cases[n_timesteps];         // Cumulative number of cases
  real R0;
}

parameters {
  real<lower = 0> gamma;
  real<lower = 0> epsilon;
  real<lower = 0> sigma_mu;
  real<lower = 0> sigma_delta;
  real alpha;
  real omega;
  real eta_mu[n_timesteps];
  real eta_delta[n_timesteps];
  real<lower=cum_cases[n_timesteps] / population, upper = 1> susc;
}

transformed parameters {
  vector[n_timesteps] foi;
  real susc_pop = susc * population;
  real R0_t[n_timesteps];
  real delta[n_timesteps];

  {
    vector[n_timesteps] susceptibility;
    real trans_R0_t[n_timesteps];

    trans_R0_t[1] = log(R0) + eta_mu[1];
    delta[1] = alpha + eta_delta[1];

    for (t in 1:n_timesteps) {
      susceptibility[t] =
        max([susc_pop - cum_cases[t], 0]) / susc_pop;
      if (t > 1) {
        delta[t] = alpha + omega * (delta[t - 1] - alpha) + eta_delta[t];
        trans_R0_t[t] = trans_R0_t[t - 1] + delta[t - 1] + eta_mu[t];
      }
    }

    R0_t = exp(trans_R0_t);
    foi = (to_vector(R0_t) .* to_vector(cases)) .* susceptibility + epsilon;
  }

}

model {
  gamma ~ normal(1, 1);
  epsilon ~ gamma(1, 1);
  sigma_mu ~ normal(0, 0.1);
  sigma_delta ~ normal(0, 0.1);
  omega ~ normal(0, 0.1);
  alpha ~ normal(0, 1);

  for (t in 2:n_timesteps) {
    cases[t] ~ neg_binomial_2(foi[t - 1], 1 / gamma);
  }

  for (t in 1:n_timesteps) {
    eta_mu[t] ~ normal(0, sigma_mu);
    eta_delta[t] ~ normal(0, sigma_delta);
  }
}

generated quantities {
  vector[n_timesteps + n_pred_timesteps - 1] pred_cases;
  vector[n_timesteps - 1] log_lik;

  for (t in 2:n_timesteps) {
    log_lik[t - 1] = neg_binomial_2_lpmf(cases[t] | foi[t - 1], 1 / gamma);
    if (foi[t - 1] < 1e+6) {
      pred_cases[t - 1] = neg_binomial_2_rng(foi[t - 1], 1 / gamma);
    } else {
      pred_cases[t - 1] = -1;
    }
  }
  if (foi[n_timesteps] < 1e+6) {
    pred_cases[n_timesteps] = neg_binomial_2_rng(foi[n_timesteps], 1 / gamma);
  } else {
    pred_cases[n_timesteps] = -1;
  }

  if (n_pred_timesteps > 1) {
    real pred_cum_cases = cum_cases[n_timesteps];
    real pred_susceptibility;
    real pred_eta_delta[n_pred_timesteps - 1];
    real pred_eta_mu[n_pred_timesteps - 1];
    real pred_delta[n_pred_timesteps];
    real pred_trans_R0_t[n_pred_timesteps];

    pred_delta[1] = delta[n_timesteps];
    pred_trans_R0_t[1] = log(R0_t[n_timesteps]);

    for (t in (n_timesteps + 1):(n_timesteps + n_pred_timesteps - 1)) {
      int t_pred = t - n_timesteps + 1;
      real pred_foi;

      pred_cum_cases += pred_cases[t - 1];
      pred_susceptibility = max([susc_pop - pred_cum_cases, 0]) / susc_pop;

      pred_eta_mu[t_pred - 1] = normal_rng(0, sigma_mu);
      pred_eta_delta[t_pred - 1] = normal_rng(0, sigma_delta);

      pred_delta[t_pred] = alpha + omega * (pred_delta[t_pred - 1] - alpha) + pred_eta_delta[t_pred - 1];
      pred_trans_R0_t[t_pred] = pred_trans_R0_t[t_pred - 1] + pred_delta[t_pred - 1] + pred_eta_mu[t_pred - 1];

      pred_foi = exp(pred_trans_R0_t[t_pred]) * pred_cases[t - 1] * pred_susceptibility + epsilon;

      if (pred_foi < 1e+8) {
        if (exp(pred_trans_R0_t[t_pred]) < 0) {
          pred_cases[t] = -2;
        } else if (pred_cases[t - 1] < 0) {
          pred_cases[t] = -3;
        } else if (pred_susceptibility < 0) {
          pred_cases[t] = -4;
        } else if (epsilon < 0) {
          pred_cases[t] = -5;
        } else {
          pred_cases[t] = neg_binomial_2_rng(pred_foi, 1 / gamma);
        }
      } else {
        pred_cases[t] = -1;
        
      }
    }
  }
}