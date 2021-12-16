data {
  int<lower=0> n_areas;               // Number of areas
  int<lower=0> n_timesteps;           // Number of time steps
  matrix[n_areas, n_areas] space;     // spatial interaction
  int cases[n_areas, n_timesteps];    // Number of cases
}

parameters {
  real<lower=0> beta;
  real<lower=0> gamma;
  real<lower=0> delta;
  real<lower=0> epsilon;
  real<lower=0> overdisp;
}

transformed parameters {
  matrix[n_areas, n_timesteps] foi;
  {
    matrix[n_areas, n_timesteps] scaled_cases;

    for (t in 1:n_timesteps) {
      for (i in 1:n_areas) {
        scaled_cases[i, t] = pow(cases[i, t], delta);
      }
    }

    foi =
      (diag_matrix(rep_vector(beta, n_areas)) + delta * space) *
      scaled_cases +
      epsilon;
  }
}

model {
  beta ~ gamma(1, 1);
  gamma ~ gamma(1, 1);
  delta ~ gamma(1, 1);
  epsilon ~ gamma(1, 1);
  overdisp ~ gamma(1, 1);

  for (t in 2:n_timesteps) {
    for (i in 1:n_areas) {
      cases[i, t] ~ neg_binomial_2(foi[i, t-1], 1 / overdisp);
    }
  }
}

generated quantities {
  matrix[n_areas, n_timesteps] pred_cases;
  vector[n_areas * (n_timesteps-1)] log_lik;

  for (i in 1:n_areas) {
    for (t in 2:n_timesteps) {
      log_lik[(t-2) * n_areas + i] = neg_binomial_2_lpmf(cases[i, t] | foi[i, t-1], 1 / overdisp);
      pred_cases[i, t-1] = neg_binomial_2_rng(foi[i, t-1], 1 / overdisp);
    }
    pred_cases[i, n_timesteps] = neg_binomial_2_rng(foi[i, n_timesteps], 1 / overdisp);
  }
}

