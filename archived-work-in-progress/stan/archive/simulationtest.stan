functions{
  real my_pos_normal_rng(real mu, real sigma) {
    real p_lower_bound = normal_cdf(0, mu, sigma);
    real u = uniform_rng(0.9999999, 1);
    real y = mu + sigma * inv_Phi(u);
    return y;
  }
}

data {
  int a;
}

parameters{
  real b;
}

model {
  a ~ normal(b, 2);
}

generated quantities{
  real test[100];
  for (i in 1:100){
    test[i] = my_pos_normal_rng(-0.5, 2);
  }
}
