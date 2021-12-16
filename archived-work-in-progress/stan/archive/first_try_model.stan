data {
  int n;
  int <lower = 0> past_incidences[n];
  real weight[n];
}

/*transformed data{
  real[n] adjusted_affected = 0;
  adjusted_affected[1] = past_incidences[1] * weight[1];
  for (i in 2:n){
    adjusted_affected[i] = adjusted_affected[i-1] + past_incidences[i] * weight[i];
  } 
}
*/
parameters{
  real <lower = 0> R;
  real <lower = 0> phi;
  // real weight;
  // construct a prior with the density as mean? but what variance? 
}

model {
  for (i in 2:n){
    real adjusted_affected = 0;
    for (s in 1:(i-1)){
      adjusted_affected += past_incidences[s] * exp(gamma_lpdf(i - s | 2.706556, 0.1768991));
    }
    
    target += neg_binomial_2_lpmf(past_incidences[i] | R * adjusted_affected, phi);   
  }
  R ~ gamma(0.15, 0.1);
}

