// now model has an R that can evolve. 

data {
  int n;
  int <lower = 0> past_incidences[n];
  // real weight[n];
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
  real <lower = 0> R[n];
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
    
    target += neg_binomial_2_lpmf(past_incidences[i] | R[i] * adjusted_affected, phi);   
  }

   //cannot be computed, as incidences before are 0
  R[1] ~ normal(99, 0.0000001);
  R[2] ~ gamma(0.15, 0.1);
  for (i in 3:n){
    R[i] ~ normal(R[i-1], 0.1);
  }

  phi ~ gamma(0.1,0.1);
  //R ~ gamma(0.15, 0.1);
}

