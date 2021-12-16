functions{
  real my_pos_normal_rng(real mu, real sigma) {
    real p_lower_bound = normal_cdf(0, mu, sigma);
    real u;
    real y;

    if (p_lower_bound > 0.9999999){
      p_lower_bound = 0.9999999;
    }
    u = uniform_rng(p_lower_bound, 1);
    y = mu + sigma * inv_Phi(u);
    if (y <= 0) {
      y = 0.00000001;
    }
    if (y > 50){
      y = 50;
    }
    return y;  
  }
}

data {
  int T;
  int <lower = 0> past_incidences[T];
  int tau;
  int n_pred;
}

transformed data{
  real infectiousness[T];
  real w[T + n_pred]; // - 1 actually
  infectiousness[1] = 1;

  for (i in 1:(T + n_pred)){
    if (i > 40){
      w[i] = 0;
    } else {
      w[i] = gamma_cdf(i + 0.5, 2.706556, 0.1768991) - gamma_cdf(i  - 0.5, 2.706556, 0.1768991); 
    }
  }

  for (s in 2:T){
    infectiousness[s] = 0;
    for (i in 1:(s - 1)){
      infectiousness[s] += past_incidences[i] * w[s - i];
    }
  }
}


parameters{
  real <lower = 0> R[T];
  real <lower = 0> disp;
  real <lower = 0> sigma_epsilon;
  real <lower = 0> sigma_eta;
  real <lower = -1, upper = 1> phi;
  real delta[T];
  real D;
}


model {
  for (s in (tau + 1):T){
    for (i in (s-tau + 1):s){
      // hier war irgendwann mal R[s] * infectiousness[i] = inf??
      past_incidences[i] ~ neg_binomial_2(R[s] * infectiousness[i], disp);
    }
  }

  for (s in 1:(tau)){
    R[s] ~ gamma((0.15)/2, (0.1)/2); //normal(1,1);
    delta[s] ~ normal(D, sigma_eta);
  }
  

  for (s in tau:(T-1)){
    R[s + 1] ~ normal(R[s] + delta[s], sigma_epsilon); 
    delta[s+1] ~ normal(D + phi * (delta[s] - D), sigma_eta);
  }

  D ~ normal(0,1);
  phi ~ normal(0, 0.1);

  // try gamma prior
  sigma_eta ~ inv_gamma(1, 1); 
  sigma_epsilon ~ inv_gamma(1, 1);


  disp ~ gamma(0.05, 0.05);  
}

generated quantities{
  //// posterior draws
  int <lower = 0> Inc_post[T];

  real I_pred[n_pred];
  real infectiousness_pred[n_pred];
  real r_pred[n_pred];
  real delta_pred[n_pred];


  // ==================================================
  // posterior draws
  // ==================================================
  for (s in 1:T){
    Inc_post[s] = neg_binomial_2_rng(R[s] * infectiousness[s], disp);  
  }

  // ==================================================
  // predictions in the future
  // ==================================================

  // ==================================================
  // do everything for prediction period s = 1 
  // --------------------------------------------------
  infectiousness_pred[1] = 0;
  for (i in 1:T){
    infectiousness_pred[1] += past_incidences[i] * w[T + 1 - i]; 
  }  
  
  r_pred[1] = my_pos_normal_rng(R[T] + delta[T], sigma_epsilon);
  delta_pred[1] = normal_rng(D + phi * (delta[T] - D), sigma_eta);
  I_pred[1] = neg_binomial_2_rng(r_pred[1] * infectiousness_pred[1], disp);
  // ==================================================



  // ==================================================
  // do everything for all prediction periods s >= 2
  // --------------------------------------------------
  for (s in 1:(n_pred - 1)){
    r_pred[s + 1] = my_pos_normal_rng(r_pred[s] + delta_pred[s], sigma_epsilon);  
    delta_pred[s + 1] = normal_rng(D + phi * (delta_pred[s] - D), sigma_eta);

    // infectiousness from observed cases
    infectiousness_pred[s + 1] = 0;  
    for (i in 1:T){
      infectiousness_pred[s + 1] +=  past_incidences[i] * w[T + s + 1 - i]; 
    }  
    //infectiousness from predicted cases
    for (j in 1:(s)){
      infectiousness_pred[s + 1] += I_pred[j] * w[s + 1 - j];  
    }
    I_pred[s + 1] = neg_binomial_2_rng(r_pred[s + 1] * infectiousness_pred[s + 1], disp);

  }

}











  /*


  for (s in 1:(n_pred - 1)){
    r_pred[s + 1] = my_pos_normal_rng(r_pred[s] + delta_pred[s], sigma_epsilon);
    
    delta_pred[s + 1] = normal_rng(D + phi * (delta_pred[s] - D), sigma_eta);
     
    //I_pred[s + 1] = 0; 
    I_pred[s + 1] = neg_binomial_2_rng(r_pred[s+1] * infectiousness_pred[s+1], disp);
  }


  // r_pred[3] = 0; // my_pos_normal_rng(r_pred[s] + delta_pred[s], sigma_epsilon);
  // delta_pred[3] = 0;// normal_rng(D + phi * (delta_pred[s] - D), sigma_eta);
  //I_pred[3] = 0; // neg_binomial_2_rng(r_pred[s+1] * infectiousness_pred[s+1], disp);


// Test for period = 3

  for (s in 3:3){
    // infectiousness from observed cases
    infectiousness_pred[s] = 0;  
    for (i in 1:t){
      infectiousness_pred[s] +=  past_incidences[i] * w[t + s - i]; 
    }  
    //infectiousness from predicted cases
    for (j in 1:(s-1)){
      infectiousness_pred[s] += I_pred[j] * w[s - j]; //compared to 
    }
  }
I_pred[3] = neg_binomial_2_rng(r_pred[3] * infectiousness_pred[3], disp);
/*I_pred[4] = neg_binomial_2_rng(r_pred[4] * infectiousness_pred[4], disp);


  for (s in 3:3){
    r_pred[s + 1] = my_pos_normal_rng(r_pred[s] + delta_pred[s], sigma_epsilon);  
    delta_pred[s + 1] = normal_rng(D + phi * (delta_pred[s] - D), sigma_eta);
  }
  for (s in 4:4){
    // infectiousness from observed cases
    infectiousness_pred[s] = 0;  
    for (i in 1:t){
      infectiousness_pred[s] +=  past_incidences[i] * w[t + s - i]; 
    }  
    //infectiousness from predicted cases
    for (j in 1:(s-1)){
      infectiousness_pred[s] += I_pred[j] * w[s - j]; //compared to 
    }
  }
I_pred[4] = neg_binomial_2_rng(r_pred[3] * infectiousness_pred[3], disp);
*/



  


// ======================================0
// notes


// here in this case, infectiousness[t] means all values including the value in t-1 summed up and weighted

// the values for the inverse gamma prior for sigma_epsilon and sigma_eta are just values I chose by eye-balling the distribution

// in the generated data block, for the infectiousness line (infectiousness_pred[s] += past_incidences[i] * w[t + s - i];) t + 1 corresponds to s

// the last delta_t is not defined by data. Therefore we have to sample one.  


