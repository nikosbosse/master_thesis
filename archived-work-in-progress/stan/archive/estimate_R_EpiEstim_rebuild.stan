data {
  int t;
  int <lower = 0> past_incidences[t];
  //real weight[t];
  int tau;
  int n_pred;
}

transformed data{
  // here in this case, infectiousness[t] means all values including the value in t-1 summed up and weighted
  real infectiousness[t];
  real w[t + n_pred - 1];
  infectiousness[1] = 0;

  for (i in 1:t + n_pred -1){
    w[i] = gamma_cdf(i + 0.5, 2.706556, 0.1768991) - gamma_cdf(i  - 0.5, 2.706556, 0.1768991); 
  }

  for (s in 2:t){
    infectiousness[s] = 0;
    for (i in 1:(s - 1)){
      infectiousness[s] += past_incidences[i] * w[s - i];
    }
  }
}

parameters{
  real <lower = 0> R[t];
  real <lower = 0> phi;
}

model {
  for (s in (tau + 1):t){

    for (i in (s-tau + 1):s){
      // target += neg_binomial_2_lpmf(/*past_incidences[i]*/ 5 | R[s] * /*infectiousness[i]*/ 1, phi); 
      target += neg_binomial_2_lpmf(past_incidences[i] | R[s] * infectiousness[i], phi); 
            

      //past_incidences[i] ~ neg_binomial_2(R[s] * infectiousness[i], phi);
    }
  }
  for (i in 1:t){
    R[i] ~ gamma(0.15, 0.1);    
  }
  phi ~ gamma(0.05, 0.05);  
  

}

generated quantities{
  real I_pred[n_pred];
  real infectiousness_pred[n_pred];

  infectiousness_pred[1] = 0;
  for (i in 1:t){
    infectiousness_pred[1] += past_incidences[i] * w[t + 1 - i]; //compared to above, s = t+1 now
  }  
  I_pred[1] = neg_binomial_2_rng(R[t] * infectiousness_pred[1], phi);


  for (s in 2:n_pred){

    infectiousness_pred[s] = 0;  
    // infectiousness from observed cases
    for (i in 1:t){
      infectiousness_pred[s] += past_incidences[i] * w[t + s - i]; //compared to above, s = t+1 now
    }  

    //infectiousness from predicted cases
    if (s == 1) continue; //the first infectiousness is only determined by observed cases
    for (j in 1:(s-1)){
      infectiousness_pred[s] += I_pred[j] * w[s - j]; //compared to 
    }

    //problem: this just takes the R[t] as a fixed value --> uncertainty not accounted for
    I_pred[s] = neg_binomial_2_rng(R[t] * infectiousness_pred[s], phi);

  }
}
  
/*



    infectiousness_pred[2] += past_incidences[i] * w[t + 2 - i] + I_pred[2-1] * w[]

  I_pred[1] = neg_binomial_2_rng(R[t] * infectiousness[t], phi);


  //assuming that R[t+1] = R[t] (and therefore that all R[t+h] will be constant?)
  I_pred[1] = neg_binomial_2_rng(R[t] * infectiousness[t], phi)


  real I_hat_1;
  //real I_hat_2; 
  I_hat_1 = neg_binomial_2_rng(R[t] * infectiousness[t], phi);
  //I_hat_2 = neg_binomial_2_rng(R[t] * I_hat_1, phi);*/










  
/*  Explanation: for a tau of say 2, R_3 is the first value that can be estimated. R1 cannot be estimated, because the incidence before that was 0. R2 cannot be estimated, because it would have to be the same as R1 which cannot be estimated. So we start at R_3. */

/*
tau = 2
t = 10
erster Index von 3 bis 10
  dann von 2 bis 3 (2,3)
    dann für 2: 1
    und für 3: 1, 2*/

/*
  // running index from beginning till end. 
  for (s in (tau + 1):t){
    real adjusted_affected[tau];
    // the adjusted_affected[1] will be the oldest one, adjusted_affected[tau] will be the most recent one.

    // running index so that the process is repeated for every value in the sliding window. 
    int counter = 0;
    for (j in (s-tau + 1):s) {
      //for every time period in the sliding window all past incidences have to be weighted and summed up. 
      counter +=1;
      adjusted_affected[counter] = 0;
      for (i in 1:(j-1)){
        real weight;
        weight = gamma_cdf(j - i + 0.5, 2.706556, 0.1768991) - gamma_cdf(j - i  - 0.5, 2.706556, 0.1768991);

        adjusted_affected[counter] += past_incidences[i] * weight;   
      }
      target += neg_binomial_2_lpmf(past_incidences[s] | R[s] * adjusted_affected[counter], phi);  
      R[s] ~ gamma(0.15, 0.1);
    }
  }  
  phi ~ gamma (0.05, 0.05);*/



