data {
  int<lower=1> N;
  array[N] int y1;
  array[N] int y2;
  int inference;
}

parameters {
  real intercept1;
  real intercept2;
  real sigma_eta;
  array[N] real eta;                   // Random effects
}

transformed parameters{
  real sigma_eta1 = abs(sigma_eta);
  real sigma_eta2 = sigma_eta;
}

model {
  // Priors
  intercept1 ~ normal(2, 1);
  intercept2 ~ normal(2, 1);
  sigma_eta ~ normal(0, 4);
  for(i in 1:N){
    eta[i] ~ std_normal();
  }

  // Likelihood using poisson_log_lpmf
  if(inference==1){
    for (i in 1:N) {
      target += poisson_log_lpmf(y1[i] | intercept1 + eta[i]*sigma_eta1);
      target += poisson_log_lpmf(y2[i] | intercept2 + eta[i]*sigma_eta2);
    }
  }
}

generated quantities {
  real loglik=0;
   for (i in 1:N) {
    loglik += poisson_log_lpmf(y1[i] | intercept1 + eta[i]*sigma_eta1);
    loglik += poisson_log_lpmf(y2[i] | intercept2 + eta[i]*sigma_eta2);
  }
}
