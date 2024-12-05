data {
  int<lower=0> N;
  array[N] int y_pois;
}

parameters {
  real<lower=0> lambda;
}

model {
  lambda ~ normal(10,3);
  
  for(i in 1:N){
    y_pois[i] ~ poisson(lambda);
  }
}

generated quantities {
  vector[N] loglik;
  for(i in 1:N){
    loglik[i] = poisson_lpmf(y_pois[i] | lambda);
  }
}
