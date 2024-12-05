data {
  int<lower=1> N_x;
  int<lower=1> J;
  array[J,N_x] int y;
  int inference;
}

transformed data {
}

parameters {
  vector[J] intercept;
  vector<lower=0>[J] sigma;
  array[J] row_vector[N_x] eta;                   // Random effects
  cholesky_factor_corr[J] rho_chol;
}

transformed parameters{
  matrix[J,N_x] eta2 = rho_chol * to_matrix(eta);
}

model {
  // Priors
  intercept ~ normal(4, 1);
  sigma ~ exponential(1);
  rho_chol ~ lkj_corr_cholesky(1.0);
  for(i in 1:J){
    eta[i] ~ std_normal();//multi_normal_cholesky(mu0,rho_chol);
  }

  //Likelihood using poisson_log_lpmf
  if(inference==1){
    for(i in 1:J){
      target += poisson_log_lpmf(y[i,]| intercept[i] + eta2[i,]*sigma[i]);
    }
  }
}

generated quantities {
  matrix[J,J] Sigma = rho_chol * rho_chol';
}
