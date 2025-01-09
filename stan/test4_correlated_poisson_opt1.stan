data {
  int<lower=1> N;
  int<lower=1> N_x;
  int<lower=1> J;
  array[N] int x;
  array[N] int y;
  array[N] int y_id;
  int inference;
}

transformed data {
}

parameters {
  vector[J] intercept;
  vector<lower=0>[J] sigma;
  array[N_x] vector[J] eta;                   // Random effects
  cholesky_factor_corr[J] rho_chol;
}

transformed parameters{
  array[N_x] vector[J] eta2;
  matrix[J,J] rho2_chol = diag_post_multiply(rho_chol,  sigma);
  for(i in 1:N_x){
    eta2[i] = rho2_chol * eta[i];
  }
}

model {
  // Priors
  intercept ~ normal(4, 1);
  sigma ~ exponential(1);
  rho_chol ~ lkj_corr_cholesky(1.0);
  for(i in 1:N_x){
    eta[i] ~ std_normal();//multi_normal_cholesky(mu0,rho_chol);
  }

  //Likelihood using poisson_log_lpmf
  if(inference==1){
    for (i in 1:N) {
      target += poisson_log_lpmf(y[i] | intercept[y_id[i]] + eta2[x[i],y_id[i]]);
    }
  }
}

generated quantities {
  matrix[J,J] Sigma = rho_chol * rho_chol';
}
