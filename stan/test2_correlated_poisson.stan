data {
  int<lower=1> N;
  int N_week;
  int N_outcome;
  int N_corr;
  int N_corr2;
  array[N] int outcome_id;
  array[N] int y;
  //matrix[N_outcome,N_corr2] pos_m1;
  //matrix[N,N_outcome] pos_m2;
  array[N] vector[N_corr2] pos_m;
  array[N] int week_id;
  int n_pop;
  //vector[N_corr] sigma_eta;
}

transformed data {
}

parameters {
  vector[N_outcome] intercept;
  array[N_week] vector[N_corr] eta;                   // Random effects
  vector<upper=0>[N_corr] sigma_eta;
}

transformed parameters{
  vector[N_corr2] sigma_eta2 = append_row(abs(sigma_eta),sigma_eta);
  vector[N] effect_cor_ind;
  for(i in 1:N){
    effect_cor_ind[i] = dot_product(to_vector(pos_m[i,]) .* sigma_eta2,
                                    append_row(eta[week_id[i],],eta[week_id[i],]));
                                    
  }
}

model {
  
  //vector[N_outcome] effect_cor = pos_m1 * sigma_eta2;
  //vector[N] effect_cor_ind = (pos_m2 * effect_cor;
  
  // Priors
  intercept ~ normal(-4, 1);
  sigma_eta ~ normal(0, 4);
  for(i in 1:N_week){
    eta[i] ~ std_normal();
  }

  // Likelihood using poisson_log_lpmf
  for (i in 1:N) {
    target += poisson_log_lpmf(y[i] | intercept[outcome_id[i]] + effect_cor_ind[i]+log(n_pop));
  }
}

generated quantities {
  vector[N] mean_pois;
  for (i in 1:N) {
    mean_pois[i] = exp(intercept[outcome_id[i]] + effect_cor_ind[i]+log(n_pop));
  }
  real loglik=0;
   for (i in 1:N) {
    loglik += poisson_log_lpmf(y[i] | intercept[outcome_id[i]] + effect_cor_ind[i]+log(n_pop));
  }
}
