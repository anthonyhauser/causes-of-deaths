functions {
  //see https://github.com/avehtari/casestudies/blob/master/Birthdays
  // basis function (exponentiated quadratic kernel)
    matrix PHI_EQ(int N, int M, real L, vector x) {
      matrix[N,M] A = rep_matrix(pi()/(2*L) * (x+L), M);
      vector[M] B = linspaced_vector(M, 1, M);
      matrix[N,M] PHI = sin(diag_post_multiply(A, B))/sqrt(L);
      for (m in 1:M) PHI[,m] = PHI[,m] - mean(PHI[,m]); // scale to have mean 0
      return PHI;
    }
  // spectral density (exponentiated quadratic kernel)
  vector diagSPD_EQ(real alpha, real lambda, real L, int M) {
    vector[M] B = linspaced_vector(M, 1, M);
    return sqrt( alpha^2 * sqrt(2*pi()) * lambda * exp(-0.5*(lambda*pi()/(2*L))^2*B^2) );
  }

  vector diagSPD_periodic(real alpha, real lambda, int M) {
    real a = 1/lambda^2;
    int one_to_M[M];
    for (m in 1:M) one_to_M[m] = m;
    vector[M] q = sqrt(alpha^2 * 2 / exp(a) * to_vector(modified_bessel_first_kind(one_to_M, a)));
    return append_row(q,q);
  }

  matrix PHI_periodic(int N, int M, real w0, vector x) {
    matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector(M, 1, M));
    return append_col(cos(mw0x), sin(mw0x));
  }
}

// load data objects
data {
  //dimensions
  int N;
  int N_week; //number of weeks
  int N_year; //number of years
  int N_reg;
  int N_pand;
  
  //deaths and pop
  array[N] int deaths;
  vector[N_pand] deaths_pand;
  array[N] real n_pop;
  array[N_pand] real n_pop_pand;
  
  //variables in regression
  matrix[N,N_reg] X_reg;
  matrix[N_pand,N_reg] X_reg_pand;
  
  //variables in GP: week and year
  array[N] int year;
  array[N] int week;
  array[N_pand] int year_pand;
  array[N_pand] int week_pand;
  
  //locations at which GPs are evaluated
  vector[N_year] x_year;
  vector[N_week] x_week;
  
  //basis function GP
  int<lower=1> J_week;   // number of cos and sin functions for periodic
  real<lower=0> c_year; // factor c to determine the boundary value L
  int M_year; //number of basis functions
  
  //hyperparameters
  array[2] real p_lambda_week;
  
  int inference;
}

transformed data {
  // normalize data
  real x_week_mean = mean(x_week);
  real x_week_sd = sd(x_week);
  vector[N_week] xn_week = (x_week - x_week_mean)/x_week_sd;

  real x_year_mean = mean(x_year);
  real x_year_sd = sd(x_year);
  vector[N_year] xn_year = (x_year - x_year_mean)/x_year_sd;
  
  // compute boundary value
  real L_year = c_year*max(xn_year);
  
  // compute basis functions for f1
  real period_week = max(x_week)/x_week_sd;
  matrix[N_week,2*J_week] PHI_week = PHI_periodic(N_week, J_week, 2*pi()/period_week, xn_week);
  matrix[N_year,M_year] PHI_year = PHI_EQ(N_year, M_year, L_year, xn_year);
    
}

parameters {
  real mu0;
  vector[N_reg] beta_reg;
  array[2*J_week] real beta_week; // basis function coefficients for f
  real <lower=0> lambda_week;      // lengthscale of f
  real<lower=0> alpha_week;
  array[M_year] real beta_year; // basis function coefficients for f
  real <lower=0> lambda_year;      // lengthscale of f
  real<lower=0> alpha_year;
  
}

transformed parameters {
  // compute spectral densities for f1
  array[2*J_week] real diagSPD_week = to_array_1d(diagSPD_periodic(alpha_week, lambda_week, J_week));
  array[M_year] real diagSPD_year = to_array_1d(diagSPD_EQ(alpha_year, lambda_year, L_year, M_year));
  // compute f
  array[N_week] real f_week = to_array_1d(PHI_week * (to_vector(diagSPD_week) .* to_vector(beta_week)));
  array[N_year] real f_year = to_array_1d(PHI_year * (to_vector(diagSPD_year) .* to_vector(beta_year)));
  
  //regression
  vector[N] Y_reg = X_reg * beta_reg;
  
  //log-transformed number of deaths
  array[N] real mu;
  for(i in 1:N){
    mu[i] = mu0 + Y_reg[i] + f_week[week[i]]  + f_year[year[i]] + log(n_pop[i]);
  }
}

model {
  // intercept and regression parameters
  mu0 ~ normal(-8, 2);
  beta_reg ~ normal(0, 1);
  
  // GP parameters
  beta_week ~ normal(0, 1);
  lambda_week ~ gamma(p_lambda_week[1], p_lambda_week[2]); // scale the data?, lambda ~ normal(p_lambda[1], p_lambda[2]);
  alpha_week ~ normal(0, 1); //alpha ~ normal(p_alpha[1], p_alpha[2]);
  beta_year ~ normal(0, 1);
  lambda_year ~ normal(0, 1); // scale the data?, lambda ~ normal(p_lambda[1], p_lambda[2]);
  alpha_year ~ normal(0, 1); //alpha ~ normal(p_alpha[1], p_alpha[2]);
  
  // likelihood
  if(inference==1){
    target += poisson_log_lpmf(deaths | mu);
  }
}

generated quantities {
  vector[N] deaths_pred;
  vector[N_pand] mu_pand;
  vector[N_pand] deaths_pand_pred;

  //predicted deaths
  for(i in 1:N){
    deaths_pred[i] = poisson_log_rng(mu[i]);
  }
  vector[N_pand] Y_reg_pand = X_reg_pand * beta_reg;
  for(i in 1:N_pand){
    mu_pand[i] = mu0 + Y_reg_pand[i] + f_week[week_pand[i]]  + f_year[year_pand[i]] + log(n_pop_pand[i]);
    deaths_pand_pred[i] = poisson_log_rng(mu_pand[i]);
  }
  
  //excess mortality for each data row
  vector[N_pand] excess = deaths_pand - deaths_pand_pred;
  vector[N_pand] rel_excess = excess./deaths_pand_pred;
  //excess mortality overall
  real overall_deaths_pand_pred = sum(deaths_pand_pred);
  real overall_excess = sum(excess);
  real overall_rel_excess = overall_excess/sum(deaths_pand_pred);
}
