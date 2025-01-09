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
    array[M] int one_to_M;
    for (m in 1:M) one_to_M[m] = m;
    vector[M] q = sqrt(alpha^2 * 2 / exp(a) * to_vector(modified_bessel_first_kind(one_to_M, a)));
    return append_row(q,q);
  }
  
  matrix PHI_periodic(int N, int M, real w0, vector x) {
    matrix[N,M] mw0x = diag_post_multiply(rep_matrix(w0*x, M), linspaced_vector(M, 1, M));
    return append_col(cos(mw0x), sin(mw0x));
  }
}

data {
  int N; //number of datapoint
  int N_all;
  int<lower=1> N_x;               // Number of time points
  int N_reg;
  int<lower=1> N_cause;             // Number of causes
  
  // Deaths and populations by age group
  vector[N_all] deaths_all;
  array[N] int deaths;
  array[N_all] real n_pop_all;
  array[N] real n_pop;
  array[N_all] int cause_id_all;
  array[N] int cause_id;
  
  // Variables for regression
  matrix[N_all,N_reg] X_reg_all;
  matrix[N,N_reg] X_reg;
  
  // Time points and corresponding locations
  vector[N_x] x;                  // Time points
  array[N_all] int week_id_all;
  array[N] int week_id;
  
  // GP basis function settings
  int<lower=1> J_week;            // Number of periodic basis functions
  real<lower=0> c_year;           // Boundary scaling factor
  int M_year;                     // Number of EQ basis functions
  
  // Hyperprior parameters
  array[2] real p_intercept;      // Prior for intercept
  array[2] real p_alpha_week;     // Prior for weekly GP scale
  array[2] real p_lambda_week;    // Prior for weekly GP lengthscale
  array[2] real p_alpha_year;     // Prior for yearly GP scale
  array[2] real p_lambda_year;    // Prior for yearly GP lengthscale
  
  int inference;
}

transformed data {
  // normalize data
  real x_mean = mean(x);
  real x_sd = sd(x);
  vector[N_x] xn = (x - x_mean)/x_sd;
  
  // compute boundary value
  real L_year = c_year*max(xn);
  
  // compute basis functions for f
  real period_year = (365.25/7)/x_sd; //number of weeks divided by sd
  matrix[N_x,2*J_week] PHI_week = PHI_periodic(N_x, J_week, 2*pi()/period_year, xn);
  matrix[N_x,M_year] PHI_year = PHI_EQ(N_x, M_year, L_year, xn);
  
}

parameters {
  vector[N_cause] mu0;                           // Intercept
  array[N_cause] vector[N_reg] beta_reg;             // Regression coefficients
  
  // GPs by age group
  vector <lower=0> [N_cause] alpha_week;       // Weekly GP scale by age
  vector <lower=0> [N_cause] lambda_week;      // Weekly GP lengthscale by age
  array[N_cause] vector[2 * J_week] beta_week; // Basis coefficients for weekly GP
  
  vector <lower=0> [N_cause] alpha_year;       // Yearly GP scale by age
  vector <lower=0> [N_cause] lambda_year;      // Yearly GP lengthscale by age
  array[N_cause] vector[M_year] beta_year; // Basis coefficients for yearly GP
}

transformed parameters {
  array[N_cause] vector[2*J_week] diagSPD_week;
  array[N_cause] vector[M_year] diagSPD_year;
  
  array[N_cause] vector[N_x] f_week;
  array[N_cause] vector[N_x] f_year;
  // compute spectral densities for f
  for(g in 1:N_cause){
    diagSPD_week[g] = diagSPD_periodic(alpha_week[g], lambda_week[g], J_week);
    diagSPD_year[g] = diagSPD_EQ(alpha_year[g], lambda_year[g], L_year, M_year);
    // compute f
    f_week[g] = PHI_week * (diagSPD_week[g] .* beta_week[g]);
    f_year[g] = PHI_year * (diagSPD_year[g] .* beta_year[g]);
  }
  
  //log-transformed number of deaths
  array[N] real mu;
  for(i in 1:N){
    mu[i] = mu0[cause_id[i]] + X_reg[i] * beta_reg[cause_id[i]] + f_week[cause_id[i],week_id[i]] + f_year[cause_id[i],week_id[i]] + log(n_pop[i]);
  }
}

model {
  // Priors
  mu0 ~ normal(p_intercept[1], p_intercept[2]);
  
  //GP: variance and lengthscale
  lambda_week ~ lognormal(p_lambda_week[1], p_lambda_week[2]);
  alpha_week ~ normal(p_alpha_week[1], p_alpha_week[2]);
  lambda_year ~ lognormal(p_lambda_year[1], p_lambda_year[2]);
  alpha_year ~ normal(p_alpha_year[1], p_alpha_year[2]);
  //GP and regression parameters
  for (g in 1:N_cause) {
    beta_reg[g] ~ normal(0, 1);
    beta_week[g] ~ normal(0, 1);
    beta_year[g] ~ normal(0, 1);
  }
  
  // Likelihood
  if(inference==1){
    target += poisson_log_lpmf(deaths | mu);
  }
}

generated quantities {
  //predicted deaths
  vector[N_all] deaths_all_pred;
  vector[N_all] mu_all;
  for(i in 1:N_all){
    mu_all[i] = mu0[cause_id_all[i]] + X_reg_all[i] * beta_reg[cause_id_all[i]] + f_week[cause_id_all[i],week_id_all[i]] +
                    f_year[cause_id_all[i],week_id_all[i]] + log(n_pop_all[i]);
    deaths_all_pred[i] = poisson_log_rng(mu_all[i]);
  }
  //excess mortality for each data row
  vector[N_all] excess = deaths_all - deaths_all_pred;
}
