functions {
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
}

// load data objects
data {
  int N;
  int N_year; //number of year
  int N_reg;
  
  array[N] int deaths;
  
  matrix[N,N_reg] X_reg;
 
  array[N] int year;
  array[N] real n_pop;

  vector[N_year] x_year;
  
  real<lower=0> c_year;
  int M_year;
 
  int inference;

}

transformed data {
  // normalize data
  //real xmean = mean(x);
  //real xsd = sd(x);
  //vector[N_x] xn = (x - xmean)/xsd;
  // compute boundary value
  real L_year = c_year*max(x_year); 
  // compute basis functions for f1
  matrix[N_year,M_year] PHI_year = PHI_EQ(N_year, M_year, L_year, x_year);
    
}

parameters {
  real mu0;
  vector[N_reg] beta_reg;
  array[M_year] real beta_year; // basis function coefficients for f
  real <lower=0> lambda_year;      // lengthscale of f
  real<lower=0> alpha_year;
  
}

transformed parameters {
  // compute spectral densities for f1
  array[M_year] real diagSPD_year = to_array_1d(diagSPD_EQ(alpha_year, lambda_year, L_year, M_year));
  // compute f
  array[N_year] real f_year = to_array_1d(PHI_year * (to_vector(diagSPD_year) .* to_vector(beta_year)));
  
  vector[N] Y_reg = X_reg * beta_reg;
  
  vector[N] mu;
  for(i in 1:N){
    mu[i] = mu0 + Y_reg[i] + f_year[year[i]]  -log(n_pop[i]);
  }
}

model {
  // GP parameters
  mu0 ~ normal(0, 5);
  beta_reg ~ normal(0, 5);
  beta_year ~ normal(0, 1);
  lambda_year ~ exponential(1); // scale the data?, lambda ~ normal(p_lambda[1], p_lambda[2]);
  alpha_year ~ exponential(1); //alpha ~ normal(p_alpha[1], p_alpha[2]);
  
  
  // likelihood
  if(inference==1){
    target += poisson_lpmf(deaths | exp(mu));
  }
}

generated quantities {
  

}
