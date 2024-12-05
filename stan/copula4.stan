functions {
  real gauss_copula_cholesky_lpdf(matrix u, matrix L) {
    array[rows(u)] row_vector[cols(u)] q;
    for (n in 1:rows(u)) {
      q[n] = inv_Phi(u[n]);
    }

    return multi_normal_cholesky_lpdf(q | rep_row_vector(0, cols(L)), L)
            - std_normal_lpdf(to_vector(to_matrix(q)));
  }

  vector gauss_copula_cholesky_pointwise(matrix u, matrix L) {
    int N = rows(u);
    int J = cols(u);
    matrix[J,J] Sigma_inv = chol2inv(L);
    vector[J] inv_sigma_inv = inv(diagonal(Sigma_inv));
    matrix[N, J] log_lik_mat;
    matrix[N, J] G;
    matrix[N, J] q;

    for (n in 1:N) {
      q[n] = inv_Phi(u[n]);
    }

    G = q * Sigma_inv;

    for (n in 1:N) {
      for (j in 1:J) {
        log_lik_mat[n, j] = normal_lpdf(q[n, j] | q[n, j] - G[n, j] * inv_sigma_inv[j], sqrt(inv_sigma_inv[j]))
                              - std_normal_lpdf(q[n, j]);
      }
    }
    return to_vector(log_lik_mat);
  }

  matrix uvar_bounds(array[,] int pois_y, vector lambda,
                     int is_upper) {
    int N = size(pois_y);
    int J = dims(pois_y)[2];
    matrix[N, J] u_bounds;

    for (n in 1:N) {
      if (is_upper == 0) {
        for(j in 1:J){
          u_bounds[n, j] = pois_y[n,j] == 0.0
                          ? 0.0 : poisson_cdf(pois_y[n,j] - 1 | lambda[j]);
        }
      } else {
        for(j in 1:J){
         u_bounds[n, j] = poisson_cdf(pois_y[n,j] | lambda[j]);
        }
      }
    }

    return u_bounds;
  }
}

data {
  int<lower=0> N;
  int<lower=0> J;
  array[N,J] int pois_y;
  array[2] real lambda_p;
}

parameters {
  vector<lower=50,upper=150>[J] lambda;
  matrix<lower=0,upper=1>[N, J] u_raw;
  cholesky_factor_corr[J] rho_chol;
}

model {
  matrix[N, J] u;
  matrix[N, J] Lb=uvar_bounds(pois_y, lambda, 0);     // lower bound for uniform variates
  matrix[N, J] Ub=uvar_bounds(pois_y, lambda, 1);     // upper bound for uniform variates
  matrix[N, J] Db=Ub-Lb;     // = Ub - Lb (useful for Jacobian)
  

  for (n in 1:N) {
    for(j in 1:J){
      if(Lb[n,j]>0.99999){
        print(n);
        print(j);
        print(pois_y[n,j]);
        print(lambda[j]);
        print("------------");
      } 
    }
  }
  
  //---------------------------
  // Priors
  //---------------------------
  rho_chol ~ lkj_corr_cholesky(1.0);
  lambda ~ normal(lambda_p[1],lambda_p[2]);
  
  //---------------------------
  // Complete data likelihood
  //---------------------------
  // Binary outcomes
  for(j in 1:J){
    target += log(Db); // Jacobian adjustment
    u[, j] = Lb[,j] + Db[,j] .* u_raw[, j]; 
  }

  u ~ gauss_copula_cholesky(rho_chol);
}

generated quantities {

}
