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
    if (is_upper == 0) {
      for (n in 1:N) {
        for(j in 1:J){
          u_bounds[n, j] = pois_y[n,j] == 0.0
                          ? 0.0 : poisson_cdf(pois_y[n,j] - 1 | lambda[j]);
          if(u_bounds[n,j]>0.99){
            print(n);
            print(j);
            print(pois_y[n,j]);
            print(lambda);
            print("------------");
          } 
        }
      }
    }else {
      for (n in 1:N) {
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
  matrix<
    lower=uvar_bounds(pois_y, lambda, 0),
    upper=uvar_bounds(pois_y, lambda, 1)
  >[N, J] u;
  cholesky_factor_corr[J] rho_chol;
}

model {
   //---------------------------
  // Priors
  //---------------------------
  rho_chol ~ lkj_corr_cholesky(1.0);
  lambda ~ normal(lambda_p[1],lambda_p[2]);

  u ~ gauss_copula_cholesky(rho_chol);
}

generated quantities {

}
