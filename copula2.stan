functions {
  real gauss_copula_cholesky_lpdf(matrix u0, matrix L, matrix u_lwb, matrix u_upb) {
    matrix[rows(u0),cols(u0)] u = u_lwb + (u_upb-u_lwb) .* u0;
    array[rows(u)] row_vector[cols(u)] q;
    for (n in 1:rows(u)) {
      q[n] = inv_Phi(u[n]);
    }

    return multi_normal_cholesky_lpdf(q | rep_row_vector(0, cols(L)), L)
            - std_normal_lpdf(to_vector(to_matrix(q)));
  }

  vector gauss_copula_cholesky_pointwise(matrix u0, matrix L, matrix u_lwb, matrix u_upb) {
    matrix[rows(u0),cols(u0)] u = u_lwb + (u_upb-u_lwb) .* u0;
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

  matrix uvar_bounds(array[] int pois_y1, array[] int pois_y2,
                     real lambda1, real lambda2,
                     int is_upper) {
    int N = size(pois_y1);
    matrix[N, 2] u_bounds;

    for (n in 1:N) {
      if (is_upper == 0) {
        u_bounds[n, 1] = pois_y1[n] == 0.0
                          ? 0.0 : poisson_cdf(pois_y1[n] - 1 | lambda1);
        u_bounds[n, 2] = pois_y2[n] == 0.0
                          ? 0.0 : poisson_cdf(pois_y2[n] - 1 | lambda2);
      } else {
        u_bounds[n, 1] = poisson_cdf(pois_y1[n] | lambda1);
        u_bounds[n, 2] = poisson_cdf(pois_y2[n] | lambda2);
      }
    }

    return u_bounds;
  }
}

data {
  int<lower=0> N;
  array[N] int pois_y1;
  array[N] int pois_y2;
}

parameters {
  real<lower=0> lambda1;
  real<lower=0> lambda2;
  matrix<lower=0,upper=1>[N, 2] u0;
  cholesky_factor_corr[2] rho_chol;
}

model {
  matrix[N, 2] u_lwb=uvar_bounds(pois_y1, pois_y2, lambda1, lambda2, 0);
  matrix[N, 2] u_upb=uvar_bounds(pois_y1, pois_y2, lambda1, lambda2, 1);
  u0 ~ gauss_copula_cholesky(rho_chol,u_lwb,u_upb);
}

generated quantities {
  real rho = multiply_lower_tri_self_transpose(rho_chol)[1, 2];
}
