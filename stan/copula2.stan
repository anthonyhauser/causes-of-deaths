functions {
  real gaussian_copula_mvn_chol_lpdf(
    matrix Q, matrix L
  ) {
    int n = rows(Q);
    int K = cols(Q);
    matrix[K,K] Rhoinv = chol2inv(L);
    // M = (Rho^(-1) - I) * Q'Q = A * Q'Q
    matrix[K,K] M = add_diag(Rhoinv, -1.) .* crossprod(Q);
    // tr(A %*% Q'Q) = sum(A * Q'Q)
    return -n * sum(log(diagonal(L))) - 0.5 * sum(M);
  }
}
data {
  int<lower=0> n;                       // sample size
  int<lower=0> J;                      // number of binary margins
  array[n, J] int<lower=0,upper=1> y; // binary random variates
}
transformed data {
  matrix[n,J] ymat = to_matrix(y);
}
parameters {
  cholesky_factor_corr[J] L;
  vector<lower=0,upper=1>[J] p;
  matrix<lower=0,upper=1>[n,J] u_raw;
}
model {
  //---------------------------
  // Temporary variables
  //---------------------------
  matrix[n,J] Q;    // Latent Q matrix (Q[, j] = Phi_inv(U[, j]))
  vector[n] Lb;     // lower bound for uniform variates
  vector[n] Ub;     // upper bound for uniform variates
  vector[n] Db;     // = Ub - Lb (useful for Jacobian)
  //---------------------------
  // Priors
  //---------------------------
  L ~ lkj_corr_cholesky(1.0);
  
  //---------------------------
  // Complete data likelihood
  //---------------------------
  // Binary outcomes
  for(j in 1:J){
    Lb = ymat[, j] * (1 - p[j]);       // lower bound of latent uniform
    Ub = 1 - (1 - ymat[, j]) * p[j];   // upper bound of latent uniform
    Db = Ub - Lb;                       // useful for jacobian adjustment
    target += log(Db);                  // Jacobian adjustment
    Q[, j] = inv_Phi(Lb + Db .* u_raw[, j]); 
  }
  // Copula density
  Q ~ gaussian_copula_mvn_chol(L);
}
generated quantities {
  matrix[J,J] Corr = multiply_lower_tri_self_transpose(L);
}