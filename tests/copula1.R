#Models with copula.

#https://discourse.mc-stan.org/t/copula-regression-example-gaussian-poisson/35071/9
#https://users.aalto.fi/~johnsoa2/notebooks/CopulaIntro.html#generating-arbitrarily-distributed-correlated-data-with-copulas
#https://mistis.inrialpes.fr/docs/Nelsen_2006.pdf
#https://mc-stan.org/math/prim_2prob_2poisson__lpmf_8hpp_source.html


lambda_r1 <- 10
lambda_r2 <- 20
rho_r <- 0.5

z_discrete <- matrix(rnorm(1000), ncol = 2) %*%
  chol(matrix(c(1, rho_r, rho_r, 1), nrow = 2))
y_discrete <- pnorm(z_discrete)
y_discrete[,1] <- qpois(y_discrete[,1], lambda_r1)
y_discrete[,2] <- qpois(y_discrete[,2], lambda_r2)



copula_discrete_mod <- cmdstan_model(paste0(code_root_path,"stan/copula3.stan"))
fit1 <- copula_discrete_mod$sample(
  adapt_delta=0.8,
  data = list(N = nrow(y_discrete),
              pois_y1 = y_discrete[,1],
              pois_y2 = y_discrete[,2]),
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100)
fit$summary(variables = c("lambda1", "lambda2", "rho"))
fit1$summary(variables = c("u[1,1]"))
fit1$summary(variables = c("u"))
fit$time()$total

fit=fit1

#Check posterior distributions between chains
d=fit$draws()[,,]
n_iter_per_chain = d[,1,1] %>% length()
d1=as.data.frame(ftable(d[,,dimnames(d)[[3]]=="u[323,1]"]))
d2=as.data.frame(ftable(d[,,grepl("lambda1",dimnames(d)[[3]])]))
d3= full_join(d1 %>% dplyr::select(iteration,chain,u=Freq),
              d2 %>% dplyr::select(iteration,chain,lambda1=Freq)) 


d3 %>%
  ggplot(aes(x=u,y=lambda1)) +
  geom_point()

corr()


################################################################################
J=3
N=100
lambda = rep(100,J)
rho_m = matrix(NA,nrow=J,ncol=J)
rho_m <- matrix(runif(J^2, min = -J, max = J), J, J)
rho_m <- (rho_m + t(rho_m)) / 2  # Symmetrize
# Force diagonal elements to 1 (correlation matrix property)
diag(rho_m) <- 1

# Check positive definiteness and correct if necessary
library(Matrix)
rho_m <- nearPD(rho_m, corr = TRUE)$mat %>% as.matrix()
print(rho_m)

z_discrete <- matrix(rnorm(J*N), ncol = J) %*% chol(rho_m)
u_discrete <- pnorm(z_discrete)
y_discrete <- qpois(u_discrete, lambda)

data.frame(x=y_discrete[,1],y=y_discrete[,2]) %>% 
ggplot(aes(x=x,y=y)) +
  geom_point()

data_list = list(N = nrow(y_discrete),
                J = ncol(y_discrete),
                pois_y = y_discrete,
                lambda_p = c(lambda[1],lambda[1]*0.2))

initfun <- function() {list(lambda=structure(rnorm(data_list$J,
                                                   data_list$lambda_p[1],
                                                   data_list$lambda_p[2]),dim=data_list$J))}

copula_discrete_mod <- cmdstan_model(paste0(code_root_path,"stan/copula6.stan"))
fit1 <- copula_discrete_mod$sample(
  #init=initfun,
  adapt_delta=0.8,
  data=data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100)
fit1$summary(variables = c("lambda", "phi","rho_chol"))
fit1$time()$total

library(rstan)
library(shinystan)
copula_discrete_mod <- stan_model(paste0(code_root_path,"stan/copula6.stan"))
fit1 <- sampling(
  copula_discrete_mod,
  data = data_list,
  iter = 1000,             # Number of iterations
  chains = 4,              # Number of chains
  #init = initfun,          # Use the defined init function
  seed = 123,              # Set a seed for reproducibility
  control = list(adapt_delta = 0.8)  # Control settings
)
launch_shinystan(fit1)
summary(fit1)$summary %>% View()

#Check posterior distributions between chains
d=fit$draws()[,,]
n_iter_per_chain = d[,1,1] %>% length()
d1=as.data.frame(ftable(d[,,dimnames(d)[[3]]=="u[323,1]"]))
d2=as.data.frame(ftable(d[,,grepl("lambda1",dimnames(d)[[3]])]))
d3= full_join(d1 %>% dplyr::select(iteration,chain,u=Freq),
              d2 %>% dplyr::select(iteration,chain,lambda1=Freq)) 
d3 %>%
  ggplot(aes(x=u,y=lambda1)) +
  geom_point()



##################################################################################################
y_pois=c(rpois(20,10),500,501,20,100)
data_list = list(N = length(y_pois),
                 y_pois = y_pois)

initfun <- function() {list(lambda=structure(rnorm(data_list$J,
                                                   data_list$lambda_p[1],
                                                   data_list$lambda_p[2]),dim=data_list$J))}

poisson_mod <- cmdstan_model(paste0(code_root_path,"stan/poisson1.stan"))
fit1 <- poisson_mod$sample(
  #init=initfun,
  adapt_delta=0.8,
  data=data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100)
fit1$summary() %>% View()
