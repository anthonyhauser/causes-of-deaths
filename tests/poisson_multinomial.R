corr12=-1
n=1000

df = data.frame(log_mu1=log(0.01),log_mu2=log(0.01),corr12=corr12,
                mu_corr = rnorm(100,mean=0,sd=abs(corr12))) %>% 
  dplyr::mutate(mu1=exp(log_mu1 + mu_corr+log(n)),
                mu2=if_else(corr12<0,exp(log_mu2 - mu_corr+log(n)),
                           exp(log_mu2 + mu_corr + log(n)))) %>% 
  rowwise() %>% 
  dplyr::mutate(y1=rpois(1,mu1),
                y2=rpois(1,mu2))
mean_y1=mean(df$y1)
mean(df$y2)
cor(df$y1,df$y2)
df %>% 
  dplyr::mutate(y1_bin = as.numeric(y1<mean_y1)) %>%
  group_by(y1_bin) %>% dplyr::summarise(mean=mean(y2))

df %>% 
ggplot(aes(x = y1, y = y2)) +
  geom_density_2d(aes(color = ..level..), size = 1) +
  scale_color_viridis_c() +  # Optional color scale
  theme_minimal() +
  labs(title = "2D Density Plot of Bimodal Distribution", x = "X Variable", y = "Y Variable")

df %>% 
  ggplot(aes(x=y1,y=y2))+
  geom_point()+
  xlim(c(0,100))+ylim(c(0,100))



#############################################################
N_outcome=2
N_corr = (N_outcome)*(N_outcome-1)/2
N_corr2 = N_corr*2
df2 = data.frame(outcome_id = rep(1:N_outcome,each=dim(df)[1]),
                 week_id = rep(1:dim(df)[1],N_outcome),
                 y = c(df$y1,df$y2))
N=length(df2$y)
N_week = max(df2$week_id)

pos_df = expand.grid(outcome2=1:N_outcome,outcome1=1:N_outcome) %>% 
  as.data.frame() %>%
  filter(outcome1!=outcome2) %>% dplyr::select(outcome1,outcome2)
pos_df0 = pos_df %>% filter(outcome1<=outcome2) %>% 
  dplyr::mutate(par_id=row_number())
pos_df = rbind(pos_df0,
               pos_df0 %>% dplyr::select(outcome1=outcome2,outcome2=outcome1,par_id)) %>% 
  dplyr::mutate(par_id2 = row_number())

pos_m1 = matrix(0,nrow = N_outcome,ncol=N_corr2)
for(i in 1:N_outcome){
  pos_m1[i,pos_df %>% filter(outcome1==i) %>% pull(par_id2)]=1
}
pos_m = pos_m1[df2$outcome_id,]
# pos_m2 = matrix(0,nrow = N,ncol=N_outcome)
# for(i in 1:N){
#   pos_m2[i,df2[i,"outcome_id"]]=1
# }

data_list = list(N=length(df2$y),
                 N_outcome = N_outcome,
                 N_week=N_week,
                 N_corr = N_corr,
                 N_corr2 = N_corr2,
                 y = df2$y,
                 outcome_id = df2$outcome_id,
                 week_id = df2$week_id,
                 pos_m = pos_m,
                 n_pop = n,
                 inference=1)

mod1 <- cmdstan_model(paste0(code_root_path,"stan/test1_correlated_poisson.stan"))
mod2 <- cmdstan_model(paste0(code_root_path,"stan/test2_correlated_poisson.stan"))


fit <- mod1$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
data_list$sigma_eta=-1
fit_m1 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_m1$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

data_list$sigma_eta=0
fit_0 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_0$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

data_list$sigma_eta=1
fit_1 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_1$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

data_list$sigma_eta=2
fit_2 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_2$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

data_list$sigma_eta=-1.5
fit_m1.5 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_m1.5$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

data_list$sigma_eta=-2
fit_m2 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_m2$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

data_list$sigma_eta=-4
fit_m4 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_m4$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

data_list$sigma_eta=-10
fit_m10 <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_m10$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

fit_2$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_1$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_0$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_m1$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_m1.5$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_m2$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_m4$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_m10$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

d = fit_m4$summary(variables = c("mean_pois"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
d[1:10,]
d[101:110,]

d = fit_m4$summary(variables = c("eta"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
d



fit_pos <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_pos$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_pos$summary(variables = c("sigma_eta"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
d = fit_pos$summary(variables = c("mean_pois"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
d[1:10,]
d[101:110,]

fit_neg <- mod2$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
fit_neg$summary(variables = c("loglik"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
fit_neg$summary(variables = c("sigma_eta"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
d = fit_neg$summary(variables = c("mean_pois"), "mean",~quantile(.x, probs = c(0.025, 0.975)))
d[1:10,]
d[101:110,]

data_list$inference=1
fit_all <- mod1$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)

fit_all$diagnostic_summary()
fit_all$sampler_diagnostics()
fit_all$summary() %>% 
  arrange(-rhat)


d=fit_all$draws()[,,]
n_iter_per_chain = d[,1,1] %>% length()
d1=as.data.frame(ftable(d[,,grepl("sigma_eta",dimnames(d)[[3]]) &
                            !grepl("sigma_eta2",dimnames(d)[[3]])]))
d2=as.data.frame(ftable(d[,,grepl("loglik",dimnames(d)[[3]])]))
d3 = full_join(d1 %>% dplyr::select(iteration,chain,sigma_eta=Freq),
          d2 %>% dplyr::select(iteration,chain,loglik=Freq)) %>% 
  dplyr::mutate(iteration=as.numeric(iteration))
d3$sigma_eta %>% range()
d3 %>% 
  ggplot(aes(x=sigma_eta)) + 
  geom_histogram()

d3 %>% 
  ggplot(aes(x=sigma_eta,y=loglik,col=chain)) + 
  geom_point()

d3 %>% filter(chain==4,iteration<10) %>% 
  ggplot(aes(x=iteration,y=sigma_eta)) +
  geom_point()+geom_line()


data_list$inference=0
fit_prior <- mod1$sample(
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
d=fit_prior$draws()[,,]
n_iter_per_chain = d[,1,1] %>% length()
d1=as.data.frame(ftable(d[,,grepl("sigma_eta",dimnames(d)[[3]])]))
d2=as.data.frame(ftable(d[,,grepl("loglik",dimnames(d)[[3]])]))
full_join(d1 %>% dplyr::select(iteration,chain,sigma_eta=Freq),
          d2 %>% dplyr::select(iteration,chain,loglik=Freq)) %>% 
  ggplot(aes(x=sigma_eta)) + 
  geom_histogram()

full_join(d1 %>% dplyr::select(iteration,chain,sigma_eta=Freq),
          d2 %>% dplyr::select(iteration,chain,loglik=Freq)) %>% 
  ggplot(aes(x=sigma_eta,y=loglik)) + 
  geom_point()+
  ylim(c(-5000,-2000))+xlim(c(-2,2))

library(rstan)
fit <- stan(file = "stan/test1_correlated_poisson.stan",
            data = data_list, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)
launch_shinystan(fit)



################################################################################
library(cmdstanr)
library(dplyr)

J=5
n <- 100
sigma=c(1,1)*0.5
corr12 = -1
sigma %*% t(sigma) 
R <- matrix(c(1, corr12,
              corr12, 1) * (sigma %*% t(sigma)), 
            nrow = 2, ncol = 2, byrow = TRUE)
R
mu <- c(X = 0, Y = 0)
df=MASS::mvrnorm(n, mu = mu, Sigma = R) %>% as.data.frame() 
colnames(df) = c("mu_corr1","mu_corr2")
df = df %>% 
  dplyr::mutate(mu1=exp(log(10)+mu_corr1),
                mu2=exp(log(10)+mu_corr2)) %>% 
  rowwise() %>% 
  dplyr::mutate(y1=rpois(1,mu1),
                y2=rpois(1,mu2)) %>% ungroup()
apply(df,2,sd)

#Simulate data
# corr12=0.1
# df = data.frame(log_mu1=log(10),log_mu2=log(10),corr12=corr12,
#                 mu_corr = rnorm(100,mean=0,sd=abs(corr12))) %>% 
#   dplyr::mutate(mu1=exp(log_mu1 + mu_corr),
#                 mu2=if_else(corr12<0,exp(log_mu2 - mu_corr),
#                             exp(log_mu2 + mu_corr))) %>% 
#   rowwise() %>% 
#   dplyr::mutate(y1=rpois(1,mu1),
#                 y2=rpois(1,mu2))

#plot of the two correlated Poisson distributions
df %>% 
  ggplot(aes(x=mu_corr1,y=mu_corr2))+
  geom_point()
df %>% 
  ggplot(aes(x=mu1,y=mu2))+
  geom_point() +
  xlim(c(0,100))+ylim(c(0,100))
df %>% 
  ggplot(aes(x=y1,y=y2))+
  geom_point()+
  xlim(c(0,100))+ylim(c(0,100))

data_list=list(N=dim(df)[1],
               y1=df$y1,
               y2=df$y2,
               inference=1)

#Stan model fit
mod3 <- cmdstan_model(paste0(code_root_path,"stan/test3_correlated_poisson.stan"))
fit <- mod3$sample(
  adapt_delta=0.99,
  iter_warmup=2000,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)

#High rhat
fit$summary() %>% 
  arrange(-rhat)
#The correlation parameter is not well estimated
fit$summary(variables = c("sigma_eta"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

#Check posterior distributions between chains
d=fit$draws()[,,]
n_iter_per_chain = d[,1,1] %>% length()
d1=as.data.frame(ftable(d[,,grepl("sigma_eta",dimnames(d)[[3]]) &
                            !grepl("sigma_eta2|sigma_eta1",dimnames(d)[[3]])]))
d2=as.data.frame(ftable(d[,,grepl("loglik",dimnames(d)[[3]])]))
d3= full_join(d1 %>% dplyr::select(iteration,chain,sigma_eta=Freq),
          d2 %>% dplyr::select(iteration,chain,loglik=Freq)) 
#Posterior distribution of the correlation parameter
d3 %>% 
  ggplot(aes(x=sigma_eta)) + 
  geom_histogram()
#Two chains with low loglikelihood
d3 %>%
  ggplot(aes(x=sigma_eta,y=loglik,col=chain)) +
  geom_point()+geom_line()



data_list=list(N=dim(df)[1]*2,
               N_x=dim(df)[1],
               x=rep(1:dim(df)[1],2),
               y=c(df$y1,df$y2),
               y_id = rep(1:2,each=dim(df)[1]),
               inference=1)
mod4 <- cmdstan_model(paste0(code_root_path,"stan/test4_correlated_poisson.stan"))
fit <- mod4$sample(
  adapt_delta=0.8,
  iter_warmup=1000,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)

#High rhat
fit$summary() %>% 
  arrange(-rhat)
#The correlation parameter is not well estimated
fit$summary(variables = c("Sigma","rho_chol","sigma","intercept"), "mean",~quantile(.x, probs = c(0.025, 0.975)))

################################################################################################################################################################
#Multivariate normal random effect on the mean parameter of Poisson distribution
library(Matrix)
library(MASS)

#Generate data
J=6
n=500
lambda = rep(100,J)
sigma = rep(0.5,J)
rho_m = matrix(NA,nrow=J,ncol=J)
rho_m <- matrix(runif(J^2, min = -J, max = J), J, J)
rho_m <- (rho_m + t(rho_m)) / 2  # Symmetrize
#force diagonal elements to 1 (correlation matrix property)
diag(rho_m) <- 1
#check positive definiteness and correct if necessary
rho_m <- nearPD(rho_m, corr = TRUE)$mat %>% as.matrix()# Check positive definiteness and correct if necessary
print(rho_m)
#data frame
df = MASS::mvrnorm(n, mu = rep(0,J), Sigma = rho_m *(sigma %*% t(sigma)))
df = data.frame(re = as.vector(df),
                outcome_id = rep(1:J,each=dim(df)[1]),
                x=rep(1:dim(df)[1],J)) %>% 
  left_join(data.frame(outcome_id = 1:J,
                       lambda=lambda),by="outcome_id") %>% 
  dplyr::mutate(mu = exp(log(lambda)+re)) %>% 
  rowwise() %>% 
  dplyr::mutate(y=rpois(1,mu)) %>% ungroup()

#data list
data_list=list(N=dim(df)[1],
               N_x=df$x %>% max(),
               J=df$outcome_id %>% max(),
               y_id = df$outcome_id,
               y=df$y,
               x=df$x,
               inference=1)
#Stan model
mod4 <- cmdstan_model(paste0(code_root_path,"stan/test4_correlated_poisson.stan"))
fit <- mod4$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
#Diagnostics
#High Rhat
fit$summary() %>% 
  arrange(-rhat)
#The correlation parameter is not well estimated
fit$summary(variables = c("sigma","intercept"), "mean",~quantile(.x, probs = c(0.025, 0.975)))


fit$summary(variables = c("Sigma"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
  tidyr::extract(variable,into=c("var","row_id","col_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
  dplyr::mutate(row_id=as.numeric(row_id),col_id=as.numeric(col_id)) %>% 
  left_join(data.frame(sim_value = as.vector(rho_m),
                       row_id = rep(1:J,J),
                       col_id = rep(1:J,each=J)),by=c("row_id","col_id")) %>% 
  ggplot(aes(x=col_id,y=mean,ymin=`2.5%`,ymax=`97.5%`))+
  geom_pointrange()+
  geom_point(aes(y=sim_value),shape=2,col="blue")+
  facet_grid(.~row_id)+
  theme_bw()

y_df = matrix(df$y,nrow= df$x %>% max(), ncol=df$outcome_id %>% max())
data_list=list(N_x=dim(y_df)[1],
               J=dim(y_df)[2],
               y=t(y_df),
               inference=1)
#Stan model
mod4 <- cmdstan_model(paste0(code_root_path,"stan/test4_correlated_poisson_opt.stan"))
fit <- mod4$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  show_messages = TRUE,#FALSE,
  refresh = 100, # print update every 500 iters
)
#Diagnostics
#High Rhat
fit$summary() %>% 
  arrange(-rhat)
#The correlation parameter is not well estimated
fit$summary(variables = c("sigma","intercept"), "mean",~quantile(.x, probs = c(0.025, 0.975)))


fit$summary(variables = c("Sigma"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
  tidyr::extract(variable,into=c("var","row_id","col_id"),
                 regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
  dplyr::mutate(row_id=as.numeric(row_id),col_id=as.numeric(col_id)) %>% 
  left_join(data.frame(sim_value = as.vector(rho_m),
                       row_id = rep(1:J,J),
                       col_id = rep(1:J,each=J)),by=c("row_id","col_id")) %>% 
  ggplot(aes(x=col_id,y=mean,ymin=`2.5%`,ymax=`97.5%`))+
  geom_pointrange()+
  geom_point(aes(y=sim_value),shape=2,col="blue")+
  facet_grid(.~row_id)+
  theme_bw()
