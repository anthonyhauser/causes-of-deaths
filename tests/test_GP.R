source("R/000_setup.R")

#data
data_all = readRDS("data/d.RDS")
data = data_all %>% 
  filter(cause=="cardiovascular.dis",age_class=="0-39") %>% 
  dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                year.id = year-min(year)+1,
                date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")),
                week.id = (as.numeric(date)-as.numeric(min(date)))/7 + 1)
     
data_fit = data %>% filter(year<2020)
data_pand = data %>% filter(year>=2020)
X_reg = get_covariables_stan(data_fit, c("sex"),c("M"))
X_reg_pand = get_covariables_stan(data_pand, c("sex"),c("M"))

N = dim(data_fit)[1]
N_pand = dim(data_pand)[1]
x = data$week.id %>% unique()
x_mean=mean(x)
x_sd = sd(x)
xn = (x-mean(x))/x_sd
N_x = length(x)


###########################################################################
#Model 1: trend over years

#choose prior for lengthscale and tuning parameter c and m
l=rlnorm(100000,0,0.4)
hist(l)
mean(l)
tuning_parameter_cond_EQ(l,xn)
      
#data list
data_list = list(
  N=N,
  N_x = N_x,
  N_reg = dim(X_reg)[2],
  N_pand = N_pand,
  
  deaths = as.integer(data_fit$n),
  n_pop = structure(data_fit$n.pop,dim=N),
  deaths_pand = as.integer(data_pand$n),
  n_pop_pand = structure(data_pand$n.pop,dim=N_pand),
  
  X_reg = X_reg,
  X_reg_pand = X_reg_pand,
  
  week_id = structure(as.integer(data_fit$week.id),dim=N),
  week_id_pand = structure(as.integer(data_pand$week.id),dim=N_pand),
  
  x = x,
  
  M_year = 20, 
  c_year = 5,
  
  p_intercept = c(-8,2),
  p_alpha_year = c(0,0.1),
  p_lambda_year = c(0, 0.4),
  
  inference=0
)

#prior predictive check
mod1 <- stan_model("stan/mod3_GP_year.stan")
fit1_prior = sampling(mod1, data_list,iter=1000,chains=4,cores=4,
                     control=list(adapt_delta=0.99))

rstan::summary(fit1_prior,par=c("beta_reg","lambda_year","alpha_year","mu0"))$summary %>%
  as_tibble(rownames=NA)  %>% 
  dplyr::mutate(variable = rownames(.))

#inference
data_list$inference=1
fit1 = sampling(mod1, data_list,iter=1000,chains=4,cores=4,
                control=list(adapt_delta=0.99))

#plots
#shinystan::launch_shinystan(fit1)
rstan::summary(fit1,par=c("beta_reg","lambda_year","alpha_year","mu0"))$summary %>%
  as_tibble(rownames=NA)  %>% 
  dplyr::mutate(variable = rownames(.))

rstan::summary(fit1,par=c("f_year"))$summary %>%
  tibble::as_tibble() %>% 
  dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  dplyr::mutate(var =  "f_year",
                year = data$date %>% unique() %>% sort()) %>% 
  ggplot(aes(x=year,y=est)) +
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2) +
  geom_point()+
  geom_line()+
  theme_bw()

###########################################################################
#Model 2: trend over years and seasonality

#choose prior for lengthscale of periodic GP and tuning parameter m
l=rlnorm(100000,-0,0.4)
hist(l)
mean(l)
tuning_parameter_cond_periodic(l)

#data list
data_list = within(data_list,{
  p_intercept = c(-10,2)
  J_week = 20
  p_lambda_week = c(0,0.4)
  p_alpha_week = c(0,0.1)
  inference=1
})

#prior predictive check
# mod2 <- stan_model("stan/mod_GP_year_season.stan")
# fit2_prior = sampling(mod2, data_list,iter=1000,chains=4,cores=4,
#                      control=list(adapt_delta=0.99))

initfun <- function() { list(lambda_week=rlnorm(1,data_list$p_lambda_week[1],data_list$p_lambda_week[2]),
                             lambda_year=rlnorm(1,data_list$p_lambda_year[1],data_list$p_lambda_year[2])) }

mod2_cmdstan <- cmdstan_model("stan/mod3_GP_year_season.stan")
fit2 <- mod2_cmdstan$sample(
  init=initfun,
  adapt_delta=0.99,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

fit2$sampler_diagnostics()[,,"divergent__"] %>% apply(2,sum)

fit2$cmdstan_diagnose()

stanfit <- rstan::read_stan_csv(fit2$output_files())
library(shinystan)
launch_shinystan(stanfit)

fit2$summary(variables = c("beta_reg","lambda_year","alpha_year","lambda_week","alpha_week","mu0"),
             "mean",~quantile(.x, probs = c(0.01,0.025, 0.5, 0.975,0.99)),"rhat", "ess_bulk", "ess_tail")

shinystan::launch_shinystan(fit2_prior)

rstan::summary(fit2_prior,par=c("beta_reg","lambda_year","alpha_year","lambda_week","alpha_week","mu0"))$summary %>%
  as_tibble(rownames=NA)  %>% 
  dplyr::mutate(variable = rownames(.))

d=extract(fit2_prior) %>% as.data.frame()


rstan::summary(fit2,par=c("f_year"))$summary
fit2$summary(variables="f_year","mean",~quantile(.x, probs = c(0.025, 0.5, 0.975))) %>%
  tibble::as_tibble() %>% 
  dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  dplyr::mutate(var =  "f_year",
                year = data$date %>% unique() %>% sort()) %>% 
  ggplot(aes(x=year,y=est)) +
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2) +
  geom_point()+
  geom_line()+
  theme_bw()

#rstan::summary(fit2,par=c("f_week"))$summary %>%
fit2$summary(variables="f_week","mean",~quantile(.x, probs = c(0.025, 0.5, 0.975))) %>%
  tibble::as_tibble() %>% 
  dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  dplyr::mutate(var =  "f_week",
                year = data$date %>% unique() %>% sort()) %>% 
  filter(year(year)>=2019) %>% 
  ggplot(aes(x=year,y=est)) +
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.2) +
  geom_point()+
  geom_line()+
  theme_bw()
