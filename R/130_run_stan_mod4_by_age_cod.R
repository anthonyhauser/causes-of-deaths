run_stan_mod4 = function(cod_agg_pop_df, age_class, cause, run.model=TRUE,
                                    reg_var = c("sex"),reg_var_ref=c("M"),
                                    save.date){
  #name to save
  name_cod_age=paste0(cod_df %>% filter(cod_full==cause) %>% pull(cod_1word),"_",age_class)
  if(file.exists(paste0(code_root_path,"results/",save.date,"/mod4_stan_diag_",name_cod_age,".RDS"))){
    stan_diag = readRDS(paste0(code_root_path,"results/",save.date,"/mod4_stan_diag_",name_cod_age,".RDS"))
    print(stan_diag)
    return(NULL)
  }
  
  print("Compile stan")
  #mod4_cmdstan <- cmdstan_model("stan/mod4_GP_year_season.stan")
  mod4_cmdstan <- cmdstan_model(paste0(code_root_path,"stan/mod4_GP_year_season.stan"))
 
  ###########################################################################
  #data
  print("Data")
  data = cod_agg_pop_df %>% 
    filter(cod_group==cause,age_class==.env$age_class) %>% 
    dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                  #year.id = cal_year-min(cal_year)+1,
                  date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                  week.id = as.numeric(1+(date-min(date))/7)) %>% #week.id = dense_rank(date)) %>% 
    mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
      phase <- phases %>%
        filter(d >= start_date & d <= end_date) %>%
        pull(phase)
      if (length(phase) == 0) NA_real_ else phase
    }))
  
  data_fit = data %>% filter(cal_year<2020)
  data_pand = data %>% filter(cal_year>=2020)
  X_reg_all = get_covariables_stan(data, reg_var,reg_var_ref)
  X_reg = get_covariables_stan(data_fit, reg_var,reg_var_ref)
  X_reg_pand = get_covariables_stan(data_pand, reg_var,reg_var_ref)
  
  N = dim(data_fit)[1]
  N_pand = dim(data_pand)[1]
  N_all = N+N_pand
  x = data$week.id %>% unique()
  x_mean=mean(x)
  x_sd = sd(x)
  xn = (x-mean(x))/x_sd
  N_x = length(x)
  
  p_mean_mu0 = c("0-17"=-15, "18-39"=-14, "40-64"=-12, "65-79"=-11, "80+"=-9)[age_class]
  
  #lengthscale for long-term trend
  l=rlnorm(100000,0.5,0.5)
  hist(l)
  mean(l)
  quantile(l,probs=c(0.025,0.5,0.975))
  tuning_parameter_cond_EQ(l,xn,x_sd)
  
  data_list = list(
    N_all = N_all,
    N=N,
    N_x = N_x,
    N_reg = dim(X_reg)[2],
    N_pand = N_pand,
    
    deaths = as.integer(data_fit$n),
    deaths_pand = as.integer(data_pand$n),
    deaths_all = as.integer(data$n),
    n_pop = structure(data_fit$n.pop,dim=N),
    n_pop_pand = structure(data_pand$n.pop,dim=N_pand),
    n_pop_all = structure(data$n.pop,dim=N_all),
    
    X_reg = X_reg,
    X_reg_pand = X_reg_pand,
    X_reg_all = X_reg_all,
    
    week_id = structure(as.integer(data_fit$week.id),dim=N),
    week_id_pand = structure(as.integer(data_pand$week.id),dim=N_pand),
    week_id_all = structure(as.integer(data$week.id),dim=N_all),
    
    x = x,
    
    M_year = 20, 
    c_year = 5,
    J_week = 20,
    
    p_intercept = c(p_mean_mu0,2),
    p_alpha_year = c(0,0.1),
    p_lambda_year = c(0.5, 0.5),#c(0,0.4)
    p_lambda_week = c(1,0.4),#c(0.8,0.4),
    p_alpha_week = c(0,0.1),
    
    inference=1
  )
  
  data.frame(x=rlnorm(100000,meanlog=data_list$p_lambda_week[1],sdlog=data_list$p_lambda_week[2])) %>% 
    dplyr::mutate(mean=mean(x),median=median(x)) %>% 
    ggplot(aes(x=x)) +
    geom_histogram()+
    geom_vline(aes(xintercept=mean),col="orange")+
    geom_vline(aes(xintercept=median),col="red")+
    xlim(c(0,20))
  
  #init function
  initfun <- function() { list(lambda_week=rlnorm(1,data_list$p_lambda_week[1],data_list$p_lambda_week[2]),
                               lambda_year=rlnorm(1,data_list$p_lambda_year[1],data_list$p_lambda_year[2])) }
  
  ###########################################################################
  #Model
  if(run.model){
    #run model in cmdstan
    print("Run Stan")
    fit4 <- mod4_cmdstan$sample(
      init=initfun,
      adapt_delta=0.99,
      data = data_list,
      chains = 4, 
      parallel_chains = 4,
      show_messages = TRUE,#FALSE,
      refresh = 500, # print update every 500 iters
    )
    
    if(fit4$diagnostic_summary()$num_divergent %>% length()<4){
      return(NULL)
    }
    
    stan_diag =  data.frame(time = fit4$time()$chains[,"total"] %>% max(),
                            num_successful_chains =  fit4$diagnostic_summary()$num_divergent %>% length(),
                            num_divergent = fit4$diagnostic_summary()$num_divergent %>% sum(),#fit$sampler_diagnostics()
                            num_max_treedepth = fit4$diagnostic_summary()$num_max_treedepth %>% sum(),
                            ebfmi = fit4$diagnostic_summary()$ebfmi %>% min()) %>% 
      dplyr::mutate(cod_group=cause,
                    age_class=age_class) %>% 
      left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
      dplyr::mutate(is.stan.ok = num_successful_chains==4 & num_divergent==0 & ebfmi>0.2)
    
    #Save the chains
    # fit3$save_output_files(dir = paste0(code_root_path,"/results/stan_chains"),
    #                        basename = paste0("chain_mod3","_",age_class,"_",cause))
  }else{
    #load csv files
    #files = list.files(paste0(code_root_path,"/results/stan_chains"))
    #csv_files= files[grepl(gsub("\\+","\\\\+",paste0("chain_mod3","_",age_class,"_",cause)),files)]
    #fit3 <-as_cmdstan_fit(paste0(code_root_path,"/results/stan_chains/",csv_files)[1:4])
  }
  
  #divergence
  print(paste(c("divergence: ",fit4$sampler_diagnostics()[,,"divergent__"] %>% apply(2,sum)),sep=" "))
  
  # fit3$summary(variables = c("beta_reg","lambda_year","alpha_year","lambda_week","alpha_week","mu0"),
  #              "mean",~quantile(.x, probs = c(0.01,0.025, 0.5, 0.975,0.99)),"rhat", "ess_bulk", "ess_tail")
  ###########################################################################
  #Deaths and excess mortality
  #number of deaths aggregated over sex, pandemic
  print("Produce estimate")
  #by week
  t0=Sys.time()
  data_pred_week = aggregate_stan(data, fit4, cmdstan=TRUE,
                                            groups=c("cal_year","cal_week","age_class","date")) %>% 
    dplyr::mutate(cod_group=cause) %>% 
    left_join(stan_diag %>% dplyr::select(cod_group,cod_1word, age_class, is.stan.ok),by=c("cod_group","age_class"))
  t1=Sys.time()
  #by COVID-19 phase
  data_pred_phase = aggregate_stan(data, fit4, cmdstan=TRUE,
                                             groups=c("covid_phase","age_class")) %>% 
    dplyr::mutate(cod_group=cause) %>% 
    left_join(stan_diag %>% dplyr::select(cod_group,cod_1word, age_class, is.stan.ok),by=c("cod_group","age_class"))
  t2=Sys.time()
  #by year
  data_pred_year = aggregate_stan(data, fit4, cmdstan=TRUE,
                                            groups=c("cal_year","age_class")) %>% 
    dplyr::mutate(cod_group=cause) %>% 
    left_join(stan_diag %>% dplyr::select(cod_group,cod_1word, age_class, is.stan.ok),by=c("cod_group","age_class")) %>% 
    left_join(data %>% group_by(cal_year) %>%
                dplyr::summarise(n_week = length(unique(cal_week)),.groups="drop"),by="cal_year")
  t3=Sys.time()

  ###########################################################################
  #sex
  reg_effect = fit4$summary(variables = c("beta_reg"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    dplyr::mutate(var = colnames(X_reg_all)) %>% 
    tidyr::separate(col = var, into = c("var", "ref", "level"), sep = "\\.")
  
  #GP
  #periodic
  week_GP = fit4$summary(variables = c("f_week"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("var","week.id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`,week.id) %>%
    dplyr::mutate(cod_group = cause,
                  age_class = age_class,
                  week.id=as.numeric(week.id),
                  weeks_from_start = week.id %% (365.25/7),
                  corr_date = days_to_datetime_2020(weeks_from_start*7)) %>% arrange(weeks_from_start) %>% 
    left_join(stan_diag %>% dplyr::select(cod_group,cod_1word, age_class, is.stan.ok),by=c("cod_group","age_class"))
  #long term trend
  year_GP = fit4$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("var","week.id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`,week.id) %>%
    dplyr::mutate(cod_group = cause,
                  age_class = age_class,
                  week.id=as.numeric(week.id)) %>% 
    left_join(data %>% dplyr::select(week.id,date) %>% unique(),by="week.id") %>% 
    left_join(stan_diag %>% dplyr::select(cod_group,cod_1word, age_class, is.stan.ok),by=c("cod_group","age_class"))
  t4=Sys.time()
  
  print(t4-t0)
  
  #save
  print("Save")
  saveRDS(stan_diag, file=paste0(code_root_path,"results/",save.date,"/mod4_stan_diag_",name_cod_age,".RDS"))
  saveRDS(reg_effect, file=paste0(code_root_path,"results/",save.date,"/mod4_reg_effect_",name_cod_age,".RDS"))
  saveRDS(data_pred_week, file=paste0(code_root_path,"results/",save.date,"/mod4_data_pred_week_",name_cod_age,".RDS"))
  saveRDS(data_pred_phase, file=paste0(code_root_path,"results/",save.date,"/mod4_data_pred_phase_",name_cod_age,".RDS"))
  saveRDS(data_pred_year, file=paste0(code_root_path,"results/",save.date,"/mod4_data_pred_year_",name_cod_age,".RDS"))
  
  saveRDS(year_GP, file=paste0(code_root_path,"results/",save.date,"/mod4_year_GP_",name_cod_age,".RDS"))
  saveRDS(week_GP, file=paste0(code_root_path,"results/",save.date,"/mod4_week_GP_",name_cod_age,".RDS"))
  
  print(data_pred_phase)
  
  return(stan_diag)
}
