run_stan_mod5_by_cod = function(cod_agg_pop_df, age_class, run.model=TRUE,
                                    reg_var = c("sex"),reg_var_ref=c("M"),
                                    save.date){
  #name to save
  if(file.exists(paste0(code_root_path,"results/",save.date,"/mod5_stan_diag_",age_class,".RDS"))){
    stan_diag = readRDS(paste0(code_root_path,"results/",save.date,"/mod5_stan_diag_",age_class,".RDS"))
    print(stan_diag)
    return(NULL)
  }
  
  print("Compile stan")
  #mod4_cmdstan <- cmdstan_model("stan/mod4_GP_year_season.stan")
  mod5_cmdstan <- cmdstan_model(paste0(code_root_path,"stan/mod5_GP_year_season_causes.stan"))
  
  ###########################################################################
  #data
  print("Data")
  data = cod_agg_pop_df %>% 
    filter(age_class==.env$age_class,cod_group!="COVID-19") %>% 
    filter(cod_group %in% c("Cardiovascular Diseases","External Causes","Infectious and Parasitic Diseases",
                            "Mental and Neurological Disorders",
                            "Neoplasms (Cancers)","No Specific Causes", "Respiratory Diseases", "Suicide")) %>% 
    dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                  age_id = as.numeric(age_class),
                  cod_group_id = as.numeric(factor(cod_group)),
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
  X_reg_all = get_covariables_stan(data, reg_var,reg_var_ref)
  X_reg = get_covariables_stan(data_fit, reg_var,reg_var_ref)
  
  N = dim(data_fit)[1]
  N_all = dim(data)[1]
  N_cause = data$cod_group_id %>% unique() %>% length()
  x = data$week.id %>% unique()
  x_mean=mean(x)
  x_sd = sd(x)
  xn = (x-mean(x))/x_sd
  N_x = length(x)
  
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
    N_cause = N_cause,
    
    deaths = as.integer(data_fit$n),
    deaths_all = as.integer(data$n),
    cause_id = as.integer(data_fit$cod_group_id),
    cause_id_all = as.integer(data$cod_group_id),
    n_pop = structure(data_fit$n.pop,dim=N),
    n_pop_all = structure(data$n.pop,dim=N_all),
    
    X_reg = X_reg,
    X_reg_all = X_reg_all,
    
    week_id = structure(as.integer(data_fit$week.id),dim=N),
    week_id_all = structure(as.integer(data$week.id),dim=N_all),
    
    x = x,
    
    M_year = 20, 
    c_year = 5,
    J_week = 20,
    
    p_intercept = c(-10,2),
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
  initfun <- function() { list(lambda_week=structure(rlnorm(data_list$N_cause,data_list$p_lambda_week[1],data_list$p_lambda_week[2]),dim=data_list$N_cause),
                               lambda_year=structure(rlnorm(data_list$N_cause,data_list$p_lambda_year[1],data_list$p_lambda_year[2]),dim=data_list$N_cause)) }
  
  ###########################################################################
  #Model
  if(run.model){
    #run model in cmdstan
    print("Run Stan")
    fit5 <- mod5_cmdstan$sample(
      init=initfun,
      adapt_delta=0.99,
      data = data_list,
      chains = 4, 
      parallel_chains = 4,
      show_messages = TRUE,#FALSE,
      refresh = 100, # print update every 500 iters
    )
    
    if(fit5$diagnostic_summary()$num_divergent %>% length()<4){
      return(NULL)
    }
    
    stan_diag =  data.frame(time = fit5$time()$chains[,"total"] %>% max(),
                            num_successful_chains =  fit5$diagnostic_summary()$num_divergent %>% length(),
                            num_divergent = fit5$diagnostic_summary()$num_divergent %>% sum(),#fit$sampler_diagnostics()
                            num_max_treedepth = fit5$diagnostic_summary()$num_max_treedepth %>% sum(),
                            ebfmi = fit5$diagnostic_summary()$ebfmi %>% min()) %>% 
      dplyr::mutate(age_class=age_class) %>% 
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
  print(paste(c("divergence: ",fit5$sampler_diagnostics()[,,"divergent__"] %>% apply(2,sum)),sep=" "))
  
  # fit3$summary(variables = c("beta_reg","lambda_year","alpha_year","lambda_week","alpha_week","mu0"),
  #              "mean",~quantile(.x, probs = c(0.01,0.025, 0.5, 0.975,0.99)),"rhat", "ess_bulk", "ess_tail")
  if(file.exists(paste0(code_root_path,"results/",save.date,"/mod5_stan_diag_",age_class,".RDS")) &
     stan_diag$is.stan.ok){
  ###########################################################################
  #Deaths and excess mortality
  #number of deaths aggregated over sex, pandemic
  print("Produce estimate")
  #by week
  t0=Sys.time()
  data_pred_week_cause = aggregate_stan(data, fit5, cmdstan=TRUE,
                                  groups=c("cal_year","cal_week","age_class","cod_group","date")) %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
  data_pred_week = aggregate_stan(data, fit5, cmdstan=TRUE,
                                  groups=c("cal_year","cal_week","age_class","date")) %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
  t1=Sys.time()
  #by COVID-19 phase
  data_pred_phase_cause = aggregate_stan(data, fit5, cmdstan=TRUE,
                                   groups=c("covid_phase","age_class","cod_group")) %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
  data_pred_phase = aggregate_stan(data, fit5, cmdstan=TRUE,
                                   groups=c("covid_phase","age_class")) %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
  t2=Sys.time()
  #by year
  data_pred_year_cause = aggregate_stan(data, fit5, cmdstan=TRUE,
                                  groups=c("cal_year","age_class","cod_group")) %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class")) %>% 
    left_join(data %>% group_by(cal_year) %>%
                dplyr::summarise(n_week = length(unique(cal_week)),.groups="drop"),by="cal_year")
  data_pred_year = aggregate_stan(data, fit5, cmdstan=TRUE,
                                  groups=c("cal_year","age_class")) %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class")) %>% 
    left_join(data %>% group_by(cal_year) %>%
                dplyr::summarise(n_week = length(unique(cal_week)),.groups="drop"),by="cal_year")
  t3=Sys.time()
  
  ###########################################################################
  #sex
  reg_effect = fit5$summary(variables = c("beta_reg"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("var","cod_group_id","row_id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(row_id=as.numeric(row_id),
                  cod_group_id=as.numeric(cod_group_id)) %>% 
    left_join(data.frame(var = colnames(X_reg_all)) %>% dplyr::mutate(row_id = dplyr::row_number()) %>% 
                tidyr::separate(col = var, into = c("var", "ref", "level"), sep = "\\."),by="row_id") %>% 
    left_join(data %>% dplyr::select(cod_group_id,cod_group) %>% unique(),by="cod_group_id")
  
  #GP
  #periodic
  week_GP = fit5$summary(variables = c("f_week"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("var","cod_group_id","week.id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`,week.id,cod_group_id) %>%
    dplyr::mutate(age_class = age_class,
                  cod_group_id=as.numeric(cod_group_id),
                  week.id=as.numeric(week.id),
                  weeks_from_start = week.id %% (365.25/7),
                  corr_date = days_to_datetime_2020(weeks_from_start*7)) %>% arrange(weeks_from_start) %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class")) %>% 
    left_join(data %>% dplyr::select(cod_group_id,cod_group) %>% unique(),by="cod_group_id")
  #long term trend
  year_GP = fit5$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
    tidyr::extract(variable,into=c("var","cod_group_id","week.id"),
                   regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
    as_tibble() %>% 
    dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`,week.id,cod_group_id) %>%
    dplyr::mutate(age_class = age_class,
                  cod_group_id=as.numeric(cod_group_id),
                  week.id=as.numeric(week.id)) %>% 
    left_join(data %>% dplyr::select(week.id,date) %>% unique(),by="week.id") %>% 
    left_join(data %>% dplyr::select(cod_group_id,cod_group) %>% unique(),by="cod_group_id") %>% 
    left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
  t4=Sys.time()
  
  print(t4-t0)
  
  #save
  print("Save")
    saveRDS(stan_diag, file=paste0(code_root_path,"results/",save.date,"/mod5_stan_diag_",age_class,".RDS"))
    saveRDS(reg_effect, file=paste0(code_root_path,"results/",save.date,"/mod5_reg_effect_",age_class,".RDS"))
    
    saveRDS(data_pred_week, file=paste0(code_root_path,"results/",save.date,"/mod5_data_pred_week_",age_class,".RDS"))
    saveRDS(data_pred_phase, file=paste0(code_root_path,"results/",save.date,"/mod5_data_pred_phase_",age_class,".RDS"))
    saveRDS(data_pred_year, file=paste0(code_root_path,"results/",save.date,"/mod5_data_pred_year_",age_class,".RDS"))
    
    saveRDS(data_pred_week_cause, file=paste0(code_root_path,"results/",save.date,"/mod5_data_pred_week_cause_",age_class,".RDS"))
    saveRDS(data_pred_phase_cause, file=paste0(code_root_path,"results/",save.date,"/mod5_data_pred_phase_cause_",age_class,".RDS"))
    saveRDS(data_pred_year_cause, file=paste0(code_root_path,"results/",save.date,"/mod5_data_pred_year_cause_",age_class,".RDS"))
    
    saveRDS(year_GP, file=paste0(code_root_path,"results/",save.date,"/mod5_year_GP_",age_class,".RDS"))
    saveRDS(week_GP, file=paste0(code_root_path,"results/",save.date,"/mod5_week_GP_",age_class,".RDS"))
  }
  print(data_pred_phase)
  
  return(stan_diag)
}
