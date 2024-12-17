run_stan_mod8_by_cod = function(cod_agg_pop_nuts_df, age_class, run.model=TRUE,
                                save.date){
  #name to save
  if(file.exists(paste0(code_root_path,"results/",save.date,"/mod8_stan_diag_",age_class,".RDS"))){
    stan_diag = readRDS(paste0(code_root_path,"results/",save.date,"/mod8_stan_diag_",age_class,".RDS"))
    print(stan_diag)
    return(NULL)
  }
  
  print("Compile stan")
  mod8_cmdstan <- cmdstan_model(paste0(code_root_path,"stan/mod8_GP_year_season_causes.stan"))
  #mod8_cmdstan <- cmdstan_model(paste0("stan/mod8_GP_year_season_causes.stan"))
  
  causes = c("Cardiovascular Diseases","Respiratory Diseases", "Mental and Neurological Disorders",
             "Infectious and Parasitic Diseases",
             "Neoplasms (Cancers)","Suicide","External Causes",
             "Other Causes")
  
  ###########################################################################
  #data
  print("Data")
  data = cod_agg_pop_nuts_df %>% 
    filter(age_class==.env$age_class) %>% #cod_group!="COVID-19") %>% 
    filter(cod_group %in% c(causes,"COVID-19")) %>% 
    dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                  sex_id = as.numeric(sex),
                  age_id = as.numeric(age_class),
                  cod_group_id = as.numeric(factor(cod_group,levels=c(causes,"COVID-19"))),
                  #year.id = cal_year-min(cal_year)+1,
                  date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                  week.id = as.numeric(1+(date-min(date))/7)) %>% #week.id = dense_rank(date)) %>% 
    mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
      phase <- phases %>%
        filter(d >= start_date & d <= end_date) %>%
        pull(phase)
      if (length(phase) == 0) NA_real_ else phase
    })) %>% 
    arrange(cod_group_id,week.id)
  
  #aggregate
  data = data %>%
    group_by(age_class,cal_year,cal_week,cod_group,age_id,sex_id,NUTS2_id,
             cod_group_id,date,week.id,covid_phase) %>% 
    dplyr::summarise(n=sum(n),
                     n.pop=sum(n.pop)) %>% ungroup() %>% 
    arrange(cod_group_id,sex_id,NUTS2_id,week.id)
  
  data_all_causes = data
  data = data_all_causes %>% 
    filter(cod_group!="COVID-19")
  
  data_fit = data %>% filter(cal_year<2020)
  #X_reg_all = get_covariables_stan(data %>% filter(cod_group_id==1), reg_var,reg_var_ref)
  #X_reg = get_covariables_stan(data_fit %>% filter(cod_group_id==1), reg_var,reg_var_ref)
  x = data_fit$week.id %>% unique()
  x_all = data$week.id %>% unique()
  
  deaths_df = xtabs(n ~ cod_group_id + sex_id + NUTS2_id + week.id, data = data)
  deaths_fit_df = xtabs(n ~ cod_group_id + sex_id + NUTS2_id + week.id, data = data_fit)
  n_pop_all_df = xtabs(n.pop ~ sex_id + NUTS2_id + week.id,
                       data = data %>% filter(cod_group_id==1))
  n_pop_df =  xtabs(n.pop ~ sex_id + NUTS2_id + week.id,
                    data = data_fit %>% filter(cod_group_id==1))
  
  #prior mu0
  cod_agg_pop_nuts_df %>% 
    group_by(age_class,cod_group) %>% 
    dplyr::summarise(mu0=log(sum(n)/sum(n.pop)),.groups = "drop_last") %>% 
    dplyr::summarise(median=median(mu0),min=min(mu0),max=max(mu0))
  p_mean_mu0 = c("0-17"=-14, "18-39"=-13, "40-64"=-11, "65-79"=-9, "80+"=-7)[age_class]
  
  data_list=list(N = dim(deaths_fit_df)[4],
                 N_all = dim(deaths_df)[4],
                 N_cause = dim(deaths_fit_df)[1],
                 N_sex = dim(deaths_fit_df)[2],
                 N_region = dim(deaths_fit_df)[3],
                 deaths = deaths_fit_df,
                 deaths_all = deaths_df,
                 
                 n_pop = n_pop_df,
                 n_pop_all = n_pop_all_df,
                 x_all = x_all,
                 
                 M_year = 20, 
                 c_year = 5,
                 J_week = 20,
                 
                 p_intercept = c(p_mean_mu0,2),
                 p_sigma = c(rep(0.5,dim(deaths_fit_df)[2]),5),
                 p_alpha_year = c(0,0.1),
                 p_lambda_year = c(0.5, 0.5),#c(0,0.4)
                 p_lambda_week = c(1,0.4),#c(0.8,0.4),
                 p_alpha_week = c(0,0.1),
                 
                 inference=1)
  
  #lengthscale for long-term trend
  l=rlnorm(100000,0.5,0.5)
  hist(l)
  mean(l)
  quantile(l,probs=c(0.025,0.5,0.975))
  tuning_parameter_cond_EQ(l,(x-mean(x))/sd(x),sd(x))
  
  data.frame(x=rlnorm(100000,meanlog=data_list$p_lambda_week[1],sdlog=data_list$p_lambda_week[2])) %>% 
    dplyr::mutate(mean=mean(x),median=median(x)) %>% 
    ggplot(aes(x=x)) +
    geom_histogram()+
    geom_vline(aes(xintercept=mean),col="orange")+
    geom_vline(aes(xintercept=median),col="red")+
    xlim(c(0,20))
  
  #init function
  data_list$p_intercept
  initfun <- function() { list(sigma = structure(abs(rnorm(data_list$N_cause,0,0.5)),dim=data_list$N_cause),
                               mu0 = structure(rnorm(data_list$N_cause,data_list$p_intercept[1],data_list$p_intercept[2]),dim=data_list$N_cause),
                               lambda_week=structure(rlnorm(data_list$N_cause,data_list$p_lambda_week[1],data_list$p_lambda_week[2]),dim=data_list$N_cause),
                               lambda_year=structure(rlnorm(data_list$N_cause,data_list$p_lambda_year[1],data_list$p_lambda_year[2]),dim=data_list$N_cause)) }
  
  ###########################################################################
  #Model
  if(run.model){
    #run model in rstan
    if(FALSE){
      library(rstan)
      mod8_rstan <- stan_model(paste0(code_root_path,"stan/mod8_GP_year_season_causes.stan"))
      fit8_rstan <- rstan::sampling(
        mod8_rstan,
        cores = getOption("mc.cores", 4L),
        data = data_list,
        iter = 1000,             # Number of iterations
        chains = 4,              # Number of chains
        init = initfun,          # Use the defined init function
        control = list(adapt_delta = 0.99)  # Control settings
      )
    }
    #run model in cmdstan
    print("Run Stan")
    fit8 <- mod8_cmdstan$sample(
      init=initfun,
      iter_sampling=1000,
      iter_warmup =1000,
      adapt_delta=0.99,
      data = data_list,
      chains = 4,
      parallel_chains = 4,
      show_messages = TRUE,#FALSE,
      refresh = 100 # print update every 500 iters
    )
    
    # Chain 3 Exception initializing step size.
    # Chain 3 Exception: Exception: Error in function boost::math::bessel_ik<long double>(long double,long double) in CF2_ik:
    #   Series evaluation exceeded 1000000 iterations, giving up now.
    # (in 'C:/Users/an4818/AppData/Local/Temp/RtmpaA8fsT/model-581429403a41.stan', line 21, column 4 to column 98)
    # (in 'C:/Users/an4818/AppData/Local/Temp/RtmpaA8fsT/model-581429403a41.stan', line 107, column 4 to column 78)
    
    chains = fit8$time()$chains %>% filter(!is.na(total)) %>% pull(chain_id) %>% .[1:4]
    print("Stan summary")
    stan_diag =  data.frame(time = fit8$time()$chains %>% filter(!is.na(total)) %>% pull(total) %>% max(),
                            num_successful_chains =  fit8$diagnostic_summary()$num_divergent %>% length(),
                            num_divergent = fit8$diagnostic_summary()$num_divergent %>% sum(),#fit$sampler_diagnostics()
                            num_max_treedepth = fit8$diagnostic_summary()$num_max_treedepth %>% sum(),
                            ebfmi = fit8$diagnostic_summary()$ebfmi %>% min(),
                            rhat = fit8$summary() %>% filter(!is.na(rhat)) %>% pull(rhat) %>% max()) %>% 
      dplyr::mutate(age_class=age_class) %>% 
      dplyr::mutate(is.stan.ok = num_successful_chains>=4 & num_divergent==0 & ebfmi>=0.3 & rhat<1.1)
    print(stan_diag)
    
    temp_rds_file <- paste0(code_root_path,"/results/",save.date,"/","mod8_",age_class,"_","fit.RDS")
    fit8$save_object(file = temp_rds_file)
    #fit8 <- readRDS(temp_rds_file)
    
    if(!stan_diag$is.stan.ok){
      return(NULL)
    }
    
    #Save the chains
    # fit3$save_output_files(dir = paste0(code_root_path,"/results/stan_chains"),
    #                        basename = paste0("chain_mod3","_",age_class,"_",cause))
  }else{
    #load csv files
    #files = list.files(paste0(code_root_path,"/results/stan_chains"))
    #csv_files= files[grepl(gsub("\\+","\\\\+",paste0("chain_mod3","_",age_class,"_",cause)),files)]
    #fit3 <-as_cmdstan_fit(paste0(code_root_path,"/results/stan_chains/",csv_files)[1:4])
  }
  
    if(!file.exists(paste0(code_root_path,"results/",save.date,"/mod8_stan_diag_",age_class,".RDS")) &
     stan_diag$is.stan.ok){
    ###########################################################################
    #Deaths and excess mortality
    #number of deaths aggregated over sex, pandemic
    print("Produce estimate")
    #by week
    t0=Sys.time()
    data_pred_week_cause = aggregate_stan_mod6(data, fit8, cmdstan=TRUE,
                                               groups=c("cal_year","cal_week","age_class","cod_group","date"),chains=chains) %>% 
      left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
    data_pred_week = aggregate_stan_mod6(data, fit8, cmdstan=TRUE,
                                         groups=c("cal_year","cal_week","age_class","date"),chains=chains) %>% 
      left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
    t1=Sys.time()
    print(t1-t0)
    #by COVID-19 phase
    data_pred_phase_cause = aggregate_stan_mod6(data, fit8, cmdstan=TRUE,
                                                groups=c("covid_phase","age_class","cod_group"),chains=chains) %>% 
      left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
    data_pred_phase = aggregate_stan_mod6(data, fit8, cmdstan=TRUE,
                                          groups=c("covid_phase","age_class"),chains=chains) %>% 
      left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
    t2=Sys.time()
    print(t2-t1)
    #by year
    data_pred_year_cause = aggregate_stan_mod6(data, fit8, cmdstan=TRUE,
                                               groups=c("cal_year","age_class","cod_group"),chains=chains) %>% 
      left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class")) %>% 
      left_join(data %>% group_by(cal_year) %>%
                  dplyr::summarise(n_week = length(unique(cal_week)),.groups="drop"),by="cal_year")
    data_pred_year = aggregate_stan_mod6(data, fit8, cmdstan=TRUE,
                                         groups=c("cal_year","age_class"),chains=chains) %>% 
      left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class")) %>% 
      left_join(data %>% group_by(cal_year) %>%
                  dplyr::summarise(n_week = length(unique(cal_week)),.groups="drop"),by="cal_year")
    t3=Sys.time()
    print(t3-t2)
    ###########################################################################
    #sex
    sex_effect = fit8$summary(variables = c("beta_sex"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>%
      tidyr::extract(variable,into=c("var","cod_group_id","sex_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>%
      dplyr::mutate(sex_id=as.numeric(sex_id),
                    cod_group_id=as.numeric(cod_group_id)) %>%
      left_join(data %>% dplyr::select(cod_group_id,cod_group) %>% unique(),by="cod_group_id") %>% 
      left_join(data %>% dplyr::select(sex_id,sex) %>% unique(),by="sex_id")
    print(sex_effect)
    #region
    nuts_effect = fit8$summary(variables = c("beta_region"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>%
      tidyr::extract(variable,into=c("var","cod_group_id","NUTS2_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>%
      dplyr::mutate(NUTS2_id=as.numeric(NUTS2_id),
                    cod_group_id=as.numeric(cod_group_id)) %>%
      left_join(data %>% dplyr::select(cod_group_id,cod_group) %>% unique(),by="cod_group_id") %>% 
      left_join(data %>% dplyr::select(NUTS2_id,NUTS2_name) %>% unique(),by="NUTS2_id")
    print(nuts_effect)
    #Correlation
    sigma = fit8$summary(variables = c("sigma"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      #summary_cmdstanr(fit=fit8, variable = "sigma",chains=chains) %>% #needed if we wants to select some chains (successful chains)
      tidyr::extract(variable,into=c("var","cod_group_id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(cod_group_id=as.numeric(cod_group_id)) %>% 
      dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`,cod_group_id) %>%
      left_join(data_all_causes %>% dplyr::select(cod_group,cod_group_id) %>% unique(),by=c("cod_group_id"))
    Sigma_mat = fit8$summary(variables = c("Sigma"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      #summary_cmdstanr(fit=fit8, variable = "Sigma",chains=chains) %>% 
      tidyr::extract(variable,into=c("var","cod_group_id","cod_group_id2"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(cod_group_id=as.numeric(cod_group_id),
                    cod_group_id2=as.numeric(cod_group_id2)) %>% 
      dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`,cod_group_id,cod_group_id2) %>%
      left_join(data_all_causes %>% dplyr::select(cod_group,cod_group_id) %>% unique(),by=c("cod_group_id")) %>% 
      left_join(data_all_causes %>% dplyr::select(cod_group2=cod_group,cod_group_id2=cod_group_id) %>% unique(),by=c("cod_group_id2"))
    
    #GP
    #periodic
    week_GP = fit8$summary(variables = c("f_week"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      #summary_cmdstanr(fit=fit8, variable = "f_week",chains=chains) %>% 
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
      left_join(data_all_causes %>% dplyr::select(cod_group_id,cod_group) %>% unique(),by="cod_group_id")
    
    #long term trend
    year_GP = fit8$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
      #summary_cmdstanr(fit=fit8, variable = "f_year",chains=chains) %>% 
      tidyr::extract(variable,into=c("var","cod_group_id","week.id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      as_tibble() %>% 
      dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`,week.id,cod_group_id) %>%
      dplyr::mutate(age_class = age_class,
                    cod_group_id=as.numeric(cod_group_id),
                    week.id=as.numeric(week.id)) %>% 
      left_join(data %>% dplyr::select(week.id,date) %>% unique(),by="week.id") %>% 
      left_join(data_all_causes %>% dplyr::select(cod_group_id,cod_group) %>% unique(),by="cod_group_id") %>% 
      left_join(stan_diag %>% dplyr::select(age_class, is.stan.ok),by=c("age_class"))
    t4=Sys.time()
    print(t4-t1)
    print(t4-t0)
    
    #deaths_covid_est
    deaths_covid_est = fit8$summary(variables = c("deaths_covid_est")) %>% 
      tidyr::extract(variable,into=c("var","cod_group_id","week.id"),
                     regex =paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
      dplyr::mutate(week.id =  data_list$N + as.numeric(week.id),
                    cod_group_id=as.numeric(cod_group_id)) %>% 
      left_join(data_covid,by=c("cod_group_id","week.id")) 
    
    #save
    if(!file.exists(paste0(code_root_path,"results/",save.date,"/mod8_stan_diag_",age_class,".RDS"))){#check again in case another node in the cluster save results
      print("Save")
      saveRDS(stan_diag, file=paste0(code_root_path,"results/",save.date,"/mod8_stan_diag_",age_class,".RDS"))
      saveRDS(sex_effect, file=paste0(code_root_path,"results/",save.date,"/mod8_sex_effect_",age_class,".RDS"))
      saveRDS(nuts_effect, file=paste0(code_root_path,"results/",save.date,"/mod8_nuts_effect_",age_class,".RDS"))
      
      saveRDS(data_pred_week, file=paste0(code_root_path,"results/",save.date,"/mod8_data_pred_week_",age_class,".RDS"))
      saveRDS(data_pred_phase, file=paste0(code_root_path,"results/",save.date,"/mod8_data_pred_phase_",age_class,".RDS"))
      saveRDS(data_pred_year, file=paste0(code_root_path,"results/",save.date,"/mod8_data_pred_year_",age_class,".RDS"))
      
      saveRDS(data_pred_week_cause, file=paste0(code_root_path,"results/",save.date,"/mod8_data_pred_week_cause_",age_class,".RDS"))
      saveRDS(data_pred_phase_cause, file=paste0(code_root_path,"results/",save.date,"/mod8_data_pred_phase_cause_",age_class,".RDS"))
      saveRDS(data_pred_year_cause, file=paste0(code_root_path,"results/",save.date,"/mod8_data_pred_year_cause_",age_class,".RDS"))
      
      saveRDS(sigma, file=paste0(code_root_path,"results/",save.date,"/mod8_sigma_",age_class,".RDS"))
      saveRDS(Sigma_mat, file=paste0(code_root_path,"results/",save.date,"/mod8_Sigma_mat_",age_class,".RDS"))
      
      saveRDS(year_GP, file=paste0(code_root_path,"results/",save.date,"/mod8_year_GP_",age_class,".RDS"))
      saveRDS(week_GP, file=paste0(code_root_path,"results/",save.date,"/mod8_week_GP_",age_class,".RDS"))
      
      saveRDS(deaths_covid_est, file=paste0(code_root_path,"results/",save.date,"/mod8_deaths_covid_est_",age_class,".RDS"))
    }
  }
  print(data_pred_phase)
  
  return(stan_diag)
}
