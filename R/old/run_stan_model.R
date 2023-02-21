run_stan_mod1_by_age_cause = function(data_all, age_classes, causes){
  
  mod1_GP_norm1 <- stan_model("mod1_GP_normalized.stan")
  mod1_GP_norm2 <- stan_model("mod1_GP_normalized2.stan")
  
  data_pred_fit=list()
  data_pred_pand=list()
  sample_deaths_pand_pred=list()
  excess=list()
  week_GP=list()
  year_GP=list()
  list_id=0
  for(j in 1:length(age_classes)){
    for(i in 1:length(causes)){
      list_id = list_id +1
      
      age_class_i = age_classes[j]
      cause_i = causes[i]
      print(cause_i)
      print(age_class_i)
      
      ###########################################################################################################
      #data
      data = data_all %>% 
        filter(cause==cause_i,age_class==age_class_i) %>% 
        dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                      year.id = year-min(year)+1,
                      date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")),
                      week.id = (as.numeric(date)-as.numeric(min(date)))/7)
      
      data %>% select(year,week,week.id,date) %>% View()
      data_fit = data %>% filter(year<2020)
      data_pand = data %>% filter(year>=2020)
      X_reg = get_covariables_stan(data_fit, c("sex"),c("M"))
      X_reg_pand = get_covariables_stan(data_pand, c("sex"),c("M"))
      
      N = dim(data_fit)[1]
      N_pand = dim(data_pand)[1]
      x = data$week.id %>% unique() %>% 
      N_x=dim()
      
      #data list
      data_list = list(
        N=N,
        N_week = data$week %>% unique() %>% length(),
        N_year = data$year.id %>% unique() %>% length(),
        N_reg = dim(X_reg)[2],
        N_pand = N_pand,
        
        deaths = as.integer(data_fit$n),
        n_pop = structure(data_fit$n.pop,dim=N),
        deaths_pand = as.integer(data_pand$n),
        n_pop_pand = structure(data_pand$n.pop,dim=N_pand),
        
        X_reg = X_reg,
        X_reg_pand = X_reg_pand,
        
        year = structure(as.integer(data_fit$year.id),dim=N),
        week = structure(as.integer(data_fit$week),dim=N),
        year_pand = structure(as.integer(data_pand$year.id),dim=N_pand),
        week_pand = structure(as.integer(data_pand$week),dim=N_pand),
        
        x_year = data$year.id %>% unique() %>% sort(),
        x_week= data$week %>% unique() %>% sort(),
        
        M_week = 15, 
        c_week = 1.5,
        M_year = 5, 
        c_year = 1.5,
        J_week = 30,
        
        p_lambda_week = c(2,5),
        
        inference=1
      )

      ###########################################################################################################
      #run model in rstan
      fit1 = sampling(mod1_GP_norm1, data_list,iter=1000,chains=4,cores=4,
                     control=list(adapt_delta=0.99))
      
      #run model in rstan
      fit2 = sampling(mod1_GP_norm2, data_list,iter=1000,chains=4,cores=4,
                      control=list(adapt_delta=0.99))
      
      
      ###########################################################################################################
      #number of deaths aggregated over sex
      data_pred_fit[[list_id]] = aggregate_fit_deaths(data_fit, fit) %>% 
        dplyr::mutate(cause=cause_i)
      data_pred_pand[[list_id]] = aggregate_pand_deaths(data_pand, fit) %>% 
        dplyr::mutate(cause=cause_i)
      
      #excess
      excess[[list_id]] = rstan::summary(fit,par=c("overall_excess","overall_rel_excess"))$summary %>% 
        as_tibble() %>% 
        dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
        dplyr::mutate(var = c("overall_excess","overall_rel_excess"),
                      cause = cause_i,
                      age_class = age_class_i)
      
      #samples of the overall predicted (i.e. expected) deaths during pandemic
      sample_deaths_pand_pred[[list_id]] = rstan::extract(fit,pars="overall_deaths_pand_pred") %>% 
        as.data.frame() %>%
        dplyr::mutate(cause=cause_i,
                      age_class = age_class_i)
      #week
      week_GP[[list_id]] = rstan::summary(fit,par=c("f_week"))$summary %>%
        tibble::as_tibble() %>% 
        dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
        dplyr::mutate(var =  "f_week",
                      week = data$week %>% unique() %>% sort(),
                      cause = cause_i,
                      age_class = age_class_i)
      
      year_GP[[list_id]] = rstan::summary(fit,par=c("f_year"))$summary %>%
        tibble::as_tibble() %>% 
        dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
        dplyr::mutate(var =  "f_year",
                      year = data$year %>% unique() %>% sort(),
                      cause = cause_i,
                      age_class = age_class_i)
      
      print(excess[[list_id]])
    }
  }
  
  #save
  saveRDS(do.call(rbind,data_pred_fit), file="results/data_pred_fit.RDS")
  saveRDS(do.call(rbind,data_pred_pand), file="results/data_pred_pand.RDS")
  saveRDS(do.call(rbind,sample_deaths_pand_pred), file="results/sample_deaths_pand_pred.RDS")
  saveRDS(do.call(rbind,year_GP), file="results/year_GP.RDS")
  saveRDS(do.call(rbind,week_GP), file="results/week_GP.RDS")
  saveRDS(do.call(rbind,excess), file="results/excess.RDS")
  
  return(do.call(rbind,data_pred_pand))
}


