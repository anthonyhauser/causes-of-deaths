run_stan_mod2_by_age_cause = function(data_all, age_classes, causes){
  
  mod2_GP_norm <- stan_model("mod2_GP_normalized.stan")
  #mod3 <- stan_model("mod3.stan")
  
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
      
      #data
      data = data_all %>% 
        filter(cause==cause_i,age_class==age_class_i) %>% 
        dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                      year.id = year-min(year)+1,
                      date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")),
                      week.id = (as.numeric(date)-as.numeric(min(date)))/7 + 1)
      
      data %>% select(year,week,week.id,date) %>% View()
      data_fit = data %>% filter(year<2020)
      data_pand = data %>% filter(year>=2020)
      X_reg = get_covariables_stan(data_fit, c("sex"),c("M"))
      X_reg_pand = get_covariables_stan(data_pand, c("sex"),c("M"))
      
      N = dim(data_fit)[1]
      N_pand = dim(data_pand)[1]
      x = data$week.id %>% unique()
      N_x = length(x)
      
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
        c_year = 1.5,
        J_week = 50,
        
        p_lambda_week = c(2,5), 
        
        inference=1
      )
      
      data %>% 
        ggplot(aes(x=date,y=n,col=sex)) +
        geom_point()

      
      #run model in rstan
      fit1 = sampling(mod2_GP_norm, data_list,iter=1000,chains=4,cores=4,
                      control=list(adapt_delta=0.99))
      # fit2 = sampling(mod3, data_list,iter=1000,chains=4,cores=4,
      #                control=list(adapt_delta=0.99))
      fit = fit1
      
      
      rstan::summary(fit1,par=c("beta_reg","lambda_week","alpha_week","lambda_year","alpha_year","mu0"))$summary %>%
        as_tibble(rownames=NA)  %>% 
        dplyr::mutate(variable = rownames(.))
      
      
      S=max((x-mean(x))/sd(x))
      rstan::extract(fit,pars=c("lambda_week","lambda_year")) %>% 
        as.data.frame() %>% 
        pivot_longer(cols=everything(),names_to = "par",values_to ="lengthscale") %>% 
        dplyr::mutate(lengthscale = ifelse(par=="lambda_year",lengthscale/S,lengthscale)) %>% 
        left_join(data.frame(par=c("lambda_week","lambda_year"),
                             min.lengthscale = c(3.72/data_list$J_week,
                                                 1.75*data_list$c_year/data_list$M_year),
                             min.lengthscale2 = c(3.72/data_list$J_week,
                                                 1.75*data_list$c_year/3.2)),
                  by="par") %>% 
        dplyr::mutate(is.tuning.ok = as.numeric(lengthscale>min.lengthscale),
                      is.tuning.ok2 = ifelse(par=="lambda_year",as.numeric(data_list$c_year>=3.2*lengthscale),NA)) %>% 
        group_by(par,min.lengthscale) %>% 
        dplyr::summarise(p.tuning.ok = sum(is.tuning.ok)/n(),
                         p.tuning.ok2 = sum(is.tuning.ok2)/n(),
                         med.lengthscale = median(lengthscale),.groups="drop")
        
        
        S=max((x-mean(x))/sd(x))
        rstan::extract(fit,pars=c("lambda_week","lambda_year")) %>% 
          as.data.frame() %>% 
          pivot_longer(cols=everything(),names_to = "par",values_to ="lengthscale") %>% 
          dplyr::mutate(lengthscale = ifelse(par=="lambda_year",lengthscale/S,lengthscale)) %>% 
          dplyr::mutate(is.c.ok = ifelse(par=="lambda_year",as.numeric(data_list$c_year>3.2 * lengthscale),
                                         NA),
                        is.m.ok = ifelse(par=="lambda_year",as.numeric(data_list$M_year>1.75*3.2),
                                         as.numeric(data_list$J_week>3.72/lengthscale)),
                        is.m.ok2 = ifelse(par=="lambda_year",as.numeric(data_list$M_year>1.75*data_list$c_year/lengthscale),
                                          NA)) %>% 
          group_by(par) %>% 
          dplyr::summarise(p.c.ok = sum(is.c.ok)/n(),
                           p.m.ok = sum(is.m.ok)/n(),
                           p.m.ok2 = sum(is.m.ok2)/n(),
                           med.lengthscale = median(lengthscale),.groups="drop")
      
      
      
      
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


