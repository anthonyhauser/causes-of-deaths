run_stan_mod3_by_age_cause = function(data_all, age_classes, causes,run.model=TRUE){
  
  mod3_cmdstan <- cmdstan_model("stan/mod3_GP_year_season.stan")
  x=1:500
  x_mean = mean(x)
  x_sd = sd(x)
  xn = (x-mean(x))/x_sd
  
  
  data_pred_fit=list()
  data_pred_pand=list()
  excess=list()
  week_GP=list()
  year_GP=list()
  sample_deaths_pand_pred=list()
  deaths_pand_pred_sample=list()
  deaths_pred_sample=list()
  for(j in 1:length(age_classes)){
    for(i in 1:length(causes)){
      age_class_i = age_classes[j]
      cause_i = causes[i]
      list_id = paste0(age_class_i,"_",cause_i)
      print(cause_i)
      print(age_class_i)
      
      ###########################################################################
      #data
      data = data_all %>% 
        filter(cause==cause_i,age_class==age_class_i) %>% 
        dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                      year.id = year-min(year)+1,
                      date = as.Date(paste0(year,"-01-01")) + 7*(week-1),
                      week.id = dense_rank(date))
                      # date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")),
                      # week.id = (as.numeric(date)-as.numeric(min(date)))/7 + 1)
      
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
      
      #lengthscale for long-term trend
      l=rlnorm(100000,0.5,0.5)
      hist(l)
      mean(l)
      quantile(l,probs=c(0.025,0.5,0.975))
      tuning_parameter_cond_EQ(l,xn,x_sd)
      
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
        J_week = 20,
        
        p_intercept = c(-10,2),
        p_alpha_year = c(0,0.1),
        p_lambda_year = c(0.5, 0.5),#c(0,0.4)
        p_lambda_week = c(1,0.4),
        p_alpha_week = c(0,0.1),
        
        inference=1
      )
      
      #init function
      initfun <- function() { list(lambda_week=rlnorm(1,data_list$p_lambda_week[1],data_list$p_lambda_week[2]),
                                   lambda_year=rlnorm(1,data_list$p_lambda_year[1],data_list$p_lambda_year[2])) }
      
      ###########################################################################
      #Model
      if(run.model){
        #run model in cmdstan
        fit3 <- mod3_cmdstan$sample(
          init=initfun,
          adapt_delta=0.99,
          data = data_list,
          chains = 4, 
          parallel_chains = 4,
          show_messages = FALSE,
          refresh = 10000, # print update every 500 iters
        )
        
        #Save the chains
        fit3$save_output_files(dir = paste0(code_root_path,"/results/stan_chains"),
                               basename = paste0("chain_mod3","_",age_class_i,"_",cause_i))
      }else{
        #load csv files
        files = list.files(paste0(code_root_path,"/results/stan_chains"))
        csv_files= files[grepl(gsub("\\+","\\\\+",paste0("chain_mod3","_",age_class_i,"_",cause_i)),files)]
        fit3 <-as_cmdstan_fit(paste0(code_root_path,"/results/stan_chains/",csv_files)[1:4])
      }
      
      #divergence
      print(paste(c("divergence: ",fit3$sampler_diagnostics()[,,"divergent__"] %>% apply(2,sum)),sep=" "))

      # fit3$summary(variables = c("beta_reg","lambda_year","alpha_year","lambda_week","alpha_week","mu0"),
      #              "mean",~quantile(.x, probs = c(0.01,0.025, 0.5, 0.975,0.99)),"rhat", "ess_bulk", "ess_tail")
      t0=Sys.time()
      ###########################################################################
      #Deaths
      #number of deaths aggregated over sex, pandemic
      data_pred_fit[[list_id]] = aggregate_fit_deaths(data_fit, fit3,cmdstan=TRUE) %>% 
        dplyr::mutate(cause=cause_i)
      t1=Sys.time()
      
      #number of deaths aggregated over sex, before pandemic
      data_pred_pand[[list_id]] = aggregate_pand_deaths(data_pand, fit3,cmdstan=TRUE) %>% 
        dplyr::mutate(cause=cause_i)
      t2=Sys.time()
      
      #excess
      excess[[list_id]] = fit3$summary(variables = c("overall_excess","overall_rel_excess"),
                                       "mean",~quantile(.x, probs = c(0.025, 0.975),na.rm=TRUE)) %>% 
        as_tibble() %>% 
        dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
        dplyr::mutate(var = c("overall_excess","overall_rel_excess"),
                      cause = cause_i,
                      age_class = age_class_i)
      t3=Sys.time()
      ###########################################################################
      #GP
      #periodic
      week_GP[[list_id]] = fit3$summary(variables = c("f_week"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
        as_tibble() %>% 
        dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
        dplyr::mutate(var =  "f_week",
                      date = data$date %>% unique() %>% sort(),
                      cause = cause_i,
                      age_class = age_class_i)
      #long term trend
      year_GP[[list_id]] = fit3$summary(variables = c("f_year"), "mean",~quantile(.x, probs = c(0.025, 0.975))) %>% 
        as_tibble() %>% 
        dplyr::select(est=mean,lwb=`2.5%`,upb=`97.5%`) %>%
        dplyr::mutate(var =  "f_week",
                      date = data$date %>% unique() %>% sort(),
                      cause = cause_i,
                      age_class = age_class_i)
      t4=Sys.time()
      ###########################################################################
      #Samples
      d=fit3$draws()[,,]
      n_iter_per_chain = d[,1,1] %>% length()
      
      #Overall predicted (i.e. expected) deaths during pandemic
      sample_deaths_pand_pred[[list_id]] =
        as.data.frame(ftable(d[,,grepl("overall_deaths_pand_pred",dimnames(d)[[3]])])) %>%
        dplyr::select(overall_deaths_pand_pred=Freq) %>% 
        dplyr::mutate(cause=cause_i,
                      age_class = age_class_i)
      t5=Sys.time()
      
      #number of deaths by week, sex, before pandemic
      deaths_pred_sample[[list_id]] = data_fit %>%
        dplyr::mutate(data_row= dplyr::row_number()) %>% 
        dplyr::select(year,week,age_class,cause,sex,data_row,n,n.tot) %>% 
        left_join(.,as.data.frame(ftable(d[,,grepl("deaths_pred",dimnames(d)[[3]]) & !grepl("_deaths",dimnames(d)[[3]])])) %>% 
                    dplyr::mutate(chain = as.numeric(as.character(chain)),
                                  iteration = as.numeric(as.character(iteration)),
                                  iter=iteration+n_iter_per_chain*(chain-1)) %>% 
                    dplyr::select(iter,var=variable,values=Freq) %>% 
                    #separate(col=var,into=c("variable","data_row"),sep="\\[") %>% 
                    tidyfast::dt_separate(col=var,into=c("variable","data_row"),sep="[") %>% 
                    dplyr::mutate(data_row = as.numeric(gsub("\\]","",data_row))),by="data_row")
      
      #number of deaths by week, sex, during pandemic
      deaths_pand_pred_sample[[list_id]] = data_pand %>%
        dplyr::mutate(data_row= dplyr::row_number()) %>% 
        dplyr::select(year,week,age_class,cause,sex,data_row,n,n.tot) %>% 
        left_join(.,as.data.frame(ftable(d[,,grepl("deaths_pand_pred",dimnames(d)[[3]]) & !grepl("overall_deaths_pand_pred",dimnames(d)[[3]])])) %>% 
                    dplyr::mutate(chain = as.numeric(as.character(chain)),
                                  iteration = as.numeric(as.character(iteration)),
                                  iter=iteration+n_iter_per_chain*(chain-1)) %>% 
                    dplyr::select(iter,var=variable,values=Freq) %>% 
                    #separate(col=var,into=c("variable","data_row"),sep="\\[") %>% 
                    tidyfast::dt_separate(col=var,into=c("variable","data_row"),sep="[") %>% 
                    dplyr::mutate(data_row = as.numeric(gsub("\\]","",data_row))),by="data_row")
      t6=Sys.time()
      
      t6-t0
      print(excess[[list_id]])
    }
  }
  
  #save
  saveRDS(do.call(rbind,data_pred_fit), file="results/mod3_data_pred_fit.RDS")
  saveRDS(do.call(rbind,data_pred_pand), file="results/mod3_data_pred_pand.RDS")
  saveRDS(do.call(rbind,sample_deaths_pand_pred), file="results/mod3_sample_deaths_pand_pred.RDS")
  saveRDS(do.call(rbind,year_GP), file="results/mod3_year_GP.RDS")
  saveRDS(do.call(rbind,week_GP), file="results/mod3_week_GP.RDS")
  saveRDS(do.call(rbind,excess), file="results/mod3_excess.RDS")
  saveRDS(rbindlist(deaths_pand_pred_sample),file="results/deaths_pand_pred_sample.RDS")
  saveRDS(rbindlist(deaths_pred_sample),file="results/deaths_pred_sample.RDS")
  
  return(do.call(rbind,data_pred_pand))
}