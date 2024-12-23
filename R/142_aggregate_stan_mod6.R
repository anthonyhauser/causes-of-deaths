aggregate_stan_mod6 = function(data, fit, cmdstan=TRUE,
                          groups=c("cal_year","cal_week","age_class"),
                          chains){
  
  #get sample of deaths
  if(cmdstan){
    d=fit$draws()[,,]
    n_iter_per_chain = d[,1,1] %>% length()
    deaths_pred_sample = data %>%
      group_by(cal_year,cal_week,date,covid_phase,age_class,cod_group,cod_group_id,week.id) %>%
      dplyr::summarise(n=sum(n),.groups="drop") %>%
      #dplyr::select(cal_year,cal_week,date,covid_phase,age_class,cod_group,n,cod_group_id,week.id) %>% #sex
      left_join(.,
            as.data.frame(ftable(d[,,grepl("deaths_all_pred",dimnames(d)[[3]])])) %>% 
                  dplyr::mutate(chain = as.numeric(as.character(chain)),
                                iteration = as.numeric(as.character(iteration)),
                                iter=iteration+n_iter_per_chain*(chain-1)) %>% 
                  dplyr::filter(chain %in% chains) %>% 
                  dplyr::select(iter,var=variable,values=Freq) %>% 
                  #separate(col=var,into=c("variable","data_row"),sep="\\[") %>%
            as.data.table() %>% 
            .[, c("variable", "cod_group_id", "week.id") := .(
              str_extract(var, "^[^\\[]+"),                 # Extract prefix (before [)
              str_extract(var, "(?<=\\[)\\d+"),             # Extract first number inside []
              str_extract(var, "(?<=,)\\d+(?=\\])")         # Extract second number inside []
            )] %>% 
              dplyr::mutate(cod_group_id=as.numeric(cod_group_id),
                            week.id=as.numeric(week.id)) %>% 
              dplyr::select(-var),by=c("cod_group_id","week.id")) %>% 
      dplyr::mutate(pred = factor(variable,levels=c("deaths_all_pred0","deaths_all_pred"),labels=c("poisson","dispersed poisson")))
    print("deaths_all_pred processed")
    
  }else{
    return(NULL)
  }

  #aggregate deaths by year, week and age_class (i.e. over sex)
  deaths_pred_agg = deaths_pred_sample %>% 
    group_by_at(c(groups,"pred","iter")) %>%
    dplyr::summarise(values=sum(values),
                     n=sum(n),.groups="drop") %>% 
    dplyr::mutate(deaths=values,
                  obs_deaths=n,
                  excess=n-values,
                  rel_excess = (n-values)/values) %>% dplyr::select(-c(values,n)) %>% 
    pivot_longer(cols=c("deaths","excess","rel_excess","obs_deaths"),values_to="values",names_to="variable") %>% 
    group_by_at(c("variable",groups,"pred")) %>% 
    dplyr::summarise(est=mean(values),
                     lwb = quantile(values,probs=0.025,na.rm=TRUE),
                     upb = quantile(values,probs=0.975,na.rm=TRUE), .groups="drop")
  
  return(deaths_pred_agg)
}


