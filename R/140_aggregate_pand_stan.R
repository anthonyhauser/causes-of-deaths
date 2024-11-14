aggregate_stan = function(data, fit, cmdstan=TRUE,
                          groups=c("cal_year","cal_week","age_class")){
  
  #get sample of deaths
  if(cmdstan){
    d=fit$draws()[,,]
    n_iter_per_chain = d[,1,1] %>% length()
    deaths_pred_sample = data %>%
      dplyr::mutate(data_row= dplyr::row_number()) %>% 
      dplyr::select(cal_year,cal_week,date,covid_phase,age_class,sex,n,data_row) %>% 
      left_join(.,
                #as.data.frame(ftable(d[,,grepl(paste(variables,collapse="|"),dimnames(d)[[3]])])) %>% 
                as.data.frame(ftable(d[,,grepl("deaths_all_pred",dimnames(d)[[3]])])) %>% 
                  dplyr::mutate(chain = as.numeric(as.character(chain)),
                                iteration = as.numeric(as.character(iteration)),
                                iter=iteration+n_iter_per_chain*(chain-1)) %>% 
                  dplyr::select(iter,var=variable,values=Freq) %>% 
                  #separate(col=var,into=c("variable","data_row"),sep="\\[") %>% 
                  tidyfast::dt_separate(col=var,into=c("variable","data_row"),sep="[") %>%
                  dplyr::mutate(data_row = as.numeric(gsub("\\]","",data_row))),by="data_row")
    
  }else{
    return(NULL)
  }
  
  #aggregate deaths by year, week and age_class (i.e. over sex)
  deaths_pred_agg = deaths_pred_sample %>% 
    group_by_at(c(groups,"iter")) %>%
    dplyr::summarise(values=sum(values),
                     n=sum(n),.groups="drop") %>% 
    dplyr::mutate(deaths=values,
                  obs_deaths=n,
                  excess=n-values,
                  rel_excess = (n-values)/values) %>% select(-c(values,n)) %>% 
    pivot_longer(cols=c("deaths","excess","rel_excess","obs_deaths"),values_to="values",names_to="variable") %>% 
    group_by_at(c("variable",groups)) %>% 
    dplyr::summarise(est=mean(values),
                     lwb = quantile(values,probs=0.025,na.rm=TRUE),
                     upb = quantile(values,probs=0.975,na.rm=TRUE), .groups="drop")
  
  return(deaths_pred_agg)
}


