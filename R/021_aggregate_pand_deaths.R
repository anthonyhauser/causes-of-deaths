aggregate_pand_deaths = function(data_pand, fit, cmdstan=TRUE){
  #get sample of deaths
  if(cmdstan){
    d=fit$draws()[,,]
    n_iter_per_chain = d[,1,1] %>% length()
    deaths_pand_pred_sample = data_pand %>%
      dplyr::mutate(data_row= dplyr::row_number()) %>% 
      dplyr::select(year,week,age_class,sex,data_row) %>% 
      left_join(.,
                as.data.frame(ftable(d[,,grepl("deaths_pand_pred",dimnames(d)[[3]]) & !grepl("overall_deaths_pand_pred",dimnames(d)[[3]])])) %>% 
                  dplyr::mutate(chain = as.numeric(as.character(chain)),
                                iteration = as.numeric(as.character(iteration)),
                                iter=iteration+n_iter_per_chain*(chain-1)) %>% 
                  dplyr::select(iter,var=variable,values=Freq) %>% 
                  #separate(col=var,into=c("variable","data_row"),sep="\\[") %>% 
                  tidyfast::dt_separate(col=var,into=c("variable","data_row"),sep="[") %>%
                  dplyr::mutate(data_row = as.numeric(gsub("\\]","",data_row))),by="data_row")

  }else{
    deaths_pand_pred_sample = data_pand %>%
      dplyr::mutate(data_row= dplyr::row_number()) %>% 
      dplyr::select(year,week,age_class,sex,data_row) %>% 
      left_join(.,
                rstan::extract(fit,pars="deaths_pand_pred") %>% 
                  as.data.frame() %>% 
                  dplyr::mutate(iter = dplyr::row_number()) %>% 
                  pivot_longer(cols=contains("deaths_pand_pred"),names_to = "var",values_to="values") %>%
                  separate(col=var,into=c("variable","data_row"),sep="\\.") %>% 
                  dplyr::mutate(data_row=as.numeric(data_row)),by="data_row")
  }
  
  #aggregate deaths by year, week and age_class (i.e. over sex)
  deaths_pand_pred_agg = deaths_pand_pred_sample %>% 
    group_by(year,week,age_class,iter) %>%
    dplyr::summarise(values=sum(values), .groups="drop_last") %>% 
    dplyr::summarise(est=mean(values),
                     lwb = quantile(values,probs=0.025),
                     upb = quantile(values,probs=0.975), .groups="drop")
  
  #combine observed and predicted data
  d = data_pand %>%
    group_by(year,week,age_class) %>% 
    dplyr::summarise(n=sum(n),.groups="drop") %>% 
    left_join(deaths_pand_pred_agg, by=c("year","week","age_class")) %>% 
    dplyr::mutate(date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")))
  
  return(d)
}


