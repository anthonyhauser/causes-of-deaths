#this function was created at a later point to adapt estimates according to redefined covid-19 phases
aggregate_stan_group = function(age_class="80+",chains=1:4,mod="mod8",save.date,
                                 groups=c("covid_phase","age_class","cod_group")){
  causes = c("Cardiovascular Diseases","Respiratory Diseases", "Mental and Neurological Disorders",
             "Infectious and Parasitic Diseases",
             "Neoplasms (Cancers)","Suicide","External Causes",
             "Other Causes")
  
  if(FALSE){#debugging
    age_class = "80+"
    chains = 1:4
    mod = "mod8"
    save.date = "20241218"
  }
  
  groups=c("covid_phase","cal_year","cal_week","cod_group")
  
  #get data (reported deaths)
  cod_agg_pop_nuts_df = readRDS(paste0(code_root_path,"/savepoint/cod_agg_pop_nuts_df.RDS"))
  data = cod_agg_pop_nuts_df %>% 
    filter(age_class==.env$age_class,cod_group %in% c(causes,"COVID-19"),cal_year>=2020) %>% 
    dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                  sex_id = as.numeric(sex),
                  age_id = as.numeric(age_class),
                  cod_group_id = as.numeric(factor(cod_group,levels=c(causes,"COVID-19"))),
                  #year.id = cal_year-min(cal_year)+1,
                  date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                  week.id = as.numeric(1+(date-min(date))/7),
                  covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
                                                phase <- phases %>%
                                                  filter(d >= start_date & d <= end_date) %>%
                                                  pull(phase)
                                                if (length(phase) == 0) NA_real_ else phase
                                                     })) %>% #week.id = dense_rank(date)) %>%
    arrange(cod_group_id,week.id)
  
  #get sample of deaths
  fit <- readRDS(paste0(code_root_path,"results/",save.date,"/",mod,"_",age_class,"_fit.RDS"))
  d=fit$draws(c("deaths_all_pred","deaths_all_pred0"))[,,]
  n_iter_per_chain = d[,1,1] %>% length()
  deaths_pred_sample = data %>%
    group_by(cal_year,cal_week,date,age_class,cod_group,cod_group_id,week.id,covid_phase) %>%
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
  
  # dplyr::mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
  #                                       phase <- phases %>%
  #                                         filter(d >= start_date & d <= end_date) %>%
  #                                         pull(phase)
  #                                       if (length(phase) == 0) NA_real_ else phase
  #                                            }),
  #               week_id2 = week.id -min(week.id)+1,
  #               month_id = (week_id2 -1) %/% 4) %>% 
  
  if(FALSE){
    deaths_pred_agg %>% 
      filter(variable=="excess",pred=="poisson",cod_group!="Other Causes") %>% 
             #covid_phase %in% 1:7) %>% 
      ggplot(aes(x=covid_phase,y=est,ymin=lwb,ymax=upb))+
      geom_pointrange()+
      geom_hline(yintercept = 0)+
      facet_grid(cod_group~age_class,scales = "free")
  }
  deaths_pred_sample %>% 
    filter(pred=="poisson",cod_group=="Cardiovascular Diseases",iter==1) %>% View()
  deaths_pred_agg %>% 
    filter(variable=="deaths",pred=="poisson",cod_group=="Cardiovascular Diseases") 
  
  res_list$data_pred_phase_cause %>% 
    filter(variable=="deaths",pred=="poisson",
           age_class=="80+",cod_group=="Cardiovascular Diseases") 
  
  
  
  deaths_pred_sample %>% 
    filter(pred=="poisson",cod_group=="Cardiovascular Diseases",iter==1) %>% View()
  deaths_pred_agg %>% 
    filter(variable=="deaths",pred=="poisson",
           age_class=="80+",cod_group=="Cardiovascular Diseases",cal_year==2020,cal_week==1) 
  
  res_list$data_pred_week_cause %>% 
    filter(variable=="deaths",pred=="poisson",
           age_class=="80+",cod_group=="Cardiovascular Diseases",cal_year==2020,cal_week==1) 
  
  
  
  
  
  fit$summary(variables="Sigma")
  res_list$Sigma_mat 
  
  return(deaths_pred_agg)
}


