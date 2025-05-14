cumulative_excess = function(age_class="80+",chains=1:4,mod="mod8",save.date){
  causes = c("Cardiovascular Diseases","Respiratory Diseases", "Mental and Neurological Disorders",
             "Infectious and Parasitic Diseases",
             "Neoplasms (Cancers)","Suicide","External Causes",
             "Other Causes")
  
  if(FALSE){#debugging
    age_class = "0-17"
    chains = 1:4
    mod = "mod8"
    save.date = "20241218"
  }
  
  #get data (reported deaths)
  cod_agg_pop_nuts_df = readRDS(paste0(code_root_path,"/savepoint/cod_agg_pop_nuts_df.RDS"))
  data = cod_agg_pop_nuts_df %>% 
    filter(age_class==.env$age_class) %>% 
    filter(cod_group %in% c(causes,"COVID-19")) %>% 
    dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                  sex_id = as.numeric(sex),
                  age_id = as.numeric(age_class),
                  cod_group_id = as.numeric(factor(cod_group,levels=c(causes,"COVID-19"))),
                  #year.id = cal_year-min(cal_year)+1,
                  date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                  week.id = as.numeric(1+(date-min(date))/7)) %>% #week.id = dense_rank(date)) %>% 
    arrange(cod_group_id,week.id)
  
  #get sample of deaths
  fit <- readRDS(paste0(code_root_path,"results/",save.date,"/",mod,"_",age_class,"_fit.RDS"))
  d=fit$draws(c("deaths_all_pred","deaths_all_pred0"))[,,]
  n_iter_per_chain = d[,1,1] %>% length()
  deaths_pred_sample = data %>%
    group_by(cal_year,cal_week,date,age_class,cod_group,cod_group_id,week.id) %>%
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
  
  ##############################################################################
  #cumulative excess mortality by cause
  cum_excess_pand_df = deaths_pred_sample %>% 
    dplyr::mutate(excess = n - values,
                  excess = replace_na(excess,0)) %>% #for covid 
    filter(cal_year>=2020) %>% 
    arrange(cod_group_id,pred,iter,week.id) %>% 
    group_by(cod_group_id,pred,iter) %>% 
    dplyr::mutate(cum_excess = cumsum(excess),
                  cum_deaths = cumsum(n),
                  cum_expected = cumsum(values),
                  rel_cum_excess = cum_excess/values[week.id==max(week.id)]) %>% ungroup() %>% 
    group_by(age_class,cod_group,cod_group_id,pred) %>% 
    dplyr::mutate(cum_expected_mean = mean(cum_expected[week.id==max(week.id)])) %>% ungroup() %>% 
    group_by(cal_year,cal_week,date,age_class,cod_group,cod_group_id,week.id,pred) %>% 
    dplyr::summarise(excess_mean = mean(cum_excess),
                     excess_lwb = quantile(cum_excess,probs = 0.025),
                     excess_upb = quantile(cum_excess,probs = 0.975),
                     rel_excess_mean = mean(rel_cum_excess),
                     rel_excess_lwb = quantile2(rel_cum_excess,probs = 0.025),
                     rel_excess_upb = quantile2(rel_cum_excess,probs = 0.975),
                     rel_excess_mean2 = mean(excess_mean/cum_expected_mean),
                     rel_excess_lwb2 = quantile2(cum_excess/cum_expected_mean,probs = 0.025),
                     rel_excess_upb2 = quantile2(cum_excess/cum_expected_mean,probs = 0.975), .groups="drop")
  
  ##############################################################################
  #Cumulative all-cause excess mortality (with and without covid)
  #aggregate deaths over all causes (without covid)
  d_nocovid = deaths_pred_sample %>% 
    filter(cod_group!="COVID-19",cal_year>=2020) %>% 
    dplyr::mutate(cod_group="all",
                  cod_group_id=1) %>% 
    group_by(cal_year,cal_week,date,age_class,cod_group,cod_group_id,week.id,iter,variable,pred) %>% 
    dplyr::summarise(n=sum(n),
                     values = sum(values),.groups="drop") 
  #Number of deaths due to COVID
  d_covid = deaths_pred_sample %>% 
    filter(cod_group=="COVID-19",cal_year>=2020) %>% 
    group_by(cal_year,cal_week,date,age_class,week.id) %>% 
    dplyr::summarise(n_covid=sum(n),.groups="drop")
  
  #Calculate all-cause excess with and without covid
  cum_excess_allcause_pand_df = d_nocovid %>% 
    left_join(d_covid,by=c("cal_year","cal_week","date","week.id","age_class")) %>% 
    cross_join(data.frame(with_covid=c(0,1))) %>% 
    dplyr::mutate(n=if_else(with_covid==1,n+n_covid,n),
                  excess = n - values) %>% 
    dplyr::select(-c(n_covid)) %>% 
    arrange(cod_group_id,pred,iter,week.id,with_covid) %>% 
    group_by(cod_group_id,pred,iter,with_covid) %>% 
    dplyr::mutate(cum_excess = cumsum(excess),
                  cum_deaths = cumsum(n),
                  cum_expected = cumsum(values),
                  rel_cum_excess = cum_excess/values[week.id==max(week.id)]) %>% ungroup() %>% 
    group_by(age_class,cod_group,cod_group_id,pred,with_covid) %>% 
    dplyr::mutate(cum_expected_mean = mean(cum_expected[week.id==max(week.id)])) %>% ungroup() %>% 
    group_by(cal_year,cal_week,date,age_class,cod_group,cod_group_id,week.id,pred,with_covid) %>% 
    dplyr::summarise(excess_mean = mean(cum_excess),
                     excess_lwb = quantile(cum_excess,probs = 0.025),
                     excess_upb = quantile(cum_excess,probs = 0.975),
                     rel_excess_mean = mean(rel_cum_excess),
                     rel_excess_lwb = quantile2(rel_cum_excess,probs = 0.025),
                     rel_excess_upb = quantile2(rel_cum_excess,probs = 0.975),
                     rel_excess_mean2 = mean(excess_mean/cum_expected_mean),
                     rel_excess_lwb2 = quantile2(cum_excess/cum_expected_mean,probs = 0.025),
                     rel_excess_upb2 = quantile2(cum_excess/cum_expected_mean,probs = 0.975), .groups="drop")
  
  if(FALSE){
    cum_excess_pand_df %>% 
      filter(pred=="poisson",cod_group!="Other Causes") %>% 
      ggplot(aes(x=date,y=excess_mean,ymin=excess_lwb,ymax=excess_upb))+
      geom_ribbon(alpha=0.1)+
      geom_line()+
      facet_grid(age_class~cod_group,scales = "free")
    cum_excess_pand_df %>% 
      filter(pred=="poisson",cod_group!="Other Causes") %>% 
      ggplot(aes(x=date,y=rel_excess_mean2,ymin=rel_excess_lwb2,ymax=rel_excess_upb2))+
      geom_ribbon(alpha=0.1)+
      geom_line()+
      facet_grid(age_class~cod_group,scales = "free")+
      scale_y_continuous(labels = scales::percent)
    
    cum_excess_pand_df %>% 
      filter(pred=="poisson",cod_group=="External Causes")
  }
  
  return(list(cum_excess_pand_df = cum_excess_pand_df,
              cum_excess_allcause_pand_df = cum_excess_allcause_pand_df))
}


