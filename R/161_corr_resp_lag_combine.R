corr_resp_lag_combine = function(age_class="80+",chains=1:4,mod="mod8",save.date){
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
  
  #get data (reported deaths)
  cod_agg_pop_nuts_df = readRDS(paste0(code_root_path,"/savepoint/cod_agg_pop_nuts_df.RDS"))
  data = cod_agg_pop_nuts_df %>% 
    filter(age_class==.env$age_class) %>% 
    filter(cod_group %in% c(causes,"COVID-19")) %>% 
    dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                  sex_id = as.numeric(sex),
                  age_id = as.numeric(age_class),
                  cod_group_id = as.numeric(factor(cod_group,levels=c(causes,"COVID-19"))),
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
    left_join(.,
              as.data.frame(ftable(d[,,grepl("deaths_all_pred",dimnames(d)[[3]])])) %>% 
                dplyr::mutate(chain = as.numeric(as.character(chain)),
                              iteration = as.numeric(as.character(iteration)),
                              iter=iteration+n_iter_per_chain*(chain-1)) %>% 
                dplyr::filter(chain %in% chains) %>% 
                dplyr::select(iter,var=variable,values=Freq) %>% 
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
  
  #cumulative excess: summary posterior estimates
  cum_excess_pand_df = deaths_pred_sample %>% 
    dplyr::mutate(values = if_else(cod_group=="COVID-19",0,values),
                  excess = n - values) %>% #for covid 
    filter(cal_year>=2020) %>% 
    group_by(cal_year,cal_week,date,age_class,cod_group,cod_group_id,week.id,pred) %>% 
    dplyr::summarise(excess_mean = mean(excess),
                     excess_lwb = quantile(excess,probs = 0.025),
                     excess_upb = quantile(excess,probs = 0.975),.groups="drop")
  #Adapt structure of the data
  reshaped_df <- cum_excess_pand_df %>%
    filter(is.na(pred) | pred=="poisson") %>% 
    dplyr::mutate(week.id=week.id-min(week.id)+1) %>% 
    dplyr::select(week.id,age_class,cod_group,excess_mean) %>% 
    pivot_wider(names_from = cod_group, values_from = excess_mean) %>% 
    arrange(week.id)
  
  #jobs
  jobs <- CJ(y = setdiff(causes, "Respiratory Diseases"),
             lag = -8:8)
  
  # Run the function across all combinations
  pb <- progress_bar$new(total = nrow(jobs))
  results <- lapply(seq_len(nrow(jobs)), function(j) {
    pb$tick()
    row <- jobs[j]
    corr_resp_lag_noci(
      data = reshaped_df,
      x = c("COVID-19", "Respiratory Diseases"),
      y = row$y,
      lag = row$lag)
    })
  # Combine once
  df_res <- rbindlist(results)
  
  # df_res = rbindlist(lapply(setdiff(causes,c("Respiratory Diseases")), function(y) {
  #   print(y)
  #   rbindlist(lapply(-15:15,function(l) corr_resp_lag(data=reshaped_df,x=c("COVID-19","Respiratory Diseases"),
  #                                                     y=y,lag=l)))
  # }))
  
  if(FALSE){
    df_res %>% 
      filter(var!="(Intercept)",lag>=-8,lag<=8) %>% 
      ggplot(aes(x=lag,y=est,ymin=lwb,ymax=upb,fill=var))+
      geom_ribbon(alpha=0.1)+
      geom_line(aes(col=var))+
      geom_point(aes(col=var))+
      geom_line(aes(y=pcor_est),linetype="dashed")+
      geom_line(aes(y=r_squared),col="black",linetype="dashed")+
      geom_hline(yintercept = 0,linetype="dashed") +
      scale_y_continuous(limits=c(-1,1))+
      facet_wrap(.~y)
  }
  return(df_res)
}


