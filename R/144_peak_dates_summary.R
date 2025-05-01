peak_dates_summary = function(age_class="80+",chains=1:4,mod="mod8",save.date){
  causes = c("Cardiovascular Diseases","Respiratory Diseases", "Mental and Neurological Disorders",
             "Infectious and Parasitic Diseases",
             "Neoplasms (Cancers)","Suicide","External Causes",
             "Other Causes")
  
  cod_agg_pop_nuts_df = readRDS(paste0(code_root_path,"/savepoint/cod_agg_pop_nuts_df.RDS"))
  data = cod_agg_pop_nuts_df %>% 
    filter(cod_group %in% c(causes),age_class==.env$age_class) %>% 
    dplyr::select(age_class,cod_group,cal_year,cal_week) %>% unique() %>% 
    dplyr::mutate(cod_group_id = as.numeric(factor(cod_group,levels=c(causes))),
                  date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                  week.id = as.numeric(1+(date-min(date))/7)) %>% 
    arrange(age_class,cod_group_id,week.id)
  
  
  #get sample of deaths
  fit <- readRDS(paste0(code_root_path,"results/",save.date,"/",mod,"_",age_class,"_fit.RDS"))
  d=fit$draws("f_week")[,,]
  n_iter_per_chain = d[,1,1] %>% length()
  f_week_sample = data %>%
    left_join(.,
              as.data.frame(ftable(d[,,grepl("f_week",dimnames(d)[[3]])])) %>% 
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
                dplyr::select(-var),by=c("cod_group_id","week.id"))
  
  #aggregate deaths by year, week and age_class (i.e. over sex)
  seasonal_peak_est= f_week_sample %>%
    filter(cal_year<=2019) %>% 
    group_by(cod_group,iter,cal_year) %>% 
    slice_max(values)
  
  # Apply per cod_group
  peak_dates_summary_df <- seasonal_peak_est %>%
    dplyr::mutate(doy = yday(date)) %>% 
    group_by(cod_group) %>%
    group_modify(~summarise_peak_dates(.x)) %>%
    ungroup() %>% 
    dplyr::mutate(age_class = age_class)
  print(peak_dates_summary_df)
  
  return(peak_dates_summary_df)
}


