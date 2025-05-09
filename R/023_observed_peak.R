observed_peak = function(cod_agg_pop_df){
  peak_date = cod_agg_pop_df %>% 
    dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
    #smooth aggregated deaths
    dplyr::filter(cod_group!="COVID-19") %>% 
    dplyr::mutate(period_id = if_else(month(date) >= 7, year(date), year(date) - 1),
                  period_name = paste0(period_id, "/", substr(period_id + 1, 3, 4))) %>%
    group_by(cod_group,age_class,cal_year,cal_week,date,period_name,period_id) %>% 
    dplyr::summarise(n=sum(n),.groups="drop") %>% 
    group_by(cod_group,age_class) %>% 
    arrange(date) %>%
    dplyr::mutate(smoothed = rollmean(n, k = 5, fill = NA, align = "center")) %>% ungroup() %>% 
    #peak
    group_by(cod_group, age_class, period_name,period_id) %>%
    dplyr::mutate(mean_n = mean(n)) %>% 
    slice_max(smoothed) %>%
    slice_max(n) %>%
    dplyr::mutate(n_dates = n(), interval = max(date) - min(date),
                  rel_peak = smoothed/mean_n) %>%
    dplyr::summarise(n_dates = n(),
                     rel_peak = mean(rel_peak),
                     interval = max(date)-min(date),
                     date = if ((max(date) - min(date)) <= 15) mean(date) else as.Date(NA), .groups = "drop") %>% #we only keep peak estimates when the maxium values are all within 2 weeks
    dplyr::mutate(shift_dir = case_when(date < as.Date("2019-07-01") ~ ceiling(as.numeric(as.Date("2019-07-01") - date) / 365.25),
                                        date > as.Date("2020-06-30") ~ -ceiling(as.numeric(date - as.Date("2020-06-30")) / 365.25),
                                        TRUE ~ 0)) %>%
    dplyr::mutate(across(c(date), ~ . + years(shift_dir))) %>%
    dplyr::select(-shift_dir)
  return(peak_date)
}
  