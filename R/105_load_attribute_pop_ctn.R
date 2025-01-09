load_attribute_pop_ctn = function(cod_agg_df){#pop = get_pop_by_age_sex_year_ctn()
  #same as load_attribute_pop but by ctn
  #https://www.pxweb.bfs.admin.ch/pxweb/fr/px-x-0102010000_101/px-x-0102010000_101/px-x-0102010000_101.px
  #load pop
  pop_df = get_pop_by_age_sex_year_ctn()
  #summarize by year, age_class sex (year correspond to the 31 December)
  pop = pop_df %>% 
    dplyr::mutate(n = as.numeric(population),
                  age_class = cut(age,breaks = c(-1,18,40,65,80,Inf), labels = c("0-17","18-39","40-64","65-79","80+"),
                                  right=FALSE)) %>% 
    group_by(cal_year=year,age_class,sex,ctn,ctn_id,NUTS2_id,NUTS2_name) %>% 
    dplyr::summarise(n=sum(n),.groups="drop")
  
  #add a column with pop of the next year
  pop = inner_join(pop %>% select(cal_year,age_class,sex,ctn,ctn_id,NUTS2_id,NUTS2_name,n_end = n),
                   pop %>% dplyr::mutate(cal_year=cal_year+1) %>%
                     select(cal_year,age_class,sex,ctn,ctn_id,NUTS2_id,NUTS2_name,n_start = n),
                   by=c("cal_year","age_class","sex","ctn","ctn_id","NUTS2_id","NUTS2_name")) %>% 
    dplyr::select(cal_year,age_class,sex,ctn,ctn_id,NUTS2_id,NUTS2_name,n_start,n_end)
  
  #extend the pop dataset to include weeks as in the death data
  time = cod_agg_df %>%
    dplyr::select(cal_year,cal_week) %>% unique() %>% 
    group_by(cal_year) %>% 
    dplyr::mutate(week.max=max(cal_week)) %>% ungroup() %>% 
    arrange(cal_year,cal_week)
  
  #Linear interpolation of the population at a given week
  pop = pop %>% left_join(time,by=c("cal_year"),relationship = "many-to-many") %>% 
    dplyr::mutate(n.interpolated = n_start + (n_end-n_start) * (cal_week-1)/(week.max)) %>% 
    dplyr::select(cal_year,cal_week,age_class,sex,ctn,ctn_id,NUTS2_id,NUTS2_name,n=n.interpolated)
  
  return(pop)
}