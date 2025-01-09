load_attribute_pop = function(cod_agg_df){#pop = get_pop_by_age_sex_year_ctn()
  #load pop
  pop.list = lapply(2010:2021,function(x){
    d = read_excel("data/pop_CH_age_sex_2010_2021.xlsx",sheet=as.character(x))[-c(1,2),c(1,3,4)] %>% as_tibble()
    colnames(d) = c("age","male","female")
    d <- d %>%
      filter(!is.na(male),!is.na(female)) %>% 
      pivot_longer(cols=c("male","female"),names_to="sex",values_to = "n") %>% 
      dplyr::mutate(year=x)
    return(d)
  })
  
  #summarize by year, age_class sex (year correspond to the 31 December)
  pop = do.call(rbind,pop.list) %>% 
    dplyr::mutate(sex=recode(sex,`male`="M",`female`="F"),
                  age = as.numeric(ifelse(age=="105 ou plus",105,age)),
                  n = as.numeric(n),
                  age_class = cut(age,breaks = c(-1,18,40,65,80,Inf), labels = c("0-17","18-39","40-64","65-79","80+"),
                                  right=FALSE)) %>% 
    group_by(cal_year=year,age_class,sex) %>% 
    dplyr::summarise(n=sum(n),.groups="drop")
  
  #add a column with pop of the next year
  pop = inner_join(pop %>% select(cal_year,age_class,sex,n_end = n),
                   pop %>% dplyr::mutate(cal_year=cal_year+1) %>% select(cal_year,age_class,sex,n_start = n),
                   by=c("cal_year","age_class","sex")) %>% 
    dplyr::select(cal_year,age_class,sex,n_start,n_end)
  
  #extend the pop dataset to include weeks as in the death data
  time = cod_agg_df %>%
    dplyr::select(cal_year,cal_week) %>% unique() %>% 
    group_by(cal_year) %>% 
    dplyr::mutate(week.max=max(cal_week)) %>% ungroup() %>% 
    arrange(cal_year,cal_week)
  
  #Linear interpolation of the population at a given week
  pop = pop %>% left_join(time,by=c("cal_year"),relationship = "many-to-many") %>% 
    dplyr::mutate(n.interpolated = n_start + (n_end-n_start) * (cal_week-1)/(week.max)) %>% 
    dplyr::select(cal_year,cal_week,age_class,sex,n=n.interpolated)
  
  return(pop)
}