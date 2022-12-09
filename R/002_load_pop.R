load_pop = function(death_data){
  #load pop
  pop.list = lapply(2011:2021,function(x){
    d = read_excel("data/pop_CH_age_sex_2010_2021.xlsx",sheet=as.character(x))[-c(1,2),c(1,3,4)] %>% as_tibble()
    colnames(d) = c("age","male","female")
    d <- d %>%
      filter(!is.na(male),!is.na(female)) %>% 
      pivot_longer(cols=c("male","female"),names_to="sex",values_to = "n") %>% 
      dplyr::mutate(year=x)
    return(d)
  })
  
  #summarize by year, age_class sex
  pop = do.call(rbind,pop.list) %>% 
    dplyr::mutate(sex=recode(sex,`male`="M",`female`="F"),
                  age = as.numeric(ifelse(age=="105 ou plus",105,age)),
                  n = as.numeric(n),
                  age_class = factor(as.numeric(age>39) + as.numeric(age>59) + as.numeric(age>69) + as.numeric(age>79),
                                     levels = c(0,1,2,3,4), labels=c("0-39","40-59","60-69","70-79","80+"))) %>% 
    group_by(year,age_class,sex) %>% 
    dplyr::summarise(n=sum(n),.groups="drop")
  
  #add a column with pop of the next year
  pop = inner_join(pop %>% select(year,age_class,sex,n0 = n),
             pop %>% dplyr::mutate(year=year-1) %>% select(year,age_class,sex,n1 = n),
             by=c("year","age_class","sex"))
  
  #extend the pop dataset to include weeks as in the death data
  time = death_data %>%
    dplyr::select(year,week) %>% unique() %>% 
    group_by(year) %>% 
    dplyr::mutate(week.max=max(week)) %>% ungroup()
  
  #Linear interpolation of the population at a given week
  pop = pop %>% left_join(time,by="year") %>% 
    dplyr::mutate(n.interpolated = n0 + (n1-n0) * (week-1)/week.max) %>% 
    dplyr::select(year,week,age_class,sex,n=n.interpolated)
  
  return(pop)
}