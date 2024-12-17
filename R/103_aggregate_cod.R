aggregate_cod = function(cod_ind_df,agg_var = c("age_class","sex")){
  #Checking for outliers
  if(FALSE){
    #year
    cod_ind_df %>% 
      group_by(cal_year) %>% 
      dplyr::summarise(n=n(),.groups="drop") %>% filter(cal_year<1995|cal_year>2021)
    #week
    cod_ind_df$cal_week %>% range()
    cod_ind_df %>% filter(cal_week==53) %>% 
      group_by(cal_year) %>% 
      dplyr::summarise(n=n(),.groups="drop")
    #age
    cod_ind_df$age %>% range()
    #sex
    cod_ind_df$sex %>% unique()
    #ctn
    cod_ind_df$ctn %>% unique() %>% sort()
  }
  #Aggregate
  #clean data and define icd10 and age groups
  cod_ind_df2 = cod_ind_df %>%
    filter(cal_year %in% c(2011:2021)) %>% #, cal_week<=52) %>%
    dplyr::mutate(age_class = cut(age,breaks = c(-1,18,40,65,80,Inf), labels = c("0-17","18-39","40-64","65-79","80+"),
                                  right=FALSE)) %>% 
    left_join(canton_df,by="ctn_id")
  #aggregate
  df = cod_ind_df2 %>% 
    group_by_at(c(agg_var,"cal_year","cal_week","cod_group")) %>% #ctn
    dplyr::summarise(n=n(),.groups="drop")
  
  #all combination
  df_cross_join = cross_join(df %>% dplyr::select(cal_year,cal_week) %>% unique() %>% arrange(cal_year,cal_week),
                             df %>% dplyr::select(age_class) %>% unique()) %>% 
    cross_join(df %>% dplyr::select(sex) %>% unique()) %>% 
    cross_join(df %>% dplyr::select(cod_group) %>% unique())
  if("NUTS2_id" %in% agg_var){
    df_cross_join <- df_cross_join %>% 
      cross_join(df %>% dplyr::select(NUTS2_id,NUTS2_name) %>% unique())
  }
  cod_agg_df = df %>% 
    full_join(df_cross_join) %>% 
    dplyr::mutate(n=replace_na(n,0))
  
  
  
  
  # #Select main categories
  # d.main.cat <- d3 %>% 
  #   #number of total deaths
  #   group_by(year,week,age_class,sex) %>% 
  #   dplyr::mutate(n.tot=n[which(cause2=="total")]) %>% ungroup() %>%
  #   #keep main causes
  #   filter(cause2 %in% c("tot.cancer","tot.infect.dis","dementia","diabetes","tot.accident","tot.resp.dis","cardiovascular.dis","covid")) %>%
  #   arrange(year,week,age_class,sex) 
  # 
  # #Define the number of deaths due to other causes (i.e. not due to the main categories)
  # d.other = d.main.cat %>% 
  #   group_by(year,week,age_class,sex,n.tot) %>% 
  #   dplyr::summarise(n.tot.main.cat = sum(n),.groups="drop") %>% 
  #   dplyr::mutate(n.other = n.tot-n.tot.main.cat) 
  # 
  # #Bind deaths from main categories and from other causes
  # d4 = rbind(d.main.cat %>% dplyr::select(year,week,age_class,sex,cause=cause2,n,n.tot),
  #            d.other %>% dplyr::mutate(cause="tot.other") %>% 
  #              dplyr::select(year,week,age_class,sex,cause,n=n.other,n.tot)) %>% 
  #   arrange(year,week,age_class,sex) 
  
  return(cod_agg_df)
}
