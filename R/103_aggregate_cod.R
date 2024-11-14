aggregate_cod = function(cod_ind_df){
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
    left_join(data.frame(chapter = 1:22,
                         icd10Chapter = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", 
                                          "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", 
                                          "XIX", "XX", "XXI", "XXII")),by="icd10Chapter") %>% 
    dplyr::mutate(cod_group = case_when(
      chapter %in% 1 ~ "Infectious and Parasitic Diseases",
      chapter == 2 ~ "Neoplasms (Cancers)",
      chapter %in% c(3,4) ~ "Blood, Endocrine, and Metabolic Diseases",
      chapter %in% c(5,6) ~ "Mental and Neurological Disorders",
      chapter %in% c(7,8,12) ~ "Eye, Ear and Skin Conditions",
      chapter %in% 9 ~ "Cardiovascular Diseases",
      chapter %in% 10 ~ "Respiratory Diseases",
      chapter %in% c(11,14) ~ "Digestive and Genitourinary Diseases",
      chapter == 13 ~ "Musculoskeletal Diseases",
      chapter %in% 15:17 ~ "Pregnancy, Perinatal, and Congenital Conditions",
      icd10Title_block=="Intentional self-harm" ~ "Suicide",
      chapter %in% c(20) ~ "External Causes",
      chapter %in% c(18) ~ "No Specific Causes",
      icd10_cat %in% c("U07.1","U07.2") ~ "COVID-19",
      icd10_cat == "U12.9" ~ "COVID-19 vaccine")) %>% 
    dplyr::mutate(age_class = cut(age,breaks = c(-1,18,40,65,80,Inf), labels = c("0-17","18-39","40-64","65-79","80+"),
                                  right=FALSE))
  #aggregate
  df = cod_ind_df2 %>% 
    group_by(age_class,sex,cal_year,cal_week,cod_group) %>% #ctn
    dplyr::summarise(n=n(),.groups="drop")
  
  #all combination
  cod_agg_df = df %>% 
    full_join(cross_join(df %>% dplyr::select(cal_year,cal_week) %>% unique() %>% arrange(cal_year,cal_week),
                         df %>% dplyr::select(age_class) %>% unique()) %>% 
                cross_join(df %>% dplyr::select(sex) %>% unique()) %>% 
                #cross_join(df %>% dplyr::select(ctn) %>% unique()) %>% 
                cross_join(df %>% dplyr::select(cod_group) %>% unique())) %>% 
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
