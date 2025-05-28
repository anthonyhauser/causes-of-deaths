aggregate_by_underlying_and_comorb = function(cod_ind_df, n_week_agg = 5){
  #Data preparation
  d_prepped = cod_ind_df %>% 
    #wider and select columns
    dplyr::select(ind_id,age,sex,cal_week,cal_year,outcome,cod_group) %>% 
    pivot_wider(id_cols=c(ind_id,age,sex,cal_week,cal_year),names_from="outcome",values_from="cod_group") %>% 
    #Select data used in the analysis and create date variables for grouping
    filter(cal_year>=2010) %>% #first filter to reduce size, we keep 2010 as some iso year 2011 is calendar year 2010
    dplyr::mutate(date = ISOweek2date(paste0(cal_year, "-W", sprintf("%02d", cal_week), "-1"))) %>% 
    filter(year(date)>=2011,year(date)<=2021) %>% #filter years included in the analysis
    dplyr::mutate(week_id = as.numeric(1+(date-min(date))/7),#year_month = floor_date(date, unit = "month"),
                  week_id = ceiling(week_id /n_week_agg),
                  #age_group = factor(as.numeric(age >= 50) + as.numeric(age >= 65) + as.numeric(age >= 80)),
                  age_class = cut(age,breaks = c(-1,18,40,65,80,Inf), labels = c("0-17","18-39","40-64","65-79","80+"),
                                  right=FALSE),
                  id = row_number()) %>% 
    group_by(week_id) %>% 
    dplyr::mutate(mean_date=mean(unique(date))) %>% ungroup()

  #Step 1: define underyling and comorbidity for each deaths (deaths may have multiple rows)
  comorbid_col = c("BEGLEIT_KRANK_A_GES_T","BEGLEIT_KRANK_B_GES_T")
  comorbid_long <- d_prepped %>%
    dplyr::select(c(ind_id, age_class, mean_date, ENDG_U_CD_GES_T,
                    all_of(comorbid_col))) %>%
    pivot_longer(cols = comorbid_col,
                 names_to = "comorb_type",
                 values_to = "comorbid_cause") %>%
    filter(!is.na(comorbid_cause)) %>%
    distinct(ind_id, age_class, mean_date, ENDG_U_CD_GES_T, comorbid_cause) %>% 
    group_by(ind_id) %>% 
    dplyr::mutate(is_underlying_in_comorbid = any(comorbid_cause==ENDG_U_CD_GES_T)) %>% ungroup()
  
  # Step 2: Identify individuals with NO comorbidities
  no_comorbid <- d_prepped %>%
    filter(if_all(comorbid_col, ~ is.na(.x))) %>% 
    transmute(ind_id, age_class, mean_date, ENDG_U_CD_GES_T, comorbid_cause = "No comorbidity")

  # Step 3: Combine both
  combined <- bind_rows(comorbid_long %>% 
                          dplyr::mutate(comorbid_cause2 = comorbid_cause),
                        comorbid_long %>% 
                          filter(!is_underlying_in_comorbid) %>% 
                          dplyr::select(ind_id,age_class,mean_date,ENDG_U_CD_GES_T) %>% distinct() %>% 
                          dplyr::mutate(comorbid_cause = NA,
                                        comorbid_cause2 = ENDG_U_CD_GES_T),
                        no_comorbid %>% 
                          dplyr::mutate(comorbid_cause2 = ENDG_U_CD_GES_T)) %>% 
    dplyr::select(-is_underlying_in_comorbid) %>% 
    distinct() %>% arrange(ind_id)
  
  # Step 4: Count number of individuals per group and comorbid cause
  counts_underlying_comorbid <- combined %>%
    filter(!is.na(comorbid_cause)) %>% 
    group_by(mean_date, age_class, ENDG_U_CD_GES_T, comorbid_cause) %>%
    dplyr::summarise(n_underlying_comorbid = n(), .groups = "drop")
  
  counts_underlying_comorbid2 <- combined %>% 
    group_by(mean_date, age_class, ENDG_U_CD_GES_T, comorbid_cause2) %>%
    dplyr::summarise(n_underlying_comorbid2 = n(), .groups = "drop")
  
  # Step 5: Count total deaths per group
  counts_underlying <- combined %>%
    group_by(mean_date, age_class, ENDG_U_CD_GES_T) %>%
    dplyr::summarise(n_underlying = length(unique(ind_id)), .groups = "drop")
  
  counts_comorbid <- combined %>%
    filter(!is.na(comorbid_cause)) %>% 
    group_by(mean_date, age_class, comorbid_cause) %>%
    dplyr::summarise(n_comorbid = length(unique(ind_id)), .groups = "drop")
  
  counts_comorbid2 <- combined %>%
    group_by(mean_date, age_class, comorbid_cause2) %>%
    dplyr::summarise(n_comorbid2 = length(unique(ind_id)), .groups = "drop")
  
  #Step 6: Combine
  agg_underlying_comorb_df <- counts_underlying_comorbid %>% 
    full_join(counts_underlying_comorbid2,by=c("mean_date"="mean_date","age_class"="age_class",
                                               "ENDG_U_CD_GES_T"="ENDG_U_CD_GES_T","comorbid_cause"="comorbid_cause2")) %>% 
    left_join(counts_underlying,c("mean_date"="mean_date","age_class"="age_class", "ENDG_U_CD_GES_T"="ENDG_U_CD_GES_T")) %>% 
    left_join(counts_comorbid,c("mean_date"="mean_date","age_class"="age_class", "comorbid_cause"="comorbid_cause")) %>% 
    left_join(counts_comorbid2,c("mean_date"="mean_date","age_class"="age_class", "comorbid_cause"="comorbid_cause2")) %>% 
    dplyr::mutate(prop_comorbid = n_underlying_comorbid / n_underlying,
                  prop_underlying = n_underlying_comorbid / n_comorbid,
                  prop_comorbid2 = n_underlying_comorbid2 / n_underlying,
                  prop_underlying2 = n_underlying_comorbid2 / n_comorbid2)
  
  return(agg_underlying_comorb_df)
}



