combine_cod_icd10 = function(cod_df0,icd10_chapter_block,icd10_cat,icd_var = "ENDG_U_CD_GES_T",
                             filter_cod_groups=NULL){
  cod_ind_df = cod_df0 %>%
    #add a dot when needed
    mutate_at(icd_var,function(x) ifelse(nchar(x)==3,x, gsub('^(.{3})(.*)$', '\\1.\\2',x))) %>% 
    dplyr::select(ind_id = LAUF_KS_N, age=P_ALTER_ERFUELLT_N, sex=GESCHLECHT_CD_GES_T,ctn=WOHNKANTON_AKT_N,
                  cal_week= EREIGNIS_KW_GES_N, cal_year=EREIGNIS_KJ_GES_N,
                  icd10_cat=all_of(icd_var)) %>% 
    dplyr::mutate(icd10 = gsub("\\.[0-9]*","",icd10_cat)) %>% 
    #add icd10 chapter, block and category
    left_join(icd10_chapter_block,by="icd10") %>% 
    left_join(icd10_cat %>% dplyr::select(icd10_cat,icd10Title_cat),by=c("icd10_cat"))
  
  #add cod group
  cod_ind_df = cod_ind_df %>% 
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
      icd10_cat == "U12.9" ~ "COVID-19 vaccine"))
  
  if(!is.null(filter_cod_groups)){
    cod_ind_df = cod_ind_df %>% 
      dplyr::mutate(cod_group = case_when(is.na(cod_group) ~ NA,
                                          cod_group %in% filter_cod_groups ~ cod_group,
                                          TRUE ~ "Other Causes"))
  }
  
  return(cod_ind_df)
}