combine_cod_icd10 = function(cod_df0,icd10_chapter_block,icd10_cat){
  cod_ind_df = cod_df0 %>%
    #add a dot when needed
    mutate_at(c("ENDG_U_CD_GES_T"),function(x) ifelse(nchar(x)==3,x, gsub('^(.{3})(.*)$', '\\1.\\2',x))) %>% 
    dplyr::select(age=P_ALTER_ERFUELLT_N, sex=GESCHLECHT_CD_GES_T,ctn=WOHNKANTON_AKT_N,
                  cal_week= EREIGNIS_KW_GES_N, cal_year=EREIGNIS_KJ_GES_N,icd10_cat=ENDG_U_CD_GES_T) %>% 
    dplyr::mutate(icd10 = gsub("\\.[0-9]*","",icd10_cat)) %>% 
    #add icd10 chapter, block and category
    left_join(icd10_chapter_block,by="icd10") %>% 
    left_join(icd10_cat %>% dplyr::select(icd10_cat,icd10Title_cat),by=c("icd10_cat"))
  
  return(cod_ind_df)
}