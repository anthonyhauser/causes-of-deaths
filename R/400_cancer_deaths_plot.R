cancer_deaths_plot = function(cod_ind_df, n_week_agg = 5){
  ##############################################################################
  #ICD10 block 2/3 for cancer
  #load icd10 table for cancer
  icd10_df =  read_excel(paste0(data_folder,"icd10_icd11_mapping.xlsx"))
  icd10_block_cancer = rbind(icd10_df %>% filter(`10ClassKind`=="Block",`10DepthInKind`==3,icd10Chapter=="II"),
                             icd10_df %>% filter(`10ClassKind`=="Block",`10DepthInKind`==2,icd10Chapter=="II",icd10Code!="C00-C75"),
                             icd10_df %>% filter(`10ClassKind`=="Block",`10DepthInKind`==1,icd10Chapter=="II",icd10Code!="C00-C97")) %>% 
    dplyr::select(icd10Code,icd10Chapter,icd10Title) %>% 
    tidyr::separate(icd10Code,c("start","end"),sep="-")
  
  #long table with corresponding block2/3 for each each icd10 category 
  get_seq_icd10 = function(start,end){
    d = data.frame(letters = LETTERS) %>% 
      cross_join(data.frame(numbers=as.character(0:99)) %>% 
                   dplyr::mutate(numbers = ifelse(nchar(numbers)==1,paste0("0",numbers),as.character(numbers)))) %>% 
      unite(icd10,c(letters,numbers),sep="")
    return(d[which(d$icd10==start):which(d$icd10==end),"icd10"])
  }
  icd10_block_cancer_long=list()
  for(i in 1:dim(icd10_block_cancer)[1]){
    seq = get_seq_icd10(start=as.character(icd10_block_cancer[i,"start"]),end=as.character(icd10_block_cancer[i,"end"]))
    icd10_block_cancer_long[[i]] = icd10_block_cancer[i,] %>% 
      cross_join(data.frame(icd10 = seq)) %>% 
      # cross_join(data.frame(block_n = as.numeric(b[i,"start"]):as.numeric(b[i,"end"]))) %>% 
      # dplyr::mutate(block_n = ifelse(nchar(block_n)==1,paste0("0",block_n),as.character(block_n))) %>% 
      # unite(icd10,c(letter,block_n),remove=FALSE,sep="") %>% 
      dplyr::select(icd10Chapter,icd10,icd10Title)
    print(icd10_block_cancer_long[[i]])
    #print(i)
  }
  icd10_block_cancer_long <- do.call("rbind", icd10_block_cancer_long)
  
  ##############################################################################
  #Individual mortality data
  #Filtering on cancer (either main cause or comorbidities)
  d_prepped0 = cod_ind_df %>% 
    filter(cal_year>=2019,outcome %in% c("ENDG_U_CD_GES_T","BEGLEIT_KRANK_A_GES_T","BEGLEIT_KRANK_B_GES_T")) %>% #first filter to reduce size, we keep 2010 as some iso year 2011 is calendar year 2010
    group_by(ind_id) %>% 
    dplyr::mutate(has_cancer=any(icd10Title_chapter=="Neoplasms")) %>% ungroup() %>% 
    filter(has_cancer) %>% dplyr::select(-has_cancer)
  
  #prepare data: add cancer type, time period etc
  d_prepped0 = d_prepped0 %>% 
    dplyr::select(ind_id,age,sex,cal_week,cal_year,outcome,cod_group) %>% 
    left_join(d_prepped %>%  dplyr::select(ind_id,outcome,icd10),by=c("outcome","ind_id")) %>% 
    left_join(icd10_block_cancer_long %>% dplyr::select(icd10,icd10Title_block2 = icd10Title),by="icd10") %>% 
    dplyr::mutate(icd10Title_block2 = replace_na(icd10Title_block2,"No cancer")) %>%
    dplyr::filter(!is.na(cod_group)) %>% #remove rows with missing cod_group, which is due to the fact that they have only 0 or 1 comorbidity
    #Select data used in the analysis and create date variables for grouping
    dplyr::mutate(date = ISOweek2date(paste0(cal_year, "-W", sprintf("%02d", cal_week), "-1"))) %>% 
    filter(year(date)>=2020,year(date)<=2021) %>% #filter years included in the analysis
    dplyr::mutate(week_id = as.numeric(1+(date-min(date))/7),#year_month = floor_date(date, unit = "month"),
                  week_id = ceiling(week_id /n_week_agg),
                  #age_group = factor(as.numeric(age >= 50) + as.numeric(age >= 65) + as.numeric(age >= 80)),
                  age_class = cut(age,breaks = c(-1,18,40,65,80,Inf), labels = c("0-17","18-39","40-64","65-79","80+"),
                                  right=FALSE),
                  id = row_number()) %>% 
    group_by(week_id) %>% 
    dplyr::mutate(mean_date=mean(unique(date))) %>% ungroup()
  
  ##############################################################################
  #Group by survival
  survival_group_tb <- tribble(
    ~icd10Title_block2,                                                                                                               ~survival_category,
    "Malignant neoplasms of eye, brain and other parts of central nervous system",                                                    "Low",
    "Malignant neoplasms of respiratory and intrathoracic organs",                                                                    "Low",
    "No cancer",                                                                                                                      "No cancer",
    "Malignant neoplasms, stated or presumed to be primary, of lymphoid, haematopoietic and related tissue",                          "Medium",
    "Malignant neoplasms of urinary tract",                                                                                           "Medium",
    "Malignant neoplasms of female genital organs",                                                                                   "Medium",
    "Malignant neoplasms of male genital organs",                                                                                     "High",
    "Malignant neoplasms of digestive organs",                                                                                        "Medium",
    "Neoplasms of uncertain or unknown behaviour",                                                                                    "Medium",
    "Malignant neoplasms of lip, oral cavity and pharynx",                                                                            "Medium",
    "Melanoma and other malignant neoplasms of skin",                                                                                 "High",
    "Malignant neoplasms of mesothelial and soft tissue",                                                                             "Low",
    "Malignant neoplasm of breast",                                                                                                   "High",
    "Malignant neoplasms of bone and articular cartilage",                                                                            "Low",
    "Malignant neoplasms of ill-defined, secondary and unspecified sites",                                                            "Low",
    "Benign neoplasms",                                                                                                               "High",
    "Malignant neoplasms of thyroid and other endocrine glands",                                                                      "High",
    "In situ neoplasms" ,                                                                                                             "High")
  
  
  #adapt groups
  d_prepped = d_prepped0 %>% 
    left_join(survival_group_tb, by="icd10Title_block2") %>% dplyr::select(-icd10Title_block2) %>% rename(icd10Title_block2=survival_category)
  
  ##############################################################################
  #Aggregate data
  
  #combination, used later
  df_comb =  cross_join(d_prepped %>% dplyr::select(age_class) %>% unique(),
                        d_prepped %>% dplyr::select(mean_date) %>% unique()) %>%
    cross_join(d_prepped %>% dplyr::select(all_cond = icd10Title_block2) %>% unique()) %>% 
    cross_join(data.frame(cause_cancer=c(FALSE,TRUE))) %>% 
    arrange(age_class,mean_date,cause_cancer)
  
  #Total deaths
  df_deaths = d_prepped %>% 
    filter(outcome=="ENDG_U_CD_GES_T") %>% 
    dplyr::mutate(cause_cancer=icd10Title_block2!="No cancer") %>% 
    group_by(age_class,mean_date,cause_cancer) %>% 
    dplyr::summarise(n=n(),.groups="drop")
  
  #Main cause
  df_main = d_prepped %>% 
    filter(outcome=="ENDG_U_CD_GES_T") %>% 
    group_by(age_class,mean_date, main_cond = icd10Title_block2) %>% 
    dplyr::summarise(n=n())
  
  #Any condition (main or comorbidities)
  df_any <- d_prepped %>% 
    distinct(ind_id, age_class,mean_date, all_cond = icd10Title_block2) %>%  # Keep one row per individual and ICD block
    #take only the 
    # dplyr::mutate(all_cond = factor(all_cond, levels = c("Low", "Medium", "High"),ordered = TRUE)) %>%         # set risk order
    # arrange(ind_id, all_cond) %>%      # highest risk first
    # group_by(ind_id) %>% slice_head(n = 1) %>%  ungroup() %>%
    dplyr::filter(all_cond!="No cancer") %>% #remove condition 
    left_join(d_prepped %>% 
                filter(outcome=="ENDG_U_CD_GES_T") %>% 
                dplyr::mutate(cause_cancer=icd10Title_block2!="No cancer") %>% 
                dplyr::select(ind_id,cause_cancer), by="ind_id") %>% 
    group_by(age_class,mean_date,all_cond,cause_cancer) %>% 
    dplyr::summarise(n=n(),.groups = "drop")
  
  #Add total deaths
  df_any = df_any %>% 
    #add total number of deaths by age group and date
    full_join(df_deaths %>% 
                dplyr::select(age_class,mean_date,cause_cancer,n_deaths=n), by=c("age_class","mean_date","cause_cancer")) %>% 
    #add all different combinations
    full_join(df_comb, by=c("age_class","mean_date","all_cond","cause_cancer")) %>% 
      dplyr::mutate(n=replace_na(n,0),
                    n_deaths=replace_na(n_deaths,0))
    
  d1 = df_any %>% 
    #filter on cause of deaths: cancer or other
    #dplyr::filter(cause_cancer) %>% 
    #aggregate after filtering
    group_by(age_class,mean_date,all_cond,cause_cancer) %>% dplyr::summarise(n=sum(n),
                                                                n_deaths = sum(n_deaths),
                                                                p=n/n_deaths,.groups="drop") %>% 
    arrange(age_class,mean_date,all_cond)
  
  if(FALSE){
    d1 %>% filter(age_class %in% c("65-79","80+"),all_cond!="No cancer") %>% #"No cancer" is removed as always 0 because as we focus on cancer comorbidities
      ggplot(aes(x=mean_date,y=n,col=all_cond))+
      geom_line()+
      facet_wrap(cause_cancer~age_class)+
      scale_x_date(name="Time")+
      scale_y_continuous(name="Number of individuals with cancer dying from other causes")+
      scale_color_manual(name="Condition",values=c("red","orange","forestgreen"),
                         breaks = c("Low","Medium","High"),
                         labels = c("Low-survival cancer","Medium-survival cancer","High-survival cancer"))
  }
  
  ##############################################################################
  #Plots
  p1 = df_deaths  %>% 
    group_by(age_class,mean_date,cause_cancer) %>% 
    dplyr::summarise(n=sum(n)) %>% 
    dplyr::mutate(cause_cancer=as.numeric(cause_cancer)) %>% 
    rbind(df_deaths  %>% 
            group_by(age_class,mean_date) %>% 
            dplyr::summarise(n=sum(n),.groups="drop") %>% 
            dplyr::mutate(cause_cancer=2)) %>% 
    filter(age_class %in% c("65-79","80+")) %>% 
    ggplot(aes(x=mean_date,y=n,col=factor(cause_cancer)))+
    geom_line(linewidth=1)+
    annotate("text",label="",x=as.Date("2020-01-01"),y=0)+
    facet_wrap(.~age_class)+
    scale_x_date(name="",date_labels = "%b %y")+
    scale_y_continuous(name="Number of individuals\ndying with cancer")+
    scale_color_manual(name="Cause of death",values=c("blue","violet","black"),
                       breaks = c(0,1,2),
                       labels = c("No cancer","Cancer","Total"))+
    theme(legend.position = "bottom",
          panel.spacing = unit(1.5, "lines"),
          legend.margin = margin(-8, 0, 8, 0))
  
  p2 = df_main %>% filter(age_class %in% c("65-79","80+"),main_cond!="No cancer") %>% 
    ggplot(aes(x=mean_date,y=n,col=main_cond))+
    geom_line(linewidth=1)+
    annotate("text",label="",x=as.Date("2020-01-01"),y=0)+
    facet_wrap(.~age_class)+
    scale_x_date(name="",date_labels = "%b %y")+
    scale_y_continuous(name="Number of individuals\ndying with cancer")+
    scale_color_manual(name="Cause of death",values=c("red","orange","forestgreen","blue"),
                       breaks = c("Low","Medium","High","No cancer"),
                       labels = c("Low-survival cancer","Medium-survival cancer","High-survival cancer","No cancer"))+
    theme(legend.position = "bottom",
          panel.spacing = unit(1.5, "lines"),
          legend.margin = margin(-8, 0, 8, 0))
  
  p3 = d1 %>% filter(!cause_cancer,
                     age_class %in% c("65-79","80+"),all_cond!="No cancer") %>% #"No cancer" is removed as always 0 because as we focus on cancer comorbidities
    ggplot(aes(x=mean_date,y=n,col=all_cond))+
    geom_line(linewidth=1)+
    facet_wrap(.~age_class)+
    scale_x_date(name="",date_labels = "%b %y")+
    scale_y_continuous(name="Number of individuals with\ncancer dying from other causes")+
    scale_color_manual(name="Condition",values=c("red","orange","forestgreen"),
                       breaks = c("Low","Medium","High"),
                       labels = c("Low-survival cancer","Medium-survival cancer","High-survival cancer"))+
    theme(legend.position = "bottom",
          panel.spacing = unit(1.5, "lines"),
          #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.margin = margin(-8, 0, 8, 0))
  
  return(cowplot::plot_grid(p1,p2,p3,
                   labels=c("A.","B.","C."),
                   ncol=1))
}



