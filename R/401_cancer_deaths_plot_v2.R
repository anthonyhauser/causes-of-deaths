cancer_deaths_plot2 = function(cod_ind_df, n_week_agg = 1, k=9){
  ##############################################################################
  #ICD10 block 2/3 for cancer
  #load icd10 table for cancer
  icd10_df = read_excel(paste0(data_folder,"icd10_icd11_mapping.xlsx"))
  icd10_block_cancer = rbind(icd10_df %>% filter(`10ClassKind`=="Block",`10DepthInKind`==3,icd10Chapter=="II"),
                             icd10_df %>% filter(`10ClassKind`=="Block",`10DepthInKind`==2,icd10Chapter=="II",icd10Code!="C00-C75"),
                             icd10_df %>% filter(`10ClassKind`=="Block",`10DepthInKind`==1,icd10Chapter=="II",icd10Code!="C00-C97")) %>% 
    dplyr::select(icd10Code,icd10Chapter,icd10Title) %>% 
    tidyr::separate(icd10Code,c("start","end"),sep="-")
  #categories
  icd10_category_cancer = icd10_df %>% filter(`10ClassKind`=="Category",icd10Chapter=="II",`10DepthInKind`==1) %>% 
    dplyr::select(icd10=icd10Code,icd10Title2=icd10Title)
  
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
    #print(icd10_block_cancer_long[[i]])
    #print(i)
  }
  icd10_block_cancer_long <- do.call("rbind", icd10_block_cancer_long) %>% 
    left_join(icd10_category_cancer,by="icd10")
  
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
    left_join(d_prepped0 %>%  dplyr::select(ind_id,outcome,icd10),by=c("outcome","ind_id")) %>% 
    left_join(icd10_block_cancer_long %>% dplyr::select(icd10,icd10Title_block2 = icd10Title,icd10Title_cat = icd10Title2),by="icd10") %>% 
    dplyr::mutate(icd10Title_block2 = replace_na(icd10Title_block2,"No cancer"),
                  icd10Title_cat = replace_na(icd10Title_cat,"No cancer")) %>%
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
    ~icd10Title_block2, ~survival_65_79, ~survival_80_plus,
    "Malignant neoplasms of eye, brain and other parts of central nervous system", "Low", "Low",
    "Malignant neoplasms of respiratory and intrathoracic organs", "Low", "Low",
    "No cancer", "No cancer", "No cancer",
    "Malignant neoplasms, stated or presumed to be primary, of lymphoid, haematopoietic and related tissue", "Medium", "Low",
    "Malignant neoplasms of urinary tract", "Medium", "Medium",
    "Malignant neoplasms of female genital organs", "Medium", "Low",
    "Malignant neoplasms of male genital organs", "High", "Medium",
    "Malignant neoplasms of digestive organs", "Medium", "Low",
    "Neoplasms of uncertain or unknown behaviour", "Medium", "Medium",
    "Malignant neoplasms of lip, oral cavity and pharynx", "Medium", "Medium",
    "Melanoma and other malignant neoplasms of skin", "High", "Medium",
    "Malignant neoplasms of mesothelial and soft tissue", "Low", "Low",
    "Malignant neoplasm of breast", "High", "Medium",
    "Malignant neoplasms of bone and articular cartilage", "Low", "Low",
    "Malignant neoplasms of ill-defined, secondary and unspecified sites", "Low", "Low",
    "Benign neoplasms", "High", "High",
    "Malignant neoplasms of thyroid and other endocrine glands", "High", "Medium",
    "In situ neoplasms", "High", "High") %>% 
    pivot_longer(cols=c("survival_65_79","survival_80_plus"),names_to = "age_class",values_to="survival_category") %>% 
    dplyr::mutate(age_class = factor(age_class,levels=c("survival_65_79","survival_80_plus"),
                                     labels=c("65-79","80+")))
  
  survival_group_tb = survival_group_tb %>% 
    dplyr::mutate(survival_category = case_when(age_class=="80+" & icd10Title_block2=="Malignant neoplasm of breast" ~ "High",
                                                age_class=="80+" & icd10Title_block2=="Malignant neoplasms of male genital organs" ~ "High",
                                                age_class=="80+" & icd10Title_block2=="Malignant neoplasms of digestive organs" ~ "Medium",
                                                TRUE ~ survival_category))
  
  survival_group_tb2 = icd10_block_cancer_long %>% 
    dplyr::select(icd10,icd10Title_block2=icd10Title,icd10Title_cat=icd10Title2) %>% 
    cross_join(data.frame(age_class=c("65-79","80+"))) %>% 
    left_join(survival_group_tb,by=c("icd10Title_block2","age_class")) %>% 
    #adapt
    dplyr::mutate(survival_category = case_when(
      #lympohoid
      icd10 %in% c("C81") ~ if_else(age_class == "80+", "Medium", "High"), #"Medium", "Low" for the rest
      icd10 %in% c("C82","C83","C84","C85","C88") ~ if_else(age_class == "80+", "Low", "Medium"),
      icd10 %in% c("C90","C91","C92","C93","C94","C95","C96") ~ "Low",
      #digestive
      icd10 %in% c("C18","C19","C20","C21") & age_class == "65-79" ~ "Medium",#"Medium", "Low",for the rest
      icd10 %in% c("C18","C19","C20","C21") & age_class == "80+"   ~ "Low",
      icd10 %in% c("C15","C16","C17","C22","C23","C24","C25","C26") ~ "Low",
      TRUE ~ survival_category))
  
  # survival_group_tb <- tribble(
  #   ~icd10Title_block2,                                                                                                               ~survival_category,
  #   "Malignant neoplasms of eye, brain and other parts of central nervous system",                                                    "Low",
  #   "Malignant neoplasms of respiratory and intrathoracic organs",                                                                    "Low",
  #   "No cancer",                                                                                                                      "No cancer",
  #   "Malignant neoplasms, stated or presumed to be primary, of lymphoid, haematopoietic and related tissue",                          "Medium",
  #   "Malignant neoplasms of urinary tract",                                                                                           "Medium",
  #   "Malignant neoplasms of female genital organs",                                                                                   "Medium",
  #   "Malignant neoplasms of male genital organs",                                                                                     "High",
  #   "Malignant neoplasms of digestive organs",                                                                                        "Medium",
  #   "Neoplasms of uncertain or unknown behaviour",                                                                                    "Medium",
  #   "Malignant neoplasms of lip, oral cavity and pharynx",                                                                            "Medium",
  #   "Melanoma and other malignant neoplasms of skin",                                                                                 "High",
  #   "Malignant neoplasms of mesothelial and soft tissue",                                                                             "Low",
  #   "Malignant neoplasm of breast",                                                                                                   "High",
  #   "Malignant neoplasms of bone and articular cartilage",                                                                            "Low",
  #   "Malignant neoplasms of ill-defined, secondary and unspecified sites",                                                            "Low",
  #   "Benign neoplasms",                                                                                                               "High",
  #   "Malignant neoplasms of thyroid and other endocrine glands",                                                                      "High",
  #   "In situ neoplasms" ,                                                                                                             "High")
  
  
  #adapt groups
  d_prepped = d_prepped0 %>% 
    filter(age_class %in% c("65-79","80+")) %>% #filter on 65+ as we only calculate survival for this age groups
    left_join(survival_group_tb2 %>% dplyr::select(icd10,age_class,survival_category), by=c("age_class","icd10")) %>% 
    dplyr::mutate(survival_category = if_else(icd10Title_block2=="No cancer","No cancer",survival_category)) %>% 
    dplyr::rename(cancer_type=icd10Title_block2,
                  icd10Title_block2=survival_category)
  
  ##############################################################################
  #Aggregate data
  
  #combination, used later
  df_comb =  cross_join(d_prepped %>% dplyr::select(age_class) %>% unique(),
                        d_prepped %>% dplyr::select(mean_date) %>% unique()) %>%
    cross_join(d_prepped %>% dplyr::select(all_cond = icd10Title_block2) %>% unique()) %>% 
    cross_join(d_prepped %>% dplyr::select(cancer_type) %>% unique()) %>% 
    cross_join(data.frame(cause_cancer=c(FALSE,TRUE))) %>% 
    arrange(age_class,mean_date,cause_cancer)
  
  #Total deaths
  # df_deaths = d_prepped %>% 
  #   filter(outcome=="ENDG_U_CD_GES_T") %>% 
  #   dplyr::mutate(cause_cancer=icd10Title_block2!="No cancer") %>% 
  #   group_by(age_class,mean_date,cause_cancer) %>% 
  #   dplyr::summarise(n=n(),.groups="drop")
  df_deaths = d_prepped %>%
    filter(outcome=="ENDG_U_CD_GES_T") %>% 
    dplyr::mutate(cause_cancer=icd10Title_block2!="No cancer") %>% 
    group_by(age_class,mean_date,cause_cancer) %>% 
    dplyr::summarise(n0=n(),.groups="drop") %>% 
    full_join(df_comb %>% dplyr::select(age_class,mean_date,cause_cancer) %>% distinct(),
              by=c("age_class","mean_date","cause_cancer")) %>% 
    dplyr::mutate(n0=replace_na(n0,0)) %>% 
    arrange(age_class, cause_cancer, mean_date) %>%
    group_by(age_class, cause_cancer) %>%
    mutate(n = rollmean(n0, k = k, fill = NA, align = "center")) 
  
  if(FALSE){
    d_prepped0 %>% 
      filter(age_class %in% c("65-79","80+")) %>% #filter on 65+ as we only calculate survival for this age groups
      left_join(survival_group_tb2 %>% dplyr::select(icd10,age_class,survival_category), by=c("age_class","icd10")) %>% 
      filter(outcome == "ENDG_U_CD_GES_T", icd10Title_block2 != "No cancer",
             age_class %in% c("65-79","80+")) %>% 
      group_by(age_class, icd10Title_block2,survival_category ) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      ggplot(aes(x = substr(icd10Title_block2, 1, 60), y = n, fill = survival_category )) +
      geom_col(position = "stack") +
      facet_grid(age_class~.)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_fill_manual(name="Survival",values=c("red","orange","forestgreen"),
                        breaks = c("Low","Medium","High"))
  }
  
  #not used
  df_main = d_prepped %>% 
    filter(outcome=="ENDG_U_CD_GES_T") %>% 
    group_by(age_class,mean_date, main_cond = icd10Title_block2) %>% 
    dplyr::summarise(n=n(),.groups="drop") %>% 
    full_join(df_comb %>% dplyr::select(age_class,mean_date,main_cond=all_cond) %>% distinct(),
              by=c("age_class","mean_date","main_cond")) %>% 
    dplyr::mutate(n=replace_na(n,0)) %>% 
    arrange(age_class, main_cond, mean_date) %>%
    group_by(age_class, main_cond) %>%
    mutate(n = rollmean(n, k = k, fill = NA, align = "center")) 
  
  if(FALSE){
    d_prepped %>% 
      filter(outcome=="ENDG_U_CD_GES_T") %>% 
      group_by(age_class,mean_date, main_cond = icd10Title_block2) %>% 
      dplyr::summarise(n=n(),.groups="drop") %>% 
      full_join(df_comb %>% dplyr::select(age_class,mean_date,main_cond=all_cond) %>% distinct(),
                by=c("age_class","mean_date","main_cond")) %>% 
      dplyr::mutate(n=replace_na(n,0)) %>% 
      arrange(age_class, main_cond, mean_date) %>%
      group_by(age_class, main_cond) %>%
      mutate(n = rollmean(n, k = 5, fill = NA, align = "center")) 
  }
  
  #Any condition (main or comorbidities)
  df_any <- d_prepped %>% 
    dplyr::select(ind_id, age_class,mean_date,outcome,icd10Title_block2) %>% 
    pivot_wider(names_from="outcome",values_from = "icd10Title_block2") %>% 
    dplyr::mutate(rank_a = as.numeric(factor(BEGLEIT_KRANK_A_GES_T, levels = c("No cancer", "Low", "Medium", "High"))),
                  rank_b = as.numeric(factor(BEGLEIT_KRANK_B_GES_T, levels = c("No cancer", "Low", "Medium", "High"))),
                  all_cond = case_when(ENDG_U_CD_GES_T != "No cancer" ~ ENDG_U_CD_GES_T,
                                       TRUE ~ c("No cancer", "Low", "Medium", "High")[pmax(rank_a, rank_b, na.rm = TRUE)])) %>%
    dplyr::select(-rank_a, -rank_b) %>% 
    #add variables stating whether cancer is underlying cause
    left_join(d_prepped %>% 
                filter(outcome=="ENDG_U_CD_GES_T") %>% 
                dplyr::mutate(cause_cancer=icd10Title_block2!="No cancer") %>% 
                dplyr::select(ind_id,cause_cancer), by="ind_id") %>% 
    #aggregate
    group_by(age_class,mean_date,all_cond,cause_cancer) %>% 
    dplyr::summarise(n0=n(),.groups = "drop") %>% 
    #add missing combination rows and assign 0
    full_join(df_comb %>% dplyr::select(age_class,mean_date,all_cond,cause_cancer) %>% distinct() %>% 
                filter(all_cond!="No cancer"),
              by=c("age_class","mean_date","all_cond","cause_cancer")) %>% 
    dplyr::mutate(n0=replace_na(n0,0)) %>% 
    #roll mean
    arrange(age_class, all_cond, cause_cancer,mean_date) %>%
    group_by(age_class, all_cond,cause_cancer) %>%
    mutate(n = rollmean(n0, k = k, fill = NA, align = "center")) 
  
  #Add total deaths
  df_any = df_any %>% 
    #add total number of deaths by age group and date
    full_join(df_deaths %>% 
                dplyr::select(age_class,mean_date,cause_cancer,n_deaths=n), by=c("age_class","mean_date","cause_cancer")) %>% 
    dplyr::mutate(n_deaths=replace_na(n_deaths,0))
  
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
    #aggregate number by cause_cancer (i.e., deaths from cancer and deaths with but not from cancer)
    group_by(age_class,mean_date,cause_cancer) %>% 
    dplyr::summarise(n=sum(n)) %>% 
    dplyr::mutate(cause_cancer=as.numeric(cause_cancer)) %>%
    #add aggregate number over cause_cancer (i.e., deaths with cancer)
    rbind(df_deaths  %>% 
            group_by(age_class,mean_date) %>% 
            dplyr::summarise(n=sum(n),.groups="drop") %>% 
            dplyr::mutate(cause_cancer=2)) %>% 
    filter(age_class %in% c("65-79","80+")) %>% 
    #add expected number of deaths from cancer
    rbind(res_list$data_pred_week_cause %>% 
            filter(variable=="deaths",cod_group=="Neoplasms (Cancers)",age_class %in% c("65-79","80+"),pred=="poisson",
                   date>= min(df_deaths$mean_date),date<=max(df_deaths$mean_date)) %>% 
            dplyr::mutate(cause_cancer=3) %>% 
            dplyr::select(age_class,mean_date=date,cause_cancer,n=est)) %>% 
    ggplot(aes(x=mean_date,y=n,col=factor(cause_cancer),linetype=factor(cause_cancer)))+
    annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2021-03-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray") +
    geom_line(linewidth=1)+
    annotate("text",label="",x=as.Date("2020-01-01"),y=0)+
    facet_wrap(.~age_class)+
    scale_x_date(name="",date_labels = "%b %Y")+
    scale_y_continuous(name="Number of individuals dying with cancer")+
    scale_color_manual(name="Underlying cause of death",values=c("blue","violet","black","violet"),
                       breaks = c(0,1,2,3),
                       labels = c("No cancer","Cancer","Total","Cancer (expected)"))+
    scale_linetype_manual(name="Underlying cause of death",values=c(1,1,1,3),
                          breaks = c(0,1,2,3),
                          labels = c("No cancer","Cancer","Total","Cancer (expected)"))+
    theme(legend.position = "bottom",
          panel.spacing = unit(1.5, "lines"),
          legend.margin = margin(-8, 0, 8, 0))
  
  p2 = d1 %>%
    filter( age_class %in% c("65-79","80+")) %>%
    group_by(age_class,mean_date,all_cond) %>%
    dplyr::summarise(n=sum(n),.groups="drop") %>% 
    group_by(age_class,all_cond) %>%
    dplyr::mutate(n = n/mean(n,na.rm=TRUE)) %>% ungroup() %>% 
    ggplot(aes(x=mean_date,y=n,col=all_cond))+
    annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2021-03-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray") +
    geom_line(linewidth=1)+
    facet_wrap(.~age_class)+
    scale_x_date(name="",date_labels = "%b %Y")+
    scale_y_continuous(name="Relative change in total mortality with cancer",
                       limits=c(0.5,1.5),
                       labels = scales::percent)+
    scale_color_manual(name="Condition",values=c("red","orange","forestgreen"),
                       breaks = c("Low","Medium","High"),
                       labels = c("Low-survival cancer","Medium-survival cancer","High-survival cancer"))+
    theme(legend.position = "bottom",
          panel.spacing = unit(1.5, "lines"),
          #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.margin = margin(-8, 0, 8, 0))
  
  
  if(FALSE){
    d1 %>%
      filter( age_class %in% c("65-79","80+")) %>%
      group_by(age_class,mean_date,all_cond) %>%
      dplyr::summarise(n=sum(n),.groups="drop") %>% 
      ggplot(aes(x=mean_date,y=n,col=all_cond))+
      annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2021-03-01"),
               ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "gray") +
      geom_line(linewidth=1)+
      annotate("text",label="",x=as.Date("2020-01-01"),y=0)+
      facet_wrap(.~age_class)+
      scale_x_date(name="",date_labels = "%b %y")+
      scale_y_continuous()+
      scale_color_manual(name="Condition",values=c("red","orange","forestgreen"),
                         breaks = c("Low","Medium","High"),
                         labels = c("Low-survival cancer","Medium-survival cancer","High-survival cancer"))+
      theme(legend.position = "bottom",
            panel.spacing = unit(1.5, "lines"),
            legend.margin = margin(-8, 0, 8, 0))
  }
  return(cowplot::plot_grid(p1,p2,
                            labels=c("A.","B."),
                            ncol=1))
}



