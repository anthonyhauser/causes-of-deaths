source("R/000_setup.R")

###########################################################################################################################
#Load encrypted data
wd_ofsp = "L:/UNISANTE_DESS/S_SUMAC/OFSP_2023/"
data_folder = paste0(wd_ofsp,"02_data/cause_of_death/")
if(controls$load.encrypted.data){
  library(cyphr)
  library(bag.epi.enigma)
  #library(remotes)
  #You need to install bag.epi.enigma locally (required min R version 4.1), using the remotes package
  #remotes::install_local(paste0(wd_ofsp,"/06_bag_packages/bag.epi.enigma-master@722d3883b1f.zip")
  source(paste0(data_folder,"setup.R"))
  
  kr_update_file(target = "local", keyring_name = "keyring_julien",
                 keyring_folder_remote = data_folder)
  
  cod <- sent_read_list_julien(
    kr_name = "keyring_julien",
    path_encrypted_file = paste0(data_folder,"cause_of_death2.rds.enc"), # password is ofsp keyring (not ofsp keyring 2)
    keyring_folder = data_folder
  )
  cod_colnames = names(cod)
  cod = do.call("cbind", cod) %>% as.data.frame()
  colnames(cod) = cod_colnames
  saveRDS(cod,file=paste0(code_root_path,"/data/cause_of_death2.rds"))
}

###########################################################################################################################
#load ICD10 data
list = get_icd10()
icd10_chapter_block = list$icd10_chapter_block
icd10_chapter = list$icd10_chapter
icd10_cat = list$icd10_cat
icd10_chapter

################################################################################
#load CoD data
cod_df0 = readRDS(paste0(code_root_path,"/data/cause_of_death2.rds"))

#Definition of colnames
var_df = data.frame(var=colnames(cod_df0),
                    definition=c("Death ID","Year of what","Birth year","Event month","Event year","Event calendar week","Event calendar year",
                                 "Age reached","Sex",
                                 "Primary cause of death","Secondary cause of death",
                                 "First tertiary cause of death","Second tertiary cause of death","Principal cause of death",
                                 "WORT_AKT_ST_N", "WORT_SITZ_CD_N", "Canton of residency","WORT_AKT_GEM_N","ST_AKT_N","ST_KANTON_GES_N"))
#https://www.nicer.org/assets/files/data/ncd_4.1_abbrev_version_201706.pdf

#Check missing
d = cod_df0 %>% 
  summarise(across(everything(), 
                           .fns = list(na = ~ sum(is.na(.)),
                                       empty = ~ sum(.==""),
                                       na_empty = ~ sum(is.na(.)|.=="")), 
                           .names = "{.col}.{.fn}"))
d %>% 
  pivot_longer(cols = everything(),
               names_to=c("variable","stat"),values_to ="n",names_sep = "\\.") %>% 
  arrange(-n) %>% head(n=14)


################################################################################
#Combine CoD dataset with icd10 categorization
cod_ind_df = combine_cod_icd10(cod_df0,icd10_chapter_block,icd10_cat)
#no missing title chapter or block
cod_ind_df %>% filter(is.na(icd10Title_chapter)|is.na(icd10Title_block))
#check number of deaths for each chapter
cod_ind_df %>%
  group_by(icd10Chapter,icd10Title_chapter) %>% 
  dplyr::summarise(n=n(),.groups="drop") %>% 
  right_join(icd10_chapter,by=c("icd10Chapter"="icd10Chapter","icd10Title_chapter"="icd10Title")) %>%
  mutate(n=replace_na(n,0)) %>% 
  arrange(n) %>% head(n=10)
#Check categories of deaths for Provisional assignment of new diseases (including COVID)
cod_ind_df %>% filter(grepl("U",icd10)) %>% 
  group_by(icd10Title_cat) %>% 
  dplyr::summarise(n=n(),.groups="drop")
#Check categories for suicides (intentional self-harm)
cod_ind_df %>% filter(icd10Title_block=="Intentional self-harm") %>% head()

################################################################################
#Aggregate data by age, sex, calendar week and calendar year
cod_agg_df = aggregate_cod(cod_ind_df)

cod_agg_df %>% 
  filter(cal_year %in% c(2020,2021)) %>% 
  group_by(age_class,cod_group) %>% 
  dplyr::summarise(n=sum(n),.groups="drop") %>% 
  ggplot(aes(x=cod_group,y=n))+
  geom_bar(stat = "identity")+
  facet_grid(age_class~.,scales="free_y")+
  theme_bw()+
  scale_x_discrete(labels = scales::label_wrap(32))+
  theme(axis.text.x = element_text( angle = 45,
      hjust = 1,vjust=1,size = 10))

################################################################################
#Combine CoD and pop data

#load population data and interpolate for each week of the year
pop = load_attribute_pop(cod_agg_df)
#combine
cod_agg_pop_df = inner_join(cod_agg_df,
               pop %>% dplyr::rename(n.pop=n),
               by= c("cal_year","cal_week","age_class","sex")) %>% 
  arrange(cod_group,cal_year,cal_week,age_class,sex)
saveRDS(cod_agg_pop_df,file="savepoint/cod_agg_pop_df.RDS")

###########################################################################################################################
#Run models
cod_agg_pop_df = readRDS("savepoint/cod_agg_pop_df.RDS")
#causes = cod_agg_pop_df$cod_group %>% unique() %>% setdiff(.,"COVID-19")
causes = c("Cardiovascular Diseases","External Causes","Infectious and Parasitic Diseases",
           "Mental and Neurological Disorders",
           "Neoplasms (Cancers)","No Specific Causes", "Respiratory Diseases", "Suicide")
age_classes = cod_agg_pop_df$age_class %>% unique()
#causes="Cardiovascular Diseases"
#age_classes=c("0-17")
if(FALSE){
  for(j in 1:length(age_classes)){
    for(i in 1:length(causes)){
      age_class_i = age_classes[j]
      cause_i = causes[i]
      mod4_data_pred_pand = run_stan_mod4_by_age_cod(cod_agg_pop_df,
                                                 age_class=age_class_i, cause=cause_i, run.model=TRUE, save.date="20241113")
    }
  }
  #deaths_pand_pred_sample = combine_weekly_pand_deaths_by_age_and_cause(data_all)
  #deaths_pred_sample = combine_weekly_fit_deaths_by_age_and_cause(data_all)
}
#ofsp_surveillance_101295-pr

deaths_pand_pred_sample = readRDS("results/deaths_pand_pred_sample.RDS")
deaths_pred_sample = readRDS("results/deaths_pred_sample.RDS")

################################################################################################################################################################
################################################################################################################################################################
#Plot
res_list=load_results(age_classes, causes, save.date="20241113")

#mortality, fit
res_list$data_pred_week %>% 
  filter(variable=="deaths",age_class=="65-79") %>% 
  left_join(res_list$data_pred_week %>% 
              filter(variable=="obs_deaths",age_class=="65-79") %>% dplyr::select(cod_group,obs_deaths=est,cal_year,cal_week),
            by=c("cal_year","cal_week","cod_group")) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="red",alpha=0.5,size=0.8) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(cod_1word~age_class,scales="free") +
  theme_bw()

res_list$data_pred_week %>% 
  filter(variable=="deaths",age_class=="80+") %>% 
  left_join(res_list$data_pred_week %>% 
              filter(variable=="obs_deaths",age_class=="80+") %>% dplyr::select(cod_group,obs_deaths=est,cal_year,cal_week),
            by=c("cal_year","cal_week","cod_group")) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="red",alpha=0.5,size=0.8) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(cod_1word~age_class,scales="free") +
  theme_bw()

#mortality by year
res_list$data_pred_year %>% 
  filter(variable=="deaths",age_class %in% c("65-79","80+")) %>% 
  left_join(res_list$data_pred_year %>% 
              filter(variable=="obs_deaths",age_class %in% c("65-79","80+")) %>% dplyr::select(cod_group,age_class,obs_deaths=est,cal_year),
            by=c("cal_year","cod_group","age_class")) %>% 
  ggplot() +
  geom_line(aes(x=cal_year,y=est),col="black") +
  geom_ribbon(aes(x=cal_year,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=cal_year,y=obs_deaths),col="red",alpha=0.5,size=2) +
  facet_grid(cod_1word~age_class,scales="free") +
  geom_vline(aes(xintercept=2019.5))+
  theme_bw()

res_list$data_pred_phase %>% 
  filter(variable=="deaths",age_class %in% c("65-79","80+")) %>% 
  left_join(res_list$data_pred_phase %>% 
              filter(variable=="obs_deaths",age_class %in% c("65-79","80+")) %>% dplyr::select(cod_group,age_class,obs_deaths=est,covid_phase),
            by=c("covid_phase","cod_group","age_class")) %>% 
  filter(covid_phase>0) %>% 
  left_join(covid_phase %>% dplyr::select(covid_phase=phase,n_weeks,labels),by="covid_phase") %>% 
  dplyr::mutate(est=est/n_weeks,lwb=lwb/n_weeks,upb=upb/n_weeks,obs_deaths=obs_deaths/n_weeks) %>% 
  ggplot(aes(x=covid_phase,y=est,ymin=lwb,ymax=upb)) +
  geom_line(col="black") +
  geom_ribbon(fill="black",alpha=0.15) +
  geom_point(aes(y=obs_deaths),col="red",alpha=0.5,size=2) +
  facet_grid(cod_1word~age_class,scales="free") +
  scale_x_continuous(breaks=covid_phase$phase,labels=covid_phase$labels)+
  theme_bw()+
  ylab("Number of deaths by week")+
  theme(axis.text.x = element_text( angle = 45,
                                    hjust = 1,vjust=1,size = 10))


#Excess mortality, by year
res_list$data_pred_year %>% 
  filter(variable=="excess",cal_year>=2020) %>% 
  ggplot(aes(x=cod_1word,y=est,ymin=lwb,ymax=upb,fill=cal_year,group=factor(cal_year))) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.5),
           width=0.5,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.5),
                colour="black",width=0.5,alpha=.8) +
  facet_wrap(age_class~.,scales="free",ncol=2) +
  #scale_fill_discrete(guide="none") +
  labs(x="Cause",y="Absolute excess mortality") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1))


res_list$data_pred_year %>% 
  filter(variable=="rel_excess",cal_year>=2020) %>% 
  dplyr::mutate(lwb=ifelse(is.infinite(est),NA,lwb),
                upb=ifelse(is.infinite(est),NA,upb),
                est=ifelse(is.infinite(est),NA,est)) %>% 
  ggplot(aes(x=cod_1word,y=est,ymin=lwb,ymax=upb,fill=cal_year,group=factor(cal_year))) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.5),
           width=0.5,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.5),
                colour="black",width=0.5,alpha=.8) +
  facet_wrap(age_class~.,scales="free",ncol=2) +
  #scale_fill_discrete(guide="none") +
  labs(x="Cause",y="Absolute excess mortality") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1))+
  scale_y_continuous(labels = scales::percent)



#Excess mortality, by phase
res_list$data_pred_phase %>% 
  left_join(res_list$data_pred_phase %>% 
              filter(variable=="obs_deaths",age_class %in% c("65-79","80+")) %>% dplyr::select(cod_group,age_class,obs_deaths=est,covid_phase),
            by=c("covid_phase","cod_group","age_class")) %>% 
  filter(variable=="excess",covid_phase>0) %>% 
  ggplot(aes(x=cod_1word,y=est,ymin=lwb,ymax=upb,fill=covid_phase,group=factor(covid_phase))) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.5),
           width=0.5,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.5),
                colour="black",width=0.5,alpha=.8) +
  facet_grid(age_class~.,scales="free") +
  labs(x="Cause",y="Absolute excess mortality") +
  scale_fill_continuous(breaks=covid_phase$phase,labels=covid_phase$labels)+
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1)) %>% 
  theme(axis.text.x = element_text( angle = 45,
                                    hjust = 1,vjust=1,size = 10))


res_list$data_pred_phase %>% 
  left_join(res_list$data_pred_phase %>% 
              filter(variable=="obs_deaths",age_class %in% c("65-79","80+")) %>% dplyr::select(cod_group,age_class,obs_deaths=est,covid_phase),
            by=c("covid_phase","cod_group","age_class")) %>% 
  filter(variable=="rel_excess",covid_phase>0) %>% 
  dplyr::mutate(lwb=ifelse(is.infinite(est)|is.nan(est),NA,lwb),
                upb=ifelse(is.infinite(est)|is.nan(est),NA,upb),
                est=ifelse(is.infinite(est)|is.nan(est),NA,est)) %>% 
  ggplot(aes(x=cod_1word,y=est,ymin=lwb,ymax=upb,fill=covid_phase,group=factor(covid_phase))) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.5),
           width=0.5,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.5),
                colour="black",width=0.5,alpha=.8) +
  facet_wrap(age_class~.,scales="free",ncol=2) +
  #scale_fill_discrete(guide="none") +
  scale_fill_continuous(breaks=covid_phase$phase,labels=covid_phase$labels)+
  labs(x="Cause",y="Absolute excess mortality") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1))+
  scale_y_continuous(labels = scales::percent)

#GP
res_list$year_GP %>% 
  group_by(cod_1word,age_class) %>%
  dplyr::mutate(ref=as.numeric(as.numeric(date)==min(as.numeric(date))),
                est_rel=exp(est-est[ref==1])-1,
                lwb_rel=exp(lwb-est[ref==1])-1,
                upb_rel =exp(upb - est[ref==1])-1) %>% ungroup() %>%
  ggplot(aes(x=date,y=est_rel)) +
  geom_ribbon(aes(ymin=lwb_rel,ymax=upb_rel),alpha=0.2) +
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(cod_1word~age_class,scales="free") +
  scale_y_continuous(labels = scales::percent,name="Relative change in mortality risk") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust = 1))


res_list$week_GP %>% 
  group_by(cod_1word,age_class) %>%
  dplyr::mutate(ref=as.numeric(as.numeric(corr_date)==min(as.numeric(corr_date))),
                est_rel=exp(est-est[ref==1])-1,
                lwb_rel=exp(lwb-est[ref==1])-1,
                upb_rel =exp(upb - est[ref==1])-1) %>% ungroup() %>%
  ggplot(aes(x=corr_date,y=est_rel)) +
  geom_ribbon(aes(ymin=lwb_rel,ymax=upb_rel),alpha=0.2) +
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(cod_1word~age_class,scales="free") +
  scale_y_continuous(labels = scales::percent,name="Relative change in mortality") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust = 1))
