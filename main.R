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

#check which year variables to use: we will use the EREIGNIS_KJ_GES_N, as it corresponds to the iso calendar year of the event date
d_sim = cod_df0 %>% 
  dplyr::mutate(sim1 = SJAHR_N!=EREIGNIS_JJJJ_GES_N,
                sim2 = SJAHR_N!=EREIGNIS_KJ_GES_N,
                sim3 = EREIGNIS_KJ_GES_N!=EREIGNIS_JJJJ_GES_N)
#Similarity between the two variables informing about year
d_sim %>% 
  group_by(sim1,sim2,sim3) %>% 
  dplyr::summarise(n=n())
d_sim %>% filter(sim1,sim2,sim3)#all different -> very rare
d_sim %>% filter(sim1,sim3,!sim2)#EREIGNIS_JJJJ_GES_N different -> very rare
#Dissimilarity occurs mostly at calendar week 1 and 53 (means that probably one variables give the iso week and the other the actual year)
d_sim %>% filter(sim1 | sim2 | sim3) %>% 
  group_by(EREIGNIS_KJ_GES_N,EREIGNIS_KW_GES_N) %>% 
  dplyr::summarise(n=n()) %>% filter(n>20) %>% View()
#check number of calendar weeks for the iso year EREIGNIS_KJ_GES_N, 53 weeks only for the right years
d_sim %>% 
  group_by(EREIGNIS_KJ_GES_N) %>% 
  dplyr::summarise(n_weeks = max(EREIGNIS_KW_GES_N)) %>% filter(n_weeks>52)

#Definition of colnames
var_df = data.frame(var=colnames(cod_df0),
                    definition=c("Death ID","Statistical year","Birth year","Event month","Event year","Event calendar week","Event calendar year",
                                 "Age reached","Sex",
                                 "Primary cause of death","Secondary cause of death",
                                 "First tertiary cause of death","Second tertiary cause of death","Principal cause of death",
                                 "WORT_AKT_ST_N", "WORT_SITZ_CD_N", "Canton of residency","WORT_AKT_GEM_N","ST_AKT_N","ST_KANTON_GES_N"))
#https://www.nicer.org/assets/files/data/ncd_4.1_abbrev_version_201706.pdf

#Check missing
d = cod_df0 %>% 
  dplyr::summarise(across(everything(), 
                           .fns = list(na = ~ sum(is.na(.)),
                                       empty = ~ sum(.==""),
                                       na_empty = ~ sum(is.na(.)|.=="")), 
                           .names = "{.col}.{.fn}"))
d %>% 
  pivot_longer(cols = everything(),
               names_to=c("variable","stat"),values_to ="n",names_sep = "\\.") %>% 
  arrange(-n) %>% head(n=14)


# cod_df0 %>% 
#   dplyr::select(GRUND_KRANK_GES_T, FOLGE_KRANK_GES_T, BEGLEIT_KRANK_A_GES_T, BEGLEIT_KRANK_B_GES_T, ENDG_U_CD_GES_T) %>% 
#   pivot_longer(cols = everything(),
#                names_to=c("variable","stat"),values_to ="n",names_sep = "\\.")

################################################################################
#Combine CoD dataset with icd10 categorization

#CoD: principal, primary and secondary 
if(FALSE){
  cod_ind_df = combine_cod_icd10(cod_df0,icd10_chapter_block,icd10_cat,
                                 icd_var = c("ENDG_U_CD_GES_T","GRUND_KRANK_GES_T","FOLGE_KRANK_GES_T","BEGLEIT_KRANK_A_GES_T","BEGLEIT_KRANK_B_GES_T"),
                                 filter_cod_groups = causes2)
  cod_ind_df %>% filter(outcome=="ENDG_U_CD_GES_T")
  saveRDS(cod_ind_df,paste0(code_root_path,"savepoint/cod_ind_df.RDS"))
}
cod_ind_df = readRDS(paste0(code_root_path,"savepoint/cod_ind_df.RDS"))


causes = c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
           "Respiratory Diseases", "Mental and Neurological Disorders",
           "COVID-19",
           "Neoplasms (Cancers)","Suicide","External Causes","No Specific Causes")
causes2 = c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
           "Respiratory Diseases", "Mental and Neurological Disorders",
           "COVID-19",
           "Neoplasms (Cancers)","Suicide","External Causes")

causes2_df = data.frame(cod_group=c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
                                    "Respiratory Diseases", "Mental and Neurological Disorders",
                                    "COVID-19",
                                    "Neoplasms (Cancers)","Suicide","External Causes","Other Causes"),
                        cod_group_label=c("Cardiovascular","Infectious/Parasitic",
                                    "Respiratory", "Mental/Neurological",
                                    "COVID-19",
                                    "Cancers","Suicide","External","Other"),
                        order=c(1,7,4,3,9,2,6,5,8),
                        example = c("Ischaemic heart disease, heart attack", "Sepsis",
                                   "Pulmonary disease, pneumonia, influenza", "Dementia, Alzheimer, Parkinson",
                                   "COVID-19",
                                   "Lung, breast, prostate neoplasms","Intentional self-harm/poisoning","Fall, vehicule accident",
                                   "Unspecified, diabetes, senility, organ disease")) %>%
  #add chapter of cod-10
  left_join(cod_ind_df %>% 
              filter(outcome=="ENDG_U_CD_GES_T") %>% 
              dplyr::select(cod_group,chapter) %>% unique() %>% 
              arrange(cod_group,chapter) %>% 
              group_by(cod_group) %>% 
              dplyr::summarise(chapter=paste(chapter, collapse=", ")),by="cod_group") %>% 
  arrange(order)

#Tables with categories and causes
causes2_df %>% 
  dplyr::select(`Cause`=cod_group,Examples=example,Chapters=chapter) %>% 
  flextable::flextable() %>% 
  flextable::width(j=1:3, width=c(1,1,1)*2)

#some data exploration
d = cod_ind_df %>% dplyr::select(ind_id,age,sex,cal_week,cal_year,outcome,cod_group) %>% 
  pivot_wider(id_cols=c(ind_id,age,sex,cal_week,cal_year),names_from="outcome",values_from="cod_group")
d0_cardio = cod_ind_df %>% 
  group_by(ind_id) %>% 
  dplyr::mutate(is.cardio = cod_group[outcome=="ENDG_U_CD_GES_T"]=="Cardiovascular Diseases") %>% 
  ungroup() %>% 
  filter(is.cardio) %>% select(-is.cardio)
d %>% filter(ENDG_U_CD_GES_T!="COVID-19",
             (GRUND_KRANK_GES_T=="COVID-19"|FOLGE_KRANK_GES_T=="COVID-19"|
                BEGLEIT_KRANK_A_GES_T=="COVID-19"))

d %>% filter(ENDG_U_CD_GES_T=="COVID-19",GRUND_KRANK_GES_T!="COVID-19") %>% View()

################################################################################
#Number of deaths in 2020-2021 by chapter: ENDG_U_CD_GES_T
cod_ind_df %>% 
  filter(cal_year>=2020,outcome=="ENDG_U_CD_GES_T") %>% 
  group_by(icd10Chapter) %>% 
  dplyr::summarise(n=n()) %>% ungroup() %>% 
  ggplot(aes(x=icd10Chapter,y=n)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Chapter V and VI: ENDG_U_CD_GES_T
cod_ind_df %>% 
  filter(cal_year>=2020,outcome=="ENDG_U_CD_GES_T",icd10Chapter%in% c("V","VI")) %>% 
  group_by(icd10Chapter,icd10Title_block) %>% 
  dplyr::summarise(n=n()) %>% ungroup() %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  filter(p>0.01) %>%
  ggplot(aes(x=reorder(icd10Title_block,-n),y=p)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#before 2020
cod_ind_df %>% 
  filter(cal_year<=2020,outcome=="ENDG_U_CD_GES_T",icd10Chapter%in% c("V","VI")) %>% 
  group_by(icd10Chapter,icd10Title_block,icd10,icd10Title_cat1) %>% 
  dplyr::summarise(n=n()) %>% ungroup() %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  arrange(-n)
#2020-2021
cod_ind_df %>% 
  filter(cal_year>=2020,outcome=="ENDG_U_CD_GES_T") %>% 
  group_by(cod_group,icd10Title_cat1) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%  
  dplyr::mutate(p=n/sum(n)) %>% 
  slice_max(n,n=3) %>% 
  arrange(cod_group,-n) %>% View()

cod_ind_df %>% 
  filter(cal_year<2020,outcome=="ENDG_U_CD_GES_T") %>% 
  group_by(cod_group,icd10Title_cat1) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%  
  dplyr::mutate(p=n/sum(n)) %>% 
  slice_max(n,n=3) %>% 
  arrange(cod_group,-n) %>% View()

#Distribution of the COD group of the 5 outcomes
cod_ind_df %>% 
  filter(cal_year>=2020) %>% 
  group_by(outcome,cod_group) %>% 
  dplyr::summarise(n=n()) %>% ungroup() %>% 
  filter(!is.na(cod_group)) %>% 
  dplyr::mutate(outcome=factor(outcome,
                               levels=c("ENDG_U_CD_GES_T","GRUND_KRANK_GES_T","FOLGE_KRANK_GES_T","BEGLEIT_KRANK_A_GES_T","BEGLEIT_KRANK_B_GES_T"))) %>% 
  ggplot(aes(x=outcome,y=n,fill=cod_group)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Proportion of missing secondary cause (NA) by principal cause, over year
d %>%
  group_by(ENDG_U_CD_GES_T,cal_year) %>% 
  dplyr::summarise(p=sum(is.na(BEGLEIT_KRANK_A_GES_T))/n()) %>% ungroup() %>% 
  filter(cal_year%in%c(2000:2021)) %>% 
  ggplot(aes(x=cal_year,y=p,col=ENDG_U_CD_GES_T))+geom_line()+
  theme_bw()

d0_cardio %>% filter(outcome=="FOLGE_KRANK_GES_T",cod_group=="Respiratory Diseases",
                     age>80) %>% 
  group_by(icd10Title_block) %>%
  dplyr::summarise(n=n()) %>% 
  arrange(-n)

d0_cardio %>% filter(outcome=="FOLGE_KRANK_GES_T",cod_group=="Respiratory Diseases") %>% 
  group_by(icd10Title_block,icd10Title_cat) %>%
  dplyr::summarise(n=n()) %>% 
  arrange(-n) %>%
  filter(grepl("virus",icd10Title_cat))

d0_cardio %>% filter(outcome=="FOLGE_KRANK_GES_T",cod_group=="Respiratory Diseases",
                     icd10Title_block=="Influenza and pneumonia") %>% 
  group_by(icd10Title_block,icd10Title_cat) %>%
  dplyr::summarise(n=n()) %>% 
  arrange(-n)

d0_cardio %>% filter(outcome=="FOLGE_KRANK_GES_T",icd10Title_cat=="Pneumonia, unspecified") %>% View()


################################################################################
#Cancer
principal_cause="Neoplasms (Cancers)"#principal_cause="COVID-19"
secondary_cause=c("Respiratory Diseases","Cardiovascular Diseases","Mental and Neurological Disorders",
                  "Neoplasms (Cancers)","Other Causes")
#Number of deaths by month
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year>=2020, age>=50) %>% 
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                year_month = floor_date(date, unit = "month"),
                age_group=factor(as.numeric(age>=50)+as.numeric(age>=65)+as.numeric(age>=80))) %>% 
  group_by(date,year_month,age_group) %>% 
  dplyr::summarise(n=n(),.groups="drop") %>%
  group_by(year_month,age_group) %>% 
  dplyr::summarise(n=mean(n),.groups="drop") %>%
  ggplot(aes(x=year_month,y=n,col=age_group))+
  geom_point()+
  facet_grid(age_group~.,scales="free")
#Number of deaths by week
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year>=2020, age>=50,
             BEGLEIT_KRANK_A_GES_T %in% secondary_cause) %>% 
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                age_group=factor(as.numeric(age>=50)+as.numeric(age>=65)+as.numeric(age>=80))) %>% 
  group_by(date,age_group,BEGLEIT_KRANK_A_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop") %>%
  ggplot(aes(x=date,y=n,col=age_group))+
  geom_point()+
  facet_grid(age_group~BEGLEIT_KRANK_A_GES_T,scales="free")
#Distribution of deaths according to secondary cause
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year>=2020, age>=50,
             is.na(BEGLEIT_KRANK_A_GES_T) |BEGLEIT_KRANK_A_GES_T %in% secondary_cause) %>% 
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                age_group=factor(as.numeric(age>=50)+as.numeric(age>=65)+as.numeric(age>=80))) %>% 
  group_by(date,age_group,BEGLEIT_KRANK_A_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(n_covid = sum(n),
                p=n/n_covid) %>% ungroup() %>% 
  filter(n_covid>20,date>=as.Date("2020-07-01"),date<=as.Date("2021-06-01")) %>% 
  ggplot(aes(x=date,y=p,col=age_group))+
  geom_smooth(se = FALSE, method = "loess", span = 0.8,col="gray80") +
  geom_point()+
  scale_x_date(date_breaks = "1 month", labels = scales::label_date(format = "%b"))+
  facet_grid(age_group~BEGLEIT_KRANK_A_GES_T,scales="free")


d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year>=2020, age>=50,
             is.na(BEGLEIT_KRANK_A_GES_T) |BEGLEIT_KRANK_A_GES_T %in% secondary_cause) %>% 
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                year_month = floor_date(date, unit = "month"),
                age_group=factor(as.numeric(age>=50)+as.numeric(age>=65)+as.numeric(age>=80))) %>% 
  group_by(year_month,age_group,BEGLEIT_KRANK_A_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(n_covid = sum(n),
                p=n/n_covid) %>% ungroup() %>% 
  filter(n_covid>20,year_month>=as.Date("2020-08-01"),year_month<=as.Date("2021-06-01")) %>% 
  ggplot(aes(x=year_month,y=p,col=age_group))+
  geom_smooth(se = FALSE, method = "loess", span = 0.8,col="gray80") +
  geom_point()+
  scale_x_date(date_breaks = "1 month", labels = scales::label_date(format = "%b"))+
  facet_grid(age_group~BEGLEIT_KRANK_A_GES_T,scales="free")

#COVID-19


#Respiratory disease 2011-2019
principal_cause="Respiratory Diseases"
secondary_cause=c("Cardiovascular Diseases","Mental and Neurological Disorders",
                  "Neoplasms (Cancers)","Other Causes")

#Number of deaths
#overall
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year %in% 2011:2019, age>=50,
             BEGLEIT_KRANK_A_GES_T %in% secondary_cause,cal_week<=52) %>% 
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                age_group=factor(as.numeric(age>=50)+as.numeric(age>=65)+as.numeric(age>=80))) %>% 
  group_by(cal_week,age_group,BEGLEIT_KRANK_A_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(n_resp = sum(n),
                p=n/n_resp) %>% ungroup() %>% 
  filter(n_resp>5) %>% 
  ggplot(aes(x=cal_week,y=p,col=age_group))+
  geom_smooth(se = FALSE, method = "loess", span = 0.8,col="gray80") +
  geom_point()+
  facet_grid(age_group~BEGLEIT_KRANK_A_GES_T,scales="free")

################################################################################################################################################################
principal_cause="COVID-19"
principal_cause="Mental and Neurological Disorders"
principal_cause="Respiratory Diseases"
secondary_cause=c("Respiratory Diseases","Cardiovascular Diseases","Mental and Neurological Disorders",
                  "Other Causes")
secondary_cause="Cardiovascular Diseases"

#Number of deaths
#overall
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year<2020,cal_week<53) %>%
  dplyr::mutate(age_group=factor(as.numeric(age>50)+as.numeric(age>70)+as.numeric(age>80))) %>% 
  group_by(cal_week,age_group) %>% 
  dplyr::summarise(n=n(),.groups="drop") %>%
  ggplot(aes(x=cal_week,y=n,col=age_group))+geom_line()
#by secondary cause
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year<2020,cal_week<53) %>%
  dplyr::mutate(age_group=factor(as.numeric(age>50)+as.numeric(age>70)+as.numeric(age>80))) %>% 
  group_by(cal_week,age_group,FOLGE_KRANK_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop") %>%
  ggplot(aes(x=cal_week,y=n,col=age_group))+geom_line()+
  facet_wrap(.~FOLGE_KRANK_GES_T)#+ylim(c(0,0.2))
#Distribution of secondary cause for cardiovascular primary cause
#absolute, by age over week
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year<2020,cal_week<53,
             FOLGE_KRANK_GES_T %in% secondary_cause) %>%
  dplyr::mutate(age_group=factor(as.numeric(age>50)+as.numeric(age>70)+as.numeric(age>80))) %>% 
  group_by(cal_week,age_group,FOLGE_KRANK_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>% 
  ggplot(aes(x=cal_week,y=n,col=age_group))+geom_line()+
  facet_wrap(.~FOLGE_KRANK_GES_T)+labs(title=principal_cause)#+ylim(c(0,0.2))
#relative, by age over week
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year<2020,cal_week<53,
             FOLGE_KRANK_GES_T %in% secondary_cause) %>%
  dplyr::mutate(age_group=factor(as.numeric(age>50)+as.numeric(age>70)+as.numeric(age>80))) %>% 
  group_by(cal_week,age_group,FOLGE_KRANK_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>% 
  dplyr::mutate(p=n/sum(n)) %>% ungroup() %>% 
  ggplot(aes(x=cal_week,y=p,col=age_group))+geom_line()+
  facet_wrap(.~FOLGE_KRANK_GES_T)+labs(title=principal_cause)#+ylim(c(0,0.2))

#distribution of respiratory disease over 5 calendar week groups, by year
#relative
d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year>2010,cal_year<2022,cal_week<53,age>70) %>%
  dplyr::mutate(cal_week=as.numeric(cut(cal_week,c(0,10,20,30,40,55)))) %>% 
  group_by(cal_week,cal_year,FOLGE_KRANK_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  filter(FOLGE_KRANK_GES_T%in%secondary_cause) %>% 
  dplyr::mutate(year_epi = as.factor(cal_year %in% c(2015,2017,2020,2021))) %>% 
  ggplot(aes(x=cal_week,y=p,col=factor(cal_year),alpha=year_epi),group=cal_year)+
  geom_point()+geom_line()+
  annotate("text",x=1,y=0,label="")+
  facet_wrap(.~FOLGE_KRANK_GES_T,scales="free")+
  scale_alpha_manual(values=c(0.15,1))+labs(title=principal_cause)
#absolute
d %>% filter(FOLGE_KRANK_GES_T=="COVID-19") %>% 
  group_by(ENDG_U_CD_GES_T) %>% dplyr::summarise(n())

d %>% filter(ENDG_U_CD_GES_T==principal_cause,cal_year>2010,cal_year<2022,cal_week<53,age>70) %>% 
  dplyr::mutate(cal_week=as.numeric(cut(cal_week,c(0,10,20,30,40,55)))) %>% 
  group_by(cal_week,cal_year,FOLGE_KRANK_GES_T) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>% 
  filter(FOLGE_KRANK_GES_T%in%secondary_cause | is.na(FOLGE_KRANK_GES_T)) %>% 
  dplyr::mutate(year_epi = as.factor(cal_year %in% c(2015,2017,2020,2021))) %>% 
  ggplot(aes(x=cal_week,y=n,col=factor(cal_year),alpha=year_epi),group=cal_year)+
  geom_point()+geom_line()+
  annotate("text",x=1,y=0,label="")+
  facet_wrap(.~FOLGE_KRANK_GES_T,scales="free")+
  scale_alpha_manual(values=c(0.15,1))+labs(title=principal_cause)

################################################################################
#Random forest: predict covid
library(randomForest)
library(caret)
d1 = cod_ind_df %>%
  filter(cal_year>=2020) %>% 
  dplyr::select(ind_id,outcome,cod_group) %>% 
  dplyr::mutate(cod_group=replace_na(cod_group,"missing")) %>% 
  pivot_wider(id_cols=ind_id,names_from = "outcome",values_from="cod_group") %>% 
  dplyr::mutate(covid=as.factor(ENDG_U_CD_GES_T=="COVID-19")) %>% 
  dplyr::select(covid,GRUND_KRANK_GES_T,FOLGE_KRANK_GES_T,
                BEGLEIT_KRANK_A_GES_T,BEGLEIT_KRANK_B_GES_T) %>% 
  filter(GRUND_KRANK_GES_T!="COVID-19",
         FOLGE_KRANK_GES_T!="COVID-19")
 
set.seed(123)  # For reproducibility
train_index <- sample(seq_len(nrow(d1)), size = 0.7 * nrow(d1))  # 70% training
train_data <- d1[train_index, ]
test_data <- d1[-train_index, ]

d1 %>% 
  group_by(covid) %>% dplyr::summarise(n())

rf_model <- randomForest(covid ~ .,
                       data=train_data ,
                       ntree=500, mtry=2,
                       importance=TRUE)
print(rf_model)
predictions <- predict(rf_model, newdata = test_data, type = "response")
table(predictions)
confusionMatrix(predictions, test_data$covid)
################################################################################
library(ggalluvial)
cod_ind_df %>% 
  filter(cal_year>=2020) %>% 
  dplyr::mutate(outcome=factor(outcome,levels=c("GRUND_KRANK_GES_T","ENDG_U_CD_GES_T"),
                               labels=c("Primary cause (physician)","Principal cause (FSO)"))) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group,
                                   labels=causes2_df$cod_group_label)) %>% 
  filter(!is.na(outcome)) %>% 
  dplyr::select(ind_id,outcome,cod_group) %>% 
  ggplot(aes(x = outcome, stratum = cod_group, alluvium = ind_id,
              fill = cod_group)) +
  geom_flow(curve_type = "cubic")+
  geom_stratum()+
  scale_fill_discrete(name="Cause of death")+
  scale_y_continuous(name="Number of deaths (2020-2021)")+
  scale_x_discrete(name="")

cod_ind_df %>% 
  filter(cal_year>=2020) %>% 
  dplyr::mutate(outcome=factor(outcome,levels=c("FOLGE_KRANK_GES_T","ENDG_U_CD_GES_T"),
                               labels=c("Secondary cause (direct)","Principal cause (FSO)"))) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group,
                                   labels=causes2_df$cod_group_label)) %>% 
  filter(!is.na(outcome)) %>% 
  dplyr::select(ind_id,outcome,cod_group) %>% 
  ggplot(aes(x = outcome, stratum = cod_group, alluvium = ind_id,
             fill = cod_group)) +
  geom_flow(curve_type = "cubic")+
  geom_stratum()+
  scale_fill_discrete(name="Cause of death")+
  scale_y_continuous(name="Number of deaths (2020-2021)")+
  scale_x_discrete(name="")

cod_ind_df %>%
  filter(cal_year>=2020,age>=65,age<80) %>% 
  dplyr::mutate(outcome=factor(outcome,levels=c("ENDG_U_CD_GES_T","GRUND_KRANK_GES_T"),
                               labels=c("outcome1","outcome2"))) %>% 
  filter(!is.na(outcome)) %>% 
  pivot_wider(id_cols=ind_id,names_from="outcome",values_from="cod_group") %>% 
  group_by(outcome1,outcome2) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(p=n/sum(n)) %>% ungroup() %>% 
  dplyr::mutate_at(c("outcome1","outcome2"),function(x) factor(x,levels=c(causes2,"Other Causes"))) %>%
  ggplot(aes(x = outcome2, y = fct_rev(outcome1), fill = p)) +
  geom_tile() +                          # Creates the heatmap tiles
  geom_text(aes(label = scales::percent(p, accuracy = 1)),  # Add percentage labels
            color = "black", size = 2.5) +
  scale_fill_gradient(low = "lightgreen", high = "lightblue",
                      labels=scales::label_percent(accuracy = 1)) + # Adjust the color gradient
  labs(x = "Primary (GRUND_KRANK_GES_T)", y = "Principal (ENDG_U_CD_GES_T)", fill = "Count") + # Axis and legend labels
  theme_bw() +                      # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 0))+
  scale_x_discrete(position = "top")

cod_ind_df %>%
  filter(cal_year==2021,age>=65,age<80) %>% 
  dplyr::mutate(outcome=factor(outcome,levels=c("ENDG_U_CD_GES_T","BEGLEIT_KRANK_A_GES_T"),
                               labels=c("outcome1","outcome2"))) %>% 
  filter(!is.na(outcome)) %>% 
  pivot_wider(id_cols=ind_id,names_from="outcome",values_from="cod_group") %>% 
  group_by(outcome1,outcome2) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(p=n/sum(n)) %>% ungroup() %>% 
  dplyr::mutate_at(c("outcome1","outcome2"),function(x) factor(x,levels=c(causes2,"Other Causes"))) %>%
  ggplot(aes(x = outcome2, y = fct_rev(outcome1), fill = p)) +
  geom_tile() +                          # Creates the heatmap tiles
  geom_text(aes(label = scales::percent(p, accuracy = 1)),  # Add percentage labels
            color = "black", size = 2.5) +
  scale_fill_gradient(low = "lightgreen", high = "lightblue",
                      labels=scales::label_percent(accuracy = 1)) + # Adjust the color gradient
  labs(x = "Primary (GRUND_KRANK_GES_T)", y = "Principal (ENDG_U_CD_GES_T)", fill = "Count") + # Axis and legend labels
  theme_bw() +                      # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 0))+
  scale_x_discrete(position = "top")

cod_ind_df %>%
  dplyr::mutate(outcome=factor(outcome,levels=c("ENDG_U_CD_GES_T","FOLGE_KRANK_GES_T"),
                               labels=c("outcome1","outcome2"))) %>% 
  filter(!is.na(outcome)) %>% 
  pivot_wider(id_cols=ind_id,names_from="outcome",values_from="cod_group") %>% 
  group_by(outcome1,outcome2) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(p=n/sum(n)) %>% ungroup() %>% 
  dplyr::mutate_at(c("outcome1","outcome2"),function(x) factor(x,levels=c(causes2,"Other Causes"))) %>% 
  ggplot(aes(x = outcome2, y = fct_rev(outcome1), fill = p)) +
  geom_tile() +                          # Creates the heatmap tiles
  geom_text(aes(label = scales::percent(p, accuracy = 1)),  # Add percentage labels
            color = "black", size = 2.5) +
  scale_fill_gradient(low = "lightgreen", high = "lightblue",
                      labels=scales::label_percent(accuracy = 1)) + # Adjust the color gradient
  labs(x = "Secondary (FOLGE_KRANK_GES_T)", y = "Principal (ENDG_U_CD_GES_T)", fill = "Count") + # Axis and legend labels
  theme_bw() +                      # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 0))+
  scale_x_discrete(position = "top")
#principal and secondary
d %>% 
  group_by(cod_group,cod_group_secondary) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(p=n/sum(n)) %>% ungroup() %>% 
  dplyr::mutate(cod_group=factor(cod_group,levels=c(causes,"Other Causes")),
                cod_group_secondary=factor(cod_group_secondary,levels=c(causes,"Other Causes"))) %>% 
  ggplot(aes(x = cod_group_secondary, y = fct_rev(cod_group), fill = p)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(p, accuracy = 1)),
            color = "black", size = 2.5) +
  scale_fill_gradient(low = "lightyellow", high = "lightblue",
                      labels=scales::label_percent(accuracy = 1)) +
  labs(x = "Secondary", y = "Principal", fill = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0))+
  scale_x_discrete(position = "top") 

########################################
#no missing title chapter or block
cod_ind_df %>% filter(is.na(icd10Title_chapter)|is.na(icd10Title_block)) %>% dim()
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
#Aggregate data by age, sex, calendar week, calendar year and cod group
cod_agg_df = aggregate_cod(cod_ind_df %>% filter(outcome=="ENDG_U_CD_GES_T"),agg_var=c("age_class","sex"))
cod_agg_nuts_df = aggregate_cod(cod_ind_df %>% filter(outcome=="ENDG_U_CD_GES_T"),agg_var=c("age_class","sex","NUTS2_id","NUTS2_name"))

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
pop_ctn = load_attribute_pop_ctn(cod_agg_nuts_df)
pop_nuts = pop_ctn %>% 
  group_by(cal_year,cal_week,age_class,sex,NUTS2_id,NUTS2_name) %>% 
  dplyr::summarise(n=sum(n),.groups="drop")

#combine
cod_agg_pop_df = inner_join(cod_agg_df,
               pop %>% dplyr::rename(n.pop=n),
               by= c("cal_year","cal_week","age_class","sex")) %>% 
  arrange(cod_group,cal_year,cal_week,age_class,sex)
saveRDS(cod_agg_pop_df,file="savepoint/cod_agg_pop_df.RDS")

cod_agg_pop_nuts_df = inner_join(cod_agg_nuts_df,
                                 pop_nuts %>% dplyr::rename(n.pop=n),
                            by= c("cal_year","cal_week","age_class","sex","NUTS2_id","NUTS2_name")) %>% 
  arrange(cod_group,cal_year,cal_week,age_class,sex,NUTS2_id)
saveRDS(cod_agg_pop_nuts_df,file="savepoint/cod_agg_pop_nuts_df.RDS")

###########################################################################################################################
#Run models: old as causes analysed simultaneously now
cod_agg_pop_df = readRDS("savepoint/cod_agg_pop_df.RDS")

#causes = cod_agg_pop_df$cod_group %>% unique() %>% setdiff(.,"COVID-19")
causes = c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
           "Respiratory Diseases", "Mental and Neurological Disorders",
           "Other Causes",
           "Neoplasms (Cancers)","Suicide","External Causes")
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

################################################################################
#Peak estimates
if(FALSE){
  #expected peak: from model estimates of expected mortality (for mod8)
  mod="mod8"; save.date="20241218";
  peak_dates_summary_df = rbindlist(lapply(as.list(age_classes),function(x){
    print(x);
    peak_dates_summary(age_class=x,chains=1:4,mod=mod,save.date=save.date)}))
  saveRDS(peak_dates_summary_df,file=paste0("results/",save.date,"/",mod,"_peak_dates_summary_df.RDS"))
  #observed peak (with smoothed data)
  observed_peak_date_df = observed_peak(cod_agg_pop_df)
  saveRDS(observed_peak_date_df,file=paste0("results/","observed_peak_date_df.RDS"))
}
################################################################################
#Cumulative excess and excess by phase
if(FALSE){
  mod="mod8"; save.date="20241218";
  list = lapply(as.list(age_classes),function(x){
    print(x);
    cumulative_excess(age_class=x,chains=1:4,mod=mod,save.date=save.date)})
  cum_excess_pand_df = rbindlist(lapply(list, `[[`, 1))
  cum_excess_allcause_pand_df = rbindlist(lapply(list, `[[`, 2))
  saveRDS(cum_excess_pand_df,file=paste0("results/",save.date,"/",mod,"_cum_excess_pand_df.RDS"))
  saveRDS(cum_excess_allcause_pand_df,file=paste0("results/",save.date,"/",mod,"_cum_excess_allcause_pand_df.RDS"))
  
  excess_phase2_pand_df = rbindlist(lapply(as.list(age_classes),function(x){
    print(x);
    aggregate_stan_group(age_class = x, chains=1:4, mod=mod, save.date=save.date,
                         groups=c("covid_phase","age_class","cod_group"))}))
  saveRDS(excess_phase2_pand_df,file=paste0("results/",save.date,"/",mod,"_excess_phase2_pand_df.RDS"))
}


################################################################################
#Correlation of excess mortality with respiratory causes
if(FALSE){
  #Correlation (both approximated by glm and estimated by the pcorr function)
  #Note: for both functions we used the mean excess mortality and for glm we provide 95%CI of the correlation estimates -> NOT POSTERIOR ESTIMATES
  mod="mod8"; save.date="20241218";
  corr_res_df = rbindlist(lapply(age_classes,function(a){
    print(a)
    corr_resp_lag_combine(age_class=a,chains=1:4,mod=mod,save.date=save.date)
  }))
  saveRDS(corr_res_df,file=paste0("results/",save.date,"/",mod,"_corr_res_df.RDS"))
  #2. Posterior estimates of the partial correlation between excess mortality samples
  #combine posterior partial correlation that were computed in the cluster (would take too much time to run it locally)
  files = list.files(pattern = paste0(mod, "_corr_resp_lag_post"),
                     path = paste0(code_root_path, "/results/", save.date),full.names = TRUE)
  corr_post_res_df = rbindlist(lapply(files,function(x) readRDS(x)))
  saveRDS(corr_post_res_df,file=paste0("results/",save.date,"/",mod,"_corr_post_res_df.RDS"))
}



files = list.files(pattern = paste0(mod, "_corr_resp_lag_post"),
  path = paste0(code_root_path, "/results/", save.date),full.names = TRUE)
corr_post_res_df = rbindlist(lapply(files,function(x) readRDS(x)))
saveRDS(corr_post_res_df,file=paste0("results/",save.date,"/",mod,"_corr_post_res_df.RDS"))
################################################################################################################################################################
################################################################################################################################################################
#Plot
cod_agg_pop_df = readRDS("savepoint/cod_agg_pop_df.RDS")
age_classes = cod_agg_pop_df$age_class %>% unique()
res_list=load_results_mod6(age_classes, save.date="20241218",mod="mod8")

cod_agg_pop_df = cod_agg_pop_df %>%
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
  mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
    phase <- phases %>%
      filter(d >= start_date & d <= end_date) %>%
      pull(phase)
    if (length(phase) == 0) NA_real_ else phase
  }))


################################################################################
#mortality data by week aggregated over causes, by age
res_list$data_pred_week_cause %>% 
  filter(age_class=="80+",variable=="obs_deaths",pred=="dispersed poisson") %>%
  dplyr::select(date,cod_group,obs_deaths=est,cal_year,cal_week,age_class) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[1:4],
                                   labels=causes2_df$cod_group_label[1:4])) %>% 
  filter(!is.na(cod_group)) %>% 
  ggplot() +
  geom_point(aes(x=date,y=obs_deaths),col="darkred",alpha=0.5,size=1) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(cod_group~.,scales="free") +
  theme_bw()+
  scale_y_continuous(name="Deaths")+
  scale_x_date(name="Time")

################################################################################
#Correlation with COVID-19
df= cod_agg_pop_df %>%
  filter(age_class=="80+") %>% 
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
  group_by(age_class,cal_year,cal_week,cod_group,date) %>%
  dplyr::summarise(n=sum(n),
                   n.pop=sum(n.pop)) %>% ungroup() %>%
  filter(cal_year>=2020) %>% dplyr::select(date,cod_group,n)  %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[c(1:4,9)],
                                   labels=causes2_df$cod_group_label[c(1:4,9)])) %>% 
  filter(!is.na(cod_group))
df %>% 
  dplyr::mutate(is.covid=factor(as.numeric(cod_group=="COVID-19"))) %>% 
  ggplot() +
  geom_point(aes(x=date,y=n,col=is.covid),alpha=0.5,size=1) +
  facet_grid(cod_group~.,scales="free") +
  theme_bw()+
  scale_y_continuous(name="Deaths")+
  scale_x_date(name="Time")+
  scale_color_manual(breaks=c(0,1),values=c("darkred","darkblue"))+
  theme(legend.position = "none")

reshaped_df <- pivot_wider(df, names_from = cod_group, values_from = n)
# Gather data for scatter plot pairs
pairwise_data <- combn(colnames(reshaped_df)[-1], 2, simplify = FALSE, FUN = function(pair) {
  X <- reshaped_df[[pair[1]]]
  Y <- reshaped_df[[pair[2]]]
  date <- reshaped_df[["date"]]
  cor_value <- cor(X, Y, use = "complete.obs")
  data.frame(
    X = X,
    Y = Y,
    date = date,
    cod_group1=pair[1],
    cod_group2=pair[2],
    Pair = paste(pair[1], "vs", pair[2], sep = " "),
    Correlation = paste("Corr =", round(cor_value, 2))
  )
}) %>% bind_rows() %>% 
  filter(grepl("COVID-19",Pair))

ggplot(pairwise_data, aes(x = X, y = Y)) +
  geom_point(color = "black", size = 3,alpha=0.5) +          # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue",fill="blue",alpha=0.1) +  # Regression line
  facet_wrap(~ Pair, scales = "free",ncol=2) +          # Facet by pairs
  geom_text(aes(label = Correlation), x = Inf, y = -Inf, 
            hjust = 1.1, vjust = -1.1, inherit.aes = FALSE) +  # Add correlation text
  theme_bw() +
  labs(x = "COVID-19 deaths",
       y = "Deaths of ...")

calculate_lagged_correlation <- function(x, y, lags) {
  correlations <- map_dfr(lags, function(lag) {
    if (lag < 0) {
      lagged_x <- x[(1 - lag):length(x)]
      lagged_y <- y[1:(length(y) + lag)]
    } else if (lag > 0) {
      lagged_x <- x[1:(length(x) - lag)]
      lagged_y <- y[(1 + lag):length(y)]
    } else {
      lagged_x <- x
      lagged_y <- y
    }
    cor_value <- cor(lagged_x, lagged_y, use = "complete.obs")
    n <- length(lagged_x)  # Sample size for this lag
    fisher_z <- atanh(cor_value)  # Fisher's z-transformation
    se <- 1 / sqrt(n - 3)  # Standard error of Fisher's z
    z_low <- fisher_z - 1.96 * se  # Lower bound of Fisher's z
    z_high <- fisher_z + 1.96 * se  # Upper bound of Fisher's z
    ci_low <- tanh(z_low)  # Transform back to correlation
    ci_high <- tanh(z_high)  # Transform back to correlation
    data.frame(Lag = lag, Correlation = cor_value, CI_Low = ci_low, CI_High = ci_high)
  })
  return(correlations)
}

# Compute correlations for all pairs of cod_group values with lags
lags <- -10:10
correlations_by_pair <- combn(colnames(reshaped_df)[-1], 2, simplify = FALSE, FUN = function(pair) {
  x <- reshaped_df[[pair[1]]]
  y <- reshaped_df[[pair[2]]]
  correlations <- calculate_lagged_correlation(x, y, lags)
  correlations$Pair <- paste(pair[1], "vs", pair[2], sep = " ")
  return(correlations)
}) %>% bind_rows() %>% 
  filter(grepl("COVID-19",Pair))

# Plot lagged correlations with uncertainty intervals
ggplot(correlations_by_pair, aes(x = Lag, y = Correlation, group = Pair)) +
  geom_hline(yintercept=0)+
  geom_ribbon(aes(ymin = CI_Low, ymax = CI_High), fill = "black", alpha = 0.1) +  # Confidence interval ribbon
  geom_line(color = "blue", size = 1) +           # Correlation line
  geom_point(size = 3, color = "blue") +           # Points for correlation values
  facet_wrap(~ Pair, scales = "fixed",ncol=2) +
  geom_vline(xintercept = 0,lty=2)+
  theme_bw() +
  ylim(c(-1,1)) +
  labs(x = "Lag (week)",
       y = "Correlation")


################################################################################
#mortality by week aggregated over causes, by age
res_list$data_pred_week %>% 
  filter(variable=="deaths") %>% 
  #add observed deaths
  left_join(res_list$data_pred_week %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,pred),
            by=c("cal_year","cal_week","age_class","pred")) %>% 
  #add covid deaths to observed deaths
  left_join(cod_agg_pop_df %>% filter(cod_group=="COVID-19") %>% 
              group_by(age_class,cal_year, cal_week) %>% 
              dplyr::summarise(n_covid=sum(n),.groups="drop"),by=c("age_class","cal_year","cal_week")) %>% 
  dplyr::mutate(obs_deaths_with_covid=obs_deaths+n_covid) %>% 
  filter(pred=="poisson") %>% 
  dplyr::mutate(obs_deaths_with_covid=ifelse(variable=="deaths" & cal_year<2020,NA,obs_deaths_with_covid)) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="darkred",alpha=0.5,size=1) +
  geom_point(aes(x=date,y=obs_deaths_with_covid),col="orange",alpha=0.4,size=1) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(age_class~.,scales="free") +
  scale_y_continuous(name="Deaths")+
  scale_x_date(name="Time")+
  theme_bw()

#mortality by week for 80+, by cause
res_list$data_pred_week_cause %>% 
  filter(variable=="deaths") %>% 
  left_join(res_list$data_pred_week_cause %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,cod_group,pred),
            by=c("cal_year","cal_week","age_class","cod_group","pred")) %>% 
  filter(age_class=="80+",pred=="poisson") %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[1:4],
                                   labels=causes2_df$cod_group_label[1:4])) %>% 
  filter(!is.na(cod_group)) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="darkred",alpha=0.5,size=1) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(cod_group~age_class,scales="free") +
  scale_y_continuous(name="Deaths")+
  scale_x_date(name="Time")+
  theme_bw()

#mortality by week for a specific cause, by age
res_list$data_pred_week_cause %>% 
  filter(variable=="deaths") %>% 
  left_join(res_list$data_pred_week_cause %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,cod_group,pred),
            by=c("cal_year","cal_week","age_class","cod_group","pred")) %>% 
  filter(cod_group=="Other Causes",pred=="poisson") %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="red",alpha=0.5,size=0.8) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(age_class~cod_group,scales="free") +
  theme_bw()

#CONSIDER REMOVING
res_list$data_pred_week_cause %>% 
  filter(variable=="deaths") %>% 
  left_join(res_list$data_pred_week_cause %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,cod_group,pred),
            by=c("cal_year","cal_week","age_class","cod_group","pred")) %>% 
  filter(cod_group=="Cardiovascular Diseases",pred=="poisson",age_class=="80+",cal_year>=2018) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="red",alpha=0.5,size=2) +
  geom_line(aes(x=date,y=obs_deaths),col="red",alpha=0.5,size=0.8) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(age_class~cod_group,scales="free") +
  theme_bw()

################################################################################
#mortality by year, by cause
res_list$data_pred_year_cause %>% 
  filter(variable=="deaths") %>% 
  #add observed deaths
  left_join(res_list$data_pred_year_cause %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,age_class,cod_group,pred),
            by=c("cal_year","cod_group","age_class","pred")) %>% 
  filter(age_class %in% c("65-79","80+"),pred=="poisson") %>% 
  ggplot() +
  geom_line(aes(x=cal_year,y=est),col="black") +
  geom_ribbon(aes(x=cal_year,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=cal_year,y=obs_deaths),col="red",alpha=0.5,size=2) +
  facet_grid(cod_group~age_class,scales="free") +
  geom_vline(aes(xintercept=2019.5))+
  theme_bw()

#mortality by year
res_list$data_pred_year %>% 
  filter(variable=="deaths") %>% 
  #add observed deaths
  left_join(res_list$data_pred_year %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,age_class,pred),
            by=c("cal_year","age_class","pred")) %>% 
  #add covid deaths to observed deaths
  left_join(cod_agg_pop_df %>% filter(cod_group=="COVID-19") %>% 
              group_by(cal_year,age_class) %>% 
              dplyr::summarise(n_covid=sum(n),.groups="drop"),by=c("age_class","cal_year")) %>% 
  dplyr::mutate(obs_deaths_with_covid=obs_deaths+n_covid) %>% 
  filter(pred=="poisson") %>% 
  ggplot() +
  geom_line(aes(x=cal_year,y=est),col="black") +
  geom_ribbon(aes(x=cal_year,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=cal_year,y=obs_deaths),col="blue",alpha=0.5,size=2) +
  geom_point(aes(x=cal_year,y=obs_deaths_with_covid),col="red",alpha=0.5,size=2) +
  facet_grid(age_class~.,scales="free") +
  geom_vline(aes(xintercept=2019.5))+
  theme_bw()

#mortality by phase
res_list$data_pred_phase %>% 
  filter(variable=="deaths") %>% 
  #add observed deaths
  left_join(res_list$data_pred_phase %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,covid_phase,age_class,pred),
            by=c("covid_phase","age_class","pred")) %>% 
  #add covid deaths to observed deaths
  left_join(cod_agg_pop_df %>% filter(cod_group=="COVID-19") %>% 
              group_by(covid_phase,age_class) %>% 
              dplyr::summarise(n_covid=sum(n),.groups="drop"),by=c("age_class","covid_phase")) %>% 
  dplyr::mutate(obs_deaths_with_covid=obs_deaths+n_covid) %>% 
  filter(pred=="poisson",covid_phase>0) %>% 
  ggplot(aes(x=covid_phase,y=est,ymin=lwb,ymax=upb)) +
  geom_line(col="black") +
  geom_ribbon(fill="black",alpha=0.15) +
  geom_point(aes(y=obs_deaths),col="blue",alpha=0.5,size=2) +
  geom_point(aes(y=obs_deaths_with_covid),col="red",alpha=0.5,size=2) +
  facet_grid(age_class~.,scales="free") +
  scale_x_continuous(breaks=covid_phase$phase,labels=covid_phase$labels)+
  theme_bw()+
  ylab("Number of deaths by week")+
  theme(axis.text.x = element_text( angle = 45,
                                    hjust = 1,vjust=1,size = 10))

################################################################################
#Excess mortality, by year, by age class
res_list$data_pred_year %>% 
  filter(variable=="excess",pred=="poisson") %>% 
  ggplot(aes(x=cal_year,y=est,ymin=lwb,ymax=upb)) +
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

#by year, by cause, 80+
res_list$data_pred_year_cause %>% 
  left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
  filter(variable=="excess",age_class=="80+",pred=="poisson") %>% 
  ggplot(aes(x=cal_year,y=est,ymin=lwb,ymax=upb)) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.5),
           width=0.5,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.5),
                colour="black",width=0.5,alpha=.8) +
  facet_wrap(cod_1word~.,scales="free",ncol=2) +
  #scale_fill_discrete(guide="none") +
  labs(x="Cause",y="Absolute excess mortality") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1))

#by cause, by age class, 2020 and 2021
covid_pred_year = cod_agg_pop_df %>% filter(cod_group=="COVID-19") %>% 
  group_by(cal_year,age_class) %>% 
  dplyr::summarise(est=sum(n),.groups="drop") %>% 
  dplyr::mutate(variable="excess", pred="poisson",cod_group="COVID-19",
                lwb=NA,upb=NA, is.stan.ok=TRUE,n_week=NA)
res_list$data_pred_year_cause %>% 
  rbind(res_list$data_pred_year %>% dplyr::mutate(cod_group="Total")) %>% 
  rbind(covid_pred_year) %>% 
  rbind(res_list$data_pred_year %>% dplyr::mutate(cod_group="Total (incl. COVID-19)") %>% 
          left_join(covid_pred_year %>% dplyr::rename(est_covid=est) %>% 
                      dplyr::select(-c(cod_group,n_week,is.stan.ok,lwb,upb))) %>% 
          dplyr::mutate(est=est+est_covid,
                        lwb=lwb+est_covid,
                        upb=upb+est_covid) %>% select(-est_covid)) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=c(causes2_df$cod_group[1:8],"Total","COVID-19","Total (incl. COVID-19)"),
                                   labels=c(causes2_df$cod_group_label[1:8],"Total","COVID-19","Total (incl. COVID-19)"))) %>% 
  #left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
  filter(variable=="excess",cal_year>=2020,pred=="poisson") %>% #!(cod_group %in% c("Total","COVID-19","Total (incl. COVID-19)"))) %>% 
  ggplot(aes(x=cod_group,y=est,ymin=lwb,ymax=upb,fill=factor(cal_year),group=factor(cal_year))) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.7),
           width=0.7,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.7),
                colour="black",width=0.4,alpha=.8) +
  facet_wrap(age_class~.,scales="free_y",ncol=2) +
  scale_fill_manual(name="Year",values=c("#009999", "#0000FF")) +
  labs(x="Cause",y="Excess mortality") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1,size=11),
        legend.position = c(1, 0),
                legend.justification = c(1, 0))

#relative excess, by cause, by age class, 2020 and 2021
res_list$data_pred_year_cause %>% 
  left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
  filter(variable=="rel_excess",cal_year>=2020,pred=="poisson") %>% 
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
covid_pred_phase = cod_agg_pop_df %>% filter(cod_group=="COVID-19") %>% 
  group_by(covid_phase,age_class) %>% 
  dplyr::summarise(est=sum(n),.groups="drop") %>% 
  dplyr::mutate(variable="excess", pred="poisson",cod_group="COVID-19",
                lwb=NA,upb=NA, is.stan.ok=TRUE)
covid_phase_df=covid_phase
res_list$data_pred_phase_cause %>% 
  rbind(covid_pred_phase) %>% 
  filter(variable=="excess",covid_phase>=2,pred=="poisson") %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=c(causes2_df$cod_group),
                                 labels=c(causes2_df$cod_group_label)),
                covid_phase = factor(covid_phase,levels=covid_phase_df$phase[3:9],
                                   labels=covid_phase_df$labels[3:9])) %>% 
  filter(age_class %in% c("65-79","80+"),cod_group!="COVID-19") %>% 
  ggplot(aes(x=cod_group,y=est,ymin=lwb,ymax=upb,fill=factor(covid_phase),group=factor(covid_phase))) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.8),
           width=0.8,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.8),
                colour="black",width=0.8,alpha=.8) +
  facet_wrap(age_class~.,scales="free_y",ncol=1) +
  scale_fill_manual(name="Phase",values=viridis_pal()(7)) +
  labs(x="Cause",y="Excess mortality") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1,size=10))

#GP
res_list$year_GP %>% 
  left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
  group_by(cod_1word,age_class) %>%
  dplyr::mutate(ref=as.numeric(as.numeric(date)==min(as.numeric(date))),
                est_rel=exp(est-est[ref==1])-1,
                lwb_rel=exp(lwb-est[ref==1])-1,
                upb_rel =exp(upb - est[ref==1])-1) %>% ungroup() %>%
  ggplot(aes(x=date,y=est_rel)) +
  geom_vline(xintercept=as.Date("2020-01-01"),lty=2)+
  geom_ribbon(aes(ymin=lwb_rel,ymax=upb_rel),alpha=0.2) +
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(cod_1word~age_class,scales="fixed") +
  scale_y_continuous(labels = scales::percent,name="Relative change in mortality risk") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust = 1))


res_list$week_GP %>% 
  left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
  group_by(cod_1word,age_class) %>%
  dplyr::mutate(ref=as.numeric(as.numeric(corr_date)==min(as.numeric(corr_date))),
                est_rel=exp(est-est[ref==1])-1,
                lwb_rel=exp(lwb-est[ref==1])-1,
                upb_rel =exp(upb - est[ref==1])-1) %>% ungroup() %>%
  ggplot(aes(x=as.Date(corr_date),y=est_rel)) +
  geom_ribbon(aes(ymin=lwb_rel,ymax=upb_rel),alpha=0.2) +
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(cod_1word~age_class,scales="fixed") +
  scale_x_date(date_labels ="%b",name="")+
  scale_y_continuous(labels = scales::percent,name="Relative change in mortality") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust = 1))


#Sigma
cod_order = res_list$Sigma_mat %>% dplyr::select(cod_group_id,cod_group) %>% unique() %>% 
  left_join(cod_df %>% dplyr::select(cod_group=cod_full,cod_1word=cod_1word),by=c("cod_group")) %>% 
  arrange(cod_group_id) %>% pull(cod_1word)
res_list$Sigma_mat %>% 
  left_join(cod_df %>% dplyr::select(cod_group=cod_full,cod_1word=cod_1word),by=c("cod_group")) %>% 
  left_join(cod_df %>% dplyr::select(cod_group2=cod_full,cod_1word2=cod_1word),by=c("cod_group2")) %>% 
  dplyr::mutate(cod_1word = factor(cod_1word, levels=cod_order),
                cod_1word2 = factor(cod_1word2, levels=cod_order)) %>% 
  filter(cod_group_id<cod_group_id2) %>% 
  ggplot(aes(x=cod_1word,y=est,ymin=lwb,ymax=upb,col=age_class))+
  geom_hline(yintercept = 0,lty=1)+
  geom_pointrange(position=position_dodge(width=0.5))+
  scale_y_continuous(limits = c(-1,1))+
  facet_grid(cod_1word2~.)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust = 1))


# res_list$Sigma_mat %>% 
#   left_join(cod_df %>% dplyr::select(cod_group=cod_full,cod_1word=cod_1word),by=c("cod_group")) %>% 
#   left_join(cod_df %>% dplyr::select(cod_group2=cod_full,cod_1word2=cod_1word),by=c("cod_group2")) %>% 
#   dplyr::mutate(cod_1word = factor(cod_1word, levels=cod_order),
#                 cod_1word2 = factor(cod_1word2, levels=cod_order)) 
res_list$Sigma_mat %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group,
                                   labels=causes2_df$cod_group_label),
                cod_group2 = factor(cod_group2,levels=causes2_df$cod_group,
                                    labels=causes2_df$cod_group_label),
                cod_group_id=as.numeric(cod_group),
                cod_group_id2=as.numeric(cod_group2)) %>% 
  rowwise() %>% 
  dplyr::mutate(est_cri = paste0(scales::percent(est, accuracy = 1),"\n",
                                 "[",scales::percent(lwb, accuracy = 1),",",
                                 scales::percent(upb, accuracy = 1),"]")) %>% 
  filter(cod_group_id<cod_group_id2,age_class=="80+") %>% 
  ggplot(aes(x = cod_group, y = fct_rev(cod_group2), fill = abs(est))) +
  geom_tile() +
  geom_text(aes(label = est_cri),
            color = "black", size = 3) +
  scale_fill_gradient(low = "lightyellow", high = "orangered1",limits=c(0,1),
                      name="Correlation",
                      labels=scales::label_percent(accuracy = 1)) +
  labs(x = "", y = "", fill = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0,size=11),
        axis.text.y = element_text(size=11))+
  scale_x_discrete(position = "top") 


res_list$sigma %>% 
  left_join(cod_df %>% dplyr::select(cod_group=cod_full,cod_1word=cod_1word),by=c("cod_group")) %>% 
  dplyr::mutate(cod_1word = factor(cod_1word, levels=cod_order)) %>% 
  ggplot(aes(x=cod_1word,y=est,ymin=lwb,ymax=upb,col=age_class))+
  geom_pointrange(position=position_dodge(width=0.5))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust = 1))

#Sex
res_list$sex_effect %>%
  filter(sex=="F") %>%
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group,
                                   labels=causes2_df$cod_group_label)) %>% 
  ggplot(aes(x=cod_group,y=mean,ymin=`2.5%`,ymax=`97.5%`,col=age_class))+
  geom_pointrange(position=position_dodge(width=0.5),fatten=6)+
  geom_hline(aes(yintercept=0),lty=2)+
  scale_y_continuous(name="Mortality risk ratio (RR), women vs men",
                     limits=log(c(0.1,2.1)),
                     breaks=log(c(0.1,0.2,0.5,1,2,5,10)),#log(c(-0.9,-0.75,-0.5,0,1,2)+1),
                     labels = exp)+
  theme_bw()+
  scale_color_manual(name="Age class",breaks=age_classes,values=viridis_pal()(5))+
  scale_x_discrete(name="")+
  theme(axis.text.x = element_text( angle = 45,
                                    hjust = 1,vjust=1,size = 11))

#NUTS
res_list$nuts_effect %>%
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group,
                                   labels=causes2_df$cod_group_label)) %>% 
  filter(age_class=="80+") %>% 
  ggplot(aes(x=cod_group,y=mean,ymin=`2.5%`,ymax=`97.5%`,col=NUTS2_name))+
  geom_pointrange(position=position_dodge(width=0.65),fatten=6)+
  geom_hline(aes(yintercept=0),lty=2)+
  scale_y_continuous(name="Mortality risk ratio (RR)",
                     limits=log(c(0.3,2.1)),
                     breaks=log(c(0.1,0.2,0.5,1,2,5,10)),#log(c(-0.9,-0.75,-0.5,0,1,2)+1),
                     labels = exp)+
  facet_wrap(.~age_class)+
  scale_x_discrete(name="")+
  theme(axis.text.x = element_text( angle = 45,
                                    hjust = 1,vjust=1,size = 11))+
  scale_color_discrete(name="NUTS2 region")


#Correlation
data = cod_agg_pop_df %>% 
  filter(age_class==.env$age_class) %>% #cod_group!="COVID-19") %>% 
  filter(cod_group %in% c(causes,"COVID-19")) %>% 
  dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                age_id = as.numeric(age_class),
                cod_group_id = as.numeric(factor(cod_group,levels=c(causes,"COVID-19"))),
                #year.id = cal_year-min(cal_year)+1,
                date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1")),
                week.id = as.numeric(1+(date-min(date))/7)) %>% #week.id = dense_rank(date)) %>% 
  dplyr::mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
    phase <- phases %>%
      filter(d >= start_date & d <= end_date) %>%
      pull(phase)
    if (length(phase) == 0) NA_real_ else phase
  })) %>% 
  arrange(cod_group_id,week.id)

#aggregate
df = cod_agg_pop_df %>%
  filter(age_class=="80+") %>% 
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
  group_by(age_class,cal_year,cal_week,cod_group,date) %>%
  dplyr::summarise(n=sum(n),
                   n.pop=sum(n.pop)) %>% ungroup() %>%
  filter(cal_year>=2020) %>% dplyr::select(date,cod_group,n)  %>% 
  dplyr::filter(cod_group %in% c("Cardiovascular Diseases","Respiratory Diseases",
                                 "Mental and Neurological Disorders","COVID-19"))#"Neoplasms (Cancers)"

if(FALSE){
  size_df=200
  df_lag=1
  sim_values=cumsum(rnorm(size_df+df_lag,0,1))
  df0 = data.frame(t=1:size_df,
                   y=sim_values[1:size_df],#X predicts Y 
                   x=sim_values[(1+df_lag):(size_df + df_lag)])
  df=data.frame(t=rep(df0$t,2),
                cod_group=rep(c("X","Y"),each=size_df),
                n=c(df0$x,df0$y))
  df %>% 
    ggplot(aes(x=t,y=n,col=cod_group))+
    geom_line()
}

# Reshape the data so that each cod_group becomes a column
reshaped_df <- pivot_wider(df, names_from = cod_group, values_from = n)
# Gather data for scatter plot pairs
pairwise_data <- combn(colnames(reshaped_df)[-1], 2, simplify = FALSE, FUN = function(pair) {
  X <- reshaped_df[[pair[1]]]
  Y <- reshaped_df[[pair[2]]]
  date <- reshaped_df[["date"]]
  cor_value <- cor(X, Y, use = "complete.obs")
  data.frame(
    X = X,
    Y = Y,
    date = date,
    Pair = paste(pair[1], "vs", pair[2], sep = " "),
    Correlation = paste("Corr =", round(cor_value, 2))
  )
}) %>% bind_rows()

# Plot scatter plots with facet_wrap
ggplot(pairwise_data, aes(x = X, y = Y)) +
  geom_point(color = "blue", size = 3) +          # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
  facet_wrap(~ Pair, scales = "free") +          # Facet by pairs
  geom_text(aes(label = Correlation), x = Inf, y = -Inf, 
            hjust = 1.1, vjust = -1.1, inherit.aes = FALSE) +  # Add correlation text
  theme_bw() +
  labs(title = "Scatter Plots with Regression Line and Correlation",
       x = "Value of Cod Group 1",
       y = "Value of Cod Group 2")
pairwise_data %>% 
  pivot_longer(cols=c("X",Y),names_to = "variable",values_to = "n") %>% 
  ggplot(aes(x =date,y=n,col=variable)) +
  geom_line()+
  facet_wrap(~ Pair, scales = "free") +          # Facet by pairs
  geom_text(aes(label = Correlation), x = Inf, y = -Inf, 
            hjust = 1.1, vjust = -1.1, inherit.aes = FALSE) +  # Add correlation text
  theme_bw() +
  labs(title = "Scatter Plots with Regression Line and Correlation",
       x = "Value of Cod Group 1",
       y = "Value of Cod Group 2")

calculate_lagged_correlation <- function(x, y, lags) {
  correlations <- map_dfr(lags, function(lag) {
    if (lag < 0) {
      lagged_x <- x[(1 - lag):length(x)]
      lagged_y <- y[1:(length(y) + lag)]
    } else if (lag > 0) {
      lagged_x <- x[1:(length(x) - lag)]
      lagged_y <- y[(1 + lag):length(y)]
    } else {
      lagged_x <- x
      lagged_y <- y
    }
    cor_value <- cor(lagged_x, lagged_y, use = "complete.obs")
    n <- length(lagged_x)  # Sample size for this lag
    fisher_z <- atanh(cor_value)  # Fisher's z-transformation
    se <- 1 / sqrt(n - 3)  # Standard error of Fisher's z
    z_low <- fisher_z - 1.96 * se  # Lower bound of Fisher's z
    z_high <- fisher_z + 1.96 * se  # Upper bound of Fisher's z
    ci_low <- tanh(z_low)  # Transform back to correlation
    ci_high <- tanh(z_high)  # Transform back to correlation
    data.frame(Lag = lag, Correlation = cor_value, CI_Low = ci_low, CI_High = ci_high)
  })
  return(correlations)
}

# Compute correlations for all pairs of cod_group values with lags
lags <- -10:10
correlations_by_pair <- combn(colnames(reshaped_df)[-1], 2, simplify = FALSE, FUN = function(pair) {
  x <- reshaped_df[[pair[1]]]
  y <- reshaped_df[[pair[2]]]
  correlations <- calculate_lagged_correlation(x, y, lags)
  correlations$Pair <- paste(pair[1], "vs", pair[2], sep = " ")
  return(correlations)
}) %>% bind_rows()

# Plot lagged correlations with uncertainty intervals
ggplot(correlations_by_pair, aes(x = Lag, y = Correlation, group = Pair)) +
  geom_hline(yintercept=0)+
  geom_ribbon(aes(ymin = CI_Low, ymax = CI_High), fill = "black", alpha = 0.1) +  # Confidence interval ribbon
  geom_line(color = "blue", size = 1) +           # Correlation line
  geom_point(size = 3, color = "red") +           # Points for correlation values
  facet_wrap(~ Pair, scales = "fixed") +
  geom_vline(xintercept = 0,lty=2)+
  theme_bw() +
  ylim(c(-1,1)) +
  labs(title = "Lagged Correlations Between Cod Groups with Confidence Intervals",
       x = "Lag (week)",
       y = "Correlation")





df=res_list$data_pred_week_cause %>% 
  filter(variable=="excess",pred=="poisson",cal_year>=2020,age_class=="80+") %>% 
  dplyr::select(cod_group,date,est,lwb,upb) %>% 
  rbind(
    cod_agg_pop_df %>% filter(cod_group=="COVID-19",cal_year>=2020,age_class=="80+") %>% 
    dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
    group_by(cod_group,date) %>% 
    dplyr::summarise(est=sum(n),lwb=NA,upb=NA,.groups="drop")
    ) %>% 
  arrange(date,cod_group) %>% 
  dplyr::select(date,cod_group,n=est) %>% 
  dplyr::filter(cod_group %in% c("Cardiovascular Diseases","Respiratory Diseases",
                                 "Mental and Neurological Disorders","COVID-19"))





res_list$data_pred_week_cause

#mortality by week aggregated over causes, by age
res_list$data_pred_week_cause %>% 
  filter(variable=="excess",pred=="poisson") %>% 
  dplyr::select(cod_group,date,est,lwb,upb)

res_list$data_pred_week_cause %>% 
  filter(variable=="obs_deaths",pred=="poisson",cod_group=="COVID-19") %>% 
  dplyr::select(cod_group,date,est,lwb,upb)
  #add observed deaths
  left_join(res_list$data_pred_week %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,pred),
            by=c("cal_year","cal_week","age_class","pred")) %>% 
  #add covid deaths to observed deaths
  left_join(cod_agg_pop_df %>% filter(cod_group=="COVID-19") %>% 
              group_by(age_class,cal_year, cal_week) %>% 
              dplyr::summarise(n_covid=sum(n),.groups="drop"),by=c("age_class","cal_year","cal_week")) %>% 
  dplyr::mutate(obs_deaths_with_covid=obs_deaths+n_covid) %>% 
  filter(pred=="dispersed poisson") %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="blue",alpha=0.5,size=0.8) +
  geom_point(aes(x=date,y=obs_deaths_with_covid),col="red",alpha=0.5,size=0.8) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(age_class~.,scales="free") +
  theme_bw()

#mortality by week for 80+, by cause
res_list$data_pred_week_cause %>% 
  filter(variable=="deaths") %>% 
  left_join(res_list$data_pred_week_cause %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,cod_group,pred),
            by=c("cal_year","cal_week","age_class","cod_group","pred")) %>% 
  filter(age_class=="80+",pred=="poisson") %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="red",alpha=0.5,size=0.8) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(cod_group~age_class,scales="free") +
  theme_bw()


