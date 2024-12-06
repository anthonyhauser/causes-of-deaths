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
causes = c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
           "Respiratory Diseases", "Mental and Neurological Disorders",
           "COVID-19",
           "Neoplasms (Cancers)","Suicide","External Causes","No Specific Causes")
causes2 = c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
           "Respiratory Diseases", "Mental and Neurological Disorders",
           "COVID-19",
           "Neoplasms (Cancers)","Suicide","External Causes")

#CoD: principal, primary and secondary 
cod_ind_df = combine_cod_icd10(cod_df0,icd10_chapter_block,icd10_cat,icd_var = "ENDG_U_CD_GES_T",
                               filter_cod_groups = causes)
cod_ind_df2 = combine_cod_icd10(cod_df0,icd10_chapter_block,icd10_cat,icd_var = "ENDG_U_CD_GES_T",
                               filter_cod_groups = causes2)
cod_ind_df = combine_cod_icd10(cod_df0,icd10_chapter_block,icd10_cat,icd_var = "ENDG_U_CD_GES_T",
                               filter_cod_groups = causes)
cod_ind_df_primary = combine_cod_icd10(cod_df0,icd10_chapter_block,icd10_cat,icd_var = "GRUND_KRANK_GES_T",
                                       filter_cod_groups = causes)
cod_ind_df_secondary = combine_cod_icd10(cod_df0,icd10_chapter_block,icd10_cat,icd_var = "FOLGE_KRANK_GES_T",
                                         filter_cod_groups = causes)

#plot
d = cod_ind_df %>% 
  left_join(cod_ind_df_primary %>% dplyr::select(ind_id, cod_group_primary=cod_group),by="ind_id") %>% 
  left_join(cod_ind_df_secondary %>% dplyr::select(ind_id, cod_group_secondary=cod_group),by="ind_id")
#principal and primary
d %>% 
  group_by(cod_group,cod_group_primary) %>% 
  dplyr::summarise(n=n(),.groups="drop_last") %>%
  dplyr::mutate(p=n/sum(n)) %>% ungroup() %>% 
  dplyr::mutate(cod_group=factor(cod_group,levels=c(causes,"Other Causes")),
                cod_group_primary=factor(cod_group_primary,levels=c(causes,"Other Causes"))) %>% 
  ggplot(aes(x = cod_group_primary, y = fct_rev(cod_group), fill = p)) +
  geom_tile() +                          # Creates the heatmap tiles
  geom_text(aes(label = scales::percent(p, accuracy = 1)),  # Add percentage labels
            color = "black", size = 2.5) +
  scale_fill_gradient(low = "lightgreen", high = "lightblue",
                      labels=scales::label_percent(accuracy = 1)) + # Adjust the color gradient
  labs(x = "Secondary", y = "Principal", fill = "Count") + # Axis and legend labels
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
cod_agg_df = aggregate_cod(cod_ind_df)
cod_agg_df2 = aggregate_cod(cod_ind_df2)

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
pop2 = load_attribute_pop(cod_agg_df2)
#combine
cod_agg_pop_df = inner_join(cod_agg_df,
               pop %>% dplyr::rename(n.pop=n),
               by= c("cal_year","cal_week","age_class","sex")) %>% 
  arrange(cod_group,cal_year,cal_week,age_class,sex)
saveRDS(cod_agg_pop_df,file="savepoint/cod_agg_pop_df.RDS")
cod_agg_pop_df2 = inner_join(cod_agg_df2,
                            pop2 %>% dplyr::rename(n.pop=n),
                            by= c("cal_year","cal_week","age_class","sex")) %>% 
  arrange(cod_group,cal_year,cal_week,age_class,sex)
saveRDS(cod_agg_pop_df2,file="savepoint/cod_agg_pop_df2.RDS")

###########################################################################################################################
#Run models
cod_agg_pop_df = readRDS("savepoint/cod_agg_pop_df2.RDS")

#causes = cod_agg_pop_df$cod_group %>% unique() %>% setdiff(.,"COVID-19")
causes = c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
           "Respiratory Diseases", "Mental and Neurological Disorders",
           "Other Causes",
           "Neoplasms (Cancers)","Suicide","External Causes")
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
res_list=load_results_mod6(age_classes, save.date="20241204")

cod_agg_pop_df = cod_agg_pop_df %>%
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
  mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
    phase <- phases %>%
      filter(d >= start_date & d <= end_date) %>%
      pull(phase)
    if (length(phase) == 0) NA_real_ else phase
  }))


#mortality risk by sex
# res_list$reg_effect %>% 
#   filter(var=="sex") %>% 
#   ggplot(aes(x=cod_1word,y=mean,ymin=`2.5%`,ymax=`97.5%`,col=age_class))+
#   geom_pointrange(position=position_dodge(width=0.5))+
#   geom_hline(aes(yintercept=0),lty=2)+
#   scale_y_continuous(name="Mortality risk ratio (RR)",
#                      limits=log(c(0.1,2)),
#                      breaks=log(c(0.1,0.2,0.5,1,2,5,10)),#log(c(-0.9,-0.75,-0.5,0,1,2)+1),
#                      labels = exp)+# labels = function(x){return(scales::percent(exp(x)-1))})+
#   theme_bw()+
#   theme(axis.text.x = element_text( angle = 45,
#                                     hjust = 1,vjust=1,size = 10))

#mortality, fit
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
  filter(pred=="dispersed poisson") %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="blue",alpha=0.5,size=0.8) +
  geom_point(aes(x=date,y=obs_deaths_with_covid),col="red",alpha=0.5,size=0.8) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(age_class~.,scales="free") +
  theme_bw()

#mortality, fit, by cause
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
res_list$data_pred_year_cause %>% 
  left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
  filter(variable=="excess",cal_year>=2020,pred=="poisson") %>% 
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
res_list$data_pred_phase_cause %>% 
  left_join(cod_df,by=c("cod_group"="cod_full")) %>% 
  filter(variable=="excess",covid_phase>0,pred=="poisson") %>% 
  ggplot(aes(x=cod_1word,y=est,ymin=lwb,ymax=upb,fill=covid_phase,group=factor(covid_phase))) +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(position = position_dodge(width=0.8),
           width=0.8,alpha=.5) +
  # geom_point(position = position_dodge(width=0.5),
  #               colour="black",alpha=.8) +
  geom_errorbar(position = position_dodge(width=0.8),
                colour="black",width=0.8,alpha=.8) +
  facet_wrap(age_class~.,scales="free",ncol=2) +
  #scale_fill_discrete(guide="none") +
  labs(x="Cause",y="Absolute excess mortality") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust = 1))

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
  ggplot(aes(x=corr_date,y=est_rel)) +
  geom_ribbon(aes(ymin=lwb_rel,ymax=upb_rel),alpha=0.2) +
  geom_point(size=0.5)+
  geom_line()+
  facet_grid(cod_1word~age_class,scales="fixed") +
  scale_y_continuous(labels = scales::percent,name="Relative change in mortality") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust = 1))



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


res_list$Sigma_mat %>% 
  left_join(cod_df %>% dplyr::select(cod_group=cod_full,cod_1word=cod_1word),by=c("cod_group")) %>% 
  left_join(cod_df %>% dplyr::select(cod_group2=cod_full,cod_1word2=cod_1word),by=c("cod_group2")) %>% 
  dplyr::mutate(cod_1word = factor(cod_1word, levels=cod_order),
                cod_1word2 = factor(cod_1word2, levels=cod_order)) %>% 
  rowwise() %>% 
  dplyr::mutate(est_cri = paste0(scales::percent(est, accuracy = 1),"\n",
                                 "[",scales::percent(lwb, accuracy = 1),",",
                                 scales::percent(upb, accuracy = 1),"]")) %>% 
  filter(cod_group_id<cod_group_id2,age_class=="80+") %>% 
  ggplot(aes(x = cod_1word, y = fct_rev(cod_1word2), fill = abs(est))) +
  geom_tile() +
  geom_text(aes(label = est_cri),
            color = "black", size = 3) +
  scale_fill_gradient(low = "lightyellow", high = "orangered1",limits=c(0,1),
                      name="Correlation",
                      labels=scales::label_percent(accuracy = 1)) +
  labs(x = "", y = "", fill = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0))+
  scale_x_discrete(position = "top") 
