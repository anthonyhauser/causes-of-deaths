##########################################
#load arguments from ubelix array
# args9="20231129"
# for(args1 in c(3)){
#   for(args2 in c(2)){
#     for(args3 in c(3)){
#       for(args4 in c(1,2,3)){
#         for(args5 in c(1)){
#  args=c(args1,args2,args3,args4,args5,NA,NA,NA,args9) #args_all=c(4,7,1,4,1,1,1,1,20240620)

##########################################
#Setting up paths
is.sim.cluster=TRUE
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster")[[1]][1],"/")
print(code_root_path)
source(paste0(code_root_path,"R/setup.R"))

args_all=(commandArgs(TRUE))
args=as.numeric(unlist(args_all[1:8]))#args[9] should be simulation date
save.date=as.character(args_all[[9]])

print("print args")
print(args)
print(save.date)

country = c("NL","BE","UK","DE")[args[1]]
age.tag = c("v1","v2","v3","v4","v5","v6","v7","v8")[args[2]]
filter.day = c("","weekdays","weekdays_nobankhol","multrep")[args[3]]
model_name = c("mod7_ind_prior2_dev2","mod7_ind_prior2_dev2_re10","mod8_ind_prior2_dev2","mod8_ind_prior2_dev2_re10",
               "mod8_ind_prior2_dev4","mod8_ind_prior2_dev4_re10",
               "mod8_ind_prior2_dev5","mod8_ind_prior2_dev5_re10")[args[4]]
#model_name = c("mod7_ind_nonrec","mod7_ind_nonrec_re2", "mod8_ind","mod8_ind_re2")[args[4]]
# c("mod7_ind","mod7_ind_nonrec","mod7_ind_nonrec_re","mod7_ind_nonrec_re2",
#              "mod8_ind","mod8_ind_re","mod8_ind_re2")[args[4]]
recruit_wave = list(c(1),c(1,9))[[args[5]]]

country=country
zenodo.id = zenodo.id.df[country]
survey.year=2020
count.locations=""
age.tag = age.tag
n_contacts_cutoff=100
missing.contact.age="keep"
keep.age.part = "adult"

reciprocity=FALSE
filter.day=filter.day
model_name=model_name
location_x="work"
wave_x=n.wave.df[[country]]
clean.data=TRUE
save.date=save.date
return.data=FALSE
recruit_wave = recruit_wave
adapt_delta=0.8
rerun=TRUE


save.date.short = (strsplit(save.date,"_")[[1]])[1]
save.date.year = (strsplit(save.date,"_")[[1]])[2]
if(is.na(save.date.year)){
	  wave_x = n.wave.df[[country]]
}else{
	  wave_x = get(paste0("n.wave.df_",save.date.year))[[country]]
}

save.file = run_mod_ind(country=country,
                        zenodo.id = zenodo.id.df[country],
                        survey.year=2020,
                        count.locations="", #("","multcount","_extraloc"),
                        age.tag = age.tag, #c("v1","v2","v3","v4"),
                        n_contacts_cutoff=100,
                        missing.contact.age=ifelse(country=="DE","keep","remove"),#c("keep","remove")
                        keep.age.part = "adult",

                        reciprocity=FALSE,
                        filter.day=filter.day, #c("","weekdays","weekdays_nobank_hol")
                        model_name=model_name,#c("mod7_ind_nonrep","mod8_ind")
                        location_x="work",
                        wave_x = wave_x,#n.wave.df[[country]],
                        clean.data=FALSE,
                        save.date=save.date,
                        return.data=FALSE,
                        recruit_wave = recruit_wave,
                        adapt_delta=0.99,#0.95,
                        rerun=FALSE)
print(save.file)
# }}}}}

# country="UK"
# survey_data_stan0 = arrange_data_ind(survey.country = country, #takes less than 1 minute, saves results
#                                      zenodo.id = zenodo.id.df[country],
#                                      survey.year = 2020,
#                                      age.tag = "v7",
#                                      by.location = TRUE,
#                                      n_contacts_cutoff = 100,
#                                      missing.contact.age = "remove",
#                                      count.locations = "")
# survey.country = country
# zenodo.id = zenodo.id.df[country]
# survey.year = 2020
# age.tag = "v7"
# by.location = TRUE
# n_contacts_cutoff = 100
# missing.contact.age = "remove"
# count.locations = ""


if(FALSE){
  filter.day="weekdays_nobankhol"
  country="DE"
  age.tag="v7"
  save.date = "20240611"

  #############################################################################
  #Model 7
  list_mod7 = mget(load(paste0(getwd(),"/cluster/cluster_results/",
         paste(c(save.date,"mod7_ind_nonrec",filter.day[filter.day!=""],country,paste0("age",age.tag),"work","remove",100),collapse="_"),".RData"),
       envir=(NE. <- new.env())), envir=NE.)
  list_mod7_re = mget(load(paste0(getwd(),"/cluster/cluster_results/",
         paste(c(save.date,"mod7_ind_nonrec_re",filter.day[filter.day!=""],country,paste0("age",age.tag),"work","remove",100),collapse="_"),".RData"),
                        envir=(NE. <- new.env())), envir=NE.)
  list_mod7_re2 = mget(load(paste0(getwd(),"/cluster/cluster_results/",
                                  paste(c(save.date,"mod7_ind_nonrec_re2",filter.day[filter.day!=""],country,paste0("age",age.tag),"work","remove",100),collapse="_"),".RData"),
                           envir=(NE. <- new.env())), envir=NE.)
  
  #p_zero
  rbind(list_mod7$p_zero %>% dplyr::mutate(mod="mod7"),
        list_mod7_re$p_zero %>% dplyr::mutate(mod="mod7_re"),
        list_mod7_re2$p_zero %>% dplyr::mutate(mod="mod7_re2")) %>% 
    dplyr::mutate(x_shift=2*as.numeric(factor(mod))) %>% 
    left_join(list_mod7$p_data_contact %>% dplyr::select(wave_id,wave,age.id.part,p_no0_sample_mean),
              by=c("wave_id","age.id.part"),multiple="all") %>%
    dplyr::mutate(p_no0_sample_mean=ifelse(mod=="mod7",p_no0_sample_mean,NA)) %>% 
    left_join(list_mod7$date_range_by_wave %>% dplyr::select(wave,min.date),by="wave") %>% 
    dplyr::mutate(age.id.part = factor(recode(age.id.part,'1'="0-10",'2'="11-18",'3'="19-64",'4'="65+"))) %>% 
    ggplot(aes(x=min.date+x_shift))+
    geom_pointrange(aes(y=1-`50%`,ymax=1-`2.5%`,ymin=1-`97.5%`,col=mod),position = position_dodge(width=0.5),size=0.2)+
    geom_point(aes(y=p_no0_sample_mean),col="black")+
    facet_grid(age.id.part~.)+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    scale_x_date(breaks="1 month") +
    ylab("Probability of contacts")
  
  #theta
  rbind(list_mod7$p_zero %>% dplyr::mutate(mod="mod7"),
        list_mod7_re$p_zero %>% dplyr::mutate(mod="mod7_re"),
        list_mod7_re2$p_zero %>% dplyr::mutate(mod="mod7_re2")) %>% 
    dplyr::mutate(x_shift=2*as.numeric(factor(mod))) %>% 
      left_join(list_mod7$p_data_contact %>% dplyr::select(wave_id,wave,age.id.part,p_no0_sample_mean),
                                  by=c("wave_id","age.id.part"),multiple="all") %>%
    dplyr::mutate(p_no0_sample_mean=ifelse(mod=="mod7",p_no0_sample_mean,NA)) %>% 
    left_join(list_mod7$date_range_by_wave %>% dplyr::select(wave,min.date),by="wave") %>% 
      dplyr::mutate(age.id.part = factor(recode(age.id.part,'1'="0-10",'2'="11-18",'3'="19-64",'4'="65+"))) %>% 
      ggplot(aes(x=min.date+x_shift))+
      geom_pointrange(aes(y=1-`50%`,ymax=1-`2.5%`,ymin=1-`97.5%`,col=mod),position = position_dodge(width=0.5),size=0.2)+
      geom_point(aes(y=p_no0_sample_mean),col="black")+
      facet_grid(age.id.part~.)+
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    scale_x_date(breaks="1 month") +
    ylab("Probability of contacts")
  
  #############################################################################
  #Model 8
  list_mod8 = mget(load(paste0(getwd(),"/cluster/cluster_results/",
                               paste(c(save.date,"mod8_ind",filter.day[filter.day!=""],country,paste0("age",age.tag),"work","remove",100,"recruitwave19"),collapse="_"),".RData"),
                        envir=(NE. <- new.env())), envir=NE.)
  list_mod8_re = mget(load(paste0(getwd(),"/cluster/cluster_results/",
                                  paste(c(save.date,"mod8_ind_re",filter.day[filter.day!=""],country,paste0("age",age.tag),"work","remove",100,"recruitwave19"),collapse="_"),".RData"),
                           envir=(NE. <- new.env())), envir=NE.)
  list_mod8_re2 = mget(load(paste0(getwd(),"/cluster/cluster_results/",
                                   paste(c(save.date,"mod8_ind_re2",filter.day[filter.day!=""],country,paste0("age",age.tag),"work","remove",100,"recruitwave19"),collapse="_"),".RData"),
                            envir=(NE. <- new.env())), envir=NE.)
  
  #p_zero
  rbind(list_mod8$p_zero %>% dplyr::mutate(mod="mod8"),
        list_mod8_re$p_zero %>% dplyr::mutate(mod="mod8_re"),
        list_mod8_re2$p_zero %>% dplyr::mutate(mod="mod8_re2")) %>% 
    dplyr::mutate(x_shift=2*as.numeric(factor(mod))) %>% 
    left_join(list_mod8$p_data_contact %>% dplyr::select(wave_id,wave,age.id.part,p_no0_sample_mean),
              by=c("wave_id","age.id.part"),multiple="all") %>%
    dplyr::mutate(p_no0_sample_mean=ifelse(mod=="mod8",p_no0_sample_mean,NA)) %>% 
    left_join(list_mod8$date_range_by_wave %>% dplyr::select(wave,min.date),by="wave") %>% 
    dplyr::mutate(age.id.part = factor(recode(age.id.part,'1'="0-10",'2'="11-18",'3'="19-64",'4'="65+"))) %>% 
    ggplot(aes(x=min.date+x_shift))+
    geom_pointrange(aes(y=1-`50%`,ymax=1-`2.5%`,ymin=1-`97.5%`,col=mod),position = position_dodge(width=0.5),size=0.2)+
    geom_point(aes(y=p_no0_sample_mean),col="black")+
    facet_grid(age.id.part~.)+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    scale_x_date(breaks="1 month") +
    ylab("Probability of contacts")
  
  #theta
  rbind(list_mod8$lambda %>% dplyr::mutate(mod="mod8"),
        list_mod8_re$lambda %>% dplyr::mutate(mod="mod8_re"),
        list_mod8_re2$lambda %>% dplyr::mutate(mod="mod8_re2")) %>% 
    dplyr::mutate(x_shift=2*as.numeric(factor(mod))) %>% 
    left_join(list_mod8$p_data_contact %>% dplyr::select(wave_id,wave,age.id.part,n_mean_over0_sample_mean),
              by=c("wave_id","age.id.part"),multiple="all") %>%
    dplyr::mutate(n_mean_over0_sample_mean=ifelse(mod=="mod8",n_mean_over0_sample_mean,NA)) %>% 
    left_join(list_mod8$date_range_by_wave %>% dplyr::select(wave,min.date),by="wave") %>% 
    dplyr::mutate(age.id.part = factor(recode(age.id.part,'1'="0-10",'2'="11-18",'3'="19-64",'4'="65+"))) %>% 
    ggplot(aes(x=min.date+x_shift))+
    geom_pointrange(aes(y=`50%`,ymax=`2.5%`,ymin=`97.5%`,col=mod),position = position_dodge(width=0.5),size=0.2)+
    geom_point(aes(y=n_mean_over0_sample_mean),col="black")+
    facet_grid(age.id.part~.)+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    scale_x_date(breaks="1 month") +
    ylab("Number of contacts") +
    coord_cartesian(ylim=c(0,50))
  
  #rho_lambda
  rbind(list_mod8$rho_lambda %>% dplyr::mutate(mod="mod8"),
        list_mod8_re$rho_lambda %>% dplyr::mutate(mod="mod8_re"),
        list_mod8_re2$rho_lambda %>% dplyr::mutate(mod="mod8_re2")) %>% 
    dplyr::mutate(x_shift=0.1*as.numeric(factor(mod))) %>% 
    ggplot(aes(x=rep_id+x_shift,y=mean,ymin=`2.5%`,ymax=`97.5%`,col=mod))+
    geom_pointrange() +
    geom_line()+
    facet_grid(.~recruit_id)+
    scale_y_continuous(name = "Relative decrease in number of contacts", limits=c(0,1)) +
    theme(legend.position = "bottom")
  
  #rho_theta
  rbind(list_mod8$rho_theta %>% dplyr::mutate(mod="mod8"),
        list_mod8_re$rho_theta %>% dplyr::mutate(mod="mod8_re"),
        list_mod8_re2$rho_theta %>% dplyr::mutate(mod="mod8_re2")) %>% 
    dplyr::mutate(x_shift=0.1*as.numeric(factor(mod))) %>% 
    ggplot(aes(x=rep_id+x_shift,y=mean,ymin=`2.5%`,ymax=`97.5%`,col=mod))+
    geom_pointrange() +
    geom_line()+
    facet_grid(.~recruit_id)+
    theme(legend.position = "bottom")+
    scale_y_continuous(name = "OR of reporting 0 contacts", trans="log",breaks=c(1,5,10))
  
  #tau of random effect
  rbind(list_mod8_re$tau_lambda %>% dplyr::mutate(mod="mod8_re"),
        list_mod8_re2$tau_lambda %>% dplyr::mutate(mod="mod8_re2"),
        list_mod7_re$tau_lambda %>% dplyr::mutate(mod="mod7_re"),
        list_mod7_re2$tau_lambda %>% dplyr::mutate(mod="mod7_re2")) %>% 
    dplyr::mutate(x_shift=0.1*as.numeric(factor(mod))) %>% 
    ggplot(aes(x=age.id.part+x_shift,y=mean,ymin=`2.5%`,ymax=`97.5%`,col=mod))+
    geom_pointrange()
  
  ######################################################################################################################
  #all locations
  save.file.data = run_mod_ind(country=country,
                               zenodo.id = zenodo.id.df[country],
                               survey.year=2020,
                               count.locations="", #("","multcount","_extraloc"),
                               age.tag = age.tag, #c("v1","v2","v3","v4"),
                               n_contacts_cutoff=100,
                               missing.contact.age="remove",#c("keep","remove")
                               
                               reciprocity=FALSE,
                               filter.day=filter.day, #c("","weekdays","weekdays_nobank_hol")
                               model_name=model_name,#c("mod7_ind","mod8_ind")
                               location_x=c("home","school","work","transport","leisure","otherplace"),
                               wave_x=n.wave.df[[country]],
                               clean.data=FALSE,
                               save.date=args[5],
                               return.data=TRUE)
  list_agev1 = mget(load(save.file.data, envir=(NE. <- new.env())), envir=NE.)
  
  #Participation by wave and age
  list_agev1$p_raw_data$p1_data %>% dplyr::mutate(data="alldays") %>% 
    group_by(wave,data) %>% 
    dplyr::mutate(n_part=sum(n_part)) %>% 
    ggplot(aes(x=wave,y=n_part,fill=data)) +
    geom_bar(position=position_dodge(width = 0.6),stat = "identity",width = 0.5) +
    scale_x_continuous(breaks=unique(list_agev1$p_raw_data$p1_data$wave))+
    theme_bw()+
    theme(legend.position = "bottom")

  #Participation by wave and age
  list_agev1$p_raw_data$p1_data %>% 
    ggplot(aes(x=wave,y=n_part,fill=age.id.part)) +
    geom_bar(position="stack",stat = "identity") +
    theme_bw()

  #Participation by wave and survey round
  list_agev1$p_raw_data$p2_data %>% 
    ggplot(aes(x=wave,y=n_part,fill=rep_id)) +
    geom_bar(position="stack",stat = "identity") +
    theme_bw()
  
  list_agev1$p_raw_data$p2_data_bis %>% 
    ggplot(aes(x=wave,y=n_part,fill=rep_id)) +
    geom_bar(position="stack",stat = "identity") +
    facet_wrap(first_wave~.)+
    theme_bw() +
    theme(legend.position = "bottom")
  
  ## Missing location and contact age
  ### Distribution of location
  list_agev1$p_raw_data$p3_data %>% 
    dplyr::mutate(location = factor(location, levels = c("home","school","work","transport","leisure","otherplace","unknown"))) %>% 
    ggplot(aes(x=location,y=prop)) +
    geom_bar(position="stack",stat = "identity") +
    scale_y_continuous(labels = scales::percent)+
    theme_bw()
  
  ### Distribution of contact age
  #Distribution of contact age (unknown, age groups)
  list_agev1$p_raw_data$p4_data %>%
    ggplot(aes(x=age.id.cont,y=prop)) +
    geom_bar(position="stack",stat = "identity") +
    scale_y_continuous(labels = scales::percent)+
    theme_bw()
  
  ### Proportion of missing location and contact age
  # Four cases :
  # 1. Known location, known contacts
  # 2. Missing location, known contact and part age: dist of location by contact and part age
  # 3. Missing contact age, known location and part age: nothing
  # 4. Missing location and contact age: dist of location by contact age
  
  list_agev1$p_raw_data$p5_data %>% 
    ggplot(aes(x=case,y=prop)) +
    geom_bar(position="stack",stat = "identity") +
    scale_y_continuous(labels = scales::percent)+
    theme_bw()
  
  ### Distribution of location
  #Distribution of location by contact and part age (used in 3)
  list_agev1$p_raw_data$p6_data %>% 
    ggplot(aes(x=location,y=p_n_obs)) +
    geom_bar(position="stack",stat = "identity") +
    scale_y_continuous(labels = scales::percent)+
    facet_grid(age.id.part~age.id.cont)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  ### Distribution of location
  #By age of participant, when age of contact is unknown (as used in case 4)
  #Distribution of location by part age (when contact age is not known), (in 4)
  list_agev1$p_raw_data$p7_data %>% 
    ggplot(aes(x=location,y=p_n2)) +
    geom_bar(position="stack",stat = "identity") +
    scale_y_continuous(labels = scales::percent)+
    facet_grid(age.id.part~.)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  ######################################################################################################################
  ## Number of contacts
  ### Average number of contacts
  list_agev1$p_data_contact %>% 
    ggplot(aes(x=wave))+
    geom_line(aes(y=mean_expected)) +
    geom_ribbon(aes(ymin=mean_min,ymax=mean_max),alpha=0.1) +
    geom_line(aes(y=n_mean_sample_mean),col="red",linetype="dashed") +
    geom_ribbon(aes(ymin=n_mean_sample_min,ymax=n_mean_sample_max),alpha=0.1,fill="red") +
    facet_grid(age.id.part~location) +
    theme_bw() +
    coord_cartesian(ylim=c(0,5))
  
  ### Average number of contacts, conditionned over 0
  list_agev1$p_data_contact %>% 
    ggplot(aes(x=wave))+
    geom_line(aes(y=n_mean_over0_sample_mean),col="red",linetype="dashed") +
    geom_ribbon(aes(ymin=n_mean_over0_sample_min,ymax=n_mean_over0_sample_max),alpha=0.1,fill="red") +
    facet_grid(age.id.part~location) +
    theme_bw() +
    coord_cartesian(ylim=c(0,9))
  
  ### Proportion of participants with at least 1 contact
  #check that sampled values are within calculated ranges
  list_agev1$p_data_contact %>% 
    ggplot(aes(x=wave))+
    geom_ribbon(aes(ymin=p_no0_min,ymax=p_no0_max),alpha=0.1) +
    geom_line(aes(y=p_no0_sample_mean),col="red",linetype="dashed") +
    geom_ribbon(aes(ymin=p_no0_sample_min,ymax=p_no0_sample_max),alpha=0.1,fill="red") +
    facet_grid(age.id.part~location) +
    theme_bw()
  
  ######################################################################################################################
  #number of participants according to filtering on days
  load("C:/Users/antho/Dropbox/contact_matrix/data/clean_cm_data/survey_ind_data_NL_agev1_loc_remove_100.RData")
  list1 = get_stan_data_list_mod7_ind(location_x = location_x,
                                      wave_x = wave_x, nonrec= !reciprocity,
                                      country=country,
                                      filter.day="",
                                      survey_data_stan = survey_data_stan)
  list2 = get_stan_data_list_mod7_ind(location_x = location_x,
                                      wave_x = wave_x, nonrec= !reciprocity,
                                      country=country,
                                      filter.day="weekdays",
                                      survey_data_stan = survey_data_stan)
  list3 = get_stan_data_list_mod7_ind(location_x = location_x,
                                      wave_x = wave_x, nonrec= !reciprocity,
                                      country=country,
                                      filter.day="weekdays_nobankhol",
                                      survey_data_stan = survey_data_stan)
  survey_data_stan1 = list1[["survey_data_stan"]]
  survey_data_stan2 = list2[["survey_data_stan"]]
  survey_data_stan3 = list3[["survey_data_stan"]]
  rbind(survey_data_stan1$cnt_df1_ind %>% dplyr::mutate(data="1"),
        survey_data_stan2$cnt_df1_ind %>% dplyr::mutate(data="2"),
        survey_data_stan3$cnt_df1_ind %>% dplyr::mutate(data="3")) %>%
    group_by(data,wave) %>% 
    dplyr::summarise(n_part=length(unique(part_id))) %>%
    ggplot(aes(x=wave,y=n_part,fill=data)) +
    geom_bar(stat="identity",position=position_dodge(width=0.4),width=0.3) +
    theme_bw()+
    scale_x_continuous(breaks = 1:28)
}

