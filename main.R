source("R/000_setup.R")

#Load encrypted data
if(controls$load.encrypted.data){
  #load encrypted data
  #1) setwd
  setwd("C:/depot_git/covid19-madmurdock")
  #1) open the file floating_code/encryption/BAGEPI-2544-create-encoding-solution-for-ofs-mortality-data.R (in covid-madmurdock)
  file.edit('floating_code/encryption/BAGEPI-2544-create-encoding-solution-for-ofs-mortality-data.R')
  #2) run the file
  #3) enter the password found in keepass_safe.kdbx under Harvester, Harvester Keyring, when requested 
  #4) setwd back
  setwd("C:/depot_git/causes-of-deaths")
  #5) you can now use the data_bfs_excmort dataset
  save(data_bfs_excmort,file="data/data_bfs_excmort.RDS")
}

#Clean data
data_bfs_excmort = readRDS("data/data_bfs_excmort.RDS")
#clean death data
death_data = death_data_cleaning(data_bfs_excmort)
#load population data and interpolate for each week of the year
pop = load_pop(death_data)
#combine death and pop data
data_all = inner_join(death_data,
               pop %>% dplyr::rename(n.pop=n),
               by= c("year","week","age_class","sex")) %>% 
  arrange(cause,year,week,age_class,sex)
saveRDS(data_all,file="data/d.RDS")

###########################################################################################################################
#Run models
data_all = readRDS("data/d.RDS")
causes = data_all$cause %>% unique() %>% setdiff(.,"covid")
age_classes = data_all$age_class %>% unique()
# data_pred_pand = run_stan_mod1_by_age_cause(data_all, age_classes, causes)
# data_pred_pand = run_stan_mod1_all_deaths_by_age(data_all, age_classes)
if(FALSE){
  mod3_data_pred_pand = run_stan_mod3_by_age_cause(data_all, age_classes,  causes, run.model=TRUE)
  #deaths_pand_pred_sample = combine_weekly_pand_deaths_by_age_and_cause(data_all)
  #deaths_pred_sample = combine_weekly_fit_deaths_by_age_and_cause(data_all)
}

deaths_pand_pred_sample = readRDS("results/deaths_pand_pred_sample.RDS")
deaths_pred_sample = readRDS("results/deaths_pred_sample.RDS")

########################################################################################################
#Expected deaths pandemic
#by age
d1.pand.year = deaths_pand_pred_sample %>% 
  select(year,week,age_class,cause,sex,n,n.tot,iter,values) %>%
        group_by(age_class,iter) %>%
        dplyr::summarise(values=sum(values),
                         n = sum(n),.groups="drop_last") %>% 
        dplyr::summarise(deaths= n[iter==1],
                         est=median(values),
                         lwb = quantile(values,probs=0.025),
                         upb = quantile(values,probs=0.975), .groups="drop") %>% 
  left_join(.,data_all %>%
            filter(cause=="covid",year==2020) %>%
            group_by(age_class) %>% 
            dplyr::summarise(n.covid=sum(n),.groups="drop"),by="age_class") %>% 
  dplyr::mutate(deaths=deaths+n.covid)

#all
d2.pand.year=deaths_pand_pred_sample %>% 
  select(year,week,age_class,cause,sex,n,n.tot,iter,values) %>%
  group_by(iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= n[iter==1],
                   est=median(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop") %>% 
  left_join(.,data_all %>%
              filter(cause=="covid",year==2020) %>%
              dplyr::summarise(n.covid=sum(n),.groups="drop"),by=character()) %>% 
  dplyr::mutate(deaths=deaths+n.covid)


#by week, by age
d1.pand.week=deaths_pand_pred_sample %>% 
  select(year,week,age_class,cause,sex,n,n.tot,iter,values) %>%
  group_by(age_class,year,week,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= n[iter==1],
                   est=median(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop") %>% 
  left_join(.,data_all %>%
              filter(cause=="covid",year==2020) %>%
              group_by(age_class) %>% 
              dplyr::summarise(n.covid=sum(n),.groups="drop"),by="age_class") %>% 
  dplyr::mutate(deaths=deaths+n.covid)

#by week, all age
d2.pand.week=deaths_pand_pred_sample %>% 
  select(year,week,age_class,cause,sex,n,n.tot,iter,values) %>%
  group_by(year,week,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= n[iter==1],
                   est=median(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop") %>% 
  left_join(.,data_all %>%
              filter(cause=="covid",year==2020) %>%
              dplyr::summarise(n.covid=sum(n),.groups="drop"),by=character()) %>% 
  dplyr::mutate(deaths=deaths+n.covid)

#by week, all age
d.pand.year.age.cause=deaths_pand_pred_sample %>% 
  select(year,week,age_class,cause,sex,n,n.tot,iter,values) %>%
  group_by(year,age_class,cause,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= n[iter==1],
                   est=median(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop")


rbind(d1.pand.year, d2.pand.year %>% dplyr::mutate(age_class="Total"))

save(d1.pand.year,d2.pand.year,d1.pand.week,d2.pand.week,d.pand.year.age.cause,file="savepoint/aggregated_deaths_pand.RData")

########################################################################################################
#Expected deaths before 2020
#by year, by age
d1.year = deaths_pred_sample %>% 
  group_by(year,age_class,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=mean(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop")
#by year, all
d2.year = deaths_pred_sample %>% 
  group_by(year,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=mean(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop")

#by week, by age
d1.week = deaths_pred_sample %>% 
  group_by(year,week,age_class,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=mean(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop")
#by year, all
d2.week = deaths_pred_sample %>% 
  group_by(year,week,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=mean(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop")

#by year, by age, by cause
d.year.age.cause = deaths_pred_sample %>% 
  group_by(year,cause,age_class,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=mean(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop")

save(d1.year,d2.year,d1.week,d2.week,d.year.age.cause,file="savepoint/aggregated_deaths.RData")

