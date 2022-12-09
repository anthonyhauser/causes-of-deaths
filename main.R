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

d1 %>% 
  ggplot() +
  geom_line(aes(x=year,y=est),col="black") +
  geom_ribbon(aes(x=year,ymin=lwb,ymax=upb),fill="black",alpha=0.2) +
  geom_point(aes(x=year,y=deaths),col="red",alpha=0.5,size=2) +
  facet_wrap(.~cause,scales="free") +
  theme_bw()

d2=deaths_pred_sample %>% 
  group_by(year,week,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=mean(values), 
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop")  %>% 
  dplyr::mutate(date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")))

d2


#aggregate deaths by year, week and age_class (i.e. over sex)
d=deaths_pand_pred_sample %>% 
  group_by(year,week,age_class,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=mean(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop") %>% 
  dplyr::mutate(date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")))

#Non-covid deaths
rbind(deaths_pand_pred_sample %>% 
  group_by(year,age_class,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=median(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop"),
deaths_pand_pred_sample %>% 
  group_by(year,iter) %>%
  dplyr::summarise(values=sum(values),
                   n = sum(n),.groups="drop_last") %>% 
  dplyr::summarise(deaths= mean(n),#take the mean for the reported deaths but it is same across all iterations
                   est=median(values),
                   lwb = quantile(values,probs=0.025),
                   upb = quantile(values,probs=0.975), .groups="drop") %>% 
  dplyr::mutate(age_class="Total"))

#covid
data_all %>% 
  filter(cause=="covid",year==2020) %>% 
  dplyr::mutate(iter=1,values=n) %>% 
  dplyr::select(year,week,age_class,cause,sex,n,n.tot,iter,values) %>% 
  group_by(age_class) %>% 
  dplyr::summarise(n=sum(n),.groups="drop")

########################################################################################################
#Expected deaths pandemic
#by age
d1.pand.year=deaths_pand_pred_sample %>% 
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

########################################################################################################

#plot by year, all
d2.year %>% 
  rbind(d2.pand.year %>% mutate(year=2020) %>% select(-n.covid)) %>% 
  ggplot() +
  geom_line(aes(x=year,y=est),col="black") +
  geom_ribbon(aes(x=year,ymin=lwb,ymax=upb),fill="black",alpha=0.2) +
  geom_point(aes(x=year,y=deaths),col="red",alpha=0.5,size=2) +
  theme_bw()

#plot by year, by age
d1.year %>% 
  rbind(d1.pand.year %>% mutate(year=2020) %>% select(-n.covid)) %>% 
  ggplot() +
  geom_line(aes(x=year,y=est),col="black") +
  geom_ribbon(aes(x=year,ymin=lwb,ymax=upb),fill="black",alpha=0.2) +
  geom_point(aes(x=year,y=deaths),col="red",alpha=0.5,size=2) +
  theme_bw() +
  facet_wrap(.~age_class,scales="free")

#plot by week, all
d2.week %>% 
  rbind(d2.pand.week %>% mutate(year=2020) %>% select(-n.covid)) %>% 
  dplyr::mutate(date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1"))) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.2) +
  #geom_point(aes(x=date,y=deaths),col="red",alpha=0.5,size=2) +
  theme_bw()

#plot by week, all
d.year.age.cause %>% 
  rbind(d.pand.year.age.cause %>% mutate(year=2020)) %>% 
  ggplot() +
  geom_line(aes(x=year,y=est),col="black") +
  geom_ribbon(aes(x=year,ymin=lwb,ymax=upb),fill="black",alpha=0.2) +
  geom_point(aes(x=year,y=deaths),col="red",alpha=0.2,size=2) +
  facet_grid(age_class~cause,scales="free") +
  theme_bw()



data_all %>% 
  group_by(year) %>% 
  dplyr::summarise(n=sum(n))

data_all %>% 
  filter(cause=="covid",year==2020) %>% 
  group_by(age_class) %>% 
  dplyr::summarise(n=sum(n),.groups="drop")


d %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.2) +
  geom_point(aes(x=date,y=deaths),col="red",alpha=0.5,size=0.8) +
  facet_wrap(.~age_class,scales="free") +
  theme_bw()





# d.stan=d %>% filter(cause=="cardiovascular.dis") %>% 
#   dplyr::mutate(month = pmax(1,ceiling((week-1)/(365.25/(7*12)))))
# stan_glm1 <- stan_gamm4(n ~ age_class + sex  +
#                           s(year,bs="gp") +
#                           s(week,bs="gp") + offset(log(d.stan$n.pop)),
#                         data = d.stan, family = poisson, 
#                         prior = normal(0, 2.5), 
#                         prior_intercept = normal(0, 5),
#                         chains=4, cores=4,
#                         seed = 12345)
# coef(stan_glm1)

###########################################################################################################################
#Plots

readRDS("results/mod3_sample_deaths_pand_pred.RDS") %>% 
  group_by(cause,age_class) %>% 
  dplyr::mutate(it=row_number()) %>% ungroup() %>% 
  group_by(age_class,it) %>% 
  dplyr::summarise(exp_deaths=sum(overall_deaths_pand_pred),.groups="drop_last") %>% 
  dplyr::summarise(exp_deaths_med=median(exp_deaths),
                   exp_deaths_lob=quantile(exp_deaths,0.025),
                   exp_deaths_upb=quantile(exp_deaths,0.975),.groups="drop")








readRDS("results/mod3_excess_all.RDS") %>% 
  filter(var=="overall_excess") %>% 
  dplyr::mutate(cause="all (but not COVID)") %>% 
  rbind(data_all %>% 
          filter(year==2020,cause=="covid") %>% 
          group_by(age_class) %>% 
          dplyr::summarise(n=sum(n),.groups="drop") %>%
          dplyr::select(age_class=age_class,est=n) %>% 
          dplyr::mutate(lwb=NA,upb=NA,var="overall_excess",cause="covid")) %>% 
  rbind(readRDS("results/excess.RDS") %>% 
          filter(var=="overall_excess") %>% 
          group_by(age_class) %>% 
          dplyr::summarise(n=sum(est),.groups="drop") %>%
          dplyr::select(age_class=age_class,est=n) %>% 
          dplyr::mutate(lwb=NA,upb=NA,var="overall_excess",cause="all")) %>% 
  ggplot() +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(aes(x=age_class,y=est),fill="green",colour="black",size=.3,width=.8,alpha=.5) +
  geom_errorbar(aes(x=age_class,y=est,ymin=lwb,ymax=upb),
                colour="black",width=.3,size=.3,alpha=.8) +
  scale_fill_discrete(guide="none") +
  labs(x="Age",y="Excess mortality") +
  facet_grid(.~cause,scales="free") +
  theme_bw()


readRDS("results/mod3_excess.RDS") %>% 
  filter(var=="overall_excess") %>% 
  left_join(data.frame(cause =causes,
                       cause_name = c("Cardiovascular\ndisease","Dementia","Diabetes","Accident","Cancer","Infectious\ndisease","Other")),
            by="cause") %>% 
  ggplot() +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(aes(x=cause_name,y=est),fill="green",colour="black",size=.3,width=.8,alpha=.5) +
  geom_errorbar(aes(x=cause_name,y=est,ymin=lwb,ymax=upb),
                colour="black",width=.3,size=.3,alpha=.8) +
  facet_wrap(age_class~.,scales="free") +
  scale_fill_discrete(guide="none") +
  labs(x="Cause",y="Excess mortality") +
  theme_bw()

readRDS("results/mod3_excess.RDS") %>% 
  filter(var=="overall_excess") %>% 
  rbind(data_all %>% 
         filter(year==2020,cause=="covid") %>% 
         group_by(age_class) %>% 
         dplyr::summarise(n=sum(n),.groups="drop") %>%
         dplyr::select(age_class=age_class,est=n) %>% 
         dplyr::mutate(lwb=NA,upb=NA,var="overall_excess",cause="covid")) %>% 
  left_join(data.frame(cause =c(causes,"covid"),
                       cause_name = c("Cardiovascular\ndisease","Dementia","Diabetes","Accident","Cancer","Infectious\ndisease","Other","COVID")),
            by="cause") %>% 
  ggplot() +
  geom_hline(yintercept=0,colour="grey50") +
  geom_col(aes(x=cause_name,y=est),fill="green",colour="black",size=.3,width=.8,alpha=.5) +
  geom_errorbar(aes(x=cause_name,y=est,ymin=lwb,ymax=upb),
                colour="black",width=.3,size=.3,alpha=.8) +
  facet_wrap(age_class~.,scales="free") +
  scale_fill_discrete(guide="none") +
  labs(x="Cause",y="Relative excess mortality") +
  theme_bw()

readRDS("results/mod3_excess.RDS") %>% 
  filter(var=="overall_rel_excess") %>% 
  dplyr::mutate(est=ifelse(is.infinite(est),NA,est)) %>% 
  left_join(data.frame(cause =causes,
                       cause_name = c("Cardiovascular\ndisease","Dementia","Diabetes","Accident","Cancer","Infectious\ndisease","Other")),
            by="cause") %>% 
  ggplot() +
  geom_hline(yintercept=0,colour="black",size=1) +
  geom_col(aes(x=cause_name,y=est),fill="green",colour="black",size=.3,width=.8,alpha=.5) +
  geom_errorbar(aes(x=cause_name,y=est,ymin=lwb,ymax=upb),
                colour="black",width=.3,size=.3,alpha=.8) +
  facet_wrap(age_class~.,scales="free") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(guide="none") +
  labs(x="Cause",y="Relative excess mortality") +
  theme_bw()





d<-readRDS(file="data/data_bfs_excmort.RDS")









