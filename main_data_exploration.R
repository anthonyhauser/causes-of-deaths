#Data exploration

#Check the distribution of conditions (comorbidities) by cause of deaths
#Cancer
principal_cause="Neoplasms (Cancers)"
principal_cause="COVID-19"
secondary_cause=c("Respiratory Diseases","Cardiovascular Diseases","Mental and Neurological Disorders",
                  "Neoplasms (Cancers)","Other Causes")

d = cod_ind_df %>% dplyr::select(ind_id,age,sex,cal_week,cal_year,outcome,cod_group) %>% 
  pivot_wider(id_cols=c(ind_id,age,sex,cal_week,cal_year),names_from="outcome",values_from="cod_group")

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

#by month
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
secondary_cause=c("Respiratory Diseases","Cardiovascular Diseases","Mental and Neurological Disorders",
                  "Other Causes")

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


################################################################################################################################################################
################################################################################################################################################################
#Old method for Proportion of deaths of a given cause conditioned on the conditions (see above for the new method)

target_conditions <- c("Neoplasms (Cancers)", "Cardiovascular Diseases", "Mental and Neurological Disorders")
target_conditions = d_prepped$ENDG_U_CD_GES_T %>% unique()

d_prepped = cod_ind_df %>% 
  #wider and select columns
  dplyr::select(ind_id,age,sex,cal_week,cal_year,outcome,cod_group) %>% 
  pivot_wider(id_cols=c(ind_id,age,sex,cal_week,cal_year),names_from="outcome",values_from="cod_group") %>% 
  #Select data used in the analysis and create date variables for grouping
  filter(cal_year>=2010) %>% #first filter to reduce size, we keep 2010 as some iso year 2011 is calendar year 2010
  dplyr::mutate(date = ISOweek2date(paste0(cal_year, "-W", sprintf("%02d", cal_week), "-1"))) %>% 
  filter(year(date)>=2011,year(date)<=2021) %>% #filter years included in the analysis
  dplyr::mutate(week_id = as.numeric(1+(date-min(date))/7),#year_month = floor_date(date, unit = "month"),
                week_id = ceiling(week_id /n_week_agg),
                #age_group = factor(as.numeric(age >= 50) + as.numeric(age >= 65) + as.numeric(age >= 80)),
                age_class = cut(age,breaks = c(-1,18,40,65,80,Inf), labels = c("0-17","18-39","40-64","65-79","80+"),
                                right=FALSE),
                id = row_number()) %>% 
  group_by(week_id) %>% 
  dplyr::mutate(mean_date=mean(unique(date))) %>% ungroup()

condition_summary <- purrr::map_dfr(target_conditions, function(cond) {
  print(cond)
  d_prepped %>%
    mutate(condition_flag = (ENDG_U_CD_GES_T == cond |
                               BEGLEIT_KRANK_A_GES_T == cond |
                               BEGLEIT_KRANK_B_GES_T == cond),
           died_from_condition = (ENDG_U_CD_GES_T == cond),
           died_from_covid = str_detect(ENDG_U_CD_GES_T, "COVID-19"),  # adjust to match your dataset
           died_from_neither = !died_from_condition & !died_from_covid) %>%
    filter(condition_flag) %>%  # only those who died with the condition
    group_by(year_month, age_group) %>%
    summarise(total = n(),
              n_from_condition = sum(died_from_condition),
              n_from_covid = sum(died_from_covid),
              n_from_neither = sum(died_from_neither),
              prop_from_condition = n_from_condition / total,
              prop_from_covid = n_from_covid / total,
              prop_from_neither = n_from_neither / total,
              .groups = "drop" ) %>%
    mutate(condition = cond)
})

condition_long <- condition_summary %>%
  pivot_longer( cols = starts_with("n_from_"),
                names_to = "cause_type",
                values_to = "proportion" ) %>%
  mutate(cause_type = recode(cause_type,
                             n_from_condition = "condition",
                             n_from_covid = "covid",
                             n_from_neither = "other"))
condition_long %>% 
  filter(condition=="Neoplasms (Cancers)",age_group==1) %>% 
  filter(year(year_month)>=2020) %>% 
  ggplot(aes(x = year_month, y = proportion, color = cause_type)) +
  geom_line(size = 1) +
  facet_grid(age_group ~ condition,scales="free") +
  #scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c("condition" = "#1f77b4", "covid" = "#d62728", "other" = "#2ca02c")) +
  labs( title = "Proportion of death causes among individuals with selected conditions",
        x = "Year-Month",
        y = "Proportion of deaths",
        color = "Cause of Death") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 9),
        legend.position = "bottom" )

condition_long %>% 
  filter(year(year_month)>=2020,cause_type=="covid") %>% 
  ggplot(aes(x = year_month, y = proportion, color = condition)) +
  geom_line(size = 1) +
  facet_grid(age_group ~ .,scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 9),
        legend.position = "bottom" )

condition_long %>% 
  filter(year(year_month)>=2020,cause_type=="covid") %>%
  group_by(year_month,age_group) %>% 
  dplyr::mutate(n_covid = sum(proportion)) %>% 
  ggplot(aes(x = year_month, y = proportion/n_covid, color = condition)) +
  geom_line(size = 1) +
  facet_grid(age_group ~ .,scales="free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 9),
        legend.position = "bottom" )

################################################################################################################################################################
################################################################################################################################################################
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