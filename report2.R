
deaths_year4 <- read.csv(file="data/deaths_2010_2022.csv",check.names = F,sep=";",fileEncoding = "Latin1") %>%
  as_tibble() %>% 
  select(year=`Année`,week=Semaine,age=Age,deaths=NoDec_EP) %>% 
  dplyr::mutate(age=gsub(" ","",age),
                deaths=as.numeric(gsub(" ","",deaths)),
                deaths=replace_na(deaths,0)) %>% 
  filter(year>=2011,year<=2020) %>% 
  group_by(year) %>% 
  dplyr::summarise(n=sum(deaths),.groups="drop") %>% 
  arrange(year)


rbind(deaths_year_age1 %>% 
        dplyr::mutate(data="Internal (FSO), cause of deaths"),
      deaths_year_age2 %>% 
        dplyr::mutate(data="FSO, mortality by age")) %>% 
  group_by(year,data) %>% 
  dplyr::summarise(n=sum(n),.groups="drop") %>% 
  rbind(deaths_year4 %>% mutate(data="FSO, used in excess mortality")) %>% 
  ggplot() +
  geom_point(aes(x=year,y=n,col=data)) +
  geom_line(aes(x=year,y=n,col=data)) +
  scale_x_continuous(name="Year",breaks=c(2011,2015,2020)) +
  #annotate("text",x=2020,y=0,label="") +
  theme_bw()





## Expected deaths

```{r,  echo = FALSE}
expected_deaths_age = left_join(readRDS("results/sample_deaths_pand_pred.RDS") %>%
                                  group_by(cause,age_class) %>%
                                  dplyr::mutate(it=row_number()) %>% ungroup() %>%
                                  group_by(age_class,it) %>%
                                  dplyr::summarise(exp_deaths=sum(overall_deaths_pand_pred),.groups="drop_last") %>%
                                  dplyr::summarise(exp_deaths_med=median(exp_deaths),
                                                   exp_deaths_lob=quantile(exp_deaths,0.025),
                                                   exp_deaths_upb=quantile(exp_deaths,0.975),.groups="drop") %>%
                                  dplyr::mutate(exp_deaths1 = paste0(round(exp_deaths_med)," (",round(exp_deaths_lob)," - ",round(exp_deaths_upb),")")) %>%
                                  dplyr::select(age_class,exp_deaths1),
                                summ %>%
                                  dplyr::mutate(exp_deaths2 = paste0(round(exp_deaths_med)," (",round(exp_deaths_lob)," - ",round(exp_deaths_upb),")")) %>%
                                  dplyr::select(age_class=age_group,exp_deaths2),by="age_class")


expected_deaths_age %>% flextable() %>% delete_part(.,part="header") %>%
  add_header(age_class="Age", exp_deaths1 = "Expected mort. (cause of deaths)", exp_deaths2 = "Expected mort. (INLA)", top=TRUE) %>%
  hline_top(part="all",border=fp_border(color="black",width = 1.5)) %>%
  width(.,width=6,unit="cm")
```
## Excess mortality

### By age

```{r,  echo = FALSE}

#Excess cause of deaths
excess_age1 = readRDS("results/sample_deaths_pand_pred.RDS") %>%
  group_by(cause,age_class) %>%
  dplyr::mutate(it=row_number()) %>% ungroup() %>%
  group_by(age_class,it) %>%
  dplyr::summarise(exp_deaths=sum(overall_deaths_pand_pred),.groups="drop_last") %>%
  dplyr::summarise(exp_deaths_med=median(exp_deaths),
                   exp_deaths_lob=quantile(exp_deaths,0.025),
                   exp_deaths_upb=quantile(exp_deaths,0.975),.groups="drop") %>%
  left_join(deaths_year_age1 %>% filter(year==2020) %>% rename(deaths=n),by="age_class") %>%
  dplyr::mutate(excess_med=deaths - exp_deaths_med,
                excess_lob = deaths - exp_deaths_lob,
                excess_upb = deaths - exp_deaths_upb)

excess_age = left_join(excess_age1 %>%
                         dplyr::mutate(excess1 = paste0(round(excess_med)," (",round(excess_upb),",",round(excess_lob),")")) %>%
                         dplyr::select(age_class,excess1),
                       summ %>%
                         dplyr::mutate(excess2 = paste0(round(excess_med)," (",round(excess_lob),",",round(excess_upb),")")) %>%
                         dplyr::select(age_class=age_group,excess2),by="age_class")

excess_age %>% flextable() %>% delete_part(.,part="header") %>%
  add_header(age_class="Age", excess1 = "Excess (cause of deaths)", excess2 = "Excess (INLA)", top=TRUE) %>%
  hline_top(part="all",border=fp_border(color="black",width = 1.5)) %>%
  width(.,width=8,unit="cm")
```


### Total excess mortality


```{r,  echo = FALSE}
#FSO, cause of deaths
excess1 = excess_age1 %>%
  dplyr::summarise(excess_med=sum(excess_med),
                   excess_lob=sum(excess_lob),
                   excess_upb=sum(excess_upb),.groups="drop") %>%
  dplyr::mutate(excess1 = paste0(round(excess_med)," (",round(excess_upb),",",round(excess_lob),")")) %>%
  pull(excess1)

#INLA
excess2 = summ %>%
  dplyr::summarise(excess_med=sum(excess_med),
                   excess_lob=sum(excess_lob),
                   excess_upb=sum(excess_upb),.groups="drop") %>%
  dplyr::mutate(excess2 = paste0(round(excess_med)," (",round(excess_lob),",",round(excess_upb),")")) %>%
  pull(excess2)

#FSO, excess mortality
excess_fso <- read.csv2("../excess_mortality_ch/data/excess_mortality_fso_20221009.csv",sep=";",header=TRUE,encoding="UTF-8") %>%
  dplyr::select(canton=Canton,week=Fin,age.group=Age,deaths=NoDec_EP,
                expected=Attendu,lwr=Lim_min,upr=Lim_sup, significant_excess=Diff) %>%
  dplyr::mutate(monday.week=dmy(as.character(week))-6, #monday (was sunday)
                year=isoyear(monday.week),
                deaths = as.numeric(deaths),
                significant_excess = replace_na(as.numeric(significant_excess),0),
                all_excess = deaths-expected) %>%
  filter(!is.na(deaths)) %>%
  group_by(year) %>%
  dplyr::summarise(deaths = sum(deaths,na.rm=TRUE),
                   expected = sum(expected,na.rm=TRUE),
                   # lwr = sum(lwr,na.rm=TRUE),
                   # upr = sum(upr,na.rm=TRUE),
                   significant_excess = sum(significant_excess,na.rm=TRUE),
                   all_excess = sum(all_excess,na.rm=TRUE),.groups="drop") %>%
  filter(year==2020)

```


Total excess by FSO: `r excess_fso$all_excess`.

Total excess (cause of deaths model):  `r excess1`.

Total excess (INLA): `r excess2`.