
death_data_cleaning = function(data_bfs_excmort,return.raw.data=FALSE){
  
  #data
  d=data_bfs_excmort %>% 
    dplyr::select(year=EREIGNIS_JJJJ_GES_N,
                  week=EREIGNIS_WW_GES_N,
                  age_class=P_ALTER_ERFUELLT_N,
                  sex=GESCHLECHT_CD_GES_T,
                  cause=ENDG_U_CD_GES_T,
                  n=N)
  
  #Year 5008 is a typo
  if(FALSE){
    d %>% filter(year==5008) 
    
    d %>% 
      filter(age_class=="80+", cause=="01. Total") %>% 
      filter(week %in% c(52,53)) %>% 
      group_by(year,week,age_class) %>% 
      dplyr::summarize(n=sum(n)) %>% 
      pivot_wider(names_from = "week",values_from = "n")
  }
  
  #Categorize the cause of deaths
  d2 = d %>% filter(year!=5008) %>% 
    dplyr::mutate(cause2 = recode(cause,
                                  `01. Total` = "total",
                                  `02. Infektiöse Krankheiten`="tot.infect.dis",                              
                                  `03. Tuberkulose`="infect.dis",
                                  `04. AIDS`="infect.dis",                                                  
                                  `05. COVID-19`="covid",
                                  `06. Bösartige Tumore`="tot.cancer",
                                  `07. Magenkrebs`="cancer",
                                  `08. Dickdarmkrebs`="cancer",
                                  `09. Lungenkrebs`="cancer",
                                  `10. Brustkrebs`="cancer",                                               
                                  `11. Gebärmutterhalskrebs`="cancer",
                                  `12. Prostatakrebs`="cancer",                                            
                                  `13. Diabetes mellitus`="diabetes",
                                  `14. Demenz`="dementia",                                                   
                                  `15. Erkrankungen des Kreislaufsystems`="cardiovascular.dis",
                                  `16. Herzkrankheiten insgesamt`="tot.heart.dis",                               
                                  `17. Ischämische Herzkrankheiten`="heart.dis",
                                  `18. Lungenembolie`="heart.dis",                   
                                  `19. Zerebrovaskuläre Krankheiten`="cerebral.vascular.dis",
                                  `20. Atmungsorgane insgesamt`="tot.resp.dis",
                                  `21. Grippe`="flu",
                                  `22. Pneumonie`="resp.dis",                                    
                                  `23. Chronische Bronchitis`="resp.dis",
                                  `24. Asthma`="resp.dis",                                         
                                  `25. Alkoholische Leberzirrhose`="organ.dis",
                                  `26. Erkrankungen der Harnorgane`="organ.dis",                              
                                  `27. Kongenitale Missbildungen`="congenital.dis",
                                  `28. Perinatale Todesursachen`="perinatal.deaths",                              
                                  `29. Äussere Ursachen (früher: Unfälle und Gewalteinwirkungen)`="other",
                                  `30. Unfälle insgesamt`="tot.accident",                                  
                                  `31. Strassenverkehrsunfälle`="road.accident",
                                  `32. Suizid`="suicide"))
  
  if(return.raw.data){return(d2)}
  
  #remove week 53
  d2 = d2 %>% filter(week!=53)
    
  if(FALSE){
    d2 %>% filter(year==2020,sex=="M") %>% 
      group_by(cause2) %>% 
      dplyr::summarise(n=sum(n),.groups="drop") %>% arrange(n) %>% View()
  }
  
  #add missing categories when no deaths
  d3 = d2 %>% dplyr::select(year,week) %>% unique() %>% 
    left_join(expand.grid(age_class = unique(d2$age_class),
                          sex = unique(d2$sex),
                          cause2 = unique(d2$cause2)),by=character()) %>% 
    left_join(d2 %>% dplyr::select(-cause),by=c("year","week","age_class","sex","cause2")) %>% 
    dplyr::mutate(n= replace_na(n,0))
  
  #Select main categories
  d.main.cat <- d3 %>% 
    #number of total deaths
    group_by(year,week,age_class,sex) %>% 
    dplyr::mutate(n.tot=n[which(cause2=="total")]) %>% ungroup() %>%
    #keep main causes
    filter(cause2 %in% c("tot.cancer","tot.infect.dis","dementia","diabetes","tot.accident","tot.resp.dis","cardiovascular.dis","covid")) %>%
    arrange(year,week,age_class,sex) 
  
  #Define the number of deaths due to other causes (i.e. not due to the main categories)
  d.other = d.main.cat %>% 
    group_by(year,week,age_class,sex,n.tot) %>% 
    dplyr::summarise(n.tot.main.cat = sum(n),.groups="drop") %>% 
    dplyr::mutate(n.other = n.tot-n.tot.main.cat) 
  
  #Bind deaths from main categories and from other causes
  d4 = rbind(d.main.cat %>% dplyr::select(year,week,age_class,sex,cause=cause2,n,n.tot),
             d.other %>% dplyr::mutate(cause="tot.other") %>% 
               dplyr::select(year,week,age_class,sex,cause,n=n.other,n.tot)) %>% 
    arrange(year,week,age_class,sex) 

  return(d4)
}
