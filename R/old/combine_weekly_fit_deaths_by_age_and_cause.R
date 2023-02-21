combine_weekly_fit_deaths_by_age_and_cause = function(data_all){
  #location of model chains
  files = list.files(paste0(code_root_path,"/results/stan_chains"))
  
  deaths_pred_sample=list()
  for(j in 1:length(age_classes)){
    for(i in 1:length(causes)){
      age_class_i = age_classes[j]
      cause_i = causes[i]
      list_id = paste0(age_class_i,"_",cause_i)
      print(cause_i)
      print(age_class_i)
      
      #load data
      data_fit  = data_all %>% 
        filter(cause==cause_i,age_class==age_class_i) %>% 
        dplyr::mutate(sex = factor(sex,levels=c("M","F")),
                      year.id = year-min(year)+1,
                      date=ISOweek2date(paste0(year,"-W",ifelse(week<10,paste0("0",week),week),"-1")),
                      week.id = (as.numeric(date)-as.numeric(min(date)))/7 + 1) %>%
        filter(year<2020)
      
      #load cmdstan model
      csv_files= files[grepl(gsub("\\+","\\\\+",paste0("chain_mod3","_",age_class_i,"_",cause_i)),files)]
      fit <-as_cmdstan_fit(paste0(code_root_path,"/results/stan_chains/",csv_files)[1:4])
      d=fit$draws()[,,]
      n_iter_per_chain = d[,1,1] %>% length()
      
      #join data with model results
      deaths_pred_sample[[list_id]] = data_fit %>%
        dplyr::mutate(data_row= dplyr::row_number()) %>% 
        dplyr::select(year,week,age_class,cause,sex,data_row,n,n.tot) %>% 
        left_join(.,
                  as.data.frame(ftable(d[,,grepl("deaths_pred",dimnames(d)[[3]]) & !grepl("_deaths",dimnames(d)[[3]])])) %>% 
                    dplyr::mutate(chain = as.numeric(as.character(chain)),
                                  iteration = as.numeric(as.character(iteration)),
                                  iter=iteration+n_iter_per_chain*(chain-1)) %>% 
                    dplyr::select(iter,variable,values=Freq) %>% 
                    separate(col=variable,into=c("variable","data_row"),sep="\\[") %>% 
                    dplyr::mutate(data_row = as.numeric(gsub("\\]","",data_row))),by="data_row")
    }
  }
  
  deaths_pred_sample = rbindlist(deaths_pred_sample)
  
  print("Saving in rds file")
  saveRDS(deaths_pred_sample,file="results/deaths_pred_sample.RDS")
  
  return(deaths_pred_sample)
}