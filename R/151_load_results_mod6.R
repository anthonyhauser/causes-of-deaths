load_results_mod6=function(age_classes, save.date,mod="mod6"){
  
  
  stan_diag = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_stan_diag_",x,".RDS")),error=function(e) NULL)))
  data_pred_week = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_data_pred_week_",x,".RDS")),error=function(e) NULL)))
  data_pred_phase = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_data_pred_phase_",x,".RDS")),error=function(e) NULL)))
  data_pred_year = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_data_pred_year_",x,".RDS")),error=function(e) NULL)))
  data_pred_week_cause = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_data_pred_week_cause_",x,".RDS")),error=function(e) NULL)))
  data_pred_phase_cause = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_data_pred_phase_cause_",x,".RDS")),error=function(e) NULL)))
  data_pred_year_cause = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_data_pred_year_cause_",x,".RDS")),error=function(e) NULL)))
  year_GP = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_year_GP_",x,".RDS")),error=function(e) NULL)))
  week_GP = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_week_GP_",x,".RDS")),error=function(e) NULL)))
  sigma = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_sigma_",x,".RDS")) %>% 
                                                                       dplyr::mutate(age_class=x),error=function(e) NULL)))
  Sigma_mat = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_Sigma_mat_",x,".RDS")) %>% 
                                                                           dplyr::mutate(age_class=x),error=function(e) NULL)))
  #if(mod=="mod8"){
    sex_effect = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_sex_effect_",x,".RDS")) %>% 
                                                                              dplyr::mutate(age_class=x),error=function(e) NULL)))
    nuts_effect = rbindlist(lapply(as.list(age_classes),function(x) tryCatch(readRDS(paste0("results/",save.date,"/",mod,"_nuts_effect_",x,".RDS"))%>% 
                                                                               dplyr::mutate(age_class=x),error=function(e) NULL)))
  #}else{
    
  #}
 
  

  return(list(stan_diag = stan_diag,
              data_pred_week = data_pred_week,
              data_pred_phase =data_pred_phase,
              data_pred_year = data_pred_year,
              data_pred_week_cause = data_pred_week_cause,
              data_pred_phase_cause =data_pred_phase_cause,
              data_pred_year_cause = data_pred_year_cause ,
              sigma = sigma,
              Sigma_mat = Sigma_mat,
              sex_effect = sex_effect,
              nuts_effect = nuts_effect,
              year_GP = year_GP,
              week_GP = week_GP))
}
