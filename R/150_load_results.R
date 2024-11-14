load_results=function(age_classes, causes, save.date){
  names = apply(expand.grid(cod_df %>% filter(cod_full%in%causes) %>% pull(cod_1word), age_classes), 1, paste, collapse="_")
  list <- vector(mode = "list", length = length(names))
  names(list) = names
  
  stan_diag = rbindlist(lapply(as.list(names),function(x) readRDS(paste0("results/",save.date,"/mod4_stan_diag_",x,".RDS")) %>% 
                                 dplyr::mutate(cod_1word=gsub("_.*", "", x),
                                               age_class=gsub(".*_", "", x))))
  data_pred_week = rbindlist(lapply(as.list(names),function(x) readRDS(paste0("results/",save.date,"/mod4_data_pred_week_",x,".RDS"))))
  data_pred_phase = rbindlist(lapply(as.list(names),function(x) readRDS(paste0("results/",save.date,"/mod4_data_pred_phase_",x,".RDS"))))
  data_pred_year = rbindlist(lapply(as.list(names),function(x) readRDS(paste0("results/",save.date,"/mod4_data_pred_year_",x,".RDS")))) 
  year_GP = rbindlist(lapply(as.list(names),function(x) readRDS(paste0("results/",save.date,"/mod4_year_GP_",x,".RDS"))))
  week_GP = rbindlist(lapply(as.list(names),function(x) readRDS(paste0("results/",save.date,"/mod4_week_GP_",x,".RDS"))))

  return(list(stan_diag=stan_diag,
              data_pred_week = data_pred_week,
              data_pred_phase =data_pred_phase,
              data_pred_year = data_pred_year,
              year_GP = year_GP,
              week_GP = week_GP))
}
