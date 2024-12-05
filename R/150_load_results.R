load_results=function(age_classes, causes, save.date){
  names_df = cross_join(cod_df %>% filter(cod_full%in%causes) %>% dplyr::rename(cod_group=cod_full),
                        data.frame(age_class=as.character(age_classes))) %>% 
    rowwise() %>% dplyr::mutate(names = paste0(cod_1word,"_",age_class)) %>% ungroup()
  list <- vector(mode = "list", length = length(names_df$names))
  names(list) = names_df$names
  
 
  stan_diag = rbindlist(lapply(as.list(names_df$names),function(x) tryCatch(readRDS(paste0("results/",save.date,"/mod4_stan_diag_",x,".RDS")),error=function(e) NULL)))
  data_pred_week = rbindlist(lapply(as.list(names_df$names),function(x) tryCatch(readRDS(paste0("results/",save.date,"/mod4_data_pred_week_",x,".RDS")),error=function(e) NULL)))
  data_pred_phase = rbindlist(lapply(as.list(names_df$names),function(x) tryCatch(readRDS(paste0("results/",save.date,"/mod4_data_pred_phase_",x,".RDS")),error=function(e) NULL)))
  data_pred_year = rbindlist(lapply(as.list(names_df$names),function(x) tryCatch(readRDS(paste0("results/",save.date,"/mod4_data_pred_year_",x,".RDS")),error=function(e) NULL)))
  year_GP = rbindlist(lapply(as.list(names_df$names),function(x) tryCatch(readRDS(paste0("results/",save.date,"/mod4_year_GP_",x,".RDS")),error=function(e) NULL)))
  week_GP = rbindlist(lapply(as.list(names_df$names),function(x) tryCatch(readRDS(paste0("results/",save.date,"/mod4_week_GP_",x,".RDS")),error=function(e) NULL)))
  reg_effect = rbindlist(lapply(as.list(names_df$names),function(x) tryCatch(readRDS(paste0("results/",save.date,"/mod4_reg_effect_",x,".RDS")) %>% 
                                                                      cbind(names_df %>% filter(names==x)),error=function(e) NULL)))
  

  return(list(stan_diag = stan_diag %>% 
                right_join(names_df),
              data_pred_week = data_pred_week %>% 
                right_join(names_df),
              data_pred_phase =data_pred_phase %>% 
                right_join(names_df),
              data_pred_year = data_pred_year %>% 
                right_join(names_df),
              year_GP = year_GP %>% 
                right_join(names_df),
              week_GP = week_GP %>% 
                right_join(names_df),
              reg_effect = reg_effect %>% 
                right_join(names_df)))
}
