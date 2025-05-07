corr_resp_lag_noci <- function(data,
                               x = c("COVID-19", "Respiratory Diseases"),
                               y = "Cardiovascular Diseases",
                               lag = 0) {
  # Select relevant columns
  vars <- c("week.id","iter", x, y)
  df <- data[, vars]
  
  # Apply lag or lead to y
  if (lag >= 0) {
    df[[y]] <- dplyr::lag(df[[y]], lag)
  } else {
    df[[y]] <- dplyr::lead(df[[y]], -lag)
  }
  
  # Drop rows with NA after shifting
  df <- df[!is.na(df[[y]]), ]
  
 
  
  # Compute partial correlations between y and each x
  pc <- tryCatch({
    pcor_mat <- pcor(df[, c(x, y)])$estimate
    data.frame(
      pcor_est = pcor_mat[y, x],
      var = x,
      lag = lag,
      y = y,
      iter = data[1,"iter"]
    )
  }, error = function(e) {
    data.frame(
      pcor_est = NA,
      var = x,
      lag = lag,
      y = y,
      iter = data[1,"iter"]
    )
  })
  
  return(pc)
}

# corr_resp_lag_noci = function(data, x=c("COVID-19","Respiratory Diseases"),
#                               y="Cardiovascular Diseases",lag=0){
#   #data=reshaped_df; x=c("COVID-19","Respiratory Diseases"); y="Cardiovascular Diseases"; lag=0;
#   if(lag>=0){
#     df = data %>% 
#       dplyr::select(week.id,all_of(c(x,y))) %>% 
#       dplyr::mutate(!!sym(y) := dplyr::lag(!!sym(y), lag)) %>% 
#       dplyr::filter(!is.na(!!sym(y)))
#   }else{
#     df = data %>% 
#       dplyr::select(week.id,all_of(c(x,y))) %>% 
#       dplyr::mutate(!!sym(y) := dplyr::lead(!!sym(y), -lag)) %>% 
#       dplyr::filter(!is.na(!!sym(y)))
#   }
#   df_standardized <- df %>%
#     mutate(across(-week.id, ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))
#   if(FALSE){
#     df_long <- df_standardized %>%
#       pivot_longer(
#         cols = -week.id,
#         names_to = "cause",
#         values_to = "value"
#       )
#     
#     ggplot(df_long, aes(x = week.id, y = value, color = cause)) +
#       geom_line() +
#       labs(x = "Week", y = "Excess Mortality", color = "Cause of Death") +
#       theme_minimal()
#   }
#   formula = paste0(paste0("`", y, "`")," ~ ",paste(paste0("`", x, "`"),collapse=" + "))
#   glm_res = glm(formula=as.formula(formula), data=df_standardized)
#   res_df = data.frame(pcor_est = c(NA,pcor(df_standardized[,c(x,y)])$estimate[y,x]),
#                       est = glm_res$coefficients,
#                       var = names(glm_res$coefficients),
#                       lag = lag,
#                       y = y)
#   return(res_df)
# }
