corr_resp_lag = function(data, x=c("COVID-19","Respiratory Diseases"),
                         y="Cardiovascular Diseases",lag=0){
  #data=reshaped_df; x=c("COVID-19","Respiratory Diseases"); y="Cardiovascular Diseases"; lag=0;
  if(lag>=0){
    df = data %>% 
      dplyr::select(week.id,all_of(c(x,y))) %>% 
      dplyr::mutate(!!sym(y) := dplyr::lag(!!sym(y), lag)) %>% 
      dplyr::filter(!is.na(!!sym(y)))
  }else{
    df = data %>% 
      dplyr::select(week.id,all_of(c(x,y))) %>% 
      dplyr::mutate(!!sym(y) := dplyr::lead(!!sym(y), -lag)) %>% 
      dplyr::filter(!is.na(!!sym(y)))
  }
  df_standardized <- df %>%
    mutate(across(-week.id, ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))
  if(FALSE){
    df_long <- df_standardized %>%
      pivot_longer(
        cols = -week.id,
        names_to = "cause",
        values_to = "value"
      )
    
    ggplot(df_long, aes(x = week.id, y = value, color = cause)) +
      geom_line() +
      labs(x = "Week", y = "Excess Mortality", color = "Cause of Death") +
      theme_minimal()
  }
  formula = paste0(paste0("`", y, "`")," ~ ",paste(paste0("`", x, "`"),collapse=" + "))
  glm_res = glm(formula=as.formula(formula), data=df_standardized)
  ci_df =  confint.default(glm_res)
  
  
  outcome <- glm_res$y
  outcome_hat <- predict(glm_res, type = "response")
  
  rss <- sum((outcome - outcome_hat)^2)
  tss <- sum((outcome - mean(outcome))^2)
  
  r_squared <- 1 - rss / tss
  
  res_df = data.frame(pcor_est = c(NA,pcor(df_standardized[,c(x,y)])$estimate[y,x]),
                      est = glm_res$coefficients,
                      lwb = ci_df[,1],
                      upb = ci_df[,2],
                      var = rownames(ci_df),
                      lag = lag,
                      r_squared = r_squared,
                      y = y)
  return(res_df)
}