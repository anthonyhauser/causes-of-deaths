corr_resp_lag_noci = function(data, x=c("COVID-19","Respiratory Diseases"),
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
  res_df = data.frame(pcor_est = c(NA,pcor(df_standardized[,c(x,y)])$estimate[y,x]),
                      est = glm_res$coefficients,
                      var = names(glm_res$coefficients),
                      lag = lag,
                      y = y)
  return(res_df)
}




corr_resp_lag_noci <- function(data,
                               x = c("COVID-19", "Respiratory Diseases"),
                               y = "Cardiovascular Diseases",
                               lag = 0) {
  # Select relevant columns
  vars <- c("week.id", x, y)
  df <- data[, vars]
  
  # Apply lag or lead to y
  if (lag >= 0) {
    df[[y]] <- dplyr::lag(df[[y]], lag)
  } else {
    df[[y]] <- dplyr::lead(df[[y]], -lag)
  }
  
  # Drop rows with NA after shifting
  df <- df[!is.na(df[[y]]), ]
  
  # Standardize all except week.id
  df_std <- as.data.frame(df)
  for (v in setdiff(names(df_std), "week.id")) {
    df_std[[v]] <- scale(df_std[[v]], center = TRUE, scale = TRUE)[, 1]
  }
  
  # Compute partial correlations between y and each x
  pc <- tryCatch({
    pcor_mat <- pcor(df_std[, c(x, y)])$estimate
    data.frame(
      pcor_est = pcor_mat[y, x],
      var = x,
      lag = lag,
      y = y
    )
  }, error = function(e) {
    data.frame(
      pcor_est = NA,
      var = x,
      lag = lag,
      y = y
    )
  })
  
  return(pc)
}


library(future.apply)

# Set up parallel plan (adjust number of workers as needed)
future::plan(multisession)  # or multicore if you're on Unix

# Then replace `lapply` with:
pb <- progress_bar$new(total = nrow(jobs))
results <- future_lapply(seq_len(nrow(jobs)), function(j) {
  row <- jobs[j]
  corr_resp_lag_noci(
    data = reshaped_df,
    x = c("COVID-19", "Respiratory Diseases"),
    y = row$y,
    lag = row$lag)
})
