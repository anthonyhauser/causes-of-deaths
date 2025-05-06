cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

get_covariables_stan = function(data, variables, ref_levels){
  X_variables = data.frame()
  for(i in 1:length(variables)){
    X = data %>% pull(variables[i])
    if(is.factor(X)){
      df = model.matrix(~ X - 1) %>% as.data.frame() %>% dplyr::select(!grep(pattern=paste0("X",ref_levels[i]), x=colnames(.)))
      colnames(df) = paste0(variables[i],".",ref_levels[i],".", substr(colnames(df), 2, nchar(colnames(df))))
      X_variables = cbind.fill(X_variables,
                               df) 
    }else{
      df = X
      colnames(df)=paste0(variables[i],"_cont")
      X_variables = cbind.fill(X_variables,
                               X)
    }
  }
  return(X_variables)
}

days_to_datetime_2020 <- function(days) {
  # Start date is January 1, 2020
  start_date <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  
  # Add the number of days as seconds to the start date
  result_datetime <- start_date + days * 24 * 60 * 60
  
  return(result_datetime)
}


summary_cmdstanr = function(fit=fit6, variable = "sigma",chains=chains){
  d=fit$draws()[,,]
  as.data.frame(ftable(d[,,grepl(variable,dimnames(d)[[3]])])) %>% 
    filter(chain %in% chains) %>% 
    group_by(variable) %>% 
    dplyr::summarise(mean=mean(Freq),
                     `2.5%` = quantile(Freq,probs=0.025),
                     `97.5%` = quantile(Freq,probs=0.975),.groups="drop")
}

quantile2 <- function(x, probs) {
  if (any(is.na(x))) NA_real_ else quantile(x, probs = probs, na.rm = FALSE)
}


summarise_peak_dates <- function(.x) {
  sorted_doy <- sort(.x$doy)
  wrapped <- c(sorted_doy, sorted_doy[1] + 365)
  diffs <- diff(wrapped)
  max_gap <- max(diffs)
  shortest_arc <- 365 - max_gap
  start_idx <- which.max(diffs) + 1
  min_day <- wrapped[start_idx] %% 365
  
  base_date <- as.Date("2020-01-01")
  
  if (shortest_arc < 180) {
    .x <- .x %>%
      mutate(doy_shifted = (doy - min_day) %% 365)
    
    mean_day <- mean(.x$doy_shifted)
    p5_day <- quantile(.x$doy_shifted, 0.05)
    p95_day <- quantile(.x$doy_shifted, 0.95)
    
    tibble(
      arc_length = shortest_arc,
      min_day = min_day,
      mean_day = mean_day,
      p5_day = p5_day,
      p95_day = p95_day,
      min_date = base_date + days(round(min_day)),
      mean_date = base_date + days(round(min_day + mean_day)),
      p5_date = base_date + days(round(min_day + p5_day)),
      p95_date = base_date + days(round(min_day + p95_day))) %>%
      dplyr::mutate(shift_dir = case_when(mean_date < as.Date("2019-07-01") ~ 1,
                                          mean_date > as.Date("2020-06-30") ~ -1,
                                          TRUE ~ 0)) %>%
      dplyr::mutate(across( c(min_date, mean_date, p5_date, p95_date),  ~ . + years(shift_dir))) %>%
      dplyr::select(-shift_dir)
  } else {
    tibble(
      arc_length = shortest_arc,
      min_day = NA_real_,
      mean_day = NA_real_,
      p5_day = NA_real_,
      p95_day = NA_real_,
      min_date = as.Date(NA),
      mean_date = as.Date(NA),
      p5_date = as.Date(NA),
      p95_date = as.Date(NA)
    )
  }
}


get_legend2 <- function(plot, legend = NULL) {
  if (is.ggplot(plot)) {
    gt <- ggplotGrob(plot)
  } else {
    if (is.grob(plot)) {
      gt <- plot
    } else {
      stop("Plot object is neither a ggplot nor a grob.")
    }
  }
  pattern <- "guide-box"
  if (!is.null(legend)) {
    pattern <- paste0(pattern, "-", legend)
  }
  indices <- grep(pattern, gt$layout$name)
  not_empty <- !vapply(
    gt$grobs[indices], 
    inherits, what = "zeroGrob", 
    FUN.VALUE = logical(1)
  )
  indices <- indices[not_empty]
  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}