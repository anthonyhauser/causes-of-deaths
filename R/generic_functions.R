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
    X = data %>% pull(get(variables[i]))
    if(is.factor(X)){
      X_variables = cbind.fill(X_variables,
                               model.matrix(~ X - 1) %>% as.data.frame() %>% dplyr::select(!grep(pattern=paste0("X",ref_levels[i]), x=colnames(.)))) 
    }else{
      X_variables = cbind.fill(X_variables,
                               X)
    }
  }
  return(X_variables)
}