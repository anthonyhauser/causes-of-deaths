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