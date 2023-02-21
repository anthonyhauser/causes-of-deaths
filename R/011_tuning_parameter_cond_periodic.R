tuning_parameter_cond_periodic = function(l){
  lengthscale = quantile(l,probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  m = quantile(3.72/l, probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  d = rbind(lengthscale,m)
  colnames(d) = c("q1","q5","q25","q50","q75","q95","q99")
  d = cbind(data.frame(par=c("lengthscale","m.min")),
            d)
  
  return(d)
}