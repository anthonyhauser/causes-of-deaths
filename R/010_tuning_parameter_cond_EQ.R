tuning_parameter_cond_EQ = function(l,xn,x_sd=1){
  S=max(abs(xn))
  lengthscale = quantile(l,probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  lengthscale_unnorm = quantile(l*x_sd,probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  c = 3.2 * lengthscale/S
  m.1.5 = 1.75 * 1.5/(lengthscale/S)
  m.5 = 1.75 * 5/(lengthscale/S)
  m.10 = 1.75 * 10/(lengthscale/S)
  d = rbind(lengthscale,lengthscale_unnorm,c,m.1.5,m.5,m.10)
  colnames(d) = c("q1","q5","q25","q50","q75","q95","q99")
  d = cbind(data.frame(par=c("lengthscale","lengthscale (not norm)","c.min","m.min (if c=1.5)", "m.min (if c=5)", "m.min (if c=10)")),
            d)
  
  return(d)
}
