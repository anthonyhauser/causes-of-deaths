tuning_parameter_cond_EQ = function(l,xn){
  S=max(abs(xn))
  lengthscale = quantile(l,probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  c = quantile(3.2 * l/S, probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  m.1.5 = quantile(1.75 * 1.5/(l/S), probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  m.5 = quantile(1.75 * 5/(l/S), probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  m.10 = quantile(1.75 * 10/(l/S), probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
  d = rbind(lengthscale,c,m.1.5,m.5,m.10)
  colnames(d) = c("q1","q5","q25","q50","q75","q95","q99")
  d = cbind(data.frame(par=c("lengthscale","c.min","m.min (if c=1.5)", "m.min (if c=5)", "m.min (if c=10)")),
            d)
  
  return(d)
}