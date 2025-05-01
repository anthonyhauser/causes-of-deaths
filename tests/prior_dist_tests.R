lognormal = rlnorm(1000, meanlog=log(15/sd(1:500)),sdlog=1)
mean(lognormal)
quantile(lognormal,probs = c(0.01,0.99))
hist(lognormal)

gamma = rgamma(1000,shape=2,rate=5)
mean(gamma)
quantile(gamma,probs = c(0.01,0.99))
hist(gamma)