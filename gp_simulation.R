library(MASS)

##############################################################################################################################################################
#functions

#return a function f(x1,x2) of the exponentiated gaussian kernel
f_k_EQ = function(lengthscale, sd){
  f= function(x1,x2){
    return(sd^2*exp(-1/2*(x1-x2)^2/lengthscale^2 ))
  }
  return(f)
}

#return a function f(x1,x2) of the periodic kernel
f_k_periodic = function(lengthscale, sd,period){
  f= function(x1,x2){
    return(sd^2*exp(-2*sin(pi*abs(x1-x2)/period)^2/lengthscale^2))
  }
  return(f)
}

#return a realization of a gaussian process: https://www.r-bloggers.com/2018/01/r-function-for-simulating-gaussian-processes/
gaussprocess <- function(from = 0, to = 1, K = function(s, t) {min(s, t)},
                         start = 0, m = 1000) {
  # Simulates a Gaussian process with a given kernel
  #
  # args:
  #   from: numeric for the starting location of the sequence
  #   to: numeric for the ending location of the sequence
  #   K: a function that corresponds to the kernel (covariance function) of
  #      the process; must give numeric outputs, and if this won't produce a
  #      positive semi-definite matrix, it could fail; default is a Wiener
  #      process
  #   start: numeric for the starting position of the process
  #   m: positive integer for the number of points in the process to simulate
  #
  # return:
  #   A data.frame with variables "t" for the time index and "xt" for the value
  #   of the process
  
  t <- seq(from = from, to = to, length.out = m)
  Sigma <- sapply(t, function(s1) {
    sapply(t, function(s2) {
      K(s1, s2)
    })
  })
  
  path <- mvrnorm(mu = rep(0, times = m), Sigma = Sigma)
  path <- path - path[1] + start  # Must always start at "start"
  
  return(data.frame("t" = t, "xt" = path))
}


##############################################################################################################################################################
#kernel with different lengthscale and sd
lengscale_plot = c(0.5,1,10)
sd_plot = c(0.1,0.5,1)

#periodic
id_list=0
k_periodic=list()
for(i in lengscale_plot){
  for(j in sd_plot){
    f_k=f_k_periodic(lengthscale=i,sd=j,period=1)
    id_list = id_list+1
     k_periodic[[id_list]]=data.frame(t=seq(0,1,0.01),
                              k = f_k(0,seq(0,1,0.01))) %>% 
        dplyr::mutate(lengthscale=i,sd=j)
  }
}

#EQ
id_list=0
k_EQ=list()
for(i in lengscale_plot){
  for(j in sd_plot){
    f_k=f_k_EQ(lengthscale=i,sd=j)
    id_list = id_list+1
    k_EQ[[id_list]]=data.frame(t=seq(0,1,0.01),
                            k = f_k(0,seq(0,1,0.01))) %>% 
      dplyr::mutate(lengthscale=i,sd=j)
  }
}

#plots
do.call(rbind,k_periodic) %>% 
  dplyr::mutate(sd = paste0("sd=",sd)) %>% 
  ggplot(aes(x=t,y=k,col=factor(lengthscale))) +
  geom_line(size=1) +
  facet_grid(.~sd) +
  theme_bw()

do.call(rbind,k_EQ) %>% 
  dplyr::mutate(sd = paste0("sd=",sd)) %>% 
  ggplot(aes(x=t,y=k,col=factor(lengthscale))) +
  geom_line(size=1) +
  facet_grid(.~sd) +
  theme_bw()




##############################################################################################################################################################
#Simulation Gaussian process 

id_list=0
d_periodic=list()
for(i in lengscale_plot){
  for(j in sd_plot){
    f_k=f_k_periodic(lengthscale=i,sd=j,period=1)
    for(m in 1:10){
      id_list = id_list+1
      d_periodic[[id_list]]=gaussprocess(K = f_k) %>% 
        dplyr::mutate(lengthscale=i,sd=j,iter=m)
      print(paste0("lengthscale=",i,", sd=",j,", iter=",m))
    }
  }
}

#EQ
id_list=0
d_EQ=list()
for(i in lengscale_plot){
  for(j in sd_plot){
    f_k=f_k_EQ(lengthscale=i,sd=j)
    for(m in 1:10){
      id_list = id_list+1
      d_EQ[[id_list]]=gaussprocess(K = f_k) %>% 
        dplyr::mutate(lengthscale=i,sd=j,iter=m)
      print(paste0("lengthscale=",i,", sd=",j,", iter=",m))
    }
  }
}

#plot
do.call(rbind,d_periodic) %>% 
  dplyr::mutate(sd = paste0("sd=",sd),
                lengthscale=paste0("l=",lengthscale)) %>% 
  ggplot(aes(x=t,y=xt,col=factor(iter))) +
  geom_line() +
  facet_grid(lengthscale~sd) +
  theme_bw()

do.call(rbind,d_EQ) %>% 
  dplyr::mutate(sd = paste0("sd=",sd),
                lengthscale=paste0("l=",lengthscale)) %>% 
  ggplot(aes(x=t,y=xt,col=factor(iter))) +
  geom_line() +
  facet_grid(lengthscale~sd) +
  theme_bw()
