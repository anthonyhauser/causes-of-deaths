library(tidyverse)
library(rstan)
library(ISOweek)
library(lubridate)
library(flextable)
library(officer)
library(readxl)
library(data.table)
library(tidyfast)

#cmdstanr
library(cmdstanr)
set_cmdstan_path("C:/TEMP/.cmdstan/cmdstan-2.35.0")
cmdstan_path()

if(FALSE){#check cmdstan
  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  mod <- cmdstan_model(file)
  mod$exe_file()
  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
  
  fit <- mod$sample(
    data = data_list,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 500 # print update every 500 iters
  )
}



code_root_path=getwd()
path_functions = list.files(pattern="[.]R$", path=paste0(code_root_path,"/R/"), full.names=TRUE)
path_functions = path_functions[!grepl("000",path_functions)]
print(path_functions)
sapply(path_functions, source)

controls=list(load.encrypted.data=FALSE)
