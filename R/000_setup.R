library(tidyverse)
library(rstan)
library(ISOweek)
library(lubridate)
library(flextable)
library(officer)
library(readxl)
library(cmdstanr)
library(data.table)
library(tidyfast)


code_root_path=getwd()
path_functions = list.files(pattern="[.]R$", path=paste0(code_root_path,"/R/"), full.names=TRUE)
path_functions = path_functions[!grepl("000",path_functions)]
print(path_functions)
sapply(path_functions, source)

controls=list(load.encrypted.data=FALSE)
