library(tidyverse)
library(rstan)
library(ISOweek)
library(lubridate)
library(flextable)
library(officer)
library(readxl)
library(data.table)
library(tidyfast)

library(scales)

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

#covid phase
covid_phase <- data.frame(
  start_date = as.Date(c("1995-01-02", "2019-11-04", "2020-03-02", "2020-06-08", "2020-09-28", 
                         "2021-03-01", "2021-08-02", "2021-11-01"),),
  phase = 0:7,
  labels=c("Before 2020","Nov19-Feb20","Mar-Mai20 (1st wave)","Jun-Sep20"," Oct20-Feb21\n(2nd/3rd wave)","Mar-Jul21",
           "Aug-Oct21","Nov-Dec21")) %>%
  arrange(start_date) %>%
  mutate(end_date = lead(start_date, default = as.Date("2022-01-03")) - 1,
         n_weeks = as.numeric((1+end_date-start_date)/7))

cod_df = data.frame(cod_full=c("Cardiovascular Diseases","External Causes","Infectious and Parasitic Diseases",
          "Mental and Neurological Disorders",
          "Neoplasms (Cancers)","No Specific Causes", "Respiratory Diseases", "Suicide"),
          cod_1word = c("cardiovascular","external","infectious","mental","cancer","nocause","respiratory","suicide"))

#load R files
code_root_path=getwd()
path_functions = list.files(pattern="[.]R$", path=paste0(code_root_path,"/R/"), full.names=TRUE)
path_functions = path_functions[!grepl("000",path_functions)]
print(path_functions)
sapply(path_functions, source)

controls=list(load.encrypted.data=FALSE)


days_to_datetime_2020 <- function(days) {
  # Start date is January 1, 2020
  start_date <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  
  # Add the number of days as seconds to the start date
  result_datetime <- start_date + days * 24 * 60 * 60
  
  return(result_datetime)
}