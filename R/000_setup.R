# library(ISOweek)
# library(lubridate)
# library(data.table)
# library(tidyfast)
library(pacman)
pacman::p_load(ISOweek, lubridate, data.table, tidyfast, tidyr, dplyr,purrr,ggplot2,stringr)

#cmdstanr
library(cmdstanr)
cmdstan_path()

if(!grepl("ahauser6",getwd())){
  set_cmdstan_path("C:/TEMP/.cmdstan/cmdstan-2.35.0")
  library(tidyverse)
  library(flextable)
  library(officer)
  library(readxl)
  library(scales)
}

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

theme_set(theme_bw())

#covid phase
covid_phase0 <- data.frame(
  start_date = as.Date(c("1995-01-02", "2019-11-04", "2020-03-02", "2020-06-08", "2020-09-28", 
                         "2021-03-01", "2021-08-02", "2021-11-01"),),
  phase = 0:7,
  labels=c("Before 2020","Nov19-Feb20","Mar-Mai20 (1st wave)","Jun-Sep20"," Oct20-Feb21\n(2nd/3rd wave)","Mar-Jul21",
           "Aug-Oct21","Nov-Dec21")) %>%
  arrange(start_date) %>%
  mutate(end_date = lead(start_date, default = as.Date("2022-01-03")) - 1,
         n_weeks = as.numeric((1+end_date-start_date)/7))
covid_phase <- data.frame(
  start_date = as.Date(c("1995-01-02", "2019-11-04", "2020-02-24","2020-06-08","2020-09-28",
                         "2021-02-15","2021-06-21","2021-10-11","2021-12-20"),),
  phase = 0:8,
  labels=c("0. Before 2020","Nov19-Feb20","1. Feb-Jun20 (1st wave)","2. Jun-Sep20","2b. Oct20-Feb21\n(2nd/3rd wave)","3. Feb-Jun21",
           "4. Jun-Oct21","5. Oct-Dec21","6. Dec21")) %>%
  arrange(start_date) %>%
  mutate(end_date = lead(start_date, default = as.Date("2022-01-03")) - 1,
         n_weeks = as.numeric((1+end_date-start_date)/7))

cod_df = data.frame(cod_full=c("Cardiovascular Diseases","External Causes","Infectious and Parasitic Diseases",
          "Mental and Neurological Disorders",
          "Neoplasms (Cancers)","No Specific Causes", "Respiratory Diseases", "Suicide","Other Causes","COVID-19"),
          cod_1word = c("cardiovascular","external","infectious","mental","cancer","nocause","respiratory","suicide","other","covid-19"))

#canton
canton_df <- data.frame(
  ctn_name = c("Zurich", "Berne", "Lucerne", "Uri", "Schwyz", "Obwald", "Nidwald", "Glaris", 
               "Zoug", "Fribourg", "Soleure", "Bâle-Ville", "Bâle-Campagne", "Schaffhouse", 
               "Appenzell Rh.-Ext.", "Appenzell Rh.-Int.", "Saint-Gall", "Grisons", 
               "Argovie", "Thurgovie", "Tessin", "Vaud", "Valais", "Neuchâtel", "Genève", "Jura"),
  ctn = c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", 
          "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", 
          "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", 
          "GE", "JU"),
  ctn_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
             17, 18, 19, 20, 21, 22, 23, 24, 25, 26)) %>%
  left_join(data.frame(ctn = c("VD","VS","GE","BE","FR","SO","NE","JU","BS","BL","AG","ZH","GL",
                               "SH","AR","AI","SG","GR","TG","LU","UR","SZ","OW","NW","ZG","TI"),
                       NUTS2_id = c(rep(c(1:7),c(3,5,3,1,7,6,1))),
                       NUTS2_name = rep(c("Lake Geneva","Mittelland","Northwest","Zurich","Eastern","Central","Ticino"),
                                        c(3,5,3,1,7,6,1))))

#load R files
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster")[[1]][1],"/")
path_functions = list.files(pattern="[.]R$", path=paste0(code_root_path,"/R/"), full.names=TRUE)
path_functions = path_functions[!grepl("000",path_functions)]
print(path_functions)
sapply(path_functions, source)

controls=list(load.encrypted.data=FALSE)



