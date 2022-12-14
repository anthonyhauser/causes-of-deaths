#setup
pkg.min=TRUE #if TRUE, load only necessary package to update excess mortality
code_root_path = paste0(getwd(),"/")
print(code_root_path)
source("R/setup.R")

#Define last days of collected temperature data
resolution = "single_levels" #c("land","single_levels")
last_day_ERA5 = c("land" = "2022-07-31", "single_levels" = "2022-12-04") #This should be a Sunday.
#Define last Sunday of temperature data collection. To find the last Sunday of available data go to:
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
#and select 2022 and the last available month and day

#define week of death data
death_data_week="W48"
#select model to run
x=d_mod[4,]

##############################################################################################################################
#Block 1: Temperature data

#Step 1: data from 2009 to 2022

#Step 2: Update data in 2022, combine them and save them

#Two options:
#1) accessing them with:
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form

#2) use download_TemperatureERA5()
cds.user <- "149961" # Insert your CDS user here
cds.key <- "288f1fb2-1ce4-45a1-842f-704c25c82818" #"Insert_your_CDS_API_KEY_here"

load(paste0("data/temperature/",as.character(x["resolution"]),"/GetTemperature.List_2022.RData"))
date_tmp_min = GetTemperature.2022$date %>% max()
date_tmp_max = last_day_ERA5[as.character(x["resolution"])]
download_TemperatureERA5(cds.user, cds.key,
                         year_min = 2022,
                         year_max = 2022,
                         date_min = date_tmp_min, #start at the last day we have data
                         date_max = ymd(date_tmp_max)+1, #end one day after Sunday, to have the whole week till Sunday complete
                         resolution = as.character(x["resolution"]),
                         save_name = paste0("temperature_2022_CH_from_",date_tmp_min,"_to_",ymd(date_tmp_max)+1,".nc"),
                         save_path = paste0("data/temperature/",as.character(x["resolution"]),"/2022"))

# Step 2.2: clean data, combine 2022 datasets
GetTemperature.2022 =  clean_TemperatureERA5(year_min = 2022, year_max=2022,
                                             date_min = "2022-01-01",
                                             date_max = date_tmp_max,
                                             resolution = as.character(x["resolution"]),
                                             aggregate.2022 = TRUE,
                                             load_name = NA) %>% 
  as_tibble()
save(GetTemperature.2022, file=paste0("data/temperature/", as.character(x["resolution"]),"/GetTemperature.List_2022.RData"))

#Step 2.3: aggregate by week and save data ->data
load(paste0("data/temperature/",as.character(x["resolution"]),"/GetTemperature.List_2009_2021.RData"))
GetTemperature.List = c(GetTemperature.List.2009.2021, list(GetTemperature.2022))
#By canton ->data
weighted_average_temp_by_canton(GetTemperature.List, last_day_ERA5,
                                geo_region = "NUTS3", resolution = as.character(x["resolution"]))

##############################################################################################################################
#Block 3: Deaths

#Step 1: Load FSO deaths data
#1. Go to https://www.bfs.admin.ch/bfs/fr/home/statistiques/population.html: Décès selon la classe d'âge quinquennale, le sexe, la semaine et le canton
#2. Download the latest data in 2022, check the latest week in the data and define the parameter death_data_week
#3. Save the data as paste0(data/ch_deaths_2022_",death_data_week,".csv")
#3. Run the following function
save_deaths(death_data_week)

#Step 2: Laboratory deaths data (DONE WITH BAG COMPUTER)
#labd = readRDS("data/excess_mortality_labo_deaths.rds")
#clean_lab_deaths(labd) #save as savepoint/labo_deaths.rds

##############################################################################################################################
#Block 4: Population


##############################################################################################################################
#Block 5: Combine datasets and save them in savepoint
combine_datasets(last_day_ERA5, geo_region = as.character(x["geo_region"]), resolution = as.character(x["resolution"]),
                 add.temperature = as.logical(x["add.temperature"]),
                 save_data10_19=TRUE, temp.method=as.character(x["temp.method"]))


##############################################################################################################################
#Block 6: Model run (done in UBELIX)

##############################################################################################################################
#Block 7: Combine UBELIX results and make predictions
#poisson predicted deaths 2020-2022
make_predictions(geo_region = x["geo_region"], resolution = as.character(x["resolution"]),last_day_ERA5,
                 run_date=as.character(x["ubelix_date"]),add.temperature = as.logical(x["add.temperature"]),temp.method=x["temp.method"],
                 mod=x["mod"])

##############################################################################################################################
#load samples and take a subsample, clean and calculate excess
dat = load_samples(geo_region = x["geo_region"], resolution = as.character(x["resolution"]), run_date=x["ubelix_date"],
                   add.temperature = as.logical(x["add.temperature"]), temp.method = x["temp.method"], mod = x["mod"],
                   death_data_week = death_data_week, pred = TRUE, last_day_ERA5=last_day_ERA5)
dat2 = clean_samples(dat) #takes about 30sec
dat3 = get_excess(dat2) #takes about 1min
print(paste0("Date range (Monday): ", range(dat3$week)))#this gives the date of the monday of the last week with data

#merge with labo deaths (not used here), phase date range and NUTS data
merg = dat3 %>%
  dplyr::mutate(labo_deaths=NA) %>% 
  dplyr::left_join(phases,by="week")  %>% 
  dplyr::filter(!is.na(phase)) %>%
  left_join(NUTS %>% dplyr::select(Kanton,region=NUTS2_name),
            by=c("canton"="Kanton"))

#Summarise (by week phase, age_group, region)
l_summ_week_temp = summarise_by(merg %>% filter(!is.na(deaths)), by=c("week"))
l_summ_phase_temp = summarise_by(merg %>% filter(!is.na(deaths)), by=c("phase"))
l_summ_age_temp = summarise_by(merg %>% filter(!is.na(deaths)), by=c("age_group"))
l_summ_region_temp = summarise_by(merg %>% filter(!is.na(deaths)), by=c("region"))
l_summ_age_phase_temp = summarise_by(merg %>% filter(!is.na(deaths)), by=c("age_group","phase"))
l_summ_age_week_temp = summarise_by(merg %>% filter(!is.na(deaths)), by=c("age_group","week"))
l_summ_region_phase_temp =summarise_by(merg %>% filter(!is.na(deaths)), by=c("region","phase"))
l_summ_region_week_temp =summarise_by(merg %>% filter(!is.na(deaths)), by=c("region","week"))

save(l_summ_week_temp,l_summ_phase_temp,l_summ_age_temp,l_summ_region_temp,l_summ_age_phase_temp,
     l_summ_age_week_temp,l_summ_region_phase_temp,l_summ_region_week_temp,
     file = paste0("results/summaries_single_levels_",last_day_ERA5[as.character(x["resolution"])],"_run_date_",x["ubelix_date"],".RData"))

##############################################################################################################################
#Plots and tables

rmarkdown::render('report_bag.Rmd',
                  output_file = paste0("reports/report_inla_single_levels_",last_day_ERA5[as.character(x["resolution"])],"_run_date_",x["ubelix_date"]))