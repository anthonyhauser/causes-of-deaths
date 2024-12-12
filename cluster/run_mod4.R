##########################################
#load arguments from ubelix array
# args9="20231129"
# for(args1 in c(3)){
#   for(args2 in c(2)){
#     for(args3 in c(3)){
#       for(args4 in c(1,2,3)){
#         for(args5 in c(1)){
#  args=c(args1,args2,args3,args4,args5,NA,NA,NA,args9) #args_all=c(4,7,1,4,1,1,1,1,20240620)

##########################################
#Setting up paths
is.sim.cluster=TRUE
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster")[[1]][1],"/")
source(paste0(code_root_path,"R/000_setup.R"))

args_all=(commandArgs(TRUE)) #args_all=c(rep(1,8),"20241212")
args=as.numeric(unlist(args_all[1:8]))#args[9] should be simulation date
save.date=as.character(args_all[[9]])

print("print args")
print(args)
print(save.date)

# Check if the folder exists, and create it if it doesn't
folder_path <- paste0(code_root_path,"results/", save.date)
if (!dir.exists(folder_path)) {
  dir.create(folder_path)
  cat("Folder created:", folder_path, "\n")
} else {
  cat("Folder already exists:", folder_path, "\n")
}

#load data
cod_agg_pop_df = readRDS(paste0(code_root_path,"/savepoint/cod_agg_pop_df2.RDS"))
#causes = cod_agg_pop_df$cod_group %>% unique() %>% setdiff(.,"COVID-19")
causes = c("Cardiovascular Diseases","Respiratory Diseases", "Mental and Neurological Disorders",
           "Infectious and Parasitic Diseases",
           "Neoplasms (Cancers)","Suicide","External Causes",
           "Other Causes")
age_classes = cod_agg_pop_df$age_class %>% unique()

mod4_data_pred_pand = run_stan_mod4(cod_agg_pop_df,
                                               age_class=age_classes[args[1]], cause=causes[args[2]],
                                               run.model=TRUE, save.date=save.date)



