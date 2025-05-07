##########################################
#load arguments from ubelix array
# args9="20231129"
# for(args1 in c(3)){
#   for(args2 in c(2)){
#     for(args3 in c(3)){
#       for(args4 in c(1,2,3)){
#         for(args5 in c(1)){
#  args=c(args1,args2,args3,args4,args5,NA,NA,NA,args9)

##########################################
#Setting up paths
is.sim.cluster=TRUE
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/cluster")[[1]][1],"/")
source(paste0(code_root_path,"R/000_setup.R"))

args_all=(commandArgs(TRUE)) #args_all=c(1,1,"20241218")
args=as.numeric(unlist(args_all[1:2]))#args[9] should be simulation date
save.date=as.character(args_all[[3]])

print("print args")
print(args)
print(save.date)


#load data

age_classes = c("0-17","18-39","40-64","65-79","80+")
causes_corr =  causes = c("Cardiovascular Diseases", "Mental and Neurological Disorders",
                          "Infectious and Parasitic Diseases",
                          "Neoplasms (Cancers)","Suicide","External Causes",
                          "Other Causes")

corr_resp_lag_post_cluster = function(age_class=age_classes[args[1]],
                                      y = causes_corr[args[2]],
                                      chains=1:4,mod="mod8",
                                      save.date)
if(FALSE){
  age_class = age_classes[args[1]]
  y = causes_corr[args[2]]
  chains = 1:4
  mod = "mod8"
}
