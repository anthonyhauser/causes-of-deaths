#Setup
Sys.setlocale("LC_TIME", "C")
wd = getwd()
code_root_path = paste0(strsplit(wd, split="/manuscript")[[1]][1],"/")
print(code_root_path)
source(paste0(code_root_path,"R/000_setup.R"))
knitr::opts_chunk$set(echo = FALSE, warning =FALSE, message=FALSE)


wd_ofsp = "L:/UNISANTE_DESS/S_SUMAC/OFSP_2023/"
data_folder = paste0(wd_ofsp,"02_data/cause_of_death/")
save.date="20241218"
mod="mod8"

col_age = viridisLite::viridis(5,begin = 0,end=0.95)[5:1]
col_cause = viridisLite::viridis(8,begin = 0,end=0.95, option = "C")#viridis() as in scale_color_viridis_d function
show_col(col_age)

################################################################################
#Individual dataset
cod_ind_df = readRDS(paste0(code_root_path,"savepoint/cod_ind_df.RDS"))

causes2 = c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
            "Respiratory Diseases", "Mental and Neurological Disorders",
            "COVID-19",
            "Neoplasms (Cancers)","Suicide","External Causes")

causes2_df = data.frame(cod_group=c("Cardiovascular Diseases","Infectious and Parasitic Diseases",
                                    "Respiratory Diseases", "Mental and Neurological Disorders",
                                    "COVID-19",
                                    "Neoplasms (Cancers)","Suicide","External Causes","Other Causes"),
                        cod_group_label=c("Cardiovascular","Infectious/Parasitic",
                                          "Respiratory", "Mental/Neurological",
                                          "COVID-19",
                                          "Cancers","Suicide","External","Other"),
                        order=c(1,7,4,3,9,2,6,5,8),
                        example = c("Ischaemic heart disease, heart attack", "Sepsis",
                                    "Pulmonary disease, pneumonia, influenza", "Dementia, Alzheimer, Parkinson",
                                    "COVID-19",
                                    "Lung, breast, prostate neoplasms","Intentional self-harm/poisoning","Fall, vehicule accident",
                                    "Unspecified, diabetes, senility, organ disease")) %>%
  #add chapter of cod-10
  left_join(cod_ind_df %>%
              filter(outcome=="ENDG_U_CD_GES_T") %>%
              dplyr::select(cod_group,chapter) %>% unique() %>%
              arrange(cod_group,chapter) %>%
              group_by(cod_group) %>%
              dplyr::summarise(chapter=paste(chapter, collapse=", ")),by="cod_group") %>%
  arrange(order)

################################################################################
#Aggregated dataset and results
add_apo = function(x){format(x, big.mark=",")}

cod_agg_pop_df = readRDS(paste0(code_root_path,"savepoint/cod_agg_pop_df.RDS")) #used to add covid-19 deaths in Figure 2
cod_agg_pop_nuts_df = readRDS(paste0(code_root_path,"savepoint/cod_agg_pop_nuts_df.RDS")) #used for Figure 1.1
age_classes = cod_agg_pop_df$age_class %>% unique() #used
setwd(code_root_path) #in order to load res_list
res_list = load_results_mod6(age_classes, save.date="20241218",mod="mod8") #used

#not used
# cod_agg_pop_df = cod_agg_pop_df %>%
#   dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
#   mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
#     phase <- phases %>%
#       filter(d >= start_date & d <= end_date) %>%
#       pull(phase)
#     if (length(phase) == 0) NA_real_ else phase
#   }))

################################################################################################################################################################
#Figure 1.1
causes2_df_twolines = causes2_df %>% 
  dplyr::mutate(cod_group_label = ifelse(cod_group_label=="Mental/Neurological",
                                         "Mental/\nNeurological",cod_group_label),
                cod_group_label = ifelse(cod_group_label=="Infectious/Parasitic",
                                         "Infectious/\nParasitic",cod_group_label))

cod_agg_pop_nuts_df %>% 
  filter(cal_year %in% c(2020,2021),age_class=="0-17") %>% 
  group_by(cod_group) %>% 
  dplyr::summarise(n = sum(n), .groups = "drop")

p1 = cod_agg_pop_nuts_df %>% 
  filter(cal_year %in% c(2020,2021)) %>% 
  group_by(age_class) %>% 
  dplyr::summarise(n = sum(n), .groups = "drop") %>% 
  dplyr::mutate(p = n / sum(n),
         label = paste0(signif(100 * p, 2), "%")) %>% 
  ggplot(aes(x = age_class, y = n)) +
  geom_col(fill = "gray") +
  geom_text(aes(label = label), vjust = -0.5) +
  scale_x_discrete(name = "Age class") +
  scale_y_continuous(name = "Deaths")

p2 = rbind(cod_agg_pop_nuts_df %>% filter(cal_year %in% c(2020,2021)),
          cod_agg_pop_nuts_df %>% filter(cal_year %in% c(2020,2021)) %>% 
            dplyr::mutate(age_class="Total")) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:9)],#put in the right order
                                   labels=causes2_df_twolines$cod_group_label[c(1:9)]),
                cod_group_id = as.numeric(cod_group)) %>% 
  filter(!is.na(cod_group)) %>% 
  group_by(age_class,cod_group,cod_group_id) %>% 
  dplyr::summarise(n = sum(n), .groups = "drop") %>% 
  group_by(age_class) %>% 
  dplyr::mutate(p = n / sum(n)) %>% ungroup() %>% 
  ggplot(aes(x=cod_group_id,y=p,col=age_class))+
  geom_point()+
  geom_line()+
  scale_x_continuous(name="Causes of deaths",breaks=causes2_df_twolines$order,
                   labels=causes2_df_twolines$cod_group_label)+
  scale_y_continuous(name="Distribution of deaths", labels=scales::percent)+
  scale_color_manual(name="Age class",values=c(col_age,"gray"))

fig1_1 = cowplot::plot_grid(p1,p2,
                   ncol=2,rel_widths = c(1,2),
                   labels=c("A.","B."))

pdf(file=paste0(code_root_path,"/manuscript/fig1_1.pdf"),width=12,height=6)
print(fig1_1)
dev.off()

################################################################################
# Fig 1: infering cause-specific mortality
#Panel A: mortality by week for 80+, by cause
fig1 = lapply(age_classes,function(age){
  print(age)
  fig1a = res_list$data_pred_week_cause %>% 
    filter(variable=="deaths") %>% 
    left_join(res_list$data_pred_week_cause %>% 
                filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,cod_group,pred),
              by=c("cal_year","cal_week","age_class","cod_group","pred")) %>% 
    filter(age_class==age,pred=="poisson") %>% 
    dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                     labels=causes2_df_twolines$cod_group_label[c(1:5,7)])) %>% 
    filter(!is.na(cod_group)) %>% 
    ggplot() +
    geom_line(aes(x=date,y=est),col="black") +
    geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
    geom_point(aes(x=date,y=obs_deaths),col="darkred",alpha=0.5,size=1) +
    geom_vline(aes(xintercept=ymd("2020-01-01")),linetype="dashed")+
    facet_grid(cod_group~age_class,scales="free") +
    scale_y_continuous(name="Deaths")+
    scale_x_date(name="Time")+
    theme_bw()
  
  #Panel B
  fig1b = res_list$Sigma_mat %>% 
    dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                     labels=causes2_df_twolines$cod_group_label[c(1:5,7)]),
                  cod_group2 = factor(cod_group2,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                      labels=causes2_df_twolines$cod_group_label[c(1:5,7)]),
                  cod_group_id=as.numeric(cod_group),
                  cod_group_id2=as.numeric(cod_group2)) %>% 
    filter(!is.na(cod_group),!is.na(cod_group2)) %>% 
    rowwise() %>% 
    dplyr::mutate(est_cri = paste0(scales::percent(est, accuracy = 1),"\n",
                                   "[",scales::percent(lwb, accuracy = 1),",",
                                   scales::percent(upb, accuracy = 1),"]")) %>% 
    filter(cod_group_id<cod_group_id2,age_class==age) %>% 
    ggplot(aes(x = cod_group, y = fct_rev(cod_group2), fill = abs(est))) +
    geom_tile() +
    geom_text(aes(label = est_cri),
              color = "black", size = 2.5) +
    scale_fill_gradient(low = "lightyellow", high = "darkred",limits=c(0,1),
                        name="Correlation",
                        labels=scales::label_percent(accuracy = 1)) +
    labs(x = "", y = "", fill = "Count") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
    # theme(axis.text.x = element_text(angle = 45, hjust = 0,size=11),
    #       axis.text.y = element_text(size=11),
    #       plot.margin = margin(t = -10, r = 5, b = 5, l = 5))+
    # scale_x_discrete(position = "top")
  
  #Panel C
  d1 =readRDS(paste0("results/",save.date,"/","mod8","_peak_dates_summary_df.RDS")) %>%
    filter(age_class == age)  %>% 
    dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                        labels=causes2_df_twolines$cod_group_label[c(1:5,7)])) %>% 
    filter(!is.na(cod_group))
  d2 = readRDS(paste0("results/","observed_peak_date_df.RDS")) %>%
    filter(age_class == age) %>% 
    dplyr::mutate(is_2020 = period_name %in% c("2019/20","2020/21","2021/22")) %>% 
    dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                     labels=causes2_df_twolines$cod_group_label[c(1:5,7)])) %>% 
    filter(!is.na(cod_group)) %>% 
    filter(period_id!=2021) #we remove 2021-22 as we only have date up to end of 2021
  year_df = d2 %>% dplyr::select(period_id,period_name) %>% unique()

  fig1c = d1 %>%
    ggplot(aes(y = cod_group)) +
    geom_vline(xintercept = as.Date("2020-01-01"),linetype="dashed",alpha=0.5)+
    geom_point(aes(x = mean_date),size=5) +
    geom_linerange(aes(xmin = p5_date, xmax = p95_date),linewidth=1.5) +
    geom_point(aes(x=date,color = period_id,size=rel_peak,shape=is_2020),alpha=0.5,
               data = d2)+
    scale_x_date(name="Timing of the mortality peak",
                 limits = c(as.Date("2019-07-01"), as.Date("2020-06-30")),
                 breaks = seq(as.Date("2019-07-01"), as.Date("2020-06-01"), by = "1 month"),
                 labels = scales::label_date("%b")) +
    scale_y_discrete(name="",limits = rev)+
    scale_shape_manual(name="",values=c(16,15),breaks=c(FALSE,TRUE),labels=c("Before pandemic","During pandemic"))+
    scale_color_gradientn(name = "Period",
                          colours = c("yellow", "red", "darkviolet"),  # blue → white → red
                          values = scales::rescale(c(2012, 2019.5, 2021)),  # period_ids or numeric range
                          breaks = year_df$period_id[c(1, 4, 8, 11)],
                          labels = year_df$period_name[c(1, 4, 8, 11)] )+
    scale_size_continuous(name="Relative peak size",breaks=c(1.5,2))+
    theme(legend.position = "bottom")+
    guides(shape = guide_legend(ncol = 1),
           color = guide_colourbar(title = "Period", label.theme = element_text(angle = 90)))

  
  fig1 = cowplot::plot_grid(fig1a,
                     cowplot::plot_grid(fig1b + theme(legend.position = "none"),
                                        fig1c + theme(legend.position = "none"),
                                        ncol=2, rel_widths = c(1,1.26),labels = c("B.","C.")),
                     cowplot::plot_grid(get_legend2(fig1b),
                             get_legend2(fig1c),ncol=2,rel_widths=c(1,3)),
                     labels=c("A.","",""),ncol=1,rel_heights = c(1.8,1,0.2))
  pdf(file=paste0(code_root_path,"/manuscript/fig1_",age,".pdf"),width=10,height=14)
  print(fig1)
  dev.off()
  
  return(fig1)
})

################################################################################################################################################################
#Figure 2: All-cause excess mortality 

main_plot = res_list$data_pred_week %>% 
  filter(variable == "deaths") %>%
  # Add observed deaths
  left_join(res_list$data_pred_week %>%
      filter(variable == "obs_deaths") %>%
      dplyr::select(obs_deaths = est, cal_year, cal_week, age_class, pred),by = c("cal_year", "cal_week", "age_class", "pred")) %>%
  # Add COVID-19 deaths
  left_join(cod_agg_pop_df %>%
      filter(cod_group == "COVID-19") %>%
      group_by(age_class, cal_year, cal_week) %>%
      dplyr::summarise(n_covid = sum(n), .groups = "drop"),by = c("age_class", "cal_year", "cal_week")) %>%
  dplyr::mutate(obs_deaths_with_covid = obs_deaths + n_covid,
                obs_deaths_with_covid = ifelse(cal_year < 2020, NA, obs_deaths_with_covid)) %>%
  filter(pred == "poisson") %>%
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = lwb, ymax = upb), fill="black",
    alpha = 0.15,show.legend = FALSE) +
  geom_line(aes(y = est), color="black",linewidth = 0.8) +
  geom_point(aes(y = obs_deaths), color="darkred", alpha = 0.5, size = 1) +
  geom_point(aes(y = obs_deaths_with_covid), color="orange", alpha = 0.4, size = 1) +
  geom_vline(aes(xintercept = ymd("2020-01-01")), linetype = "dashed") +
  facet_grid(age_class ~ ., scales = "free_y") +
  scale_y_continuous(name = "Deaths") +
  scale_x_date(name = "") +
  theme(legend.position = "none")

# Legend for Observed deaths
df_dummy <- data.frame(x = 1, y = 1)
legend_red <- ggplot(df_dummy, aes(x, y, color = "Observed deaths")) +
  geom_point() +
  scale_color_manual(name = "", values = c("Observed deaths" = "darkred")) +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -20)) +
  guides(color = guide_legend(override.aes = list(shape = 16))) 
legend_red <- get_legend2(legend_red)

legend_orange <- ggplot(df_dummy, aes(x, y, color = "Observed deaths incl. COVID-19")) +
  geom_point() +
  scale_color_manual(name = "", values = c("Observed deaths incl. COVID-19" = "orange")) +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -20)) +
  guides(color = guide_legend(override.aes = list(shape = 16)))
legend_orange <- get_legend2(legend_orange)

legend_black <- ggplot(df_dummy, aes(x, y)) +
  geom_ribbon(aes(ymin = y - 0.1, ymax = y + 0.1, fill = "Expected deaths (95% CrI)"), alpha = 0.2) +
  geom_line(aes(y = y, color = "Expected deaths (95% CrI)"), linewidth = 0.8) +
  scale_color_manual(name="",values = c("Expected deaths (95% CrI)" = "black")) +
  scale_fill_manual(name="",values = c("Expected deaths (95% CrI)" = "black")) +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -20))
legend_black <- get_legend2(legend_black)

#Cumulative all-cause excess by age
cum_excess_allcause_pand_df = readRDS(paste0("results/",save.date,"/",mod,"_cum_excess_allcause_pand_df.RDS"))
p2 = cum_excess_allcause_pand_df %>% 
  dplyr::filter(pred == "poisson") %>% 
  ggplot(aes(group = with_covid)) +
  geom_ribbon(aes(x = date, ymin = excess_lwb, ymax = excess_upb,fill=factor(with_covid)),
              alpha = 0.1) +
  geom_line(aes(x = date, y = excess_mean, color = factor(with_covid)), size = 0.7) +
  geom_hline(yintercept = 0, linetype = 4) +
  facet_wrap(~ cod_group_age, scales = "free_y", nrow = 1) +
  scale_y_continuous(name = "Deaths") +
  scale_color_manual(name = "All-cause excess mortality",
                     breaks = c(0, 1),
                     values = c("darkred","orange"),
                     labels=c("Excluding COVID-19","Including COVID-19")) +
  scale_fill_manual(name = "All-cause excess mortality",
                    breaks = c(0, 1),
                    values =  c("darkred","orange"),
                    labels=c("Excluding COVID-19","Including COVID-19") ) +
  scale_x_date(name = "",date_labels = "%b %Y")+ 
  theme(  legend.position = c(0.95, 0.05),  # bottom right
          legend.justification = c("right", "bottom"),
          strip.text = element_text(size = 11),
          plot.margin = margin(5.5,14, 5.5, 5.5)) +
  facet_wrap(age_class ~ ., scales = "free")


fig2 <- cowplot::plot_grid(main_plot,
                           cowplot::plot_grid(legend_black, legend_red, legend_orange, nrow = 1),
                           p2, labels=c("A.","","B."),
                           ncol = 1,  rel_heights = c(1, 0.04,0.6))

pdf(file=paste0(code_root_path,"/manuscript/fig2.pdf"),width=11,height=12)
print(fig2)
dev.off()


cum_excess_allcause_pand_df %>% filter(age_class=="80+",date==ymd("2021-12-20"),pred=="poisson",with_covid==1)



################################################################################################################################################################
#Figure 3: excess by age and cod

#load Data
#Relative excess by phase
#Adapt it. Why? Because before rel_excess was calculated for each iteration and then summarised. Problem: some expected deaths samples are 0 as it is sampled from a poisson distribution
#Current method: summarise absolute excess from samples (by phase) and then divide by the summarised expected excess
excess_phase2_pand_df = readRDS(paste0("results/",save.date,"/","mod8","_excess_phase2_pand_df.RDS"))
rel_excess_phase2_pand_df = excess_phase2_pand_df %>%
  dplyr::filter(variable=="excess") %>%
  dplyr::mutate(variable="rel_excess") %>% 
  left_join(excess_phase2_pand_df %>% filter(variable=="deaths") %>% 
              dplyr::select(covid_phase,age_class,cod_group,pred,exp_deaths_mean = est),
            by=c("covid_phase","age_class","cod_group","pred")) %>% 
  dplyr::mutate(est = est/exp_deaths_mean,
                lwb = lwb/exp_deaths_mean,
                upb = upb/exp_deaths_mean)
#use the new definition of rel_excess and integrate it in the df in place of the old definition
excess_phase2_pand_df = rbind(excess_phase2_pand_df %>% filter(variable!="rel_excess"),
                              rel_excess_phase2_pand_df %>% 
                                dplyr::select(variable,covid_phase,age_class,cod_group,pred,est,lwb,upb))

excess_phase2_pand_df %>% filter(age_class %in% c("65-79","80+"),covid_phase==4,cod_group=="Cardiovascular Diseases",
                                 pred=="poisson")
excess_phase2_pand_df %>% filter(age_class %in% c("80+"),covid_phase==5,cod_group=="Mental and Neurological Disorders",
                                 pred=="poisson")
excess_phase2_pand_df %>% filter(age_class %in% c("0-17"),covid_phase==1,pred=="poisson")

#Relative cumulative excess
cum_excess_pand_df = readRDS(paste0("results/",save.date,"/","mod8","_cum_excess_pand_df.RDS"))
#Cumulative excess deaths at the end of 2021
cum_all_excess_pand_df = cum_excess_pand_df %>% filter(week.id==max(week.id),pred=="poisson") %>% 
  left_join(res_list$data_pred_week_cause %>% dplyr::filter(variable=="obs_deaths",pred=="poisson",cal_year>=2020) %>% 
              arrange(age_class,cod_group,date) %>% 
              group_by(age_class,cod_group) %>% dplyr::mutate(obs_deaths=cumsum(est)) %>% ungroup() %>% 
              dplyr::select(age_class,cod_group,date,obs_deaths) %>% 
              filter(date==max(date)),
            by=c("age_class","cod_group","date")) %>% 
  dplyr::mutate(deaths_mean = obs_deaths-excess_mean,
                deaths_lwb = obs_deaths-excess_upb,
                deaths_upb = obs_deaths-excess_lwb) %>% 
  dplyr::select(-c(rel_excess_mean,rel_excess_lwb,rel_excess_upb))
  
cum_all_excess_pand_df %>% filter(age_class=="0-17",cod_group=="Suicide")

cum_excess_pand_df %>% filter(date==as.Date("2021-12-20"),pred=="poisson",
                              age_class=="80+",cod_group %in% c("Respiratory Diseases","Mental and Neurological Disorders"))

# Filter and prepare phase-level data
phase_df0 <- rel_excess_phase2_pand_df %>%
  dplyr::mutate(cod_group = factor(cod_group,
                            levels = causes2_df$cod_group[c(1:5)],
                            labels = causes2_df$cod_group_label[c(1:5)])) %>% 
  filter(covid_phase<8,
         variable == "rel_excess",
         pred == "poisson",
         age_class == "80+",
         !is.na(cod_group)) %>% 
  left_join(covid_phase2, by = c("covid_phase" = "phase")) %>%
  mutate(phase_mid = start_date + (end_date - start_date)/2)

################################################################################
#Main figure: fig3
nrow=4
ncol=3
labels = rep("",nrow*ncol)
labels[c(1,7,2,3)] = c("A.","B.","C.","D.")
#Selection of combination of age classes and causes that are plotted
sel_plot = cross_join(data.frame(age_class=age_classes),data.frame(cause=causes2))[c(9,14,17,22,25,27,28,30,33,35,36,38),] %>% 
  mutate(rowwise_index = row_number(),
         plot_position = ((rowwise_index - 1) %% nrow) * ncol + ((rowwise_index - 1) %/% nrow) + 1) %>% 
  arrange(plot_position)
# Generate the list of plots
plots <- purrr::pmap(sel_plot, ~{
  age <- ..1
  cause <- ..2
  p <- plot_excess(cum_excess_pand_df,
                   rel_excess_phase2_pand_df,
                   covid_phase2,age, cause) +
    theme(
      legend.position = "none",
      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    )
  return(p)
})

# Customize plots for axis titles and ticks
for (i in seq_along(plots)) {
  # Add left y-axis title for plots 1, 2, 3, 4 (column 1)
  if (i %in% c(1,4,7,10)) {
    plots[[i]] <- plots[[i]] +
      theme(axis.title.y.left = element_text())
  }
  # Add right y-axis title for plots 9–12 (column 3)
  if (i %in% c(3,6,9,12)) {
    plots[[i]] <- plots[[i]] +
      theme(axis.title.y.right = element_text(color = scales::alpha("black", 0.5)))
  }
  # Add x-axis title and ticks only for bottom row (4, 8, 12)
  if (i %in% 10:12) {
    plots[[i]] <- plots[[i]] +
      theme(axis.text.x = element_text())
  }
  if (i==11) {
    plots[[i]] <- plots[[i]] +
      theme(axis.title.x = element_text())
  }
  plots[[i]] = plots[[i]] +
    scale_x_date(name = "",date_labels = "%b %Y")+ 
    ggtitle(labels[i]) +
    theme(plot.title = element_text(face = "bold",
                                    hjust = 0,          # Align left (0 = far left, 1 = far right)
                                    vjust = -2,        # Slightly above the title area
                                    size = 14),
          plot.title.position = "plot")  # Align based on entire plot, not just panel
}

# Wrap them with patchwork — use guides = 'collect' to remove repeated legends
final_plot <- wrap_plots(plots, ncol = 3, byrow = TRUE) &
  theme(legend.position = "none") &
  theme(plot.margin = margin(-5, 4, 0, 1))

fig3 = cowplot::plot_grid(final_plot,
                          cowplot::plot_grid(get_legend2(data.frame(x=1:2,y=1:2,ymin=1:2-1,ymax=2:5,col="1") %>% 
                                                  ggplot(aes(x=x,y=y,ymin=ymin,ymax=ymax,fill=col))+
                                                  geom_ribbon(alpha=0.3)+geom_line(aes(col=col))+
                                                  scale_color_manual(name = "", breaks = "1", labels = "Cumulative excess", values = "black")+
                                                  scale_fill_manual(name = "", breaks = "1", labels = "Cumulative excess", values =  "gray80")),
                                    get_legend2(plots[[1]]+theme(legend.position="bottom")),
                                    rel_widths=c(1,4)),
                          rel_heights=c(9,1),ncol=1)

# 18-39: -low mortality, difficult to interpret variation
# - Still we observed slightly lower cumulative number of CVD, notably during the delta wave early 2021
# - Higher number of cancers, with excess coinciding with covid-19 wave
pdf(file=paste0(code_root_path,"/manuscript/fig3.pdf"),width=13.5,height=10.6)
print(fig3)
dev.off()


################################################################################
#Supplementary figures: fig3
ncol=3
nrow=3

fig3_age = lapply(age_classes,function(age){
  print(age)
  #Selection of combination of age classes and causes that are plotted
  sel_plot = cross_join(data.frame(age_class=age),
                        data.frame(cause=causes2)) %>% 
    dplyr::filter(cause!="COVID-19") %>% 
    mutate(rowwise_index = row_number(),
           plot_position = ((rowwise_index - 1) %% nrow) * ncol + ((rowwise_index - 1) %/% nrow) + 1) %>% 
    arrange(plot_position)
  # Generate the list of plots
  plots <- purrr::pmap(sel_plot, ~{
    cause <- ..2
    p <- plot_excess(cum_excess_pand_df,
                     rel_excess_phase2_pand_df,
                     covid_phase2,
                     age, cause) +
      scale_x_date(name = "",date_labels = "%b %Y")+ 
      theme(legend.position = "none",
            axis.title.y.left = element_blank(),
            axis.title.y.right = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank())
    return(p)
  })
  
  # Customize plots for axis titles and ticks
  for (i in seq_along(plots)) {
    # Add left y-axis title for plots 1, 2, 3, 4 (column 1)
    if (i %in% c(1,4,7)) {
      plots[[i]] <- plots[[i]] +
        theme(axis.title.y.left = element_text())
    }
    # Add right y-axis title for plots 9–12 (column 3)
    if (i %in% c(3,6)) {
      plots[[i]] <- plots[[i]] +
        theme(axis.title.y.right = element_text())
    }
    # Add x-axis title and ticks only for bottom row (4, 8, 12)
    if (i %in% 5:7) {
      plots[[i]] <- plots[[i]] +
        theme(axis.text.x = element_text(),
              axis.title.x = element_text())
    }
  }
  
  # Your legend grob (already built from your code)
  legend1 <- get_legend2(
    data.frame(x = 1:2, y = 1:2, ymin = 1:2 - 1, ymax = 2:5, col = "1") %>%
      ggplot(aes(x = x, y = y, ymin = ymin, ymax = ymax, fill = col)) +
      geom_ribbon(alpha = 0.3) +
      geom_line(aes(col = col)) +
      scale_color_manual(name = "", breaks = "1", labels = "Cumulative excess", values = "black") +
      scale_fill_manual(name = "", breaks = "1", labels = "Cumulative excess", values = "gray80") +
      theme(legend.position = "right")
  )
  
  legend2 <- get_legend2(plots[[1]] + theme(legend.position = "bottom"))
  
  # Create main plot without legend
  final_plot <- wrap_plots(plots, ncol = ncol, byrow = TRUE) &
    theme(legend.position = "none") &
    theme(plot.margin = margin(5, 4,5, 1))
  
  # Compose final plot with legend in bottom-left
  fig <- cowplot::ggdraw() +
    cowplot::draw_plot(final_plot, x = 0, y = 0, width = 1, height = 1) +
    cowplot::draw_grob(legend1, x = 0.45, y = 0.08, width = 0.25, height = 0.1) +  # bottom-left corner
    cowplot::draw_grob(legend2, x = 0.45, y = 0.2, width = 0.4, height = 0.07)     # optional second legend
  
  
  pdf(file=paste0(code_root_path,"/manuscript/fig3_",age,".pdf"),width=13.5,height=10.6)
  print(fig)
  dev.off()
  
  return(fig)
})

################################################################################
# Check causes by age (not used)
n_causes <- 5

# Get top causes per age group in 2020
top_causes <- cod_agg_pop_nuts_df %>%
  filter(cod_group!="COVID-19",cod_group!="Other Causes") %>% 
  filter(cal_year == 2020) %>%
  group_by(age_class, cod_group) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  semi_join(causes2_df_twolines, by = "cod_group") %>%
  group_by(age_class) %>%
  slice_max(order_by = n, n = n_causes, with_ties = FALSE) %>%
  ungroup()

cod_list <- top_causes %>%
  group_by(age_class) %>%
  summarise(causes = list(cod_group), .groups = "drop") %>%
  deframe()

# List of age classes
age_classes <- unique(top_causes$age_class)

top_causes 

##############################################################################################################################################
##Figure 4: Correlation
#partial correlation from mean estimate of excess mortality
corr_res_df = readRDS(paste0("results/",save.date,"/",mod,"_corr_res_df.RDS"))
corr_res_df %>% 
  filter(var!="(Intercept)",lag>=-8,lag<=8) %>% 
  ggplot(aes(x=lag,y=est,ymin=lwb,ymax=upb,fill=var))+
  geom_ribbon(alpha=0.1)+
  geom_line(aes(col=var))+
  geom_point(aes(col=var))+
  geom_line(aes(y=pcor_est),linetype="dashed")+
  geom_line(aes(y=r_squared),col="black",linetype="dashed")+
  geom_hline(yintercept = 0,linetype="dashed") +
  scale_y_continuous(limits=c(-1,1))+
  facet_grid(y~age_class)

#posterior partial correlation (i.e., calculated from posterior samples)
corr_post_res_df = readRDS(paste0("results/",save.date,"/",mod,"_corr_post_res_df.RDS")) %>% 
  dplyr::filter(lag>=-8,lag<=8) %>% 
  group_by(age_class,y,var) %>% 
  dplyr::mutate(not_cross0 = any(corr_lwb>0 | corr_upb<0),
                lag_peak = if_else(not_cross0,lag[which.max(abs(corr_mean))],NA),
                peak = if_else(not_cross0,corr_mean[which.max(abs(corr_mean))],NA)) %>% ungroup()
#all causes and all age groups
fig4_supp = corr_post_res_df %>% 
  dplyr::mutate(y = factor(y,levels=causes2_df_twolines$cod_group,#put in the right order
                           labels=causes2_df_twolines$cod_group_label)) %>% 
  ggplot(aes(x=lag,y=corr_mean,ymin=corr_lwb,ymax=corr_upb))+
  geom_ribbon(aes(fill=var),alpha=0.1)+
  geom_line(aes(col=var))+
  geom_point(aes(col=var))+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(aes(xintercept=lag_peak,col=var),linetype=2,alpha=0.4)+
  scale_x_continuous(name="Lag (week)")+
  scale_y_continuous(name="Partial correlation",
                     limits=c(-1,1))+
  scale_color_discrete(name="Cause")+
  scale_fill_discrete(name="Cause")+
  facet_grid(y~age_class)+
  theme(legend.position = "bottom")

pdf(file=paste0(code_root_path,"/manuscript/fig4_supp.pdf"),width=10,height=12)
print(fig4_supp)
dev.off()
#80+ and some causes
fig4 = corr_post_res_df %>%
  dplyr::mutate(y = factor(y,levels=causes2_df_twolines$cod_group[c(1,2,3)],#put in the right order
                                   labels=causes2_df_twolines$cod_group_label[c(1,2,3)])) %>% 
  dplyr::filter(!is.na(y),
                age_class %in% c("65-79","80+")) %>% 
  ggplot(aes(x=lag,y=corr_mean,ymin=corr_lwb,ymax=corr_upb))+
  geom_ribbon(aes(fill=var),alpha=0.1)+
  geom_line(aes(col=var))+
  geom_point(aes(col=var))+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(aes(xintercept=lag_peak,col=var),linetype=2,alpha=0.8)+
  scale_x_continuous(name="Lag (week)")+
  scale_y_continuous(name="Partial correlation",
                     limits=c(-1,1))+
  scale_color_discrete(name="Cause")+
  scale_fill_discrete(name="Cause")+
  facet_grid(y~age_class)

pdf(file=paste0(code_root_path,"/manuscript/fig4.pdf"),width=10,height=10)
print(fig4)
dev.off()

save(fig1, fig1_1, fig2, fig3, fig3_age, fig4,
        file=paste0(code_root_path,"/manuscript/figures_data.RData"))

##############################################################################################################################################
#Supplementary figures

#S10
fig_s1 = cancer_deaths_plot(cod_ind_df, n_week_agg = 5)
pdf(file=paste0(code_root_path,"/manuscript/fig_s1.pdf"),width=8,height=8)
print(fig_s1)
dev.off()

fig_s1_2 = cancer_deaths_plot2(cod_ind_df, n_week_agg = 1,k=9)
pdf(file=paste0(code_root_path,"/manuscript/fig_s1_2.pdf"),width=8,height=6)
print(fig_s1_2)
dev.off()
  
save(fig1, fig3_age, fig4_supp, fig_s1, fig_s1_2,
       file=paste0(code_root_path,"/manuscript/supp_figures_data.RData"))

##############################################################################################################################################
#Poster


#Fig 1--------------------------------------------------------------------------
#adapt CVD naming for poster
causes2_df_twolines = causes2_df_twolines %>% dplyr::mutate(cod_group_label=ifelse(cod_group=="Cardiovascular Diseases","CVD",cod_group_label))


fig1a = res_list$data_pred_week_cause %>% 
  filter(variable=="deaths") %>% 
  left_join(res_list$data_pred_week_cause %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,cod_group,pred),
            by=c("cal_year","cal_week","age_class","cod_group","pred")) %>% 
  filter(age_class=="80+",pred=="poisson",cod_group %in% c("Respiratory Diseases","Cardiovascular Diseases")) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                   labels=causes2_df_twolines$cod_group_label[c(1:5,7)])) %>% 
  filter(!is.na(cod_group)) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="darkred",alpha=0.5,size=1) +
  geom_vline(aes(xintercept=ymd("2020-01-01")),linetype="dashed")+
  facet_grid(cod_group~.,scales="free") +
  scale_y_continuous(name="Deaths")+
  scale_x_date(name="Time")+
  theme_bw()

#Panel B
fig1b = res_list$Sigma_mat %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                   labels=causes2_df_twolines$cod_group_label[c(1:5,7)]),
                cod_group2 = factor(cod_group2,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                    labels=causes2_df_twolines$cod_group_label[c(1:5,7)]),
                cod_group_id=as.numeric(cod_group),
                cod_group_id2=as.numeric(cod_group2)) %>% 
  filter(!is.na(cod_group),!is.na(cod_group2)) %>% 
  rowwise() %>% 
  dplyr::mutate(est_cri = paste0(round(est,2),"\n",
                                 "[",round(lwb,  2),",",
                                 round(upb, 2),"]")) %>% 
  filter(cod_group_id<cod_group_id2,age_class=="80+") %>% 
  ggplot(aes(x = cod_group, y = fct_rev(cod_group2), fill = abs(est))) +
  geom_tile() +
  geom_text(aes(label = est_cri),
            color = "black", size = 2.5) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",limits=c(0,1),
                      name="Correlation") +
  labs(x = "", y = "", fill = "Count") +
  theme_bw() +
  theme(legend.position = "bottom")
#Panel C
d1 =readRDS(paste0("results/",save.date,"/","mod8","_peak_dates_summary_df.RDS")) %>%
  filter(age_class == "80+")  %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                   labels=causes2_df_twolines$cod_group_label[c(1:5,7)])) %>% 
  filter(!is.na(cod_group))
d2 = readRDS(paste0("results/","observed_peak_date_df.RDS")) %>%
  filter(age_class == "80+") %>% 
  dplyr::mutate(is_2020 = period_name %in% c("2019/20","2020/21","2021/22")) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df_twolines$cod_group[c(1:5,7)],
                                   labels=causes2_df_twolines$cod_group_label[c(1:5,7)])) %>% 
  filter(!is.na(cod_group)) %>% 
  filter(period_id!=2021) #we remove 2021-22 as we only have date up to end of 2021
year_df = d2 %>% dplyr::select(period_id,period_name) %>% unique()

fig1c = d1 %>%
  ggplot(aes(y = cod_group)) +
  geom_vline(xintercept = as.Date("2020-01-01"),linetype="dashed",alpha=0.5)+
  geom_point(aes(x = mean_date),size=5) +
  geom_linerange(aes(xmin = p5_date, xmax = p95_date),linewidth=1.5) +
  geom_point(aes(x=date,color = period_id,size=rel_peak,shape=is_2020),alpha=0.5,
             data = d2)+
  scale_x_date(name="Timing of the mortality peak",
               limits = c(as.Date("2019-07-01"), as.Date("2020-06-30")),
               breaks = seq(as.Date("2019-07-01"), as.Date("2020-06-01"), by = "1 month"),
               labels = scales::label_date("%b")) +
  scale_y_discrete(name="",limits = rev)+
  scale_shape_manual(name="",values=c(16,15),breaks=c(FALSE,TRUE),labels=c("Before pandemic","During pandemic"))+
  scale_color_gradientn(name = "Period",
                        colours = c("yellow", "red", "darkviolet"),  # blue → white → red
                        values = scales::rescale(c(2012, 2019.5, 2021)),  # period_ids or numeric range
                        breaks = year_df$period_id[c(1, 4, 8, 11)],
                        labels = year_df$period_name[c(1, 4, 8, 11)] )+
  scale_size_continuous(name="Relative peak size",breaks=c(1.5,2))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) )+
  guides(shape = guide_legend(ncol = 1),
         color = guide_colourbar(title = "Period", label.theme = element_text(angle = 90)))

fig1 = cowplot::plot_grid(fig1a,
                          cowplot::plot_grid(fig1c + theme(legend.position = "none"),
                                             fig1b + theme(legend.position = "none"),
                                             ncol=2, rel_widths = c(1,1.1),labels = c("B.","C.")),
                          cowplot::plot_grid(get_legend2(fig1c),
                                             get_legend2(fig1b),
                                             ncol=2,rel_widths=c(3,1.2)),
                          labels=c("A.","",""),ncol=1,rel_heights = c(1.2,1,0.25))

fig1
ggsave("poster/fig1_v4.pdf",width=22,height=20,units="cm")


#Fig 2--------------------------------------------------------------------------
p1 = cod_agg_pop_df %>% 
  filter(age_class=="80+",cal_year>=2020,cod_group=="COVID-19") %>% 
  dplyr::mutate(iso_week = sprintf("%04d-W%02d-1", cal_year, cal_week),  # ISO week format: YYYY-Www-d (d=day of week)
                date = ISOweek2date(iso_week)) %>%
  group_by(date) %>% 
  dplyr::summarise(n_covid=sum(n),.groups="drop") %>% 
  ggplot(aes(x=date,y=n_covid)) +
  geom_point(col="black")+
  scale_x_date(name="")+
  scale_y_continuous("COVID-19 deaths")

cum_excess_allcause_pand_df = readRDS(paste0("results/",save.date,"/",mod,"_cum_excess_allcause_pand_df.RDS"))
p2 = cum_excess_allcause_pand_df %>% 
  dplyr::filter(pred == "poisson",age_class=="80+") %>% 
  ggplot(aes(group = with_covid)) +
  geom_ribbon(aes(x = date, ymin = excess_lwb, ymax = excess_upb,fill=factor(with_covid)),
              alpha = 0.1) +
  geom_line(aes(x = date, y = excess_mean, color = factor(with_covid)), size = 0.7) +
  geom_hline(yintercept = 0, linetype = 4) +
  scale_y_continuous(name = "All-cause excess mortality") +
  scale_color_manual(name = "",
                     breaks = c(0, 1),
                     values = c("darkred","orange"),
                     labels=c("Excluding COVID-19","Including COVID-19")) +
  scale_fill_manual(name = "",
                    breaks = c(0, 1),
                    values =  c("darkred","orange"),
                    labels=c("Excluding COVID-19","Including COVID-19") ) +
  scale_x_date(name = "") +
  theme(  legend.position = "right",
          strip.text = element_text(size = 11),
          plot.margin = margin(5.5,14, 5.5, 5.5)) 

legend_plot = get_legend2(p2)

fig2 = cowplot::plot_grid(cowplot::plot_grid(p1 + theme(legend.position = "none",
                                                        plot.margin = unit(c(0.2,0.5,-0.2,0.2), "cm")),
                                      p2 + theme(legend.position = "none",
                                                 plot.margin = unit(c(0.2,0.2,-0.2,0.2), "cm")),
                                      legend_plot,
                                      ncol=3, rel_widths = c(1,1,0.4),labels = c("A.","B.","")))
fig2
ggsave("poster/fig2_v2.pdf",width=26,height=8,units="cm")


#Fig 3--------------------------------------------------------------------------
nrow=4
ncol=3
labels = rep("",nrow*ncol)
labels[c(1,7,2,3)] = c("A.","B.","C.","D.")

sel_plot2 = sel_plot %>% filter(age_class=="80+") %>% 
  dplyr::mutate(cause2 = case_when(
    cause == "Cardiovascular Diseases" ~ "CVD",
    cause == "Respiratory Diseases" ~ "Respiratory",
    cause == "Mental and Neurological Disorders" ~ "Mental/Neurological",
    cause == "Neoplasms (Cancers)" ~ "Cancers",
    TRUE ~ cause  # keep other values unchanged
  ))
plots <- purrr::pmap(sel_plot2[c(1,3,4,2),], function(age_class, cause, rowwise_index, plot_position,cause2) {
  # Add a temporary column for facet label
  temp_data <- cum_excess_pand_df %>%
    dplyr::mutate(cause_label = cause2)  # same as your cause string
  # Create plot
  p <- plot_excess(temp_data,
                   rel_excess_phase2_pand_df,
                   covid_phase2,
                   "80+",
                   cause) +
    theme(
      legend.position = "none",
      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    facet_wrap(~cause_label)  # facet by the new column
  
  return(p)
})
# Customize plots for axis titles and ticks
for (i in seq_along(plots)) {
  # Add left y-axis title for plots 1, 2, 3, 4 (column 1)
  if (i %in% c(1,3)) {
    plots[[i]] <- plots[[i]] +
      theme(axis.title.y.left = element_text())
  }
  # Add right y-axis title for plots 9–12 (column 3)
  if (i %in% c(2,4)) {
    plots[[i]] <- plots[[i]] +
      theme(axis.title.y.right = element_text(color = scales::alpha("black", 0.5)))
  }
  # Add x-axis title and ticks only for bottom row (4, 8, 12)
  if (i %in% 3:4) {
    plots[[i]] <- plots[[i]] +
      theme(axis.text.x = element_text())
  }
}

# Wrap them with patchwork — use guides = 'collect' to remove repeated legends
final_plot <- wrap_plots(plots, ncol = 2, byrow = TRUE) &
  theme(legend.position = "none") &
  theme(plot.margin = margin(2, 4, 8, 2))

fig3 = cowplot::plot_grid(final_plot,
                          cowplot::plot_grid(get_legend2(data.frame(x=1:2,y=1:2,ymin=1:2-1,ymax=2:5,col="1") %>% 
                                                           ggplot(aes(x=x,y=y,ymin=ymin,ymax=ymax,fill=col))+
                                                           geom_ribbon(alpha=0.3)+geom_line(aes(col=col))+
                                                           scale_color_manual(name = "", breaks = "1", labels = "Cumulative excess", values = "black")+
                                                           scale_fill_manual(name = "", breaks = "1", labels = "Cumulative excess", values =  "gray80")),
                                             get_legend2(plots[[1]]+guides(color = guide_legend(title = ""),
                                                                           fill  = guide_legend(title = ""))+theme(legend.position="bottom")),
                                             rel_widths=c(1,4)),
                          rel_heights=c(8,1),ncol=1)
fig3
ggsave("poster/fig3_v2.pdf",width=30,height=18,units="cm")

#Fig 4--------------------------------------------------------------------------
#posterior partial correlation (i.e., calculated from posterior samples)
corr_post_res_df = readRDS(paste0("results/",save.date,"/",mod,"_corr_post_res_df.RDS")) %>% 
  dplyr::filter(lag>=-8,lag<=8) %>% 
  group_by(age_class,y,var) %>% 
  dplyr::mutate(not_cross0 = any(corr_lwb>0 | corr_upb<0),
                lag_peak = if_else(not_cross0,lag[which.max(abs(corr_mean))],NA),
                peak = if_else(not_cross0,corr_mean[which.max(abs(corr_mean))],NA)) %>% ungroup()

#80+ and some causes
fig4 = corr_post_res_df %>%
  dplyr::mutate(y = factor(y,levels=causes2_df_twolines$cod_group[c(1,2,3)],#put in the right order
                           labels=causes2_df_twolines$cod_group_label[c(1,2,3)])) %>% 
  dplyr::filter(!is.na(y),
                age_class %in% c("80+")) %>% 
  ggplot(aes(x=lag,y=corr_mean,ymin=corr_lwb,ymax=corr_upb))+
  geom_ribbon(aes(fill=var),alpha=0.1)+
  geom_line(aes(col=var))+
  geom_point(aes(col=var))+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(aes(xintercept=lag_peak,col=var),linetype=2,alpha=0.8)+
  scale_color_discrete(name="")+
  scale_fill_discrete(name="")+
  scale_x_continuous(name="Lag (in weeks)")+
  scale_y_continuous(name="Partial correlation",
                     limits=c(-1,1))+
  facet_grid(.~y)+
  theme(legend.position = "right",
        legend.margin = margin(t = -5),         # pull legend upward
        axis.title.x = element_text(margin = margin(t = 2)))
fig4
ggsave("poster/fig4_v2.pdf",width=30,height=11,units="cm")

##############################################################################################################################################
##############################################################################################################################################
#Fig3: with labels
# # Filter and prepare weekly cumulative excess data
# cum_df <- cum_excess_pand_df %>% 
#   dplyr::mutate(cod_group = factor(cod_group,
#                                    levels = causes2_df$cod_group[c(1:5)],
#                                    labels = causes2_df$cod_group_label[c(1:5)])) %>% 
#   filter(pred == "poisson",
#          age_class == "80+",
#          !is.na(cod_group))
# #Labels on the graphs indicating cumulative excess end of 2021
# labels_df <- cum_df %>%
#   filter(week.id == max(week.id)) %>%
#   dplyr::mutate(label = paste0("Cum. excess: ", round(excess_mean))) %>%
#   left_join(cum_df %>%
#               group_by(cod_group) %>%
#               dplyr::summarise(rib_max = max(rel_excess_upb2, na.rm = TRUE), .groups = "drop") %>%
#               left_join(
#                 phase_df0 %>%
#                   group_by(cod_group) %>%
#                   summarise(point_max = max(upb, na.rm = TRUE), .groups = "drop"),  by = "cod_group") %>%
#               dplyr::mutate(y_pos = pmax(rib_max, point_max, na.rm = TRUE) + 0.02) %>%
#               dplyr::select(cod_group, y_pos),by = "cod_group")
# 
# #plot
# ggplot() +
#   # Weekly ribbon + line
#   geom_ribbon(data = cum_df,aes(x = date,ymin = rel_excess_lwb2,ymax = rel_excess_upb2),
#               fill = "gray80", alpha = 0.3) +
#   geom_line(data = cum_df, aes(x = date, y = rel_excess_mean2),
#             color = "black", size = 0.7) +
#   # Phase estimates (scaled)
#   geom_pointrange(data = phase_df0, aes(x = phase_mid,y = est,
#                                         ymin = lwb,ymax = upb, color = labels),
#                   position = position_dodge(width = 0.3),size = 0.8) +
#   # Vertical phase boundaries
#   geom_hline(yintercept=0,linetype=4)+
#   geom_vline(data = covid_phase2,
#              aes(xintercept = as.numeric(start_date)),
#              linetype = "dashed", color = "black") +
#   geom_text(data = labels_df,aes(x = date, y = y_pos, label = label),
#             inherit.aes = FALSE,
#             size = 3.5,vjust = 1, hjust = 1,color = "black")+
#   facet_wrap(~ cod_group, scales = "free_y",nrow=1) +
#   scale_y_continuous(name = "Relative excess mortality",
#                      labels = scales::percent_format(accuracy = 1)) +
#   scale_x_date(name = "Date") +
#   scale_color_viridis_d(name = "COVID Phase",option = "C") +
#   theme(axis.title.y.left = element_text(color = "black"),
#         axis.title.y.right = element_text(color = "black"),
#         legend.position = "bottom",
#         strip.text = element_text(size = 11))
