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

cod_agg_pop_df = readRDS(paste0(code_root_path,"savepoint/cod_agg_pop_df.RDS"))
age_classes = cod_agg_pop_df$age_class %>% unique()
setwd(code_root_path)
res_list = load_results_mod6(age_classes, save.date="20241218",mod="mod8")

cod_agg_pop_df = cod_agg_pop_df %>%
  dplyr::mutate(date=ISOweek2date(paste0(cal_year,"-W",ifelse(cal_week<10,paste0("0",cal_week),cal_week),"-1"))) %>% 
  mutate(covid_phase = map2_dbl(date, list(covid_phase), function(d, phases) {
    phase <- phases %>%
      filter(d >= start_date & d <= end_date) %>%
      pull(phase)
    if (length(phase) == 0) NA_real_ else phase
  }))

################################################################################
# Fig 1
causes2_df = causes2_df %>% 
  dplyr::mutate(cod_group_label = ifelse(cod_group_label=="Mental/Neurological",
                "Mental/\nNeurological",cod_group_label),
                cod_group_label = ifelse(cod_group_label=="Infectious/Parasitic",
                                         "Infectious/\nParasitic",cod_group_label))
#Panel A: mortality by week for 80+, by cause
fig1a = res_list$data_pred_week_cause %>% 
  filter(variable=="deaths") %>% 
  left_join(res_list$data_pred_week_cause %>% 
              filter(variable=="obs_deaths") %>% dplyr::select(obs_deaths=est,cal_year,cal_week,age_class,cod_group,pred),
            by=c("cal_year","cal_week","age_class","cod_group","pred")) %>% 
  filter(age_class=="80+",pred=="poisson") %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[c(1:5,7)],
                                   labels=causes2_df$cod_group_label[c(1:5,7)])) %>% 
  filter(!is.na(cod_group)) %>% 
  ggplot() +
  geom_line(aes(x=date,y=est),col="black") +
  geom_ribbon(aes(x=date,ymin=lwb,ymax=upb),fill="black",alpha=0.15) +
  geom_point(aes(x=date,y=obs_deaths),col="darkred",alpha=0.5,size=1) +
  geom_vline(aes(xintercept=ymd("2020-01-01")))+
  facet_grid(cod_group~age_class,scales="free") +
  scale_y_continuous(name="Deaths")+
  scale_x_date(name="Time")+
  theme_bw()
fig1a

#Panel B
fig1b = res_list$Sigma_mat %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[c(1:5,7)],
                                   labels=causes2_df$cod_group_label[c(1:5,7)]),
                cod_group2 = factor(cod_group2,levels=causes2_df$cod_group[c(1:5,7)],
                                    labels=causes2_df$cod_group_label[c(1:5,7)]),
                cod_group_id=as.numeric(cod_group),
                cod_group_id2=as.numeric(cod_group2)) %>% 
  filter(!is.na(cod_group),!is.na(cod_group2)) %>% 
  rowwise() %>% 
  dplyr::mutate(est_cri = paste0(scales::percent(est, accuracy = 1),"\n",
                                 "[",scales::percent(lwb, accuracy = 1),",",
                                 scales::percent(upb, accuracy = 1),"]")) %>% 
  filter(cod_group_id<cod_group_id2,age_class=="80+") %>% 
  ggplot(aes(x = cod_group, y = fct_rev(cod_group2), fill = abs(est))) +
  geom_tile() +
  geom_text(aes(label = est_cri),
            color = "black", size = 2.5) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",limits=c(0,1),
                      name="Correlation",
                      labels=scales::label_percent(accuracy = 1)) +
  labs(x = "", y = "", fill = "Count") +
  theme_bw() +
  theme(legend.position = "bottom")
  # theme(axis.text.x = element_text(angle = 45, hjust = 0,size=11),
  #       axis.text.y = element_text(size=11),
  #       plot.margin = margin(t = -10, r = 5, b = 5, l = 5))+
  # scale_x_discrete(position = "top")
fig1b

#Panel C
d1 =readRDS(paste0("results/",save.date,"/","mod8","_peak_dates_summary_df.RDS")) %>%
  filter(age_class == "80+")  %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[c(1:5,7)],
                                      labels=causes2_df$cod_group_label[c(1:5,7)])) %>% 
  filter(!is.na(cod_group))
d2 = readRDS(paste0("results/","observed_peak_date_df.RDS")) %>%
  filter(age_class == "80+") %>% 
  dplyr::mutate(is_2020 = period_name %in% c("2019/20","2020/21","2021/22")) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[c(1:5,7)],
                                   labels=causes2_df$cod_group_label[c(1:5,7)])) %>% 
  filter(!is.na(cod_group)) %>% 
  filter(period_id!=2021) #we remove 2021-22 as we only have date up to end of 2021
year_df = peak_date %>% dplyr::select(period_id,period_name) %>% unique()

fig1c = d1 %>%
  ggplot(aes(y = cod_group)) +
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
    colours = c("yellow", "darkred", "darkviolet"),  # blue → white → red
    values = scales::rescale(c(2012, 2019.5, 2021)),  # period_ids or numeric range
    breaks = year_df$period_id[c(1, 4, 8, 11)],
    labels = year_df$period_name[c(1, 4, 8, 11)] )+
  scale_size_continuous(name="Relative peak size",breaks=c(1.5,2))+
  theme(legend.position = "bottom")+
  guides(shape = guide_legend(ncol = 1),
    color = guide_colourbar(
      title = "Period",
      label.theme = element_text(angle = 90)
    )
  )
fig1c

fig1 = cowplot::plot_grid(fig1a,
                   cowplot::plot_grid(fig1b + theme(legend.position = "none"),
                                      fig1c + theme(legend.position = "none"),
                                      ncol=2, rel_widths = c(1,1.26),labels = c("B.","C.")),
                   cowplot::plot_grid(get_legend2(fig1b),
                           get_legend2(fig1c),ncol=2,rel_widths=c(1,3)),
                   labels=c("A.","",""),ncol=1,rel_heights = c(1.8,1,0.2))
pdf(file=paste0(code_root_path,"/manuscript/fig1.pdf"),width=10,height=14)
print(fig1)
dev.off()

################################################################################
#Figure 2

main_plot =res_list$data_pred_week %>% 
  filter(variable == "deaths") %>%
  # Add observed deaths
  left_join(res_list$data_pred_week %>%
      filter(variable == "obs_deaths") %>%
      select(obs_deaths = est, cal_year, cal_week, age_class, pred),by = c("cal_year", "cal_week", "age_class", "pred")) %>%
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
  scale_x_date(name = "Time") +
  theme(legend.position = "none")

# Legend for Observed deaths
df_dummy <- data.frame(x = 1, y = 1)
legend_red <- ggplot(df_dummy, aes(x, y, color = "Observed deaths")) +
  geom_point() +
  scale_color_manual(name = "", values = c("Observed deaths" = "darkred")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(shape = 16))) 
legend_red <- get_legend2(legend_red)

legend_orange <- ggplot(df_dummy, aes(x, y, color = "Observed deaths incl. COVID-19")) +
  geom_point() +
  scale_color_manual(name = "", values = c("Observed deaths incl. COVID-19" = "orange")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(shape = 16)))
legend_orange <- get_legend2(legend_orange)

legend_black <- ggplot(df_dummy, aes(x, y)) +
  geom_ribbon(aes(ymin = y - 0.1, ymax = y + 0.1, fill = "Expected deaths (95% CrI)"), alpha = 0.2) +
  geom_line(aes(y = y, color = "Expected deaths (95% CrI)"), linewidth = 0.8) +
  scale_color_manual(name="",values = c("Expected deaths (95% CrI)" = "black")) +
  scale_fill_manual(name="",values = c("Expected deaths (95% CrI)" = "black")) +
  theme(legend.position = "bottom")
legend_black <- get_legend2(legend_black)


fig2 <- cowplot::plot_grid(main_plot,
                           cowplot::plot_grid(legend_black, legend_red, legend_orange, nrow = 1),
                           ncol = 1,  rel_heights = c(1, 0.04))

pdf(file=paste0(code_root_path,"/manuscript/fig2.pdf"),width=10,height=8)
print(fig2)
dev.off()


################################################################################
#Figure 3




