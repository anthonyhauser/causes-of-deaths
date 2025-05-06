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

col_age = viridis_pal()(5)[5:1]

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
p1 = cod_agg_pop_nuts_df %>% 
  filter(cal_year == 2020) %>% 
  group_by(age_class) %>% 
  dplyr::summarise(n = sum(n), .groups = "drop") %>% 
  dplyr::mutate(p = n / sum(n),
         label = paste0(signif(100 * p, 2), "%")) %>% 
  ggplot(aes(x = age_class, y = n)) +
  geom_col(fill = "gray") +
  geom_text(aes(label = label), vjust = -0.5) +
  scale_x_discrete(name = "Age class") +
  scale_y_continuous(name = "Deaths")

p2 = cod_agg_pop_nuts_df %>%
  filter(cal_year == 2020) %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[c(1:9)],#put in the right order
                                   labels=causes2_df$cod_group_label[c(1:9)]),
                cod_group_id = as.numeric(cod_group)) %>% 
  filter(!is.na(cod_group)) %>% 
  group_by(age_class,cod_group,cod_group_id) %>% 
  dplyr::summarise(n = sum(n), .groups = "drop") %>% 
  group_by(age_class) %>% 
  dplyr::mutate(p = n / sum(n)) %>% ungroup() %>% 
  ggplot(aes(x=cod_group_id,y=p,col=age_class))+
  geom_point()+
  geom_line()+
  scale_x_continuous(name="Causes of deaths",breaks=causes2_df$order,
                   labels=causes2_df$cod_group_label)+
  scale_y_continuous(name="Distribution of deaths", labels=scales::percent)+
  scale_color_manual(name="Age class",values=col_age)

fig1_1 = cowplot::plot_grid(p1,p2,
                   ncol=2,rel_widths = c(1,2),
                   labels=c("A.","B."))

pdf(file=paste0(code_root_path,"/manuscript/fig1_1.pdf"),width=12,height=6)
print(fig1_1)
dev.off()

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
#Figure 3: excess by age and cod
cum_excess_pand_df

excess_phase2_pand_df = readRDS(paste0("results/",save.date,"/","mod8","_excess_phase2_pand_df.RDS"))
cum_excess_pand_df = readRDS(paste0("results/",save.date,"/","mod8","_cum_excess_pand_df.RDS"))

excess_phase2_pand_df
cum_excess_pand_df
covid_phase2

cum_excess_pand_df %>% 
  filter(pred=="poisson",cod_group!="Other Causes",age_class=="80+") %>% 
  dplyr::mutate(cod_group = factor(cod_group,levels=causes2_df$cod_group[c(1:5)],#put in the right order
                                   labels=causes2_df$cod_group_label[c(1:5)]),
                cod_group_id = as.numeric(cod_group)) %>% 
  filter(!is.na(cod_group)) %>% 
  ggplot(aes(x=date,y=excess_mean,ymin=excess_lwb,ymax=excess_upb))+
  geom_ribbon(alpha=0.1)+
  geom_line()+
  facet_grid(cod_group~age_class,scales = "free")


#Figure 4: Correlation


#Figure 3: Data

# Filter and prepare weekly cumulative excess data
cum_df <- cum_excess_pand_df %>% 
  filter(pred == "poisson",
         age_class == "80+",
         cod_group %in% causes2_df$cod_group[c(1:5)]) %>%
  mutate(cod_group = factor(cod_group,
                            levels = causes2_df$cod_group[c(1:5)],
                            labels = causes2_df$cod_group_label[c(1:5)]))

# Filter and prepare phase-level data
phase_df0 <- excess_phase2_pand_df %>%
  filter(covid_phase<8,
         variable == "rel_excess",
         pred == "poisson",
         age_class == "80+",
         cod_group %in% causes2_df$cod_group[c(1:5)]) %>%
  mutate(cod_group = factor(cod_group,
                            levels = causes2_df$cod_group[c(1:5)],
                            labels = causes2_df$cod_group_label[c(1:5)])) %>%
  left_join(covid_phase2, by = c("covid_phase" = "phase")) %>%
  mutate(phase_mid = start_date + (end_date - start_date)/2)

labels_df <- cum_df %>%
  filter(week.id == max(week.id)) %>%
  dplyr::mutate(label = paste0("Cum. excess: ", round(excess_mean))) %>%
  left_join(cum_df %>%
      group_by(cod_group) %>%
        dplyr::summarise(rib_max = max(rel_excess_upb2, na.rm = TRUE), .groups = "drop") %>%
      left_join(
        phase_df0 %>%
          group_by(cod_group) %>%
          summarise(point_max = max(upb, na.rm = TRUE), .groups = "drop"),  by = "cod_group") %>%
      dplyr::mutate(y_pos = pmax(rib_max, point_max, na.rm = TRUE) + 0.02) %>%
      select(cod_group, y_pos),by = "cod_group")

#plot
ggplot() +
  # Weekly ribbon + line
  geom_ribbon(data = cum_df,aes(x = date,ymin = rel_excess_lwb2,ymax = rel_excess_upb2),
              fill = "gray80", alpha = 0.3) +
  geom_line(data = cum_df, aes(x = date, y = rel_excess_mean2),
            color = "black", size = 0.7) +
  # Phase estimates (scaled)
  geom_pointrange(data = phase_df0, aes(x = phase_mid,y = est,
                                       ymin = lwb,ymax = upb, color = labels),
                  position = position_dodge(width = 0.3),size = 0.8) +
  # Vertical phase boundaries
  geom_hline(yintercept=0,linetype=4)+
  geom_vline(data = covid_phase2,
             aes(xintercept = as.numeric(start_date)),
             linetype = "dashed", color = "black") +
  geom_text(data = labels_df,aes(x = date, y = y_pos, label = label),
            inherit.aes = FALSE,
            size = 3.5,vjust = 1, hjust = 1,color = "black")+
  facet_wrap(~ cod_group, scales = "free_y",nrow=1) +
  scale_y_continuous(name = "Relative excess mortality",
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(name = "Date") +
  scale_color_viridis_d(name = "COVID Phase",option = "C") +
  theme(axis.title.y.left = element_text(color = "black"),
        axis.title.y.right = element_text(color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 11))

#dual y axis
# Calculate scale factor per cod_group
scale_df <- cum_df %>%
  group_by(cod_group) %>%
  dplyr::summarise(max_excess = max(abs(rel_excess_mean2), na.rm = TRUE)) %>%
  left_join(phase_df0 %>%
              group_by(cod_group) %>%
              summarise(max_rel = max(abs(est), na.rm = TRUE)), by = "cod_group") %>%
  dplyr::mutate(scale_factor = max_excess / max_rel)

# Join scale factor back into phase data
phase_df <- phase_df0 %>%
  left_join(scale_df %>% select(cod_group, scale_factor), by = "cod_group") %>%
  dplyr::mutate(est_scaled = est * scale_factor,
                lwb_scaled = lwb * scale_factor,
                upb_scaled = upb * scale_factor)

# Plot
ggplot() +
  # Weekly ribbon + line
  geom_ribbon(data = cum_df,aes(x = date,ymin = rel_excess_lwb2,ymax = rel_excess_upb2),
              fill = "gray80", alpha = 0.3) +
  geom_line(data = cum_df, aes(x = date, y = rel_excess_mean2),
            color = "black", size = 0.7) +
  # Phase estimates (scaled)
  geom_pointrange(data = phase_df, aes(x = phase_mid,y = est_scaled,
                                       ymin = lwb_scaled,ymax = upb_scaled, color = labels),
                  position = position_dodge(width = 0.3),size = 0.8) +
  # Vertical phase boundaries
  geom_hline(yintercept=0,linetype=4)+
  geom_vline(data = covid_phase2,
             aes(xintercept = as.numeric(start_date)),
             linetype = "dashed", color = "black") +
  facet_wrap(~ cod_group, scales = "free_y") +
  scale_y_continuous(name = "Cumulative excess deaths",
                     labels = scales::percent_format(accuracy = 1),
                     sec.axis = sec_axis(
                       trans = ~ ./1,  # identity transform (already scaled)
                       labels = scales::percent_format(accuracy = 1),
                       name = "Relative excess mortality (per phase)")) +
  scale_x_date(name = "Date") +
  scale_color_viridis_d(name = "COVID Phase",option = "C") +
  theme(axis.title.y.left = element_text(color = "black"),
        axis.title.y.right = element_text(color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 11))





# Set number of causes per age group
n_causes <- 5

# Get top causes per age group in 2020
top_causes <- cod_agg_pop_nuts_df %>%
  filter(cod_group!="COVID-19",cod_group!="Other Causes") %>% 
  filter(cal_year == 2020) %>%
  group_by(age_class, cod_group) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  semi_join(causes2_df, by = "cod_group") %>%
  group_by(age_class) %>%
  slice_max(order_by = n, n = n_causes, with_ties = FALSE) %>%
  ungroup()

cod_list <- top_causes %>%
  group_by(age_class) %>%
  summarise(causes = list(cod_group), .groups = "drop") %>%
  deframe()

# List of age classes
age_classes <- unique(top_causes$age_class)

# 1. Create plots with and without legends
plots_with_legend <- lapply(age_classes, function(age) {
  top_cod <- top_causes %>% filter(age_class == age)
  
  cum_df_sub <- cum_excess_pand_df %>%
    filter(pred == "poisson", age_class == age,
           cod_group %in% cod_list[[age]]) %>% 
    mutate(cod_group = factor(cod_group,
                              levels = causes2_df$cod_group,
                              labels = causes2_df$cod_group_label))
  
  phase_df_sub <- excess_phase2_pand_df %>%
    filter(covid_phase < 8,
           variable == "rel_excess",
           pred == "poisson",
           age_class == age) %>%
    semi_join(top_cod, by = c("age_class", "cod_group")) %>%
    mutate(cod_group = factor(cod_group,
                              levels = causes2_df$cod_group,
                              labels = causes2_df$cod_group_label)) %>%
    left_join(covid_phase2, by = c("covid_phase" = "phase")) %>%
    mutate(phase_mid = start_date + (end_date - start_date)/2)
  
  labels_df <- cum_df_sub %>%
    filter(week.id == max(week.id)) %>%
    mutate(label = paste0("Cum. excess: ", round(excess_mean))) %>%
    left_join(
      cum_df_sub %>%
        group_by(cod_group) %>%
        summarise(rib_max = max(rel_excess_upb2, na.rm = TRUE), .groups = "drop") %>%
        left_join(
          phase_df_sub %>%
            group_by(cod_group) %>%
            summarise(point_max = max(upb, na.rm = TRUE), .groups = "drop"),
          by = "cod_group") %>%
        mutate(y_pos = pmax(rib_max, point_max, na.rm = TRUE) + 0.02) %>%
        select(cod_group, y_pos),
      by = "cod_group")
  
  p <- ggplot() +
    geom_ribbon(data = cum_df_sub, aes(x = date, ymin = rel_excess_lwb2, ymax = rel_excess_upb2),
                fill = "gray80", alpha = 0.3) +
    geom_line(data = cum_df_sub, aes(x = date, y = rel_excess_mean2),
              color = "black", size = 0.7) +
    geom_pointrange(data = phase_df_sub,
                    aes(x = phase_mid, y = est, ymin = lwb, ymax = upb, color = labels),
                    position = position_dodge(width = 0.3), size = 0.8) +
    geom_hline(yintercept = 0, linetype = 4) +
    geom_vline(data = covid_phase2, aes(xintercept = as.numeric(start_date)),
               linetype = "dashed", color = "black") +
    geom_text(data = labels_df,
              aes(x = date, y = y_pos, label = label),
              inherit.aes = FALSE, size = 3.5, vjust = 0, hjust = 1, color = "black") +
    facet_wrap(~ cod_group, scales = "free_y", nrow = 1) +
    scale_y_continuous(name = "Relative excess mortality", labels = scales::percent_format(accuracy = 1)) +
    scale_x_date(name = "Date") +
    scale_color_viridis_d(name = "COVID Phase", option = "C") +
    theme(
      axis.title.y.left = element_text(color = "black"),
      legend.position = "bottom",
      strip.text = element_text(size = 11),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  return(p)
})





##############################################################################################################################################
#Correlation
#aggregate
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
  facet_grid(age_class~y)

