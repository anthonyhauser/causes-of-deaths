plot_excess = function(cum_excess_pand_df=cum_excess_pand_df,
                       excess_phase2_pand_df = excess_phase2_pand_df,
                       covid_phase2 = covid_phase2,
                       age="18-39",cause="External Causes") {
  cum_df_sub <- cum_excess_pand_df %>%
    filter(pred == "poisson", age_class == age,
           cod_group==cause) %>% 
    dplyr::mutate(cod_group = factor(cod_group,
                                     levels = causes2_df$cod_group,
                                     labels = causes2_df$cod_group_label),
                  cod_group_age = paste0(cod_group,", ",age_class))
  
  scale = cum_df_sub %>% filter(week.id==max(week.id)) %>% 
    dplyr::mutate(scale=excess_mean/rel_excess_mean2) %>% pull(scale)
  
  phase_df_sub <- excess_phase2_pand_df %>%
    filter(covid_phase < 8,
           variable == "rel_excess",
           pred == "poisson",
           age_class == age,
           cod_group==cause) %>%
    dplyr::mutate(cod_group = factor(cod_group,
                                     levels = causes2_df$cod_group,
                                     labels = causes2_df$cod_group_label)) %>%
    left_join(covid_phase2, by = c("covid_phase" = "phase")) %>%
    dplyr::mutate(phase_mid = start_date + (end_date - start_date)/2)
  
  
  p <- ggplot() +
    geom_ribbon(data = cum_df_sub, aes(x = date, ymin = excess_lwb/scale, ymax = excess_upb/scale),
                fill = "black", alpha = 0.1) +
    geom_line(data = cum_df_sub, aes(x = date, y = excess_mean/scale),
              color = "black", size = 0.7) +
    geom_pointrange(data = phase_df_sub,
                    aes(x = phase_mid, y = est, ymin = lwb, ymax = upb, color = labels),
                    position = position_dodge(width = 0.3), size = 0.8) +
    geom_hline(yintercept = 0, linetype = 4) +
    geom_vline(data = covid_phase2 %>% filter(phase>=min(phase_df_sub$covid_phase)), aes(xintercept = start_date),
               linetype = "dashed", color = "black") +
    facet_wrap(~ cod_group_age, scales = "free_y", nrow = 1) +
    scale_y_continuous(name = "Rel. excess by phase",
                       labels = scales::percent_format(accuracy = 1),
                       sec.axis = sec_axis(
                         trans = ~ .*scale,  # identity transform (already scaled)
                         name = "Cum. excess")) +
    scale_x_date(name = "Date") +
    scale_color_viridis_d(name = "COVID Phase", option = "C") +
    theme(axis.title.y.right = element_text(color = scales::alpha("black", 0.5)),
          axis.text.y.right =element_text(color = scales::alpha("black", 0.5)),
          axis.title.y.left = element_text(color = "black"),
          legend.position = "bottom",
          strip.text = element_text(size = 11),
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  return(p)
}
