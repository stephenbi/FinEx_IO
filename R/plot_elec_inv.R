### REMOVE ADJUSTMENT COSTS FROM ENERGY INVESTMENTS ###
invCost <- bind_rows(
  read.csv("P:/REMIND_3p0_dev/remind/ppca_dir.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'direct', scenario = 'PPCA-growth'),
  read.csv("P:/REMIND_3p0_dev/remind/finex_dir.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'direct', scenario = 'FinEx'),
  read.csv("P:/REMIND_3p0_dev/remind/redirloan_dir.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'direct', scenario = 'REdirect-loan'),
  read.csv("P:/REMIND_3p0_dev/remind/redirgrant_dir.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'direct', scenario = 'REdirect-grant'),
  read.csv("P:/REMIND_3p0_dev/remind/ppca_adj.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'adjust', scenario = 'PPCA-growth'),
  read.csv("P:/REMIND_3p0_dev/remind/finex_adj.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'adjust', scenario = 'FinEx'),
  read.csv("P:/REMIND_3p0_dev/remind/redirloan_adj.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'adjust', scenario = 'REdirect-loan'),
  read.csv("P:/REMIND_3p0_dev/remind/redirgrant_adj.csv", sep = "", header = F, col.names = c('period', 'region', 'variable', 'scenario', 'value')) %>% mutate(variable = 'adjust', scenario = 'REdirect-grant')
)

invCostTot <- invCost %>%
  group_by(scenario, region) %>%
  summarise(total = sum(value)) %>%
  mutate(period = 2025,
         variable = "direct")

invDir_share <- invCost %>% 
  left_join(invCostTot) %>% 
  filter(variable=="direct",
         region %in% host_regi) %>%
  mutate(share = value / total,
         variable = v_inv_fos[1])


### FINEX & REDIRECT POLICY RUNS ###
q_host_elec_inv <- q_dpe_pol %>%
  filter(
    # region %in% c("FNX", host_regi),
    variable %in% c(v_syscost, v_taxrev, v_inv_REdir, v_cap_re, v_seel_VRE, v_inco_re_adj, v_inco_re, v_inv_fos, v_inv_stor, v_grid_inv) &
      !grepl("growth",scenario) &
           period %in% seq(2025,2050,5)) %>%
  mutate(host_share = 1) %>% 
  left_join(invDir_share %>% select(region, period, scenario, variable, share)) %>%
  mutate(share = ifelse(is.na(share), 1, share)) %>%
  mutate(value = value * share) %>%
  group_by(region, variable, scenario) %>%
  mutate(value = value * inflation_05_to_19 * 5 * host_share,
         cumval = cumsum(value)) %>%
  ungroup()

### REFERENCE RUN = PPCA-GROWTH ###
q_ref_host_elec_inv <- q_dpe_pol %>%
  filter(
    # region %in% c("FNX", host_regi),
    variable %in% c(v_syscost, v_taxrev, v_inv_REdir, v_cap_re, v_seel_VRE, v_inco_re_adj, v_inco_re, v_inco_coal, v_inv_fos, v_inv_stor, v_grid_inv) &
      grepl("growth",scenario) &
           period %in% seq(2025,2050,5)) %>%
  mutate(host_share = 1) %>% 
  rename(ref_val = value) %>%
  left_join(invDir_share %>% select(region, period, scenario, variable, share)) %>%
  mutate(share = ifelse(is.na(share), 1, share)) %>%
  mutate(ref_val = ref_val * share)  %>%
  group_by(region, variable, scenario) %>%
  mutate(ref_val = ref_val * inflation_05_to_19 * 5 * host_share) %>%
  mutate(cumref = cumsum(ref_val)) %>%
  ungroup()

### DIFFERENCE ###
q_diff_host_elec_inv <- q_host_elec_inv %>%
  bind_rows(q_budget %>%
              filter(
                # region %in% c("FNX", host_regi),
                variable %in% c(v_syscost, v_taxrev, v_inv_REdir, v_cap_re, v_seel_VRE, v_inco_re_adj, v_inco_re, v_inv_fos, v_inv_stor, v_grid_inv) &
                  period %in% seq(2025,2050,5)) %>%
              mutate(host_share = 1) %>%
              mutate(scenario = as.character(scenario),
                     value = value * inflation_05_to_19 * 5 * host_share,
                     cumval = cumsum(value))
  ) %>%
  left_join(q_ref_host_elec_inv, by = c("variable", "period", "region", "unit")) %>%
  mutate(diff_val = value - ref_val,
         cumdiff = cumval - cumref) %>%
  filter(!(scenario.x %in% c(
    # "REdirect-HI_cond",
    # "REdirect-HI_cond-2040",
    "PPCA-observed"
    ))) %>%
  mutate(scenario.x = factor(x = scenario.x,
                                # levels = c("PPCA-gcpt23_HiLoFix-nonoecd50p", "FinEx-gcpt23_HiLoFix-nonoecd50p", "REdirect-gcpt23_HiLoFix-nonoecd50p", "REdirect_LO-gcpt23_HiLoFix-nonoecd50p", "REdirect_HI-gcpt23_HiLoFix-nonoecd50p", "REdirect_HI-cond-gcpt23_HiLoFix-nonoecd50p", "REdirect_HI-cond-2040-gcpt23_HiLoFix-nonoecd50p", "REdirect_nomob-gcpt23_HiLoFix-nonoecd50p", "PkBudg1150", "PkBudg500"),
                             levels = c("PPCA-growth", "FinEx", "REdirect", "REdirect-loan", "REdirect-grant", "2\u00B0C", "1.5\u00B0C"),
                                ordered = T))


 
 
  ### BAR PLOT REDIRECT BY REGION 2025 ###
  ggplot(q_diff_host_elec_inv %>%
           filter(variable %in% c(v_inv_REdir, v_inv_fos)) %>%
           filter(!grepl("GLO|FNX", region) &
                    # region %in% c("FNX",host_regi) &
                    !is.na(scenario.x) &
                    !grepl("[0-9]", scenario.x) &
                    # !grepl("Coal", variable) &
                    period == 2025) %>%
           # period %in% c(2025, 2030)) %>%
           mutate(variable = gsub("|", " | ", 
                                  gsub("Energy Investments|Elec|","", variable, fixed = T), 
                                  fixed = T)) %>% 
           mutate(neg_diff = ifelse(diff_val < 0, diff_val, 0)) %>%
           group_by(region, scenario.x, period) %>%
           mutate(min_diff = sum(neg_diff)) %>%
           group_by(region) %>%
           mutate(min_diff = min(min_diff)) %>%
           ungroup()
  ) +
    geom_col(mapping = aes(x = scenario.x,
                           # interaction(scenario.x, period),
                           y = diff_val,
                           fill = variable),
             # stat = "count",
             data = . %>% filter(period == 2025),
             color = "grey15",
             width = 0.5,
             # just = 1,
             linewidth = 0.1) +
    # geom_col(mapping = aes(x = scenario.x,
    #                        # interaction(scenario.x, period),
    #                        y = diff_val,
    #                        fill = variable),
    #          # stat = "count",
    #          data = . %>% filter(period == 2030),
    #          color = "grey15",
    #          width = 0.4,
    #          just = 0,
    #          linewidth = 0.1) +
    # geom_text(data = . %>% filter(period == 2025),
  #           mapping = aes(x = scenario.x,
  #                         y =  min_diff * 1.11 - 4,
  #                         label = period),
  #           nudge_x = -0.25,
  #           size = 2
  #           ) +
  geom_text(data = . %>% filter(period == 2030),
            mapping = aes(x = scenario.x,
                          y =  min_diff * 1.11 - 4,
                          label = period),
            nudge_x = 0.25,
            size = 2
  ) +
    labs(x = NULL,
         y = "2022-32 Investment Above Pre-Rome Expectations (Billion $2019)") +
    # facet_wrap(. ~ scenario.x,
    #            scales = "free") +
    scale_fill_brewer(palette = "Set2", direction = -1, name = "Technology") + 
    theme_bw() +
    geom_hline(yintercept = 0,
               color = 'black',
               linewidth = 0.65) +
    # facet_grid(period ~ .) +
    facet_wrap(. ~ factor(region,
                          levels = c(regOrd[-1], 'CHA', 'IND', 'USA', 'JPN', 'EUR'),
                          labels = c(paste0(regOrder[-1],'*'), 'China', 'India', 'USA', 'Japan', 'EU & UK')),
               scales = 'free_y' ) +
    theme(text = element_text(size = 1.2 * 9),
          axis.text.x = element_text(size = 1.2 * 9, angle = 45, hjust = 1),
          strip.text = element_text(size = 1.2 * 8, face = "bold"),
          legend.text = element_text(size = 1.2 * 8),
          legend.title = element_text(size = 1.2 * 8, face = "bold"),
          legend.position = c(0.87, 0.92),
          # legend.position = "right",
          legend.direction = "vertical",
          legend.background = element_blank(),
          legend.key.size = unit(3, "mm"),
          strip.background = element_rect(fill = "white"),
          # legend.box.spacing = unit(0, "mm"),
          legend.spacing = unit(0,"mm"),
          legend.margin = margin())
  
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_allocation_2025.pdf",width = 12 * 2.3, height = 10.5 * 2.3, units = "cm", dpi="retina")
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_allocation_2025.jpg",width = 12 * 2.3, height = 10.5 * 2.3, units = "cm", dpi="retina")
  

  
  

##########################################  
### BAR PLOT REDIRECT BY REGION 2025-30 ###
##########################################  
  ggplot(q_diff_host_elec_inv %>%
         filter(variable %in% c(v_inv_REdir, v_inv_fos)) %>%
         filter(!grepl("GLO|FNX", region) &
           # region %in% c("FNX",host_regi) &
             !is.na(scenario.x) &
             scenario.x!="REdirect",
                  !grepl("[0-9]", scenario.x) &
                  # !grepl("Coal", variable) &
                  period %in% c(2025, 2030)) %>%
         mutate(variable = gsub("|w/o CC", "", fixed = T,
                                gsub("|VRE support", "", fixed = T,
                                     gsub("Energy Investments|Elec|","", variable, fixed = T)), 
                                )) %>% 
         mutate(neg_diff = ifelse(diff_val < 0, diff_val, 0)) %>%
         group_by(region, scenario.x, period) %>%
         mutate(min_diff = sum(neg_diff)) %>%
         group_by(region) %>%
         mutate(min_diff = min(min_diff)) %>%
         ungroup()
       ) +
  geom_col(mapping = aes(x = scenario.x,
                           # interaction(scenario.x, period),
                         y = diff_val,
                         fill = variable),
           # stat = "count",
            data = . %>% filter(period == 2025),
           color = "grey15",
           width = 0.4,
           just = 1,
           linewidth = 0.1) +
  geom_col(mapping = aes(x = scenario.x,
                         # interaction(scenario.x, period),
                         y = diff_val,
                         fill = variable),
           # stat = "count",
           data = . %>% filter(period == 2030),
           color = "grey15",
           width = 0.4,
           just = 0,
           linewidth = 0.1) +
  geom_text(data = . %>% filter(period == 2025),
            mapping = aes(x = scenario.x,
                          y =  min_diff * 1.22 -1.5,
                          label = period),
            nudge_x = -0.25,
            size = 2.6,
            # angle = 45
            ) +
  geom_text(data = . %>% filter(period == 2030),
            mapping = aes(x = scenario.x,
                          y =  min_diff * 1.22 - 1.5,
                          label = period),
            nudge_x = 0.25,
            size = 2.6,
            # angle = 45
  ) +
  labs(x = NULL,
       y = "2022-32 Investment Above/Below Pre-Glasgow Expectations (Billion $2019)") +
  # facet_wrap(. ~ scenario.x,
  #            scales = "free") +
    scale_fill_manual(values = setNames(c(brewer.pal(n = 8, "Accent")[-c(4,5,6)],brewer.pal(n = 8, "Accent")[4]),
                                        c(rev(unique(q_diff_host_elec_inv_agg$variable)[-4]), unique(q_diff_host_elec_inv_agg$variable)[4])),
                      name = "Technology") +
    theme_bw() +
  geom_hline(yintercept = 0,
             color = 'black',
             linewidth = 0.65) +
  # facet_grid(period ~ .) +
  facet_wrap(. ~ factor(region,
                        levels = c(regOrd[-1], 'CHA', 'IND', 'USA', 'JPN', 'EUR'),
                        labels = c(paste0(regOrder[-1],'*'), 'China', 'India', 'USA', 'Japan', 'EU & UK')),
             scales = 'free_y' ) +
  theme(text = element_text(size = 1.5 *9),
        axis.text.x = element_text(size = 1.5 *9, angle = 45, hjust = 1),
        strip.text = element_text(size = 1.5 * 7, face = "bold"),
        legend.text = element_text(size = 1.5 *8),
        legend.title = element_text(size = 1.5 *7, face = "bold"),
        legend.position = c(0.845, 0.91),
        # legend.position = "bottom",
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.key.size = unit(3, "mm"),
        strip.background = element_rect(fill = "white"),
        legend.box.spacing = unit(0, "mm"),
        legend.spacing = unit(0,"mm"),
        legend.margin = margin())
  
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_allocation_2025_2030.pdf",width = 12*2.1, height = 10.5*2.1, units = "cm", dpi="retina")
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_allocation_2025_2030.jpg",width = 12*2.1, height = 10.5*2.1, units = "cm", dpi="retina")
  

  
  
  
  ###################################################  
  ### BAR PLOT REDIRECT HOSTS V NON-HOSTS 2025-30 ###
  ###################################################  
  q_diff_host_elec_inv_agg <- q_diff_host_elec_inv %>%
    filter(region %in% host_regi &
             variable %in% c(v_inv_REdir, v_inv_fos)) %>%
    group_by(scenario.x, period, variable) %>%
    summarise(diff_val = sum(diff_val)) %>%
    mutate(region = "Hosts") %>%
    bind_rows(
      q_diff_host_elec_inv %>%
        filter(!(region %in% c("GLO", "FNX", host_regi)) &
                 variable %in% c(v_inv_REdir, v_inv_fos)) %>%
        group_by(scenario.x, period, variable) %>%
        summarise(diff_val = sum(diff_val)) %>%
        mutate(region = "Non-Hosts")
    ) %>%
    filter(!is.na(scenario.x) &
             scenario.x!="REdirect",
           !grepl("[0-9]", scenario.x) &
             # !grepl("Coal", variable) &
             period %in% c(2025, 2030)) %>%
    mutate(variable = gsub("|w/o CC", "", fixed = T,
                           gsub("|VRE support", "", fixed = T,
                                gsub("Energy Investments|Elec|","", variable, fixed = T)), 
    )) %>% 
    mutate(neg_diff = ifelse(diff_val < 0, diff_val, 0)) %>%
    group_by(region, scenario.x, period) %>%
    mutate(min_diff = sum(neg_diff)) %>%
    group_by(region) %>%
    mutate(min_diff = min(min_diff)) %>%
    ungroup()
  
  
  ggplot(q_diff_host_elec_inv_agg) +
    geom_col(mapping = aes(x = scenario.x,
                           # interaction(scenario.x, period),
                           y = diff_val,
                           fill = variable),
             # stat = "count",
             data = . %>% filter(period == 2025),
             color = "grey15",
             width = 0.4,
             just = 1,
             linewidth = 0.25) +
    geom_col(mapping = aes(x = scenario.x,
                           # interaction(scenario.x, period),
                           y = diff_val,
                           fill = variable),
             # stat = "count",
             data = . %>% filter(period == 2030),
             color = "grey15",
             width = 0.4,
             just = 0,
             linewidth = 0.1) +
    geom_text(data = . %>% filter(period == 2025),
              mapping = aes(x = scenario.x,
                            y =  min_diff * 1.22 -1.5,
                            label = "2023-27"),
              nudge_x = -0.225,
              size = 3.5,
              color = "grey19"
              # angle = 45
    ) +
    geom_text(data = . %>% filter(period == 2030),
              mapping = aes(x = scenario.x,
                            y =  min_diff * 1.22 - 1.5,
                            label = "2028-32"),
              nudge_x = 0.225,
              size = 3.5,
              color = "grey19"
              # angle = 45
    ) +
    labs(x = NULL,
         y = "2023-32 Investment Above/Below Pre-Glasgow Expectations (Billion $2019)") +
    # facet_wrap(. ~ scenarree") +
    # scale_fill_manual(values = setNames(c(brewer.pal(n = 9, "Set1")[-c(1,5,6,8)], brewer.pal(n = 6, "Set1")[6]),
    # c(rev(unique(q_diff_host_elec_inv_agg$variable)[-4]), unique(q_diff_host_elec_inv_agg$variable)[4])),
    scale_fill_manual(values = setNames(c(brewer.pal(n = 8, "Accent")[-c(4,5,6)],brewer.pal(n = 8, "Accent")[4]),
                                        c(rev(unique(q_diff_host_elec_inv_agg$variable)[-4]), unique(q_diff_host_elec_inv_agg$variable)[4])),
                                        name = "Technology") +
    # scale_fill_brewer(palette = "PuOr", direction = 1, name = "Technology") + 
    theme_bw() +
    # geom_hline(yintercept = 0,
    #            color = 'black',
    #            linewidth = 0.65) +
    # facet_grid(period ~ .) +
    facet_wrap(region ~ ., ncol = 1,
                 # factor(region,
                 #          levels = c(regOrd[-1], 'CHA', 'IND', 'USA', 'JPN', 'EUR'),
                 #          labels = c(paste0(regOrder[-1],'*'), 'China', 'India', 'USA', 'Japan', 'EU & UK')),
               scales = 'free_y' ) +
    theme(text = element_text(size = 1.5 *9),
          axis.text.x = element_text(size = 1.5 *10, face = "bold"),
          strip.text = element_text(size = 1.5 * 10, face = "bold"),
          legend.text = element_text(size = 1.5 *8),
          legend.title = element_text(size = 1.5 *7, face = "bold"),
          # legend.position = c(0.845, 0.91),
          legend.position = "right",
          legend.direction = "vertical",
          legend.background = element_blank(),
          legend.key.size = unit(3, "mm"),
          strip.background = element_rect(fill = "white"),
          # legend.box.spacing = unit(0, "mm"),
          legend.spacing = unit(0,"mm"),
          legend.margin = margin())
  
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_allocation_2025_2030_aggregated.pdf",width = 12*2.1, height = 10.5*2.1, units = "cm", dpi="retina")
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_allocation_2025_2030_aggregated.jpg",width = 12*2.1, height = 10.5*2.1, units = "cm", dpi="retina")
  

    
  
  
  ### PERCENTAGES BY REGION ###
  q_reg_perc_diff_host_elec_inv <- q_diff_host_elec_inv %>% 
    filter(region %in% c("FNX",host_regi) & 
             # scenario.x == "REdirect-grant" &
             !grepl("Coal|Gas", variable) &
             variable %in% c(v_inv_REdir, v_inv_fos) &
             period %in% c(2025,2030)) %>% 
    # group_by() %>% 
    group_by(region, scenario.x, period) %>%
    mutate(reg_tot = sum(diff_val)) %>%
    mutate(percent = diff_val *100 / reg_tot)
    
  
  ggplot(q_diff_host_elec_inv %>%
           filter(region %in% host_regi &
                    scenario.x == "REdirect" &
                    !grepl("Coal|Gas", variable) &
                    period == 2025)) +
    geom_col(mapping = aes(x = factor(region,
                                      levels = regOrd[-1],
                                      labels = regOrder[-1]),
                           y = percent,
                           fill = variable),
             # stat = "count",
             data = . %>%
               filter(variable %in% c(v_inv_REdir, v_inv_fos)) %>%
               mutate(variable = gsub("|", " | ", 
                                      gsub("Energy Investments|Elec|","", variable, fixed = T), 
                                      fixed = T)),
             color = "grey15",
             linewidth = 0.1) +
    labs(x = NULL,
         y = "Public G20 REdirect Allocation (billion $2019)") +
    scale_y_continuous(labels = paste0(seq(0,100,10), "%"),
                       breaks = seq(0,100,10)) +
    # facet_wrap(. ~ scenario.x,
    #            scales = "free") +
    scale_fill_brewer(palette = "Set2", direction = -1, name = "Technology") + 
    theme_bw() +
    theme(text = element_text(size = 6),
          axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
          strip.text = element_text(size = 6, face = "bold"),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 6, face = "bold"),
          # legend.position = c(0.89, 0.82),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.key.size = unit(3, "mm"),
          strip.background = element_rect(fill = "white"),
          legend.box.spacing = unit(0, "mm"),
          legend.spacing = unit(0,"mm"),
          legend.margin = margin())
  
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_pubOnly_allocation_2025_percent.pdf",width = 12, height = 8.5, units = "cm", dpi="retina")
  ggsave("~/FinEx/output/plots/inv_seel_area/REdir_pubOnly_allocation_2025_percent.jpg",width = 12, height = 8.5, units = "cm", dpi="retina")
  
  
  
  
  
  ### COLLECTIVE PERCENTAGES ###
  q_percent_diff_host_elec_inv <- q_diff_host_elec_inv %>%
    filter(region =="FNX" &
             !grepl("Coal|Gas", variable) &
             variable %in% c(v_inv_REdir, v_inv_fos)
    ) %>%
    ungroup() %>%
    group_by(scenario.x, variable) %>%
    mutate(
      cumval = cumsum(value)
    ) %>%
    ungroup() %>%
    group_by(scenario.x) %>%
    mutate(percent = round(diff_val / sum(diff_val) * 100, 1),
           sumval = sum(value),
           cumperc = round(cumval / sumval * 100, 1))
  
  
  View(q_percent_diff_host_elec_inv %>% filter(period %in% seq(2025, 2050, 5)))
  
ggplot(q_percent_diff_host_elec_inv %>%
         filter(variable %in% v_inco_re)) +
  geom_line(aes(x = period,
                y = value,
                color = variable)) +
  facet_wrap(. ~ scenario.x)


(q_percent_diff_host_elec_inv %>%
    ungroup() %>%
    filter(variable %in% v_inco_re &
           grepl("Battery", variable),
         period == 2050,
         scenario.x == "2\u00B0C") %>%
  summarise(mean(value)) - 
q_percent_diff_host_elec_inv %>%
    ungroup() %>%
    filter(variable %in% v_inco_re &
           grepl("Battery", variable),
         period == 2025,
         scenario.x == "2\u00B0C") %>% 
    summarise(mean(value))) / 
  q_percent_diff_host_elec_inv %>%
  ungroup() %>%
  filter(variable %in% v_inco_re &
           grepl("Battery", variable),
         period == 2025,
         scenario.x == "2\u00B0C") %>% 
  summarise(mean(value))










## INDIVIDUAL VARS ##
q_host_vre_inv <- q_host_elec_inv %>%
  filter(grepl("Non-Bio Re",variable)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_host_grid_inv <- q_host_elec_inv %>%
  filter(grepl("Grid",variable)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_host_stor_inv <- q_host_elec_inv %>%
  filter(grepl("Storage",variable)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_host_coal_inv <- q_host_elec_inv %>%
  filter(grepl("Coal|",variable,fixed = TRUE)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_host_gas_inv <- q_host_elec_inv %>%
  filter(grepl("Gas|",variable,fixed = TRUE)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")



### REFERENCE SCENARIO INVESTMENTS ###
q_ref_host_elec_inv <- q_ppca_growth %>%
  filter(grepl("Energy Investments|Elec",variable,fixed=TRUE) & 
           region == "FNX" & 
           !grepl("Dynamic",scenario) &
           period > 2020 & period < 2045)

q_ref_host_vre_inv <- q_ref_host_elec_inv %>%
  filter(grepl("Non-Bio Re",variable)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_ref_host_grid_inv <- q_ref_host_elec_inv %>%
  filter(grepl("Grid",variable)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_ref_host_stor_inv <- q_ref_host_elec_inv %>%
  filter(grepl("Storage",variable)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_ref_host_coal_inv <- q_ref_host_elec_inv %>%
  filter(grepl("Coal|",variable,fixed = TRUE)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")

q_ref_host_gas_inv <- q_ref_host_elec_inv %>%
  filter(grepl("Gas|",variable,fixed = TRUE)) %>%
  group_by(scenario,period) %>%
  summarise(value = sum(value), .groups = "keep")


### DIFFERENCE ###
q_diff_vre_inv <- for (i in unique(q_host_vre_inv$scenario))  filter(q_host_vre_inv,scenario==i) - q_ref_host_vre_inv

q_host_vre_inv$value - q_ref_host_vre_inv$value





### TOTAL ENERGY SYSTEM INVESTMENTS THROUGH 2050 ###
tot_RE_inv <- q_diff_host_elec_inv %>%
  filter(
    region == "FNX" &
      variable %in% c(v_inv_REdir)) %>%
  group_by(scenario.x, region) %>%
  summarise(total = sum(value) * 1e-3) %>%
  ungroup()


tot_en_sys_cost <- q_diff_host_elec_inv %>%
  filter(
    region == "FNX" &
      variable %in% c(v_syscost, v_taxrev)) %>%
  group_by(scenario.x, region) %>%
  summarise(total = sum(value) * 1e-3) %>%
  ungroup()

r = "FNX"
rname <- ifelse(r=="FNX", "FinEx Hosts", names(regOrd)[which(regOrd==r)])


ggplot(q_diff_host_elec_inv %>% 
         filter(!is.na(scenario.x) & !grepl("PPCA", scenario.x)) %>%
         mutate(variable = gsub("Energy Investments|Elec|","", variable, fixed = T)) %>%
         filter(variable %in% gsub("Energy Investments|Elec|","", c(v_inv_REdir, v_inv_fos),fixed=T) &
                  !grepl("Coal", variable),
                period %in% seq(2025, 2050, 5) & region == r)) +
  geom_col(linewidth = 0.1,
           color = "grey30",
           width = 5,
           mapping = aes(x = period,
                         y = diff_val,
                         fill = variable)) +
  facet_wrap(.~scenario.x, scales = "free") +
  geom_label(mapping = aes(x = 2050,
                           y = 827,
                           label = paste0(ifelse(grepl("1.5", scenario.x), "$", " $"), round(total, 1), "T "),
                           color =),
             data = tot_RE_inv %>% filter(region == "FNX"),
             size = 1.8,
             color = "forestgreen"
  ) +
  geom_label(mapping = aes(x = 2050,
                           y = 640,
                           label = paste0("$", round(total, 1), "T")),
             data = tot_en_sys_cost %>%
               # rename(scenario.x = scenario) %>%
               filter(!is.na(scenario.x) & !grepl("PPCA", scenario.x) & region == "FNX"),
             size = 1.8,
             color = "black"
  ) +
  scale_fill_brewer(palette = "Set2", direction = -1, 
                    # labels = gsub("Energy Investments|Elec|","",),
                    name = "Technology",
                    guide = "none"
  ) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2025,2050,5),
                     # labels = c("2022-27", "2027-32", "2032-37", "2037-42", "2042-47", "2048-2052")
  ) +
  # scale_y_continuous(labels = paste0("$",diff_val)) +
  labs(
    # title = rname,
    x = NULL,
    y = "Investment above observed policy expectations (billion $2019)") +
  theme(text = element_text(size = 6),
        axis.text.x = element_text(size = 5, angle = 0, hjust = 0.5),
        strip.text = element_text(size = 5, face = "bold"),
        legend.text = element_text(size = 6),
        strip.background = element_rect(fill = "white"),
        legend.title = element_text(size = 6))

ggsave(paste0("~/FinEx/output/plots/inv_seel_area/bar_diff_ppcaref_ts_2050_2labs_Ctax",r,"_ann_redir_oct.pdf"),width = 11, height = 8.5, units = "cm", dpi="retina")
ggsave(paste0("~/FinEx/output/plots/inv_seel_area/bar_diff_ppcaref_ts_2050_2labs_Ctax",r,"_ann_redir_oct.png"),width = 11, height = 8.5, units = "cm", dpi="retina")
# }
