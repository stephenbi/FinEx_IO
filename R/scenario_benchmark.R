
eval_metrics <- q_dpe_pol %>% 
  filter(variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs, v_seel_VRE, v_ghg_seel, v_tot_emi) &
           period >= 2020 & period <= 2050 &
           # scenario != "REdirect" &
           region %in% c("FNX", "GLO")) %>%
  group_by(region, scenario, variable) %>%
  mutate(cumval = ifelse(unit=="%", value, cumsum(value)))

benchmarks <- q_budget %>%
  filter(variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs, v_seel_VRE, v_ghg_seel, v_tot_emi) &
           period >= 2020 & period <= 2050 &
           region %in% c("FNX", "GLO")) %>%
  group_by(region, scenario, variable) %>%
  mutate(cumval = ifelse(unit=="%", value, cumsum(value)))



#### NORMALIZED METRICS ####

scale_metrics <- eval_metrics %>% 
  mutate(scenario = as.character(scenario)) %>%
  filter(period==2050) %>%
  full_join(benchmarks %>% 
              filter(period==2050 &
                       grepl("2", scenario)) %>%
              ungroup(),
            # %>%
              # select(cumval, variable),
            ) %>%  
  left_join(benchmarks %>% 
              filter(period==2050 &
                       grepl("1.5", scenario)) %>%
              rename(benchval = cumval) %>% 
              ungroup() %>%
              select(region, benchval, variable),
            by = c("region", "variable")) %>%
  left_join(q_ppca_ref %>%
              group_by(region, variable) %>%
              mutate(cumval = ifelse(unit=="%", value, cumsum(value))) %>% 
              filter(variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs, v_seel_VRE, v_ghg_seel, v_tot_emi) &
                       period==2050 & 
                       region %in% c("FNX", "GLO")) %>%
              rename(refval = cumval) %>% 
              ungroup() %>%
              select(region, refval, variable),
            by = c("region", "variable")) %>%
  mutate(
    score =
      ifelse(
        refval > benchval,
        (refval - cumval) / (refval - benchval),
        (cumval - refval) / (benchval - refval)
      ),
    scenario = factor(x = scenario,
                      # levels = c("PPCA-Dynamic (pre-FinEx)", "FinEx", "REdirect", "REdirect-lo", "REdirect-hi", "REdirect-hiCon", "REdirect-HI_cond-2040", "2\u00B0C"),
                      levels = c("Current", "PPCA-growth", "FinEx", "REdirect", "REdirect-loan", "REdirect-grant", "2\u00B0C"),
                      labels = c("Current", "PPCA-growth", "FinEx", "REdirect", "REdirect-loan", "REdirect-grant", "2\u00B0C"),
                      ordered = T,
                      ),
    variable = factor(x = variable,
           levels = c(v_seel_coal_noccs, v_seel_gas_noccs, v_seel_VRE, v_ghg_seel, v_tot_emi),
           labels = c("Coal Power Phase-out", "Gas Power Phase-out", "VRE Deployment", "Grid Decarbonization", "Total GHG Abatement"),
           ordered = TRUE),
    region = factor(x = region, levels = c("FNX", "GLO"), labels = c("FinEx Hosts", "Global"), ordered = TRUE) 
    ) %>%
  rename(Scenario = scenario)


### PLOT NORMALIZED METRICS ###
ggplot(scale_metrics %>% 
         filter(period == 2050,
                Scenario != "REdirect",
                !grepl("zero", Scenario))) +
  geom_col(data = . %>% filter(Scenario != "2\u00B0C"),
           mapping = aes(x = fct_rev(Scenario),
               y = score * 100,
               fill = Scenario),
           position = "dodge") +
  facet_grid(variable ~ region, scales = "fixed", switch = "y") +
  theme_bw()  +
  geom_hline(data = . %>% 
               filter(period == 2050 &
                        grepl("2\u00B0C", Scenario)),
             mapping = aes(yintercept = score * 100,
                           color = Scenario),
             linewidth = 0.9) +
  # scale_fill_viridis_d(name = "DPE Scenario") +
  scale_fill_manual(values = scen_clrs) +
  # scale_fill_brewer(palette = "Set2", 
  #                   name = "Scenario",
  #                   direction = -1,
  #                   breaks = strsplit(levels(oecd10$scenario), split = "\n", fixed = T, )[1],
  #                   labels = levels(scale_metrics$Scenario)) +
  scale_color_manual(values = scen_clrs, name = "Benchmark") +
  guides(fill = guide_legend(order = 1)) +
  scale_y_continuous(labels = ~paste0(.,"%")) +
  ylab("Gap to 1.5\u00B0C pathway closed by 2050") +
  coord_flip(ylim = c(-15,100), expand = F) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 1.2 * 9),
    strip.text.x = element_text(face = "bold", size = 1.2 * 9),
    strip.text.y = element_text(size = 1.2 * 8),
    strip.background = element_rect(color = "black", fill = "white"),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black")
  )

ggsave("~/FinEx/output/plots/benchmarks/scale_metrics_2050_jul21.pdf", height = 8, width = 8, units = "in", dpi = "retina")
ggsave("~/FinEx/output/plots/benchmarks/scale_metrics_2050_jul21.jpg", height = 8, width = 8, units = "in", dpi = "retina")
 


### DATA FRAME CALCS ###
scale_metrics %>% 
  filter(period == 2050,
         !grepl("zero", Scenario), region != "Global",
         grepl("Grid", variable)) 

scale_metrics %>% 
  filter(period == 2050,
         !grepl("zero", Scenario), region == "Global",
         grepl("Grid", variable)) 


scale_metrics %>% 
  filter(period == 2050,
         !grepl("zero", Scenario), region != "Global",
         grepl("GHG", variable)) 

scale_metrics %>% 
  filter(period == 2050,
         !grepl("zero", Scenario), region == "Global",
         grepl("GHG", variable)) 
