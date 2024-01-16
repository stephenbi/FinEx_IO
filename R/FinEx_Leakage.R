####################################
### Financial Carbon Leakage GHG ###
####################################

### FINEX POLICY RUN ###
q_finex_elec_ghg <- q_dpe_pol %>%
  filter(grepl(v_ghg_seel,variable,fixed=TRUE) & 
           scenario=="FinEx" &
           period >= 2020 & period <= 2100) 

### REDIRECT SCENARIO INVESTMENTS ###
q_redir_host_elec_ghg <- q_dpe_pol %>%
  filter(grepl(v_ghg_seel,variable,fixed=TRUE) & 
           scenario=='REdirect' &
           period >= 2020 & period <= 2100) %>%
  rename(ref_val = value)

### REFERENCE SCENARIO INVESTMENTS ###
q_ppca_host_elec_ghg <- q_dpe_pol %>%
  filter(grepl(v_ghg_seel,variable,fixed=TRUE) & 
           scenario=='PPCA-growth' &
           period >= 2020 & period <= 2100) %>%
  rename(ref_val = value)


### DIFFERENCE FROM REDIRECT ###
q_diff_host_elec_ghg <- q_finex_elec_ghg %>%
  left_join(q_redir_host_elec_ghg, by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(diff_val = value - ref_val) %>%
  group_by(region) %>%
  mutate(cum_diff = cumsum(diff_val)) %>%
  ungroup()

q_cumdiff_elec_ghg <- q_diff_host_elec_ghg %>%
  filter(period %in% c(2050, 2065, 2070, 2080, 2090, 2100) &
           region %in% c("GLO", "FNX"))
# %>%
#   group_by(region, period) %>%
#   summarise(finLeakage = sum(cum_diff), .groups = "keep")


## AREA PLOT ##
ggplot(q_diff_host_elec_ghg %>% 
         filter(
           !(region %in% c("GLO", "FNX")) &
                  period <= 2070)) +
  geom_area(
            mapping = aes( 
              x = period,
              y = cum_diff,
              fill = region),
                # factor(region,
                #             levels = sort(unique(map$RegionCode)),
                #             labels = regAlpha)),
            color = "grey15",
            linewidth = 0.1) +
  geom_hline(yintercept = 0,
             color = 'grey15') +
  scale_fill_manual(values = regFill, name = "Region") +
  facet_grid(. ~ factor(variable, levels = v_ghg_seel, labels = "Power Sector GHG Emissions Leakage")) +
  ylab("Cumulative Financial Carbon Leakage (MtCO2eq)") +
  theme_bw() +
  theme(
    text = element_text(size = 6),
    legend.title = element_text(size = 5, face = 'bold'),
    legend.key.size = unit(3, 'mm'),
    axis.title.x = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box.spacing = unit(3,'mm')
  )
  
ggsave("~/FinEx/output/plots/FinExLeakage/FinEx_leakage_from_REdir_by_reg_area.pdf", dpi = "retina", width = 3.5, height = 2, units = "in")
ggsave("~/FinEx/output/plots/FinExLeakage/FinEx_leakage_from_REdir_by_reg_area.jpg", dpi = "retina", width = 3.5, height = 2, units = "in")




### DIFFERENCE FROM PPCA-GROWTH ###
q_diff_ppca_host_elec_ghg <- q_finex_elec_ghg %>%
  left_join(q_ppca_host_elec_ghg, by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(diff_val = value - ref_val) %>%
  group_by(region) %>%
  mutate(cum_diff = cumsum(diff_val)) %>%
  ungroup()

q_cumdiff_ppca_elec_ghg <- q_diff_ppca_host_elec_ghg %>%
  filter(period %in% c(2050, 2065, 2070, 2080, 2090, 2100) &
           region %in% c("GLO", "FNX"))

## AREA PLOT ##
ggplot(q_diff_ppca_host_elec_ghg %>% 
         filter(
           !(region %in% c("GLO", "FNX")) &
             period <= 2070)) +
  geom_area(
    mapping = aes( 
      x = period,
      y = cum_diff,
      fill = region),
    # factor(region,
    #             levels = sort(unique(map$RegionCode)),
    #             labels = regAlpha)),
    color = "grey15",
    linewidth = 0.1) +
  geom_hline(yintercept = 0,
             color = 'grey15') +
  scale_fill_manual(values = regFill, name = "Region") +
  facet_grid(. ~ factor(variable, levels = v_ghg_seel, labels = "Power Sector GHG Emissions Leakage")) +
  ylab("Cumulative Financial Carbon Leakage (MtCO2eq)") +
  theme_bw() +
  theme(
    text = element_text(size = 6),
    legend.title = element_text(size = 5, face = 'bold'),
    legend.key.size = unit(3, 'mm'),
    axis.title.x = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box.spacing = unit(3,'mm')
  )

ggsave("~/FinEx/output/plots/FinExLeakage/FinEx_leakage_from_ppca_by_reg_area.pdf", dpi = "retina", width = 3.5, height = 2, units = "in")
ggsave("~/FinEx/output/plots/FinExLeakage/FinEx_leakage_from_ppca_by_reg_area.jpg", dpi = "retina", width = 3.5, height = 2, units = "in")







########################################
##### COAL AND GAS FINANCE LEAKAGE #####
########################################
### FINEX & REDIRECT POLICY RUNS ###
q_finex_inv_fos <- q_dpe_pol %>%
  filter(variable %in% v_inv_fos & 
           scenario=="FinEx" &
           period >= 2020 & period <= 2100) %>%
  mutate(value = value * inflation_05_to_19)

### REdirect SCENARIO INVESTMENTS ###
q_redir_host_inv_fos <- q_dpe_pol %>%
  filter(variable %in% v_inv_fos & 
           scenario=='REdirect' &
           period >= 2020 & period <= 2100) %>%
  mutate(value = value * inflation_05_to_19) %>%
  rename(ref_val = value)

### REFERENCE SCENARIO INVESTMENTS ###
q_ppca_host_inv_fos <- q_dpe_pol %>%
  filter(variable %in% v_inv_fos & 
           scenario=='PPCA-growth' &
           period >= 2020 & period <= 2100) %>%
  mutate(value = value * inflation_05_to_19) %>%
  rename(ref_val = value)

### DIFFERENCE FROM REDIRECT ###
q_diff_host_inv_fos <- q_finex_inv_fos %>%
  left_join(q_redir_host_inv_fos, by = c("variable", "period", "region", "unit")) %>%
  mutate(diff_val = value - ref_val) %>%
  group_by(region, variable) %>%
  mutate(cum_diff = cumsum(diff_val)) %>%
  ungroup()

q_cumdiff_inv_fos <- q_diff_host_inv_fos %>%
  group_by(region, variable) %>%
  summarise(finLeakage = max(cum_diff), .groups = "keep") %>%



### AREA PLOT FOSSIL FINANCE LEAKAGE ###
ggplot() +
  geom_area(
    data = q_diff_host_inv_fos %>% 
      filter(!(region %in% c("GLO", "FNX")) &
               period <= 2050 &
               variable %in% v_inv_fos),
    mapping = aes(x = period,
      y = cum_diff,
      fill = region),
    color = "grey15",
    linewidth = 0.1) +
  scale_fill_manual(values = regFill, name = "Region") +
  ylab("Cumulative Fossil Finance Leakage (billion $2019)") +
  theme_bw() +
  facet_grid(. ~ factor(variable,
                        levels = v_inv_fos,
                        labels = c("Coal Power Finance Leakage", "Gas Power Finance Leakage"))) +
  theme(
    text = element_text(size = 6),
    legend.title = element_text(size = 5, face = 'bold'),
    legend.key.size = unit(3, 'mm'),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.spacing = unit(0,'mm')
  )

# ggsave("~/FinEx/output/plots/FinExLeakage/Fossil_finance_leakage_by_reg.pdf", dpi = "retina", width = 3.5, height = 2, units = "in")
# ggsave("~/FinEx/output/plots/FinExLeakage/Fossil_finance_leakage_by_reg.jpg", dpi = "retina", width = 3.5, height = 2, units = "in")



### DIFFERENCE FROM PPCA-GROWTH ###
q_diff_ppca_host_inv_fos <- q_finex_inv_fos %>%
  left_join(q_ppca_host_inv_fos, by = c("variable", "period", "region", "unit")) %>%
  mutate(diff_val = value - ref_val) %>%
  group_by(region, variable) %>%
  mutate(cum_diff = cumsum(diff_val)) %>%
  ungroup()

q_cumdiff_inv_fos <- q_diff_ppca_host_inv_fos %>%
  group_by(region, variable) %>%
  summarise(finLeakage = max(cum_diff), .groups = "keep") %>%
  
  
  
  ### AREA PLOT FOSSIL FINANCE LEAKAGE ###
  ggplot() +
  geom_area(
    data = q_diff_ppca_host_inv_fos %>% 
      filter(!(region %in% c("GLO", "FNX")) &
               period <= 2050 &
               variable %in% v_inv_fos),
    mapping = aes(x = period,
                  y = cum_diff,
                  fill = region),
    color = "grey15",
    linewidth = 0.1) +
  scale_fill_manual(values = regFill, name = "Region") +
  ylab("Cumulative Fossil Finance Leakage (billion $2019)") +
  theme_bw() +
  facet_grid(. ~ factor(variable,
                        levels = v_inv_fos,
                        labels = c("Coal Power Finance Leakage", "Gas Power Finance Leakage"))) +
  theme(
    text = element_text(size = 6),
    legend.title = element_text(size = 5, face = 'bold'),
    legend.key.size = unit(3, 'mm'),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.spacing = unit(0,'mm')
  )

# ggsave("~/FinEx/output/plots/FinExLeakage/Fossil_finance_leakage_by_reg.pdf", dpi = "retina", width = 3.5, height = 2, units = "in")
# ggsave("~/FinEx/output/plots/FinExLeakage/Fossil_finance_leakage_by_reg.jpg", dpi = "retina", width = 3.5, height = 2, units = "in")


########################################
##### COAL AND GAS CAPACITY LEAKAGE #####
########################################
### FINEX & REDIRECT POLICY RUNS ###
q_finex_deltacap_fos <- q_dpe_pol %>%
  filter(variable %in% v_deltacap_fos & 
           scenario=="FinEx" &
           period >= 2020 & period <= 2100 &
           period %% 5 == 0)

### REFERENCE SCENARIO INVESTMENTS ###
q_redir_host_deltacap_fos <- q_dpe_pol %>%
  filter(variable %in% v_deltacap_fos & 
           scenario=='REdirect' &
           period >= 2020 & period <= 2100) %>%
  rename(ref_val = value)


### DIFFERENCE ###
q_diff_host_deltacap_fos <- q_finex_deltacap_fos %>%
  left_join(q_redir_host_deltacap_fos, by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(diff_val = value - ref_val) %>%
  group_by(region, variable) %>%
  mutate(cum_diff = cumsum(diff_val))

q_cumdiff_deltacap_fos <- q_diff_host_deltacap_fos %>%
  group_by(region, variable) %>%
  summarise(finLeakage = sum(diff_val), .groups = "keep")


### AREA PLOT FOSSIL FINANCE LEAKAGE ###
ggplot(q_diff_host_deltacap_fos %>% 
         filter(
           # region %in% host_regi &
           !c(region %in% c("GLO", "FNX")) &
             period <= 2050 &
             variable %in% v_deltacap_fos)) +
  geom_area(
    mapping = aes( 
      x = period,
      y = cum_diff,
      fill = region),
    color = "grey15",
    linewidth = 0.1) +
  scale_fill_manual(values = regFill, name = "Region") +
  ylab("Cumulative Fossil Finance Leakage (billion $2019)") +
  theme_bw() +
  facet_grid(. ~ factor(variable,
                        levels = v_deltacap_fos,
                        labels = c("Coal Power Finance Leakage", "Gas Power Finance Leakage"))) +
  theme(
    text = element_text(size = 6),
    legend.title = element_text(size = 5, face = 'bold'),
    legend.key.size = unit(3, 'mm'),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.spacing = unit(0,'mm')
  )

######################################
##### COAL AND GAS POWER LEAKAGE #####
######################################
EJ_2_TWa = 1 / 31.536
sm_c_2_co2 = 3.666666666667
s_gwpN2O = 265
sm_tgn_2_pgc = (44/28) * s_gwpN2O * (12/44) * 0.001

### FINEX & REDIRECT POLICY RUNS ###
q_finex_elec_fos <- q_dpe_pol %>%
  filter(variable %in% c("SE|Electricity|Gas|w/o CC", "SE|Electricity|Coal|w/o CC") & 
           scenario=="FinEx" &
           period >= 2020 & period <= 2100)  %>%
  group_by(region, variable) %>%
  mutate(cumval = cumsum(value)) %>%
  mutate(cumemi = ifelse(grepl("Coal", variable),
                         (cumval * 1e-3 * 26.1 * sm_c_2_co2 + 
                           cumval * EJ_2_TWa * .04415 * sm_tgn_2_pgc * sm_c_2_co2) / 0.41,
                         (cumval * 1e-3 * 15.3 * sm_c_2_co2 + 
                           cumval * EJ_2_TWa * .00315 * sm_tgn_2_pgc * sm_c_2_co2) / 0.55))

### REFERENCE SCENARIO INVESTMENTS ###
q_redir_host_elec_fos <- q_dpe_pol %>%
  filter(
    variable %in% c("SE|Electricity|Gas|w/o CC", "SE|Electricity|Coal|w/o CC") & 
           scenario=="REdirect" &
           # grepl("REdirect_pub",scenario) &
           period >= 2020 & period <= 2100) %>%
  group_by(region, variable) %>%
  rename(ref_val = value) %>%
  mutate(ref_cumval = cumsum(ref_val)) %>%
  mutate(ref_cumemi = ifelse(grepl("Coal", variable),
                             (ref_cumval * 1e-3 * 26.1 * sm_c_2_co2 + 
                               ref_cumval * EJ_2_TWa * .04415 * sm_tgn_2_pgc * sm_c_2_co2) / 0.41,
                             (ref_cumval * 1e-3 * 15.3 * sm_c_2_co2 + 
                               ref_cumval * EJ_2_TWa * .00315 * sm_tgn_2_pgc * sm_c_2_co2) / 0.55))



### DIFFERENCE ###
q_diff_host_elec_fos <- q_finex_elec_fos %>%
  left_join(q_redir_host_elec_fos, by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(diff_val = value - ref_val) %>%
  group_by(region, variable) %>%
  mutate(cumdiff_emi = cumemi - ref_cumemi) %>%
  mutate(cum_diff = cumsum(diff_val)) 
# %>%
  # mutate(cum_leakage = cumsum(diff_emi))

# q_diff_host_elec_fos %>%

# q_cumdiff_elec_fos <- q_diff_host_elec_fos %>%
#   group_by(region, variable) %>%
#   summarise(finLeakage = sum(diff_val), .groups = "keep")

# EJ cumulative generation
# summarise(ungroup(filter(q_cumdiff_elec_fos, region %in% c("GLO","FNX"))), sum(finLeakage))

# GtCO2 cumulative emissions
q_diff_host_elec_fos %>%
  filter(region %in% c("GLO","FNX"),
         period %in% c(2050, 2070, 2080, 2090, 2100)) %>%
  select(region, variable, period, cum_diff, cumdiff_emi)
  # mutate(emissions = finLeakage * 1e-3 * 26.1 * sm_c_2_co2)
  

## AREA PLOT ##
ggplot(q_diff_host_elec_fos %>% 
         filter(region %in% c("GLO", host_regi) &
                  period <= 2070 &
                  cum_diff > 0.02)) +
  geom_area(aes(
    x = period,
    y = cum_diff,
    fill = variable)) +
  facet_grid(.~factor(region,levels=regOrd,labels = regOrder), scales = "fixed") +
  theme_bw() + 
  ylab("Cumulative Financial Fossil Leakage (EJ)") +
  scale_fill_brewer(palette = "Pastel1", name = "Fuel", labels = c("Coal", "Gas")) +
  theme(
    axis.title.x = element_blank(),
        legend.position = c(0.92, 0.91),
        legend.title.align = 0.5,
    panel.grid = element_line(colour = "grey95"),
    legend.background = element_blank(),
    text = element_text(size = 7),
    legend.title = element_text(size = 6, face = 'bold'),
    legend.key.size = unit(3, 'mm'),
    axis.text.x = element_text(angle = 45)
    )

ggsave("~/FinEx/output/plots/FinExLeakage/energy_leakage_by_fuel.pdf", dpi = "retina", width = 3.5, height = 3, units = "in")
ggsave("~/FinEx/output/plots/FinExLeakage/energy_leakage_by_fuel.jpg", dpi = "retina", width = 3.5, height = 3, units = "in")




####################################
### Financial Carbon Leakage co2 ###
####################################

### FINEX & REDIRECT POLICY RUNS ###
q_finex_elec_co2 <- q_dpe_pol %>%
  filter(grepl(v_co2_seel,variable,fixed=TRUE) & 
           scenario=="FinEx" &
           period >= 2020 & period <= 2100) 

### REFERENCE SCENARIO INVESTMENTS ###
q_redir_host_elec_co2 <- q_dpe_pol %>%
  filter(grepl(v_co2_seel,variable,fixed=TRUE) & 
           scenario=='REdirect' &
           period >= 2020 & period <= 2100) %>%
  rename(ref_val = value)


### DIFFERENCE ###
q_diff_host_elec_co2 <- q_finex_elec_co2 %>%
  left_join(q_redir_host_elec_co2, by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(diff_val = value - ref_val) %>%
  group_by(region) %>%
  mutate(cum_diff = cumsum(diff_val))

q_cumdiff_elec_co2 <- q_diff_host_elec_co2 %>%
  filter(period %in% c(2050, 2065, 2070, 2100) &
           region %in% c("GLO", "FNX"))

