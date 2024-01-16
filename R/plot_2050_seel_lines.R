
##############
#### MIFs ####
##############

frozen <- read.report(list.files("H:/FinEx/runs/",pattern = "PPCAcop26",full.names = T), as.list = F)

dpe_full <- read.report(list.files("H:/FinEx/runs/",pattern = "nonoecd",full.names = T), as.list = F)

budget <- read.report(list.files("H:/FinEx/runs/",pattern = "PkBud",full.names = T), as.list = F)

q_frozen <- as.quitte(frozen)

q_dpe_full <- as.quitte(dpe_full)

q_budget <- as.quitte(budget)

########################
#### TIME & REGIONS ####
########################
ttot <- seq(2020,2050,5)
tall <- getYears(frozen,as.integer = T)[which(getYears(frozen,as.integer = T) >= 2020 & getYears(frozen,as.integer = T) <= 2100)]

host_regi <- c("OAS", "NEU", "SSA", "MEA", "LAM", "CAZ", "REF") 

###############
#### SCENS ####
###############
q_frozen$scenario <- factor(x = q_frozen$scenario,
                            levels = c("PPCAcop26-limSE1p5x", "FinEx-PPCAcop26-gdpEU_csp", "REdirect-PPCAcop26-gdpEU_csp", "REdirect_LO-PPCAcop26-gdpEU_csp", "REdirect_HI-PPCAcop26-gdpEU_csp", "REdirect_HI-2030-PPCAcop26-gdpEU_csp", "REdirect_HI-cond-PPCAcop26-limSE1p5x"),
                            labels = c("PPCA-Static (pre-FinEx)", "FinEx", "REdirect", "REdirect-LO", "REdirect-HI", "REdirect-HI_2030", "REdirect-HI_cond"),
                            ordered = T)
q_dpe_full$scenario <- factor(x = q_dpe_full$scenario,
                              levels = c("PPCA-limSE1p5x-nonoecd50p", "FinEx-limSE4x-nonoecd50p", "REdirect-limSE4x-nonoecd50p", "REdirect_LO-limSE4x-nonoecd50p", "REdirect_HI-limSE4x-nonoecd50p", "REdirect_HI-2030-limSE4x-nonoecd50p", "REdirect_HI-cond-limSE1p5x-nonoecd50p"),
                              labels = c("PPCA-Dynamic (pre-FinEx)", "FinEx", "REdirect", "REdirect-LO", "REdirect-HI", "REdirect-HI_2030", "REdirect-HI_cond"),
                              ordered = T)

q_ppca_ref <- q_frozen %>%
  bind_rows(
    q_frozen %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) %>% 
  filter(scenario=="PPCA-Static (pre-FinEx)" & period %in% tall)

q_ppca_growth <- q_dpe_full %>%
  bind_rows(
    q_dpe_full %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) %>% 
  filter(scenario == "PPCA-Dynamic (pre-FinEx)" & period %in% tall)


q_frozen_pol <- q_frozen %>%
  bind_rows(
    q_frozen %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) %>% 
  filter(period %in% tall &
           !(scenario %in% c("PPCA-Static (pre-FinEx)"
                             )))


q_dpe_pol <- q_dpe_full %>%
  bind_rows(
    q_dpe_full %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) %>% 
  filter(period %in% tall 
         # &
         # !(scenario %in% c("REdirect-HI_2030"))
         )

q_budget <- q_budget %>%
  bind_rows(
    q_budget %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) %>% 
  filter(period %in% tall)

levels(q_budget$scenario) <- c("2\u00B0C", "1.5\u00B0C")

scen_clrs <- setNames(brewer.pal(name = "Set3", n = length(unique(c(q_dpe_pol$scenario, q_frozen_pol$scenario)))),
                      nm = unique(c(q_dpe_pol$scenario, q_frozen_pol$scenario)))

scen_clrs <- c(scen_clrs,
               setNames(brewer.pal(name = "Dark2", n = 3),
                        nm = c(unique(q_ppca_ref$scenario), "2\u00B0C", "1.5\u00B0C")))

####################
### STATIC SCENS ###
####################

########################
#### LINE PLOT COAL ####
########################
for (jj in c("GLO", "OAS", "FNX")) {
  if (jj == "OAS")  nm_reg <- " S & E Asia (ex. CN, IN, JP)"
  else if (jj == "GLO")  nm_reg <- "Global"
  else if (jj == "FNX")  nm_reg <- "FinEx Hosts"
  
  scen_brks <- as.character(unique(q_ppca_ref$scenario))
  
  ppca_glo_coal_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_coal &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Static Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),
      legend.position = "right"
    )
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_coal_path_2050, filename = paste0("~/FinEx/output/plots/coal_exit_line/",jj,"_static_seel_coal_ppca_only_2050.jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  for (ii in as.character(levels(q_frozen_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_coal_path_2050 <-
      ppca_glo_coal_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_frozen_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_coal &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_coal_path_2050, filename = paste0("~/FinEx/output/plots/coal_exit_line/",jj,"_static_seel_coal_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT GAS ####
  ########################
  scen_brks <- as.character(unique(q_ppca_ref$scenario))
  
  ppca_glo_gas_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_gas &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Static Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    )
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_gas_path_2050, filename = paste0("~/FinEx/output/plots/gas_exit_line/",jj,"_static_seel_gas_ppca_only_2050.jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  for (ii in as.character(levels(q_frozen$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_gas_path_2050 <-
      ppca_glo_gas_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_frozen_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_gas &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
      expand_limits(y = 0)
    
    ggsave(ppca_glo_gas_path_2050, filename = paste0("~/FinEx/output/plots/gas_exit_line/",jj,"_static_seel_gas_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  
  
  ########################
  #### LINE PLOT VRE ####
  ########################
  scen_brks <- as.character(unique(q_ppca_ref$scenario))
  
  ppca_glo_VRE_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_VRE &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Static Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    )
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_VRE_path_2050, filename = paste0("~/FinEx/output/plots/VRE_line/",jj,"_static_seel_VRE_ppca_only_2050.jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  for (ii in as.character(levels(q_frozen_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_VRE_path_2050 <-
      ppca_glo_VRE_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_frozen_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_VRE &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_VRE_path_2050, filename = paste0("~/FinEx/output/plots/VRE_line/",jj,"_static_seel_VRE_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT SEEL ####
  ########################
  scen_brks <- as.character(unique(q_ppca_ref$scenario))
  
  ppca_glo_seel_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Static Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    )
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_seel_path_2050, filename = paste0("~/FinEx/output/plots/seel_line/",jj,"_static_seel_ppca_only_2050.jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  for (ii in as.character(levels(q_frozen_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_seel_path_2050 <-
      ppca_glo_seel_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_frozen_pol %>%
                  filter(scenario == ii &
                           variable == v_seel &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_seel_path_2050, filename = paste0("~/FinEx/output/plots/seel_line/",jj,"_static_seel_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT EMI ####
  ########################
  scen_brks <- as.character(unique(q_ppca_ref$scenario)) 
            
  ppca_glo_emi_seel_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_emi_seel &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "MtCO2/yr",
         title = paste("Static Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    ) 
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_emi_seel_path_2050, filename = paste0("~/FinEx/output/plots/emi_seel_line/",jj,"_emi_seel_ppca_budget_2050.jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  for (ii in as.character(levels(q_frozen_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_emi_seel_path_2050 <-
      ppca_glo_emi_seel_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_emi_seel &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_emi_seel_path_2050, filename = paste0("~/FinEx/output/plots/emi_seel_line/",jj,"_emi_seel_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  #################
  ### DPE SCENS ###
  #################
  
  ########################
  #### LINE PLOT COAL ####
  ########################
  scen_brks <- c(as.character(unique(q_ppca_ref$scenario))
            #      , 
            # as.character(levels(q_ppca_growth$scenario))
            )
  
  ppca_glo_coal_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_coal &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario", guide = "none") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    )
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_coal_path_2050, filename = paste0("~/FinEx/output/plots/coal_exit_line/",jj,"_seel_coal_ppca_only_2050.jpg"),dpi = "retina", height = 5.75, width = 5.5, units = "in")
  
  for (ii in as.character(levels(q_dpe_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_coal_path_2050 <-
      ppca_glo_coal_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_coal &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario", guide = "none")
    
    ggsave(ppca_glo_coal_path_2050, filename = paste0("~/FinEx/output/plots/coal_exit_line/",jj,"_seel_coal_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 5.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT GAS ####
  ########################
  scen_brks <- c(as.character(unique(q_ppca_ref$scenario))
            #      , 
            # as.character(unique(q_ppca_growth$scenario))
            )
  
  ppca_glo_gas_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_gas &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    )
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_gas_path_2050, filename = paste0("~/FinEx/output/plots/gas_exit_line/",jj,"_seel_gas_ppca_only_2050.jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
  
  for (ii in as.character(levels(q_dpe_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_gas_path_2050 <-
      ppca_glo_gas_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_gas &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_gas_path_2050, filename = paste0("~/FinEx/output/plots/gas_exit_line/",jj,"_seel_gas_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
    
  }
  
  
  
  ########################
  #### LINE PLOT VRE ####
  ########################
  scen_brks <- c(as.character(unique(q_ppca_ref$scenario)), 
                 as.character(unique(q_budget$scenario))
            #      , 
            # as.character(unique(q_ppca_growth$scenario))
            )
  
  ppca_glo_VRE_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_VRE &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    ) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              data = q_budget %>%
                filter(variable == v_seel_VRE &
                         # region %in% c("GLO",host_regi) &
                         region == jj &
                         period %in% ttot),
              size = 1.2)
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_VRE_path_2050, filename = paste0("~/FinEx/output/plots/VRE_line/",jj,"_seel_VRE_ppca_budget_2050.jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
  
  for (ii in as.character(levels(q_dpe_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_VRE_path_2050 <-
      ppca_glo_VRE_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_VRE &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_VRE_path_2050, filename = paste0("~/FinEx/output/plots/VRE_line/",jj,"_seel_VRE_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT SEEL ####
  ########################
  scen_brks <- c(as.character(unique(q_ppca_ref$scenario)), 
                            as.character(unique(q_budget$scenario)))
  
  ppca_glo_seel_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    ) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              data = q_budget %>%
                filter(variable == v_seel &
                         # region %in% c("GLO",host_regi) &
                         region == jj &
                         period %in% ttot),
              size = 1.2)
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_seel_path_2050, filename = paste0("~/FinEx/output/plots/seel_line/",jj,"_seel_ppca_budget_2050.jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
  
    for (ii in as.character(levels(q_dpe_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_seel_path_2050 <-
      ppca_glo_seel_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_seel &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_seel_path_2050, filename = paste0("~/FinEx/output/plots/seel_line/",jj,"_seel_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT EMI ####
  ########################
  scen_brks <- c(as.character(unique(q_ppca_ref$scenario)), 
                 as.character(unique(q_budget$scenario)))
  
  ppca_glo_emi_seel_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_emi_seel &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "MtCO2/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg)) +
    theme(
      plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    ) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              data = q_budget %>%
                filter(variable == v_emi_seel &
                         # region %in% c("GLO",host_regi) &
                         region == jj &
                         period %in% ttot),
              size = 1.2)
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_emi_seel_path_2050, filename = paste0("~/FinEx/output/plots/emi_seel_line/",jj,"_emi_seel_ppca_budget_2050.jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
  
  for (ii in as.character(levels(q_dpe_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_emi_seel_path_2050 <-
      ppca_glo_emi_seel_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_emi_seel &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_emi_seel_path_2050, filename = paste0("~/FinEx/output/plots/emi_seel_line/",jj,"_emi_seel_2050_pol_",ii,".jpg"),dpi = "retina", height = 5.75, width = 6.5, units = "in")
    
  }
}

