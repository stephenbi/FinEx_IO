# for (jj in c("GLO", "FNX")) {
  if (jj == "OAS") {  nm_reg <- " S & E Asia (ex. CN, IN, JP)"
  }else if (jj == "GLO") { nm_reg <- "Global"
  }else if (jj == "FNX") { nm_reg <- "FinEx Hosts"
  }
  
  ggplot() +
  geom_line(mapping = aes(x = period,
                          y = value,
                          color = factor(scenario),
                          linetype=model),
            data = q_ppca_ref %>%
              filter(variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs, v_seel_VRE) &
                       region %in% c("GLO", "FNX") &
                       # !grepl("")
                       period %in% ttot) %>%
              mutate(variable = ifelse(grepl("Coal", variable),
                                       "Coal-Fired Electricity", 
                                       ifelse(grepl("Gas", variable),
                                              "Gas-Fired Electricity",
                                              "Solar & Wind Electricity")),
                     region = ifelse(region == "GLO", "Global", "FinEx Hosts"),
                     value = ifelse(grepl("Emissions", variable),
                                    value * 1e-3,
                                    value)),
            linewidth = 1.2)  +
  geom_line(mapping = aes(x = period,
                          y = value,
                          color = factor(scenario),
                          linetype = model),
            data = q_dpe_pol %>%
              filter(scenario != "REdirect" &
                       variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs, v_seel_VRE) &
                       region %in% c("GLO", "FNX") &
                       period %in% ttot) %>%
              # mutate(model = paste0(model,"-COALogit")) %>%
              mutate(variable = ifelse(grepl("Coal", variable),
                                       "Coal-Fired Electricity", 
                                       ifelse(grepl("Gas", variable),
                                              "Gas-Fired Electricity",
                                              "Solar & Wind Electricity")),
                     region = ifelse(region == "GLO", "Global", "FinEx Hosts"),
                     value = ifelse(grepl("Emissions", variable),
                                    value * 1e-3,
                                    value)),
            linewidth = 1.2,
            alpha = 0.85) +
  geom_line(mapping = aes(x = period,
                          y = value,
                          color = factor(scenario)),
            data = q_budget %>%
              filter(variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs, v_seel_VRE) &
                       region %in% c("GLO", "FNX") &
                       # region == jj &
                       period %in% ttot) %>%
              mutate(variable = ifelse(grepl("Coal", variable),
                                       "Coal-Fired Electricity", 
                                       ifelse(grepl("Gas", variable),
                                              "Gas-Fired Electricity",
                                              "Solar & Wind Electricity")),
                     region = ifelse(region == "GLO", "Global", "FinEx Hosts"),
                     value = ifelse(grepl("Emissions", variable),
                                    value * 1e-3,
                                    value)),
            linewidth = 1.2,
            alpha = 0.85,
            linetype = 3) + 
    facet_grid(variable ~ region, scales = "free") +
    scale_y_continuous(name = "Electricity Production (EJ/yr)"
                       # ,
                       # sec.axis = sec_axis(trans = ~., 
                       #                     name = "GHG Emissions (MtCO2eq/yr)")
                       ) +
  theme_bw() +
  scale_color_manual(values = scen_clrs,
                     breaks = rev(names(scen_clrs)),
                     labels = rev(names(scen_clrs)),
                     name = "Scenario") +
  expand_limits(y = 0) +
  labs(x = "") +
  scale_linetype_manual(values = c("REMIND"=3,"REMIND-COALogit"=1), 
                        labels = c("REMIND", "REMIND-COALogit"),
                        name = "Model") +
  theme(
    plot.title = element_text(size = 0.63 *15, face = "bold"),
    axis.text = element_text(size = 0.63 *14),
    axis.title = element_text(size = 0.63 *14),
    legend.text = element_text(size = 0.63 *14),
    legend.title = element_text(size = 0.63 *14),
    legend.position = "right",
    # axis.text.x = element_text(angle = 45)
  )

  ggsave(filename = paste0("~/FinEx/output/plots/thermal_exit_line/GLO_FNX_coal_gas_VRE_2050.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  ggsave(filename = paste0("~/FinEx/output/plots/thermal_exit_line/GLO_FNX_coal_gas_VRE_2050.pdf"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  
  ################################
  ### CHINA & INDIA COAL & GAS ###
  ################################
  ggplot() +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario),
                            linetype=model),
              data = q_frozen_pol %>%
                filter(variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs) &
                         region %in% c("CHA", "IND") &
                         # !grepl("")
                         period %in% ttot,
                       scenario != "REdirect") %>%
                mutate(variable = ifelse(grepl("Coal", variable),
                                         "Coal-Fired Electricity", 
                                         ifelse(grepl("Gas", variable),
                                                "Gas-Fired Electricity",
                                                "Power Sector Emissions")),
                       region = ifelse(region == "CHA", "China", "India"),
                       value = ifelse(grepl("Emissions", variable),
                                      value * 1e-3,
                                      value)),
              linewidth = 1.2)  +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario),
                            linetype = model),
              data = q_dpe_pol %>%
                filter(scenario != "REdirect" &
                         variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs) &
                         region %in% c("CHA", "IND") &
                         period %in% ttot) %>%
                # mutate(model = paste0(model,"-COALogit")) %>%
                mutate(variable = ifelse(grepl("Coal", variable),
                                         "Coal-Fired Electricity", 
                                         ifelse(grepl("Gas", variable),
                                                "Gas-Fired Electricity",
                                                "Power Sector Emissions")),
                       region = ifelse(region == "CHA", "China", "India"),
                       value = ifelse(grepl("Emissions", variable),
                                      value * 1e-3,
                                      value)),
              size = 1.2) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              data = q_budget %>%
                filter(variable %in% c(v_seel_coal_noccs, v_seel_gas_noccs) &
                         region %in% c("CHA", "IND") &
                         # region == jj &
                         period %in% ttot) %>%
                mutate(variable = ifelse(grepl("Coal", variable),
                                         "Coal-Fired Electricity", 
                                         ifelse(grepl("Gas", variable),
                                                "Gas-Fired Electricity",
                                                "Power Sector Emissions")),
                       region = ifelse(region == "CHA", "China", "India"),
                       value = ifelse(grepl("Emissions", variable),
                                      value * 1e-3,
                                      value)),
              size = 1.2,
              linetype = 3) + 
    facet_wrap(region ~ variable, scales = "free") +
    scale_y_continuous(name = "Electricity Production (EJ/yr)"
                       # ,
                       # sec.axis = sec_axis(trans = ~., 
                       #                     name = "GHG Emissions (MtCO2eq/yr)")
    ) +
    theme_bw() +
    scale_color_manual(values = scen_clrs,
                       breaks = rev(names(scen_clrs)),
                       labels = rev(names(scen_clrs)),
                       name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "") +
    scale_linetype_manual(values = c("REMIND"=3,"REMIND-COALogit"=1), 
                          labels = c("REMIND", "REMIND-COALogit"),
                          name = "Model") +
    theme(
      plot.title = element_text(size = 0.63 *15, face = "bold"),
      axis.text = element_text(size = 0.63 *14),
      axis.title = element_text(size = 0.63 *14),
      legend.text = element_text(size = 0.63 *14),
      legend.title = element_text(size = 0.63 *14),
      legend.position = "right",
      # axis.text.x = element_text(angle = 45)
    )
  
  ggsave(filename = paste0("~/FinEx/output/plots/thermal_exit_line/CHA_IND_coal_gas_2050.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  ggsave(filename = paste0("~/FinEx/output/plots/thermal_exit_line/CHA_IND_coal_gas_2050.pdf"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  
  
  
  
  #################################
  ### REGIONAL GAS TRAJECTORIES ###
  #################################
  ggplot() +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario),
                            linetype=model),
              data = q_ppca_ref %>%
                filter(variable %in% c(v_seel_gas_noccs) &
                         scenario!="REdirect" &
                         !(region %in% c("GLO", "FNX")) &
                         # region == jj &
                         period %in% ttot) %>%
                mutate(variable = ifelse(grepl("Coal", variable),
                                         "Coal-Fired Electricity", 
                                         ifelse(grepl("Gas", variable),
                                                "Gas-Fired Electricity",
                                                "Power Sector Emissions")),
                       # region = ifelse(region == "GLO", "Global", "FinEx Hosts"),
                       value = ifelse(grepl("Emissions", variable),
                                      value * 1e-3,
                                      value)),
              linewidth = 1.2)  +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario),
                            linetype = model),
              data = q_dpe_pol %>%
                filter(!grepl("public", scenario) &
                         scenario!="REdirect" &
                         variable %in% c(v_seel_gas_noccs) &
                         !(region %in% c("GLO", "FNX")) &
                         period %in% ttot) %>%
                # mutate(model = paste0(model,"-COALogit")) %>%
                mutate(variable = ifelse(grepl("Coal", variable),
                                         "Coal-Fired Electricity", 
                                         ifelse(grepl("Gas", variable),
                                                "Gas-Fired Electricity",
                                                "Power Sector Emissions")),
                       # region = ifelse(region == "GLO", "Global", "FinEx Hosts"),
                       value = ifelse(grepl("Emissions", variable),
                                      value * 1e-3,
                                      value)),
              size = 1.2) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              data = q_budget %>%
                filter(variable %in% c(v_seel_gas_noccs) &
                         !(region %in% c("GLO", "FNX")) &
                         # region == jj &
                         period %in% ttot) %>%
                mutate(variable = ifelse(grepl("Coal", variable),
                                         "Coal-Fired Electricity", 
                                         ifelse(grepl("Gas", variable),
                                                "Gas-Fired Electricity",
                                                "Power Sector Emissions")),
                       # region = ifelse(region == "GLO", "Global", "FinEx Hosts"),
                       value = ifelse(grepl("Emissions", variable),
                                      value * 1e-3,
                                      value)),
              size = 1.2,
              linetype = 3) + 
    facet_wrap(. ~ factor(region,
                          levels = c(regOrd[-1], 'CHA', 'IND', 'USA', 'JPN', 'EUR'),
                          labels = c(paste0(regOrder[-1],'*'), 'China', 'India', 'USA', 'Japan', 'EU & UK')),
               scales = 'free' ) +
    scale_y_continuous(name = "Gas-Fired Electricity Production (EJ/yr)"
                       # ,
                       # sec.axis = sec_axis(trans = ~., 
                       #                     name = "GHG Emissions (MtCO2eq/yr)")
    ) +
    theme_bw() +
    scale_color_manual(values = scen_clrs,
                       breaks = rev(names(scen_clrs)),
                       labels = rev(names(scen_clrs)),
                       name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "") +
    scale_linetype_manual(values = c("REMIND"=3,"REMIND-COALogit"=1), 
                          labels = c("REMIND", "REMIND-COALogit"),
                          name = "Model") +
    guides(color = guide_legend(title.position = "top"),
           linetype = guide_legend(title.position = "top", direction = "vertical")) +
    theme(
      plot.title = element_text(size = 0.63 *15, face = "bold"),
      axis.text = element_text(size = 0.63 *14),
      axis.title = element_text(size = 0.63 *14),
      legend.text = element_text(size = 0.63 *13),
      legend.title = element_text(size = 0.63 *14, face = "bold"),
      legend.title.align = 0.5,
      legend.position = "bottom",
      # axis.text.x = element_text(angle = 45)
    )
  
  ggsave(filename = paste0("~/FinEx/output/plots/gas_exit_line/all_regi_gas_2050.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  ggsave(filename = paste0("~/FinEx/output/plots/gas_exit_line/all_regi_gas_2050.pdf"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  
####################
### SCEN BY SCEN ###
####################

########################
#### LINE PLOT COAL ####
########################
for (jj in c("GLO", "OAS", "FNX")) {
  if (jj == "OAS") {  nm_reg <- " S & E Asia (ex. CN, IN, JP)"
  }else if (jj == "GLO") { nm_reg <- "Global"
  }else if (jj == "FNX") { nm_reg <- "FinEx Hosts"
  }
  
  scen_brks <- as.character(levels(q_ppca_ref$scenario))
  
  ppca_glo_coal_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_coal_noccs &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario),
                            linetype=model),
              linewidth = 1.2) +
    theme_bw() +
    scale_color_manual(values = scen_clrs,
                       breaks = names(scen_clrs),
                       name = "Scenario") +
    expand_limits(y = 0) +
    # facet_grid(.)
    labs(x = "", 
         y = "Coal Power Generation (EJ/yr)",
         title = nm_reg
         # title = paste("Static Policy Evaluation -", nm_reg)
         ) +
    scale_linetype_manual(values = c("REMIND"=3,"REMIND-COALogit"=1), 
                          labels = c("REMIND", "REMIND-COALogit"),
                          name = "Model") +
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
  
  ggsave(ppca_glo_coal_path_2050, filename = paste0("~/FinEx/output/plots/coal_exit_line/",jj,"_static_seel_coal_ppca_only_2050_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  # for (ii in as.character(levels(q_frozen_pol$scenario))) {
  #   scen_brks <- c(scen_brks, as.character(ii))
  # 
  #   ppca_glo_coal_path_2050 <-
  #     ppca_glo_coal_path_2050 +
  #     geom_line(mapping = aes(x = period,
  #                             y = value,
  #                             color = factor(scenario),
  #                             linetype = model),
  #               data = q_frozen_pol %>%
  #                 filter(scenario == ii &
  #                          variable == v_seel_coal_noccs &
  #                          region == jj &
  #                          period %in% ttot),
  #               size = 1.2) +
  #     scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
  #     scale_linetype_manual(values = c(3,1),
  #                           labels = c("REMIND", "REMIND-COALogit"),
  #                           name = "Model",
  #                           guide = "none")
  # 
  #   ggsave(ppca_glo_coal_path_2050, filename = paste0("~/FinEx/output/plots/coal_exit_line/",jj,"_static_seel_coal_2050_pol_",ii,"_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")

  # }
  
  for (ii in as.character(levels(q_dpe_pol$scenario)[-length(levels(q_dpe_pol$scenario))])) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_coal_path_2050 <-
      ppca_glo_coal_path_2050 +
      # ggplot() +
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario),
                              linetype = model),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_coal_noccs &
                           region == jj &
                           period %in% ttot) %>%
                  mutate(model = paste0(model,"-COALogit")),
                size = 1.2) +
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_budget %>%
                  filter(variable == v_seel_coal_noccs &
                           # region %in% c("GLO",host_regi) &
                           region == jj &
                           period %in% ttot),
                size = 1.2,
                linetype = 3) + 
      # scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
      # scale_linetype_manual(values = c(3,1), 
      #                       labels = c("REMIND", "REMIND-COALogit"),
      #                       name = "Model") +
      labs(x = "",
           y = "Coal Power Generation (EJ/yr)")
      # scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")

    ggsave(ppca_glo_coal_path_2050, filename = paste0("~/FinEx/output/plots/coal_exit_line/",jj,"_seel_coal_2050_pol_",ii,"_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
}

  ########################
  #### LINE PLOT GAS ####
  ########################
  scen_brks <- c(as.character(levels(q_ppca_ref$scenario))
            #      , 
            # as.character(levels(q_ppca_preFinEx$scenario))
            )
  
  ppca_glo_gas_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel_gas_noccs &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2,
              linetype = 3) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              data = q_budget %>%
                filter(variable == v_seel_coal_noccs &
                         # region %in% c("GLO",host_regi) &
                         region == jj &
                         period %in% ttot),
              size = 1.2,
              linetype = 3) + 
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "Gas-fired electricity generation EJ/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg, "Gas Power ")) +
    theme(
      plot.title = element_blank(),
      # plot.title = element_text(size = 0.75 *15, face = "bold"),
      axis.text = element_text(size = 0.75 *14),
      axis.title = element_text(size = 0.75 *14),
      legend.text = element_text(size = 0.75 *14),
      legend.title = element_text(size = 0.75 *14),  legend.position = "right"
    )
  # +
  #   facet_wrap(. ~ region, scales = "free")
  
  ggsave(ppca_glo_gas_path_2050, filename = paste0("~/FinEx/output/plots/gas_exit_line/",jj,"_seel_gas_ppca_only_2050_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
  for (ii in as.character(levels(q_dpe_pol$scenario))) {
    scen_brks <- c(scen_brks, as.character(ii))
    
    ppca_glo_gas_path_2050 <-
      ppca_glo_gas_path_2050 + 
      geom_line(mapping = aes(x = period,
                              y = value,
                              color = factor(scenario)),
                data = q_dpe_pol %>%
                  filter(scenario == ii &
                           variable == v_seel_gas_noccs &
                           region == jj &
                           period %in% ttot),
                size = 1.2) +
      scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario")
    
    ggsave(ppca_glo_gas_path_2050, filename = paste0("~/FinEx/output/plots/gas_exit_line/",jj,"_seel_gas_2050_pol_",ii,"_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  
  
  ########################
  #### LINE PLOT VRE ####
  ########################
  scen_brks <- c(as.character(levels(q_ppca_ref$scenario)), 
                 as.character(levels(q_budget$scenario))
            #      , 
            # as.character(levels(q_ppca_preFinEx$scenario))
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
              size = 1.2,
              linetype = 3) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg, "VRE")) +
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
  
  ggsave(ppca_glo_VRE_path_2050, filename = paste0("~/FinEx/output/plots/VRE_line/",jj,"_seel_VRE_ppca_budget_2050_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
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
    
    ggsave(ppca_glo_VRE_path_2050, filename = paste0("~/FinEx/output/plots/VRE_line/",jj,"_seel_VRE_2050_pol_",ii,"_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT SEEL ####
  ########################
  scen_brks <- c(as.character(levels(q_ppca_ref$scenario)), 
                            as.character(levels(q_budget$scenario)))
  
  ppca_glo_seel_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_seel &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2,
              linetype = 3) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "EJ/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg, "Total Electricity Demand")) +
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
  
  ggsave(ppca_glo_seel_path_2050, filename = paste0("~/FinEx/output/plots/seel_line/",jj,"_seel_ppca_budget_2050_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
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
    
    ggsave(ppca_glo_seel_path_2050, filename = paste0("~/FinEx/output/plots/seel_line/",jj,"_seel_2050_pol_",ii,"_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
  
  
  ########################
  #### LINE PLOT EMI ####
  ########################
  scen_brks <- c(as.character(levels(q_ppca_ref$scenario)), 
                 as.character(levels(q_budget$scenario)))
  
  ppca_glo_emi_seel_path_2050 <- 
    ggplot(q_ppca_ref %>%
             filter(variable == v_emi_seel &
                      # region %in% c("GLO",host_regi) &
                      region == jj &
                      period %in% ttot)) +
    geom_line(mapping = aes(x = period,
                            y = value,
                            color = factor(scenario)),
              size = 1.2,
              linetype = 3) +
    theme_bw() +
    scale_color_manual(values = scen_clrs, breaks = scen_brks, name = "Scenario") +
    expand_limits(y = 0) +
    labs(x = "Period", 
         y = "MtCO2/yr",
         title = paste("Dynamic Policy Evaluation -", nm_reg, "Power Sector Emissions")) +
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
  
  ggsave(ppca_glo_emi_seel_path_2050, filename = paste0("~/FinEx/output/plots/emi_seel_line/",jj,"_emi_seel_ppca_budget_2050_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
  
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
    
    ggsave(ppca_glo_emi_seel_path_2050, filename = paste0("~/FinEx/output/plots/emi_seel_line/",jj,"_emi_seel_2050_pol_",ii,"_v5.png"),dpi = "retina", height = 5.75, width = 7.5, units = "in")
    
  }
}

