q_host_deltacap_fos <- q_dpe_pol %>%
  bind_rows(q_budget %>% 
              mutate(scenario = as.character(scenario))) %>%
  mutate(value = value * 5) %>%
  filter(variable %in% v_deltacap_fos &
           period >= 2025 &
           period <= 2050 &
           period %% 5 == 0 &
           (region == "FNX" | region == "GLO")
         )


q_ref_deltacap_fos <- q_dpe_pol %>%
  mutate(value = value * 5) %>%
  rename(redir_val = value) %>%
  filter(variable %in% v_deltacap_fos &
           period >= 2025 &
           period <= 2050 &
           period %% 5 == 0 &
           (region == "FNX" | region == "GLO") &
           scenario == "REdirect")

q_static_ref_deltacap_fos <- q_ppca_ref %>%
  mutate(value = value * 5) %>%
  rename(ppca_val = value) %>%
  filter(variable %in% v_deltacap_fos &
           period >= 2025 &
           period <= 2050 &
           period %% 5 == 0 &
           (region == "FNX" | region == "GLO") &
           grepl("PPCA", scenario))


q_diff_deltacap_fos <- q_host_deltacap_fos %>%
  left_join(q_ref_deltacap_fos, by = c("variable", "period", "model", "region", "unit")) %>%
  left_join(q_static_ref_deltacap_fos, by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(diff_ppca = value - ppca_val,
         diff_redir = value - redir_val) %>%
  group_by(region, variable, scenario) %>%
  mutate(cumdiff_ppca = cumsum(value - ppca_val),
         cumdiff_redir = cumsum(value - redir_val))

View(q_diff_deltacap_fos)

View(q_diff_deltacap_fos %>%
       group_by(region, variable, scenario.x) %>% 
       summarise(cumval = cumsum(value),
                 cumdiff_redir = cumsum(value - redir_val),
                 cumdiff_ppca = cumsum(value - ppca_val),
                 .groups = "keep"))


q_host_deltacap_fos %>% group_by(variable, scenario) %>%
  summarise(sum(value), .groups = 'keep')


q_static_ref_deltacap_fos %>% group_by(variable, scenario) %>%
  summarise(sum(ref_val), .groups = 'keep')





##############################################################################################################
################################### RENEWABLES ############################################################### 
##############################################################################################################

q_host_deltacap_re <- q_dpe_pol %>%
  bind_rows(q_budget %>% 
              mutate(scenario = as.character(scenario))) %>%
  filter(variable %in% v_deltacap_re &
           period >= 2025 &
           period <= 2050 &
           period %% 5 == 0 
         # &
         #   (region == "FNX" | region == "GLO")
  )


q_ref_deltacap_re <- q_dpe_pol %>%
  rename(redir_val = value) %>%
  filter(variable %in% v_deltacap_re &
           period >= 2025 &
           period <= 2050 &
           period %% 5 == 0 &
           # (region == "FNX" | region == "GLO") &
           scenario == "REdirect")

q_static_ref_deltacap_re <- q_ppca_ref %>%
  rename(ppca_val = value) %>%
  filter(variable %in% v_deltacap_re &
           period >= 2025 &
           period <= 2050 &
           period %% 5 == 0 &
           # (region == "FNX" | region == "GLO") &
           grepl("PPCA", scenario))


q_diff_deltacap_re <- q_host_deltacap_re %>%
  left_join(q_ref_deltacap_re, by = c("variable", "period", "model", "region", "unit")) %>%
  left_join(q_static_ref_deltacap_re, by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(diff_ppca = value - ppca_val,
         diff_redir = value - redir_val) %>%
  group_by(region, variable, scenario) %>%
  mutate(cumdiff_ppca = cumsum(value - ppca_val),
         cumdiff_redir = cumsum(value - redir_val))

View(q_diff_deltacap_re)

View(q_diff_deltacap_re %>%
       group_by(region, variable, scenario.x) %>% 
       summarise(cumval = cumsum(value),
                 cumdiff_redir = cumsum(value - redir_val),
                 cumdiff_ppca = cumsum(value - ppca_val),
                 .groups = "keep"))


q_host_deltacap_re %>% group_by(variable, scenario) %>%
  summarise(sum(value), .groups = 'keep')


q_static_ref_deltacap_re %>% group_by(variable, scenario) %>%
  summarise(sum(ref_val), .groups = 'keep')
