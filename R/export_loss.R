
### COAL AND GAS EXPORTS ###
exports <- q_dpe_pol %>%
  ungroup() %>%
  filter(variable %in% v_exports & 
           period >= 2020 & period <= 2100 &
           scenario != "PPCA-growth") %>%
  rename(EJ = value) %>%
  # mutate(value = value * 5) %>%
  full_join(q_ppca_ref %>%
              ungroup() %>%
              filter(variable %in% v_exports & 
                       period >= 2020 & period <= 2100) %>%
              # mutate(EJ = EJ * 5) %>%
              rename(obs_EJ = value) %>%
              select(-c("model","scenario")),
            by = c("variable", "period", "region", "unit")) %>%
  full_join(q_ppca_growth %>%
              ungroup() %>%
              filter(variable %in% v_exports & 
                       period >= 2020 & period <= 2100) %>%
              # mutate(EJ = EJ * 5) %>%
              rename(gro_EJ = value) %>%
              select(-"scenario"),
            by = c("variable", "period", "model", "region", "unit")) %>%
  mutate(fuel = ifelse(grepl("Coal", variable), "coal", "gas")) %>%
  full_join(q_dpe_pol %>%
              ungroup() %>%
              filter(variable %in% v_pe_prices & 
                       period >= 2020 & period <= 2100 &
                       scenario != "PPCA-growth") %>%
              group_by(region, variable, scenario) %>%
              mutate(value = value * inflation_05_to_19) %>%
              rename(price = value) %>%
              mutate(fuel = ifelse(grepl("Coal", variable), "coal", "gas")) %>%
              ungroup() %>%
              select(-c("variable", "unit")),
            by = c("fuel", "scenario", "period", "model", "region")) %>%
  full_join(q_ppca_ref %>%
              ungroup() %>%
              filter(variable %in% v_pe_prices & 
                       period >= 2020 & period <= 2100) %>%
              mutate(value = value * inflation_05_to_19) %>%
              mutate(fuel = ifelse(grepl("Coal", variable), "coal", "gas")) %>%
              rename(obs_price = value) %>%
              select(-c("variable", "model", "scenario", "unit")),
            by = c("fuel", "period", "region")) %>%
  full_join(q_ppca_growth %>%
              ungroup() %>%
              filter(variable %in% v_pe_prices & 
                       period >= 2020 & period <= 2100) %>%
              mutate(value = value * inflation_05_to_19) %>%
              mutate(fuel = ifelse(grepl("Coal", variable), "coal", "gas")) %>%
              rename(gro_price = value) %>%
              select(-c("variable", "scenario", "unit")),
            by = c("fuel", "period", "model", "region")) %>%
  full_join(q_dpe_pol %>%
              filter(variable == "Interest Rate t/(t-1)|Real" & 
                       period >= 2020 & period <= 2100 &
                       scenario != "PPCA-growth") %>%
              group_by(region, variable, scenario) %>%
              rename(rate = value) %>%
              ungroup() %>%
              select(-c("variable", "model", "unit")),
            by = c("period", "scenario", "region")) %>%
  full_join(q_ppca_ref %>%
              ungroup() %>%
              filter(variable == "Interest Rate t/(t-1)|Real" & 
                       period >= 2020 & period <= 2100) %>%
              rename(obs_rate = value) %>%
              select(-c("variable", "model","scenario","unit")),
            by = c("period", "region")) %>%
  full_join(q_ppca_growth %>%
              ungroup() %>%
              filter(variable == "Interest Rate t/(t-1)|Real" & 
                       period >= 2020 & period <= 2100) %>%
              rename(gro_rate = value) %>%
              select(-c("variable", "model","scenario","unit")),
            by = c("period", "region")) %>%
  mutate(npv = EJ * price / ((1 + rate)^(period - 2023))) %>%
  mutate(obs_npv = obs_EJ * obs_price / ((1 + obs_rate)^(period - 2023))) %>%
  mutate(gro_npv = gro_EJ * gro_price / ((1 + gro_rate)^(period - 2023))) %>%
  mutate(diff_EJ_gro = EJ - gro_EJ) %>%
  mutate(diff_EJ_obs = EJ - obs_EJ) %>%
  mutate(diff_npv_gro = npv - gro_npv) %>%
  mutate(diff_npv_obs = npv - obs_npv) %>%
  group_by(region, scenario) %>%
  mutate(cum_diff_EJ_obs = cumsum(diff_EJ_obs)) %>%
  mutate(cum_diff_EJ_gro = cumsum(diff_EJ_gro)) %>%
  mutate(cum_diff_npv_obs = cumsum(diff_npv_obs)) %>%
  mutate(cum_diff_npv_gro = cumsum(diff_npv_gro)) %>%
  ungroup()



### COAL AND GAS PRICES ###
mkt_price <- q_dpe_pol %>%
  filter(variable %in% v_pe_prices & 
           period >= 2020 & period <= 2100 &
           scenario != "PPCA-growth") %>%
  group_by(region, variable, scenario) %>%
  mutate(value = value * inflation_05_to_19) %>%
  ungroup() %>%
  full_join(q_ppca_ref %>%
              filter(variable %in% v_pe_prices & 
                       period >= 2020 & period <= 2100) %>%
              mutate(value = value * inflation_05_to_19) %>%
              rename(obs_val = value) %>%
              select(-c("model","scenario")),
            by = c("variable", "period", "region", "unit")) %>%
  full_join(q_ppca_growth %>%
              filter(variable %in% v_pe_prices & 
                       period >= 2020 & period <= 2100) %>%
              mutate(value = value * inflation_05_to_19) %>%
              rename(gro_val = value) %>%
              select(-"scenario"),
            by = c("variable", "period", "model", "region", "unit"))
  

