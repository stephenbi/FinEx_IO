require(mrremind)
require(mip)
require(quitte)
require(dplyr)
require(ggplot2)
require(RColorBrewer)
require(forcats)
require(countrycode)
require(scales)

##############
#### MIFs ####
##############

frozen <- mbind(read.report(list.files("H:/FinEx/runs/",pattern = "PPCAcop26-jul21", full.names = T), as.list = F),
                read.report(list.files("H:/FinEx/runs/",pattern = "jul21-50p-PPCAcop26", full.names = T), as.list = F))
                # read.report(file = "H:/FinEx/runs/REMIND_generic_REdirect_HI-cond-PPCAcop26-2025-only-jul21-50p_withoutPlus.mif", as.list = F))
frozen <- toolFillYears(frozen, years = 2020:2100)
q_frozen <- as.quitte(frozen)


dpe_full <- mbind(
  read.report(list.files("H:/FinEx/runs/",pattern = "jul21-50p-nonoecd_",full.names = T), as.list = F),
  read.report(file = "H:/FinEx/runs/REMIND_generic_PPCA-jul21-nonoecd_withoutPlus.mif", as.list = F)
  # read.report(file = "H:/FinEx/runs/REMIND_generic_REdirect_coopex-2030-jul21-50p-oecd_withoutPlus.mif", as.list = F)
  # read.report(file = "H:/FinEx/runs/REMIND_generic_REdirect_coopex-2030-50p-jul21-nonoecd_withoutPlus.mif", as.list = F)
)
dpe_full <- toolFillYears(dpe_full, years = 2020:2100)


# dpe_40p <- mbind(read.report(list.files("H:/FinEx/runs/",pattern = "jul21-40p-nonoecd",full.names = T), as.list = F),
#                   read.report(file = "H:/FinEx/runs/REMIND_generic_PPCA-jul21-nonoecd_withoutPlus.mif", as.list = F),
#                   read.report(file = "H:/FinEx/runs/REMIND_generic_REdirect_coopex-2030-40p-jul21-nonoecd_withoutPlus.mif", as.list = F))
# dpe_40p <- toolFillYears(dpe_40p, years = 2020:2100)
# q_dpe_40p <- as.quitte(dpe_40p) %>% filter(!grepl("nonoecd1", scenario))

dpe_full_2step <- mbind(
  read.report(list.files("H:/FinEx/runs/",pattern = "jul21-50p-nonoecd-2050",full.names = T), as.list = F),
  read.report(file = "H:/FinEx/runs/REMIND_generic_PPCA-jul21-nonoecd-2050_withoutPlus.mif", as.list = F)
  # read.report(file = "H:/FinEx/runs/REMIND_generic_REdirect_coopex-2030-50p-jul21-nonoecd_withoutPlus.mif", as.list = F)
)
dpe_full_2step <- toolFillYears(dpe_full_2step, years = 2020:2100)


budget <- read.report(list.files("H:/FinEx/runs/",pattern = "PkBud",full.names = T), as.list = F)
budget <- toolFillYears(budget, years = 2020:2100)

q_budget <- as.quitte(budget) %>% 
  mutate(scenario = factor(scenario,
                           levels = c("SSP2EU-PkBudg1150", "SSP2EU-PkBudg500"),
                           labels = c("2\u00B0C", "1.5\u00B0C")))

q_budget <- q_budget %>% bind_rows(
    q_budget %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) 

# levels(q_budget$scenario) <- c("2\u00B0C", "1.5\u00B0C")


########################
#### TIME & REGIONS ####
########################
ttot <- seq(2020,2050,5)
tall <- getYears(frozen,as.integer = T)[which(getYears(frozen,as.integer = T) >= 2020 & getYears(frozen,as.integer = T) <= 2100)]

host_regi <- c("OAS", "NEU", "SSA", "MEA", "LAM", "CAZ", "REF") 

###############
#### SCENS ####
###############
q_frozen <- q_frozen %>% mutate(scenario = factor(scenario,
                            levels = c("PPCAcop26-jul21", 
                                       "FinEx-jul21-50p-PPCAcop26", 
                                       "REdirect_nomob-jul21-50p-PPCAcop26", 
                                       # "REdirect_LO-jul21-50p-PPCAcop26", 
                                       "REdirect-loan-jul21-50p-PPCAcop26", 
                                       # "REdirect_HI-jul21-50p-PPCAcop26", 
                                       # "REdirect_HI-cond-jul21-50p-PPCAcop26", 
                                       "REdirect_coopex-2030-jul21-50p-PPCAcop26"),
                            labels = c("PPCA-observed", 
                                       "FinEx", 
                                       "REdirect", 
                                       # "REdirect-lo", 
                                       "REdirect-loan", 
                                       # "REdirect-hi", 
                                       # "REdirect-hiCon", 
                                       "REdirect-grant"),
                            ordered = T))

q_ppca_ref <- q_frozen %>%
  bind_rows(
    q_frozen %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) %>% 
  filter(scenario=="PPCA-observed")

q_frozen_pol <- q_frozen %>%
  bind_rows(
    q_frozen %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX")
  ) %>% 
  filter(period %in% tall 
         # &
         #   !(scenario %in% c("PPCA-observed"
  )


q_dpe_full <- as.quitte(dpe_full) %>% filter(variable %in% vars & !grepl("FinEx-jul|lobd_REfx", scenario))

q_dpe_full <- q_dpe_full %>% mutate(scenario = factor(scenario,
                              levels = c("PPCA-jul21-nonoecd", 
                                         "FinEx-noLObd-jul21-50p-nonoecd", 
                                         # "FinEx-jul21-50p-nonoecd", 
                                         "REdirect_nomob-jul21-50p-nonoecd", 
                                         # "REdirect_LO-jul21-50p-nonoecd", 
                                         "REdirect-loan-jul21-50p-nonoecd", 
                                         # "REdirect_HI-jul21-50p-nonoecd", 
                                         # "REdirect_HI-cond-jul21-50p-nonoecd", 
                                         # "REdirect_HI-cond-2025-only-jul21-50p-nonoecd1",
                                         # "REdirect_HI-cond-2025-only-jul21-50p-nonoecd2",
                                         # "REdirect_coopex-2030-jul21-50p-nonoecd"
                                         "REdirect_coopex-2030-jul21-50p-nonoecd"
                              ),
                              labels = c("PPCA-growth", 
                                         "FinEx", 
                                         "REdirect", 
                                         # "REdirect-lo", 
                                         "REdirect-loan", 
                                         # "REdirect-hi", 
                                         # "REdirect-hiCon", 
                                         # "REdirect-grant",
                                         "REdirect-grant"
                                         # "REdirect_coopex2050"
                                         ),
                              ordered = T))

q_ppca_growth <- q_dpe_full %>%
  mutate(model = paste0(model,"-COALogit")) %>%
  bind_rows(
    q_dpe_full %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX",
             model = paste0(model,"-COALogit"))
  ) %>% 
  filter(scenario == "PPCA-growth")


q_dpe_pol <- q_dpe_full %>%
  mutate(model = paste0(model,"-COALogit")) %>%
  bind_rows(
    q_dpe_full %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX") %>%
      mutate(model = paste0(model,"-COALogit"))
  ) 


scen_clrs <- setNames(brewer.pal(name = "Dark2", n = 8)[-c(2,5)],
                      nm = rev(c("Current (2019)", levels(q_dpe_pol$scenario))))

scen_clrs <- c(scen_clrs,
               setNames(c("steelblue1", "seagreen1", "grey50"),
                        nm = c(unique(q_budget$scenario), unique(q_ppca_ref$scenario))))

RColorBrewer::brewer.pal(n=8,"Greens") -> greens
RColorBrewer::brewer.pal(n=8,"Blues") -> blues
bluegreen <- brewer.pal(n=11,"BrBG")
RColorBrewer::brewer.pal(n=8,"Accent") -> accent
RColorBrewer::brewer.pal(n=12,"Paired") -> paired

# scen_clrs <- c(scen_clrs,
#                setNames(brewer.pal(name = "Dark2", n = length(unique(q_frozen_pol$scenario))),
#                         nm = c(unique(q_frozen_pol$scenario))))


q_dpe_full_2step <- as.quitte(dpe_full_2step) %>% filter(variable %in% vars & !grepl("FinEx-jul|lobd_REfx", scenario))

q_dpe_full_2step <- q_dpe_full_2step %>% mutate(scenario = factor(scenario,
                                                      levels = c("PPCA-jul21-nonoecd-2050", 
                                                                 "FinEx-noLObd-jul21-50p-nonoecd-2050", 
                                                                 "REdirect_nomob-jul21-50p-nonoecd-2050", 
                                                                 # "REdirect_LO-jul21-50p-nonoecd-2050", 
                                                                 "REdirect-loan-jul21-50p-nonoecd-2050", 
                                                                 # "REdirect_HI-jul21-50p-nonoecd-2050", 
                                                                 # "REdirect_HI-cond-jul21-50p-nonoecd-2050", 
                                                                 # "REdirect_HI-cond-2025-only-jul21-50p-nonoecd-20501",
                                                                 # "REdirect_HI-cond-2025-only-jul21-50p-nonoecd-20502",
                                                                 "REdirect_coopex-2030-jul21-50p-nonoecd-2050"
                                                      ),
                                                      labels = c("PPCA-growth", 
                                                                 "FinEx", 
                                                                 "REdirect", 
                                                                 # "REdirect-lo", 
                                                                 "REdirect-loan", 
                                                                 # "REdirect-hi", 
                                                                 # "REdirect-hiCon", 
                                                                 # "REdirect-grant",
                                                                 "REdirect-grant"
                                                                 # "REdirect_coopex2050"
                                                      ),
                                                      ordered = T))

q_ppca_growth_2step <- q_dpe_full_2step %>%
  mutate(model = paste0(model,"-COALogit")) %>%
  bind_rows(
    q_dpe_full_2step %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX",
             model = paste0(model,"-COALogit"))
  ) %>% 
  filter(scenario == "PPCA-growth")


q_dpe_pol_2step <- q_dpe_full_2step %>%
  mutate(model = paste0(model,"-COALogit")) %>%
  bind_rows(
    q_dpe_full_2step %>% 
      filter(region %in% host_regi) %>% 
      group_by(model, scenario, variable, unit, period) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "FNX") %>%
      mutate(model = paste0(model,"-COALogit"))
  ) 


scen_clrs_2step <- setNames(brewer.pal(name = "Set2", n = 8)[-c(2,5)],
                      nm = rev(c("Current (2019)", levels(q_dpe_pol_2step$scenario))))

scen_clrs_2step <- c(scen_clrs,
               setNames(c("steelblue1", "seagreen1", "grey50"),
                        nm = c(unique(q_budget$scenario), unique(q_ppca_ref$scenario))))

######################
###### MAPPINGS ######
######################

map <- toolGetMapping("regionmappingH12.csv",type="regional")

#######################
###### VAR NAMES ######
#######################

v_seel <- "SE|Electricity"

v_seel_all <- c("SE|Electricity|Coal",
              "SE|Electricity|Oil",
              "SE|Electricity|Gas",
              "SE|Electricity|Biomass",
              "SE|Electricity|Nuclear",
              "SE|Electricity|Hydro",
              "SE|Electricity|Solar",
              "SE|Electricity|Wind",
              "SE|Electricity|Geothermal",
              "SE|Electricity|Hydrogen")
# v_seel <- paste(v_seel,"(EJ/yr)")

v_seel_gas <- "SE|Electricity|Gas"
v_seel_gas_noccs <- "SE|Electricity|Gas|w/o CC"
v_seel_coal <- "SE|Electricity|Coal"
v_seel_coal_noccs <- "SE|Electricity|Coal|w/o CC"
v_seel_VRE <- "SE|Electricity|WindSolar"
v_seel_spv <- "SE|Electricity|Solar"
v_seel_wind <- "SE|Electricity|Wind"
v_elshare <- "FE|Electricity|Share"

v_grid_inv <- "Energy Investments|Elec|Grid|VRE support"
v_inv_stor <- "Energy Investments|Elec|Storage"

v_emi_seel <- "Emi|CO2|Energy|Supply|Electricity w/ couple prod"
v_emi_nonel <- "Emi|CO2|Energy|Supply|Non-electric"

v_ghg_seel <- "Emi|GHG|Gross|Energy|Supply|Electricity"
v_ghg_nonel <- "Emi|GHG|Energy|Supply|Non-electric"
v_tot_emi <- "Emi|GHG"

v_cumemi_seel <- "Emi|GHG|Cumulated|Gross|Energy|Supply|Electricity"
v_cumemi_nonel <- "Emi|GHG|Cumulated|Gross|Energy|Supply|Non-Electric"

# INVESTMENTS #
v_inv_REdir <- c("Energy Investments|Elec|Solar",
                 "Energy Investments|Elec|Wind",
                 "Energy Investments|Elec|Grid|VRE support",
                 "Energy Investments|Elec|Storage")

v_inv_fos <- c("Energy Investments|Elec|Coal|w/o CC", "Energy Investments|Elec|Gas|w/o CC")

v_deltacap_fos <- c("New Cap|Electricity|Coal|w/o CC", "New Cap|Electricity|Gas|w/o CC")

v_deltacap_re <- c("New Cap|Electricity|Solar", 
                   "New Cap|Electricity|Wind",
                   "New Cap|Electricity|Storage|Battery",
                   "New Cap|Electricity|Grid|VRE support")

v_cap_re <- c("Cap|Electricity|Solar", 
                   "Cap|Electricity|Wind",
                   "Cap|Electricity|Storage|Battery",
                   "Cap|Electricity|Grid|VRE support")

v_cap_fos <- c("Cap|Electricity|Coal", 
              "Cap|Electricity|Gas")
              
v_inco_re <- c("Tech|Electricity|Solar|PV|Capital Costs",
               "Tech|Electricity|Wind|Onshore|Capital Costs",
               "Tech|Electricity|Wind|Offshore|Capital Costs",
               "Tech|Electricity|Storage|Battery|For Wind Onshore|Capital Costs",
               "Tech|Electricity|Storage|Battery|For Wind Offshore|Capital Costs",
               "Tech|Electricity|Storage|Battery|For PV|Capital Costs"
)

v_inco_re_adj <- c("Tech|Electricity|Solar|PV|Capital Costs|w/ Adj Costs",
               "Tech|Electricity|Wind|Onshore|Capital Costs|w/ Adj Costs",
               "Tech|Electricity|Wind|Offshore|Capital Costs|w/ Adj Costs",
               "Tech|Electricity|Storage|Battery|For Wind Onshore|Capital Costs|w/ Adj Costs",
               "Tech|Electricity|Storage|Battery|For Wind Offshore|Capital Costs|w/ Adj Costs",
               "Tech|Electricity|Storage|Battery|For PV|Capital Costs|w/ Adj Costs"
)

v_inco_coal <- c("Tech|Electricity|Coal|Pulverised Coal w/o CC|Capital Costs|w/ Adj Costs",
               "Tech|Electricity|Coal|Gasification Combined Cycle w/o CC|Capital Costs|w/ Adj Costs",
               "Tech|Electricity|Coal|Combined Heat and Power w/o CC|Capital Costs|w/ Adj Costs"
)
v_taxrev <- "Revenue|Government|Tax|Carbon"

v_syscost <- "Energy system costs"

v_exports <- c("Trade|Exports|Coal",
               "Trade|Exports|Gas")

v_pe_prices <- c("Price|Coal|World Market",
                 "Price|Gas|World Market")

vars <- c(v_pe_prices, v_exports, v_syscost, v_taxrev, v_inco_re_adj, v_inco_re, v_seel_VRE,
          v_cap_fos, v_cap_re, v_deltacap_re, v_deltacap_fos, v_seel_all, v_seel,
          v_inv_fos, v_inv_REdir, v_emi_seel, v_emi_nonel, v_ghg_nonel, v_ghg_seel, v_inco_coal,
          v_cumemi_nonel, v_cumemi_seel, v_tot_emi, v_seel_coal_noccs, v_seel_gas_noccs)

inflation_05_to_19 <- 1.31


###############
### CONFIGS ###
###############
regOrd <- c("GLO","OAS", "SSA", "MEA", "NEU", "CAZ", "REF", "LAM") 
regOrder <- c("All FinEx Hosts","S & E Asia (exc. CN, IN, JP)","Sub-Saharan Africa","Middle East & N Africa","Non-EU Europe",
              "Canada, Australia & NZ","Former Soviet Union","Latin America")
names(regOrder) = regOrd

regAlpha <- c("Canada, Australia & NZ", "China", "EU-27", "India", "Japan", "Latin America",
              "Middle East & N Africa", "Non-EU Europe", "S&E Asia ex. CN, IN, JP", "Former Soviet Union",
              "Sub-Saharan Africa", "United States"
              )

regFill <- c(greens[4], accent[2], blues[5], bluegreen[4], rev(brewer.pal(8,"Pastel2")), "white")
names(regFill) <- c(sort(unique(map$RegionCode)), "GLO")
show_col(regFill)
# regFill[which(names(regFill)=="OAS")] <- bluegreen[8]
# regFill[which(names(regFill)=="IND")] <- blues[6]

# 
# regFill <- factor(x = regFill,
#                   levels = names(regFill),
#                   labels = c("World",regAlpha))

stackOrd_finEx <- c("Pre-FinEx Projection",
                    "Post-FinEx Projection")

color_covid <- c("Neutral"="dodgerblue",
                 "Green"="forestgreen",
                 "Brown"="tan4",
                 "Full Completion"="gray60")

fillScale_cov <- scale_fill_manual(values = color_covid,
                                   labels=c("Neutral - Historical completion rate",
                                            "Green - 50% historical completion rate",
                                            "Brown - 50% historical cancellation rate",
                                            "Full pipeline completion"),
                                   breaks = levels(color_covid),
                                   name="Status")

alpha_finEx <- c("Post-FinEx Projection"=1,
                 "Pre-FinEx Projection"=0.5)

color_finEx <- c("Post-FinEx Projection"="grey15",
                 "Pre-FinEx Projection"="grey")

###############
#### SCENS ####
###############

# q_ppca_ref <- q_frozen %>% 
#   filter(scenario=="PPCA-observed" & period %in% tall)
# 
# q_ppca_preFinEx <- q_dpe_full %>%
#   filter(scenario == "PPCA-growth" & period %in% tall)
# 
# q_frozen_pol <- q_frozen %>%
#   filter(!(scenario %in% c("PPCA-observed", "REdirect-hiCon (PPCA-observed)")) & period %in% tall)
#   
# 
# q_dpe_pol <- q_dpe_full %>%
#   filter(!(scenario %in% c("PPCA-growth")) & period %in% tall)
# 
# scen_clrs <- setNames(brewer.pal(name = "Set3", n = length(c(unique(q_ppca_ref$scenario), unique(q_dpe_full$scenario), unique(q_frozen_pol$scenario)))),
#                       nm = c(unique(q_ppca_ref$scenario), unique(q_frozen_pol$scenario), unique(q_dpe_full$scenario)))
# 
# labs <- c(as.character(unique(q_ppca_ref$scenario)), 
#           as.character(unique(q_ppca_preFinEx$scenario)))





##############################
#### CALC POL-REF CUM DIF ####
##############################
# dpe_full <- as.magpie(q_dpe_full)
# frozen_pol <- as.magpie(q_frozen_pol)
# dpe_pol <- as.magpie(q_dpe_pol)
# ppca_ref <- as.magpie(q_ppca_ref)
# 
# dif_coal_hosts <- dimSums(dpe_pol[host_regi,tall,v_seel_coal], dim = 2) - dimSums(ppca_ref[host_regi,tall,v_seel_coal], dim = 2)
# dif_coal_hosts_tot <- dimSums(dif_coal_hosts, dim = 1)
# 
# dif_gas_hosts <- dimSums(dpe_pol[host_regi,tall,v_seel_gas], dim = 2) - dimSums(ppca_ref[host_regi,tall,v_seel_gas], dim = 2)
# dif_gas_hosts_tot <- dimSums(dif_gas_hosts, dim = 1)
# 
# dif_emi_seel_hosts <- dimSums(dpe_full[host_regi,tall,v_emi_seel], dim = 2) - dimSums(ppca_ref[host_regi,tall,v_emi_seel], dim = 2)
# dif_emi_seel_hosts_tot <- dimSums(dif_emi_seel_hosts, dim = 1)
# 
# dif_emi_nonel_hosts <- dimSums(dpe_full[host_regi,tall,v_emi_nonel], dim = 2) - dimSums(ppca_ref[host_regi,tall,v_emi_nonel], dim = 2)
# dif_emi_nonel_hosts_tot <- dimSums(dif_emi_nonel_hosts, dim = 1)

