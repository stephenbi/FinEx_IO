library(mip)
library(ggnewscale)
require(readxl)
require(mrremind)
require(quitte)
require(dplyr)
require(stringr)
require(ggplot2)
library(sf)
library(s2)
library(grid)
library(gridExtra)
library(rnaturalearth)
library(hrbrthemes)
library(tidyverse)
library(rgeocodio)
library(albersusa)
library(ggthemes)
library(ggrepel)


RColorBrewer::brewer.pal(n=8,"Accent") -> accent

map <- toolGetMapping("regionmappingH12.csv",type="regional")

# Developing Countries = Low and Lower Middle Income WDI class
map_wdi <- toolGetMapping("regionmappingWDI_income.csv", type = "regional")

hosts <- unique(c(map_wdi$CountryCode[grepl("Low", map_wdi$RegionCode)], 
                  unique(preCon_finFlows$Country)))

hosts <- hosts[-which(hosts %in% c("RUS", "BRA", "AUS"))]

# hosts <- OECDmap$CountryCode[which(OECDmap$RegionCode=="Non-OECD" & 
#                                map$RegionCode %in% c("OAS", "SSA", "MEA", "IND", "REF", "LAM")  &
#                                !(OECDmap$X %in% c(
#                                  'China', 'Switzerland', 'Costa Rica', 'Peru', 'Azerbaijan', 'Russian Federation', 'Oman', 'Brazil', 'Qatar', 'Saudi Arabia', 'Singapore', 'Taiwan', 'Hong Kong', 'Kuwait', 'United Arab Emirates', 'Bahrain'
#                                )))]

# FinEx Host Regions
regHosts <- unique(map$RegionCode[which(map$CountryCode %in% hosts)])

# setConfig(mainfolder = "C:/Users/stephenb/Downloads/madrat_main/")
# path <- paste0(getConfig("sourcefolder"),"/PPCA/")
setConfig(mainfolder = "Z:/inputdata/")

# ssp2_npi <- read.report(paste0(path,"REMIND_generic_SSP2-NPi-covid-cfCHA52.mif"),as.list=F)
# ssp2eu_npi <- read.report("H:/FinEx/runs/REMIND_generic_PPCAcop26-coalElTeNoCCS_withoutPlus.mif",as.list=F)
ssp2eu_npi <- frozen[,,"PPCAcop26-jul23_jan22"]

Budg500 <- budget[,,"SSP2EU-PkBudg500"]

Budg1150 <- budget[,,"SSP2EU-PkBudg1150"]

ppca_static <- frozen[,,"PPCAcop26-jul23_jan22"]
ppca_50p <- dpe_full[,,"PPCA-jul23_jan22-nonoecd"]

RE_inv_vars <- c("Energy Investments|Elec|Grid|VRE support",
                 "Energy Investments|Elec|Storage",
                 "Energy Investments|Elec|Solar",
                 "Energy Investments|Elec|Wind")
RE_inv_vars <- paste(RE_inv_vars,"(billion US$2005/yr)")

# ren21_REcap <- read_excel(path = "C:/Users/stephenb/Documents/Coal_Finance_Exit/Data/REN21_GSR2021_Data_Pack.xlsx",sheet = "Data 47",col_types = "numeric",range="B4:O14",col_names = T,)


inflation_05_to_19 <- 1.31

setConfig(forcecache = T)
data <- readSource("IEA",subtype="EnergyBalances") * 0.0000418680000
setConfig(forcecache = F)
totalgen_2019_c <- data[,2019,"TOTAL.ELOUTPUT"]
totalgen_2019_R <- toolAggregate(totalgen_2019_c,map,NULL)

totalgen_2019_finEx <- toolAggregate(totalgen_2019_c[hosts,,], rel = map, weight = NULL, partrel = T)
totalgen_2019_R <- totalgen_2019_R[regHosts,,]

host_share <- setNames(
  mbind(setYears(totalgen_2019_finEx / totalgen_2019_R, 2020),
        setYears(totalgen_2019_finEx / totalgen_2019_R, 2025)),
  nm = NULL)

# host_share <- dimSums(totalgen_2019_finEx, dim = 1) / dimSums(totalgen_2019_R, dim = 1)

npi_RE_inv <- ssp2eu_npi[regHosts,c(2020,2025),RE_inv_vars] * inflation_05_to_19 * host_share

ppca_static_RE_inv <- ppca_static[regHosts,c(2020,2025),RE_inv_vars] * inflation_05_to_19 * host_share

q_ppca_static_RE_inv <- as.quitte(mbind(ppca_static_RE_inv,
                                        toolAggregate(ppca_static_RE_inv, rel = data.frame(from=regHosts,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr") %>%
  mutate(scenario="PPCA") %>%
  mutate(scenario=ifelse(period==2020,paste0(scenario,"_2020"),paste0(scenario,"_2025")))


b1150_RE_inv <- Budg1150[regHosts,2025,RE_inv_vars] * inflation_05_to_19 * host_share[,2025,]

q_b1150_RE_inv <- as.quitte(mbind(b1150_RE_inv,
                                  toolAggregate(b1150_RE_inv, rel = data.frame(from=regHosts,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr")

b500_RE_inv <- Budg500[regHosts,2025,RE_inv_vars] * inflation_05_to_19 * host_share[,2025,]

q_b500_RE_inv <- as.quitte(mbind(b500_RE_inv,
                                 toolAggregate(b500_RE_inv, rel = data.frame(from=regHosts,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr")

# reg_REdir_mob <- as.quitte(mag_precon_REdir_oecd_nat_mob) %>%
#   filter(value>0) %>%
#   group_by(region) %>%
#   summarise(REdir = value, .groups = "keep")
# 
# reg_REdir_HI_mob <- precon_REdir_HI_oecd_nat_mob %>%
#   rename(region=Region) %>%
#   group_by(region) %>%
#   summarise(REdir_HI = 
#               sum(REdir_oecd_mob),.groups="keep") %>%
#   filter(REdir_HI>0) %>%
#   left_join(reg_REdir_mob) %>%
#   summarise(REdir_HI = REdir_HI - REdir, .groups = 'keep')
# 
# reg_REdir_LO_mob <- precon_REdir_LO_oecd_nat_mob %>%
#   rename(region = Region) %>%
#   group_by(region) %>%
#   summarise(REdir_LO = sum(REdir_oecd_mob),.groups="keep") %>%
#   filter(REdir_LO>0) %>%
#   left_join(reg_REdir_mob) %>%
#   summarise(REdir_LO = REdir_LO - REdir, .groups = 'keep')


grant_mob_factor <- as.numeric(
  mob_rate_oecd_Energy_grant %>% ungroup() %>%
    mutate(Recipient = toolCountry2isocode(Recipient)) %>%
    filter(Recipient %in% hosts) %>%
    left_join(as.quitte(totalgen_2019_c) %>% 
                rename(Recipient=region) %>%
                select(Recipient, value), by = "Recipient") %>%
    filter(Public > 0) %>%
    summarise(weighted.mean(Mobilization_rate, value, na.rm = T)))


loan_mob_factor <- as.numeric(
  mob_rate_oecd_RE %>% ungroup() %>%
    filter(Country %in% hosts) %>% 
    left_join(as.quitte(totalgen_2019_c) %>% 
                rename(Country=region) %>%
                select(Country, value), by = "Country") %>%
    filter(Public > 0) %>%
    summarise(weighted.mean(Mobilization_rate, value, na.rm = T)))


#######################################################################################################
############################### COMPILING FINEX AND REMIND DATA #######################################
#######################################################################################################
q_finEx_inv_host_reg <- q_finEx_by_host_reg %>%
  mutate(period=2025) %>%
  mutate(scenario="REdirect_noMob") %>%
  filter(data1=="bbUSD" & data=="PubG20" & variable=="PreCon") %>%
  mutate(unit="billion US$2019/yr") %>%
  select(-c(data,data1)) %>%
  bind_rows(as.quitte(mag_precon_REdir_oecd_nat_mob) %>%
              filter(value>0) %>%
              mutate(scenario="REdirect_oecdMob",
                     period=2025)) %>%
  group_by(region) %>%
  left_join(reg_REdir_LO_mob, by = "region") %>% 
  mutate(REdir_LO = ifelse(scenario=="REdirect_noMob",
                           REdir_LO,
                           0)) %>%
  left_join(reg_REdir_HI_mob, by = "region") %>% 
  mutate(REdir_HI = ifelse(scenario=="REdirect_noMob",
                           REdir_HI,
                           0)) %>%
  mutate(REdir_M = ifelse(grepl("oecdMob", scenario),
                          value,
                          0)) %>%
  bind_rows(q_ppca_static_RE_inv %>%
              mutate(value = value * 5)) %>%
  bind_rows(q_b1150_RE_inv %>%
              mutate(value = value * 5)) %>%
  bind_rows(q_b500_RE_inv %>%
              mutate(value = value * 5))


### ADD REDIRECT AND MOBILIZATION TO NPI ###
q_REdir_tot_host_reg <- q_finEx_inv_host_reg %>%
  mutate(data = 
           ifelse(model != "REMIND" | (grepl("PPCA",scenario) & period==2025),
                  "2022-27 REdirect",
                  gsub("SSP2EU-","",scenario))) %>%
  mutate(model = 
           ifelse(model=="REMIND",
                  "REMIND",
                  ifelse(grepl("oecdMob",scenario),
                         "Private Mobilization",
                         "Public G20 Finance"))) %>%
  mutate(scenario =
           ifelse(grepl("PPCA",scenario),
                  gsub("_202[0-9]","",scenario),
                  scenario)) 


mob_range <- q_REdir_tot_host_reg %>%
  filter(data == "2022-27 REdirect" & !grepl("oecdMob",scenario)) %>%
  replace(is.na(.),0) %>%
  group_by(region, data) %>%
  summarise(REdir_HI=sum(value,REdir_HI),
            REdir_LO=sum(value,REdir_LO),
            REdir_M=sum(value,REdir_M),
            REdir=sum(value),
            .groups="keep")

#####################################################################################################################################################################################

## COMPARE GLOBAL REDIRECT VOLUME WITH INITATIVES AND PROMISES ##
q_inv_RE <- q_REdir_tot_host_reg %>% 
  filter(region=="GLO" & !grepl("2020", data)) %>%
  ungroup() %>%
  bind_rows(q_REdir_tot_host_reg %>%
              filter(region=="GLO" & grepl("REdirect_noMob", scenario)) %>%
              select(-value) %>%
              rename(value = REdir_HI) %>%
              mutate(region = "G20",
                     data = "Grant-Mobilized Private Capital",
                     scenario = "G20 Coal-to-Clean\nFinance Proposal")) %>%
  mutate(region = ifelse(grepl("REdir", scenario), "G20", region),
         data = ifelse(grepl("500", data), "1.5\u00B0C", 
                       ifelse(grepl('1150', data), '2\u00B0C', 
                              ifelse(scenario=="PPCA", "Baseline Expectations", 
                                     ifelse(grepl("noMob", scenario), "G20 Public REdirect", 
                                            ifelse(grepl("oecdMob", scenario), "Loan-Mobilized Private Capital", data))))),
         scenario = ifelse(grepl("PkBud|PPCA", scenario), "2022-27 Capital\nRequirements", 
                           ifelse(grepl("REdirect", scenario), "G20 Coal-to-Clean\nFinance Proposal", scenario))) %>%
  # G7 Partnership for Global Infrastructure and Investment
  add_row(region = "G7", data = "G7 Public Finance (PGII)",
          # scenario = "Non-Energy",
          scenario = "G7 & China\nNon-Energy Finance",
          value = 0.75 * 600,
          period = 2025) %>%
  # # EU Global Gateway (included within the PGII)
  # add_row(region = "GLO",
  #         scenario = "EU",
  #         data = "G7 & EU DevFi Plans\n(2021-2027)",
  #         value = 341,
  #         period = 2025) %>%
  # Estimated volume of PGII bank directed toward energy
  add_row(region = "G7", data = "G7 Public Finance (PGII)",
          scenario = "G7 & China\nEnergy Finance",
          # scenario = "G7 Public Finance (PGII)",
          # scenario = "Non-RE Energy",
          value = 0.125 * 600,
          period = 2025) %>%
  # Estimated volume of PGII bank directed toward renewables
  add_row(region = "G7", data = "G7 Public Finance (PGII)",
          scenario = "G7 & China\nRE Finance",
          # scenario = "RE",
          value = 0.125 * 600,
          period = 2025) %>%
  # $100 billion annual finance pledge
  add_row(region = "GLO",
          data = "5-Year $100B Goal",
          scenario = "Global $100B\nAnnual Promise",
          value = 500,
          period = 2025) %>%
  # Retroactive fulfillment of pledge
  add_row(region = "GLO",
          data = "Historical $100B Shortfall",
          scenario = "Global $100B\nAnnual Promise",
          value = 900 - (80 + 78 + 71 + 58 + 58 + 61.5 + 52 + 80 + 80),
          period = 2025) %>%
  # China Belt and Road Initiative Outlook
  add_row(region = "China", data = "Chinese Public Finance (BRI)",
          scenario = "G7 & China\nRE Finance",
          # scenario = "RE",
          value = 0.087331 * 550,
          period = 2025) %>%
  add_row(region = "China", data = "Chinese Public Finance (BRI)",
          scenario = "G7 & China\nEnergy Finance",
          # scenario = "Non-RE Energy",
          value = 0.375 * 550 - (0.087331 * 550),
          period = 2025) %>%
  add_row(region = "China", data = "Chinese Public Finance (BRI)",
          scenario = "G7 & China\nNon-Energy Finance",
          # scenario = "Non-Energy",
          value = 550 * 0.76,
          period = 2025) %>%
  group_by(scenario,region,data) %>%
  summarise(volume=sum(value),.groups="keep") %>%
  ungroup() %>%
  mutate(volume = ifelse(grepl("G7", data) & grepl("RE|\\nEnergy", scenario, ignore.case = F), volume / (1 + loan_mob_factor), volume)) 


  ##############################################################################################################################
  #### LOAN-BASED FINANCE ###
  ##############################################################################################################################
  q_inv_RE_loan <- q_inv_RE %>% 
    filter(!grepl("Grant", data)) %>%
    bind_rows(q_inv_RE %>% filter(grepl("RE|\\nEnergy", scenario, ignore.case = F)) %>%
                mutate(data = "Loan-Mobilized Private Capital",
                       volume = volume * loan_mob_factor))  %>%
    bind_rows(q_inv_RE %>% filter(grepl("100B", scenario)) %>%
                mutate(volume = volume*loan_mob_factor,
                       data = "Loan-Mobilized Private Capital")) %>%
    ungroup() %>%
    # filter(!grepl("Non-Energy", scenario)) %>%
    mutate(scenario = factor(scenario,
                             levels = c("G7 & China\nRE Finance",
                                        "G20 Coal-to-Clean\nFinance Proposal",
                                        "G7 & China\nEnergy Finance",
                                        "G7 & China\nNon-Energy Finance",
                                        "2022-27 Capital\nRequirements",
                                        "Global $100B\nAnnual Promise"),
                             ordered = T),
           data = factor(data,
                         levels = c("Loan-Mobilized Private Capital",
                                    "G20 Public REdirect",
                                    "Historical $100B Shortfall", "5-Year $100B Goal",
                                    "Chinese Public Finance (BRI)",
                                    "G7 Public Finance (PGII)",
                                    "1.5°C", "2°C", "Baseline Expectations"),
                         ordered = T),
           region = factor(region,
                           levels = c("GLO", "G20", "China", "G7"),
                           ordered = T)) %>%
    mutate(group.id = as.numeric(scenario)) %>%
    arrange(scenario, desc(data), desc(region))
  

  waterfall_inv_RE_loan <- q_inv_RE_loan %>%
    filter(!grepl("Grant", data)) %>%
    filter(!grepl("Requirements|Proposal", scenario)) %>%
    filter(!grepl("100B", scenario)) %>%
    mutate(end.Bar = cumsum(volume),
           start.Bar = c(0, head(end.Bar, -1))) %>%
    group_by(scenario) %>%
    mutate(total.by.x = sum(volume)) %>%
    select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
  
  
  waterfall_inv_RE_loan <- waterfall_inv_RE_loan %>%
    bind_rows(q_inv_RE_loan %>%
              filter(grepl("Proposal", scenario) & !grepl("Grant", data)) %>%
              mutate(end.Bar = cumsum(volume) + filter(waterfall_inv_RE_loan, 
                                                       grepl("\\nEnergy", scenario) & grepl("PGII", data))$start.Bar,
                     start.Bar = c(filter(waterfall_inv_RE_loan, 
                                          grepl("\\nEnergy", scenario) & grepl("PGII", data))$start.Bar,
                                          head(end.Bar, -1))) %>%
                       group_by(scenario) %>%
                       mutate(total.by.x = sum(volume)) %>%
                       select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
              ) %>%
    bind_rows(q_inv_RE_loan %>%
                filter(grepl("Requirements", scenario)) %>%
                arrange(scenario, desc(data)) %>%
                mutate(volume = ifelse(grepl("Baseline", data), volume, 
                                       diff(volume)),
                       end.Bar = cumsum(volume),
                       start.Bar = c(0, head(end.Bar, -1))) %>%
                group_by(scenario) %>%
                mutate(total.by.x = sum(volume)) %>%
                select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
    ) %>%
    bind_rows(q_inv_RE_loan %>%
                filter(grepl("100B", scenario)) %>%
                arrange(scenario, desc(data)) %>%
                mutate(end.Bar = cumsum(volume),
                       start.Bar = c(0, head(end.Bar, -1))) %>%
                group_by(scenario) %>%
                mutate(total.by.x = sum(volume)) %>%
                select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
    ) %>% arrange(scenario, desc(data), desc(region))
              
  
  ### LOAN-INDUCED INVESTMENT ###
  ggplot(waterfall_inv_RE_loan) +
    geom_rect(data = . %>% filter(region!="GLO" | grepl("Mobilize", data)),
              mapping = aes(x = scenario,
                  xmin = group.id-0.25, # control bar gap width
                  xmax = group.id+0.25, 
                  ymin = end.Bar,
                  ymax = start.Bar, 
                  fill = data
                  # alpha = scenario
                  ),
              # color = "#440154FF"
              color = "black"
    ) + 
    scale_fill_brewer(palette = "Blues", name = "Source of Expected Finance") +
    new_scale_fill() +
    geom_rect(data = . %>% filter(region=="GLO" & !grepl("Mobilize", data)),
              mapping = aes(x = scenario,
                            xmin = group.id-0.25, # control bar gap width
                            xmax = group.id+0.25, 
                            ymin = end.Bar,
                            ymax = start.Bar,
                            fill = data
                            # alpha = scenario
              ),
              # color = "#440154FF"
              color = "black"
    ) + 
    scale_fill_brewer(palette = "Greens", 
                      # limits = rev(levels(waterfall_inv_RE_loan$data)),
                      name = "Investment Target") +
    geom_segment(data = waterfall_inv_RE_loan %>% filter(grepl("China|G20", region) & grepl("Mobilize", data)),
                 mapping = 
                   aes(x=group.id,
                     xend = 5.25,
                     # xend=ifelse(grepl("China\\\nEnergy", scenario),
                     #             group.id+2.25,
                     #             ifelse(grepl("Proposal", scenario) & grepl("Mobilize", data),
                     #                    group.id+3.25,
                     #                    group.id+0.75)),
                     y=end.Bar,
                     yend=end.Bar),
                 color = "#440154FF",
                 # color = "black",
                 linetype = 2)  +
    scale_y_continuous(
      name = "5-Year Investment Sums (Billion $2019 USD)",
      expand=c(0,0),
      limits = c(0, 2200)
    ) +
    geom_text(
      data = waterfall_inv_RE_loan %>% filter(grepl("China|G20|GLO", region) & (grepl("Mobilize|1.5", data) | grepl("Non-Energy", scenario)) & !grepl("100B", scenario)),
      mapping = 
        aes(x = group.id,
            label = as.character(round(total.by.x,0)),
            y = end.Bar+30,
            alpha = scenario),
      color = "grey20",
      fontface = "bold"
    ) + 
    geom_text(
      data = waterfall_inv_RE_loan,
      mapping = 
        aes(x = group.id,
          label = as.character(round(volume,0)),
          y = rowSums(cbind(start.Bar,volume/2)),
          alpha = scenario,
          # color = data
        ),
      color = "grey20",
      size = 3.5,
      fontface = "bold"
    ) + 
    geom_text(
      data = . %>% filter(grepl("China|G20", region) & grepl("Mobilize", data)),
      mapping = aes(
        x = 5.35,
        y = end.Bar,
        label = as.character(round(end.Bar,0)),
        alpha = scenario
      ),
      color = "#440154FF",
      fontface = "bold"
    ) +
    scale_alpha_manual(values = c(
      "G7 & China\nRE Finance" = 1,
      "G7 & China\nEnergy Finance" = 0.4,
      "G20 Coal-to-Clean\nFinance Proposal" = 0.7,
      "G7 & China\nNon-Energy Finance" = 0.22
    ),
    guide = "none") +
    scale_color_brewer(palette = "Greys", direction = -1) +
    theme_bw() +
    scale_x_discrete(limits = 
                       c("G7 & China\nRE Finance",
                          "G20 Coal-to-Clean\nFinance Proposal",
                          "G7 & China\nEnergy Finance",
                          "G7 & China\nNon-Energy Finance",
                          "2022-27 Capital\nRequirements",
                          "Global $100B\nAnnual Promise")
    ) +
    labs(title = "Debt-Based G20 Clean Development Finance (2022-27)",
         x = "") +
    theme(axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12, face = "bold"),
          title = element_text(face = "bold"),
          legend.text = element_text(size = 12),
          # legend.title = element_text(size = 11),
          legend.position = c(0.25,0.8),
          legend.background = element_blank(),
          panel.grid = element_line(color = "grey95")
    )
  
  ggsave(file = "~/Coal_Finance_Exit/Plots/Waterfall_devFi/Debt_based_finance_waterfall.png", dpi = "retina", height = 225*1.25, width = 350*1.25, units = "mm")
  ggsave(file = "~/Coal_Finance_Exit/Plots/Waterfall_devFi/Debt_based_finance_waterfall.pdf", dpi = "retina", height = 225*1.25, width = 350*1.25, units = "mm")
  
  
##############################################################################################################################
#### GRANT-BASED FINANCE ###
##############################################################################################################################
  q_inv_RE_grant <- q_inv_RE %>% 
    filter(!grepl("Loan", data)) %>%
    bind_rows(q_inv_RE %>% filter(grepl("RE|\\nEnergy", scenario, ignore.case = F)) %>%
                mutate(data = "Grant-Mobilized Private Capital",
                       volume = volume * grant_mob_factor))  %>%
    bind_rows(q_inv_RE %>% filter(grepl("100B", scenario)) %>%
                mutate(volume = volume*grant_mob_factor,
                       data = "Grant-Mobilized Private Capital")) %>%
    ungroup() %>%
    filter(!grepl("Non-Energy", scenario) & volume < 3000) %>%
    mutate(scenario = factor(scenario,
                             levels = c("G7 & China\nRE Finance",
                                        "G20 Coal-to-Clean\nFinance Proposal",
                                        "G7 & China\nEnergy Finance",
                                        # "G7 & China\nNon-Energy Finance",
                                        "2022-27 Capital\nRequirements",
                                        "Global $100B\nAnnual Promise"),
                             ordered = T),
           data = factor(data,
                         levels = c("Grant-Mobilized Private Capital",
                                    "G20 Public REdirect",
                                    "Historical $100B Shortfall", "5-Year $100B Goal",
                                    "Chinese Public Finance (BRI)",
                                    "G7 Public Finance (PGII)",
                                    "1.5°C", "2°C", "Baseline Expectations"),
                         ordered = T),
           region = factor(region,
                           levels = c("GLO", "G20", "China", "G7"),
                           ordered = T)) %>%
    mutate(group.id = as.numeric(scenario)) %>%
    arrange(scenario, desc(data), desc(region))
  
  
  waterfall_inv_RE_grant <- q_inv_RE_grant %>%
    filter(!grepl("Loan", data)) %>%
    filter(!grepl("Requirements|Proposal", scenario)) %>%
    filter(!grepl("100B", scenario)) %>%
    mutate(end.Bar = cumsum(volume),
           start.Bar = c(0, head(end.Bar, -1))) %>%
    group_by(scenario) %>%
    mutate(total.by.x = sum(volume)) %>%
    select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
  
  
  waterfall_inv_RE_grant <- waterfall_inv_RE_grant %>%
    bind_rows(q_inv_RE_grant %>%
                filter(grepl("Proposal", scenario) & !grepl("Loan", data)) %>%
                mutate(end.Bar = cumsum(volume) + filter(waterfall_inv_RE_grant, 
                                                         grepl("\\nEnergy", scenario) & grepl("PGII", data))$start.Bar,
                       start.Bar = c(filter(waterfall_inv_RE_grant, 
                                            grepl("\\nEnergy", scenario) & grepl("PGII", data))$start.Bar,
                                     head(end.Bar, -1))) %>%
                group_by(scenario) %>%
                mutate(total.by.x = sum(volume)) %>%
                select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
    ) %>%
    bind_rows(q_inv_RE_grant %>%
                filter(grepl("Requirements", scenario)) %>%
                arrange(scenario, desc(data)) %>%
                mutate(volume = ifelse(grepl("Baseline", data), volume, 
                                       diff(volume)),
                       end.Bar = cumsum(volume),
                       start.Bar = c(0, head(end.Bar, -1))) %>%
                group_by(scenario) %>%
                mutate(total.by.x = sum(volume)) %>%
                select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
    ) %>%
    bind_rows(q_inv_RE_grant %>%
                filter(grepl("100B", scenario)) %>%
                arrange(scenario, desc(data)) %>%
                mutate(end.Bar = cumsum(volume),
                       start.Bar = c(0, head(end.Bar, -1))) %>%
                group_by(scenario) %>%
                mutate(total.by.x = sum(volume)) %>%
                select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
    ) %>% arrange(scenario, desc(data), desc(region))
  
  
  ### LOAN-INDUCED INVESTMENT ###
  ggplot(waterfall_inv_RE_grant) +
    geom_rect(data = . %>% filter(region!="GLO" | grepl("Mobilize", data)),
              mapping = aes(x = scenario,
                            xmin = group.id-0.25, # control bar gap width
                            xmax = group.id+0.25, 
                            ymin = end.Bar,
                            ymax = start.Bar, 
                            fill = data
                            # alpha = scenario
              ),
              # color = "#440154FF"
              color = "black"
    ) + 
    scale_fill_brewer(palette = "Blues", name = "Source of Expected Finance") +
    new_scale_fill() +
    geom_rect(data = . %>% filter(region=="GLO" & !grepl("Mobilize", data)),
              mapping = aes(x = scenario,
                            xmin = group.id-0.25, # control bar gap width
                            xmax = group.id+0.25, 
                            ymin = end.Bar,
                            ymax = start.Bar,
                            fill = data
                            # alpha = scenario
              ),
              # color = "#440154FF"
              color = "black"
    ) + 
    scale_fill_brewer(palette = "Greens", 
                      # limits = rev(levels(waterfall_inv_RE_grant$data)),
                      name = "Investment Target") +
    geom_segment(data = waterfall_inv_RE_grant %>% filter(grepl("China|G20", region) & grepl("Mobilize", data)),
                 mapping = 
                   aes(x=group.id,
                       xend = 4.25,
                       # xend=ifelse(grepl("China\\\nEnergy", scenario),
                       #             group.id+2.25,
                       #             ifelse(grepl("Proposal", scenario) & grepl("Mobilize", data),
                       #                    group.id+3.25,
                       #                    group.id+0.75)),
                       y=end.Bar,
                       yend=end.Bar),
                 color = "#440154FF",
                 # color = "black",
                 linetype = 2)  +
    scale_y_continuous(name = "5-Year Investment Sums (Billion $2019 USD)",
                       expand = c(0,)) +
    coord_cartesian(ylim=c(0, 2300)) +
    geom_text(
      data = waterfall_inv_RE_grant %>% filter(grepl("China|G20|GLO", region) & (grepl("Mobilize|1.5", data) | grepl("Non-Energy", scenario)) & !grepl("100B", scenario)),
      mapping = 
        aes(x = group.id,
            label = as.character(round(total.by.x,0)),
            y = end.Bar+30,
            alpha = scenario),
      color = "grey20",
      fontface = "bold"
    ) + 
    geom_text(
      data = waterfall_inv_RE_grant,
      mapping = 
        aes(x = group.id,
            label = ifelse(region=="GLO" & grepl("Mobilize", data),
                           as.character(round(volume + 3071,0)),
                           as.character(round(volume,0))),
            y = rowSums(cbind(start.Bar,volume/2)),
            alpha = scenario,
            # color = data
        ),
      color = "grey20",
      size = 3.5,
      fontface = "bold"
    ) + 
    geom_text(
      data = . %>% filter(grepl("China|G20", region) & grepl("Mobilize", data)),
      mapping = aes(
        x = 4.35,
        y = end.Bar,
        label = as.character(round(end.Bar,0)),
        alpha = scenario
      ),
      color = "#440154FF",
      fontface = "bold"
    ) +
    scale_alpha_manual(values = c(
      "G7 & China\nRE Finance" = 1,
      "G7 & China\nEnergy Finance" = 0.4,
      "G20 Coal-to-Clean\nFinance Proposal" = 0.7,
      "G7 & China\nNon-Energy Finance" = 0.22
    ),
    guide = "none") +
    scale_color_brewer(palette = "Greys", direction = -1) +
    theme_bw() +
    scale_x_discrete(limits = 
                       c("G7 & China\nRE Finance",
                         "G20 Coal-to-Clean\nFinance Proposal",
                         "G7 & China\nEnergy Finance",
                         "2022-27 Capital\nRequirements",
                         "Global $100B\nAnnual Promise")
    ) +
    labs(title = "Grant-Based G20 Clean Development Finance (2022-27)",
         x = "") +
    theme(axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12, face = "bold"),
          title = element_text(face = "bold"),
          legend.text = element_text(size = 12),
          # legend.title = element_text(size = 11),
          legend.position = c(0.25,0.8),
          legend.background = element_blank(),
          panel.grid = element_line(color = "grey95")
    )
  
  ggsave(file = "~/Coal_Finance_Exit/Plots/Waterfall_devFi/Grant_based_finance_waterfall.png", dpi = "retina", height = 225*1.25, width = 350*1.25, units = "mm")
  ggsave(file = "~/Coal_Finance_Exit/Plots/Waterfall_devFi/Grant_based_finance_waterfall.pdf", dpi = "retina", height = 225*1.25, width = 350*1.25, units = "mm")
  
  



# ggplot(q_inv_RE) +
#   geom_errorbar(data = mob_range %>% filter(region=="GLO"),
#                 mapping = aes(x = "2022-27 REdirect",
#                               ymin = REdir_LO,
#                               ymax = REdir_HI),
#                 width = 0.5,
#                 color = "darkgreen",
#                 size = 0.7) +
#   geom_col(data = . %>% filter(!grepl("PkBud",scenario) & data!="PPCA_2020"),
#            mapping=aes(x=data,
#                        y=volume,
#                        alpha = scenario,
#                        group = fill,
#                        fill = fill),
#            width=0.9,
#            position="stack") +
#   # geom_hline(data = . %>% filter(grepl("PPCA_2020",data)),
#   #            mapping = aes(linetype = scenario,
#   #                          yintercept = volume),
#   #            color = "grey80") + 
#   geom_hline(data = . %>% filter(grepl("PkBudg",data)),
#              mapping = aes(linetype = scenario,
#                            yintercept = volume),
#              color = "mediumblue",
#              alpha = 0.7) +
#   geom_text_repel(mapping = aes(
#     x = 1,
#     y = cumval,
#     color = factor(scenario,
#                    levels = c("REdirect_oecdMob", "REdirect_noMob", "PPCA"),
#                    ordered = T),
#     label = round(volume,0)),
#     data = . %>% 
#       filter(data=="2022-27 REdirect") %>% 
#       group_by(region) %>%
#       mutate(cumval = cumsum(volume)),
#     nudge_x = -0.55,
#     min.segment.length = 100,
#     direction = "y",
#     size = 3.75, 
#     box.padding = 0.1,
#     force = 0.5,
#     hjust = 0.2,
#     fontface = "bold"
#   ) +
#   # geom_text(data = . %>% filter(data == "PPCA_2020"),
#   #           mapping = aes(x = 2,
#   #                         y = volume),
#   #           label = "2017-22 RE Investment\n(REMIND estimate)",
#   #           fontface = "bold.italic",
#   #           color = "grey80",
#   #           size = 4.2) +
#   geom_text(mapping = aes(
#     x = 1,
#     y = REdir_HI,
#     label = round(REdir_HI - REdir_M,0)),
#     data = mob_range %>% filter(region=="GLO"),
#     nudge_x = -0.52,
#     check_overlap = T,
#     color = "darkgreen",
#     size = 3.3, fontface = "bold"
#   ) +
#   scale_linetype_manual(values = c("SSP2EU-PkBudg1150" = 3, "SSP2EU-PkBudg500" = 4, "PPCA" = 2),
#                         breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
#                         labels = c("1.5\u00B0C (2022-27)", "2\u00B0C (2022-27)"),
#                         name = "REMIND Benchmarks"#,
#                         # guide = "none"
#   ) +
#   # annotate(geom = "text",
#   #          x = "2022-27 REdirect",
#   #          y = mean(c(mob_range$REdir_HI[which(mob_range$region=="GLO")], 98 + mob_range$REdir[which(mob_range$region=="GLO")])),
#   #          # as.numeric(select(
#   #          #   filter(q_inv_RE, region == "GLO" & data == "PkBudg1150"), volume)))),
#   #          # q_inv_RE$volume[which(q_inv_RE$region=="GLO")])),
#   #          label = "Grant-induced\nMobilization",
#   #          fontface = "bold.italic",
#   #          color = "darkgreen",
#   #          angle = "90",
#   #          size = 3.9) +
# scale_fill_manual(values = setNames(c(accent[-c(3,7)],rep(accent[c(6,8)],2),accent[3]), 
#                                     nm = levels(q_inv_RE$fill)),
#                   breaks = levels(q_inv_RE$fill)[-c(7:10)],
#                   name = "Source of Finance\n(Disbursal Horizon)") +
#   # scale_linetype_manual(values = c("PPCA_2020" = 2,
#   #                                  "2022-27 REdirect" = 1),
#   #                       guide = "none") +
#   scale_color_brewer(palette="Accent", direction = 1, guide = "none") +
#   scale_alpha_manual(values = c("Non-Energy" = 0.1, "Non-RE Energy" = 0.5, "RE" = 1),
#                      breaks = rev(c("Non-Energy", "Non-RE Energy", "RE")),
#                      name = "Estimated Allocation of \nDevelopment Finance",
#                      labels = rev(c("Non-Energy", "Non-RE Energy", "Renewables"))) +
#   labs(x="",y="Billion USD (2019$)"
#        # title="REdirect in Context"
#   ) +
#   theme_bw() +
#   theme(text = element_text(size = 13),
#         # plot.title = element_text(size=16,face="bold"),
#         # strip.text = element_text(size=13),
#         # axis.text = element_text(size=8),
#         # axis.title = element_text(size=8),
#         # legend.text = element_text(size=7),
#         # legend.title = element_text(size=7,face="bold"),
#         legend.background = element_blank()
#         # legend.key.height = unit(25,"pt")
#         # ,
#         # legend.position = c(0.82,0.15)
#   )

# ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_hosts_REdir_Mob_bbUSD_in_context_stackBar_largeFont_GRANT_noBase.pdf",width = 10, height = 8, units = "in", dpi="retina")  
# 
# ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_hosts_REdir_Mob_bbUSD_in_context_stackBar_largeFont_GRANT_noBase.png",width = 10, height = 8, units = "in", device = "png",dpi="retina")  

