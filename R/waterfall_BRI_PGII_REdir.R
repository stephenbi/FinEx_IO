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

# FinEx Host Countries
hosts <- unique(preCon_finFlows$Country)

# FinEx Host Regions
regFinEx <- getItems(mag_precon_REdir_oecd_nat_mob,1)[which(mag_precon_REdir_oecd_nat_mob>0)]

regOrd <- c("GLO", "OAS", "SSA", "MEA", "NEU", "CAZ", "REF", "LAM") 
regOrder <- c("All FinEx Hosts", "S & E Asia ex. CN, IN, JP","Sub-Saharan Africa","Middle East & N Africa","Non-EU Europe",
              "Australia","Former Soviet Union","Brazil")
regFinEx <- regOrd[-1]

stackOrdFill <- c("oper"="gray20",
                  "Oper"="gray29",
                  "con"="tan4",
                  "Con"="tan3",
                  "preCon"="darkgreen",
                  "PreCon"="mediumseagreen")
stackOrd <- c("oper","Oper","con","Con","preCon","PreCon")
stackOrder <- c("Operating (Other)","Operating (G20)","Under Construction (Other)","Under Construction (G20)","Pre-Construction (Other)","Pre-Construction (G20)")

fillScale <- scale_fill_manual(values = stackOrdFill,
                               labels=stackOrder,
                               breaks = levels(stackOrdFill),
                               name="Status")


map <- toolGetMapping("regionmappingH12.csv",type="regional")

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

data <- readSource("IEA",subtype="EnergyBalances") * 0.0000418680000
totalgen_2019_c <- data[,2019,"TOTAL.ELOUTPUT"]
totalgen_2019_R <- toolAggregate(totalgen_2019_c,map,NULL)

totalgen_2019_finEx <- toolAggregate(totalgen_2019_c[hosts,,], rel = map, weight = NULL, partrel = T)
totalgen_2019_R <- totalgen_2019_R[regFinEx,,]

host_share <- setNames(
  mbind(setYears(totalgen_2019_finEx / totalgen_2019_R, 2020),
        setYears(totalgen_2019_finEx / totalgen_2019_R, 2025)),
  nm = NULL)

# host_share <- dimSums(totalgen_2019_finEx, dim = 1) / dimSums(totalgen_2019_R, dim = 1)

npi_RE_inv <- ssp2eu_npi[regFinEx,c(2020,2025),RE_inv_vars] * inflation_05_to_19 * host_share

ppca_static_RE_inv <- ppca_static[regFinEx,c(2020,2025),RE_inv_vars] * inflation_05_to_19 * host_share

q_ppca_static_RE_inv <- as.quitte(mbind(ppca_static_RE_inv,
                                        toolAggregate(ppca_static_RE_inv, rel = data.frame(from=regFinEx,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr") %>%
  mutate(scenario="PPCA") %>%
  mutate(scenario=ifelse(period==2020,paste0(scenario,"_2020"),paste0(scenario,"_2025")))


b1150_RE_inv <- Budg1150[regFinEx,2025,RE_inv_vars] * inflation_05_to_19 * host_share[,2025,]

q_b1150_RE_inv <- as.quitte(mbind(b1150_RE_inv,
                                  toolAggregate(b1150_RE_inv, rel = data.frame(from=regFinEx,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr")

b500_RE_inv <- Budg500[regFinEx,2025,RE_inv_vars] * inflation_05_to_19 * host_share[,2025,]

q_b500_RE_inv <- as.quitte(mbind(b500_RE_inv,
                                 toolAggregate(b500_RE_inv, rel = data.frame(from=regFinEx,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr")


reg_REdir_HI_mob <- precon_REdir_HI_oecd_nat_mob %>%
  rename(region=Region) %>%
  group_by(region) %>%
  summarise(REdir_HI = 
              sum(REdir_oecd_mob),.groups="keep")

reg_REdir_LO_mob <- precon_REdir_LO_oecd_nat_mob %>%
  rename(region = Region) %>%
  group_by(region) %>%
  summarise(REdir_LO = sum(REdir_oecd_mob),.groups="keep")


grant_mob_factor <- 3.5
  # as.numeric(
  # mob_rate_oecd_Energy_grant %>% ungroup() %>%
  #   mutate(Recipient = toolCountry2isocode(Recipient)) %>%
  #   filter(Recipient %in% hosts) %>%
  #   left_join(as.quitte(totalgen_2019_c) %>% 
  #               rename(Recipient=region) %>%
  #               select(Recipient, value), by = "Recipient") %>%
  #   filter(Public > 0) %>%
  #   summarise(weighted.mean(Mobilization_rate, value, na.rm = T)))


loan_mob_factor <- 0.66
  # as.numeric(
  # mob_rate_oecd_RE %>% ungroup() %>%
  #   filter(Country %in% hosts) %>% 
  #   left_join(as.quitte(totalgen_2019_c) %>% 
  #               rename(Country=region) %>%
  #               select(Country, value), by = "Country") %>%
  #   filter(Public > 0) %>%
  #   summarise(weighted.mean(Mobilization_rate, value, na.rm = T)))


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
q_GLO_REdir_context <- q_REdir_tot_host_reg %>% 
  filter(region=="GLO" & !grepl("2020", data)) %>%
  ungroup() %>%
  bind_rows(q_REdir_tot_host_reg %>%
              filter(region=="GLO" & grepl("REdirect_noMob", scenario)) %>%
              select(-value) %>%
              rename(value = REdir_HI) %>%
              mutate(region = "G20",
                     data = "Grant-Induced Mobilization",
                     scenario = "REdirect-grant")) %>%
  mutate(region = ifelse(grepl("REdir", scenario), "G20", region),
         data = ifelse(grepl("500", data), "1.5\u00B0C", 
                       ifelse(grepl('1150', data), '2\u00B0C', 
                              ifelse(scenario=="PPCA", "Baseline Expectations", 
                                     ifelse(grepl("noMob", scenario), "G20 REdirect", 
                                            ifelse(grepl("oecdMob", scenario), "Loan-Induced Mobilization", data))))),
         scenario = ifelse(grepl("PkBud", scenario), "2022-27 Capital\nRequirements", 
                           ifelse(grepl("REdirect|PPCA", scenario), "REdirect-loan", scenario))) %>%
  # G7 Partnership for Global Infrastructure and Investment
  # add_row(region = "G7", data = "Minilateral G7 Finance",
  #         # scenario = "Non-Energy",
  #         scenario = "G7 & China Non-Energy Loans",
  #         value = 0.75 * 600,
  #         period = 2025) %>%
  # # EU Global Gateway (included within the PGII)
  # add_row(region = "GLO",
  #         scenario = "EU",
  #         data = "G7 & EU DevFi Plans\n(2021-2027)",
  #         value = 341,
  #         period = 2025) %>%
  # Estimated volume of PGII bank directed toward energy
  add_row(region = "G7", data = "Minilateral G7 Finance",
          scenario = "G7 PGII\nEnergy Loans",
          # scenario = "Minilateral G7 Finance",
          # scenario = "Non-RE Energy",
          value = 0.125 * 600,
          period = 2025) %>%
  # Estimated volume of PGII bank directed toward renewables
  add_row(region = "G7", data = "Minilateral G7 Finance",
          scenario = "G7 PGII\nRE Loans",
          # scenario = "RE",
          value = 0.125 * 600,
          period = 2025) %>%
  # $100 billion annual finance pledge
  add_row(region = "GLO",
          data = "$100B Annual Loans",
          scenario = "Global $100B\nAnnual Promise",
          value = 500,
          period = 2025) %>%
  # Retroactive fulfillment of pledge
  # add_row(region = "GLO",
  #         data = "Historical $100B Shortfall",
  #         scenario = "Global $100B\nAnnual Promise",
  #         value = 900 - (80 + 78 + 71 + 58 + 58 + 61.5 + 52 + 80 + 80),
  #         period = 2025) %>%
  # China Belt and Road Initiative Outlook
  add_row(region = "China", data = "China's 14th FYP (2021-25)",
          scenario = "China BRI\nRE Loans",
          # scenario = "RE",
          value = 0.0734 * 550,
          period = 2025) %>%
  add_row(region = "China", data = "China's 14th FYP (2021-25)",
          scenario = "China BRI\nEnergy Loans",
          # scenario = "Non-RE Energy",
          value = 0.375 * 550 - (0.0734 * 550),
          period = 2025) %>%
  # add_row(region = "China", data = "China's 14th FYP (2021-25)",
  #         scenario = "G7 & China Non-Energy Loans",
  #         # scenario = "Non-Energy",
  #         value = 550 * 0.76,
          # period = 2025) %>%
  group_by(scenario,region,data) %>%
  summarise(volume=sum(value),.groups="keep") %>%
  ungroup() %>%
  mutate(volume = ifelse(grepl("G7", data) & grepl("RE|\\nEnergy", scenario, ignore.case = F), volume / (1 + loan_mob_factor), volume)) 


##############################################################################################################################
#### LOAN-BASED FINANCE ###
##############################################################################################################################
q_GLO_REdir_context_loan <- q_GLO_REdir_context %>% 
  filter(!grepl("Grant", data)) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("RE|\\nEnergy", scenario) & grepl("G7|China", region)) %>%
              mutate(data = "Loan-Induced Mobilization",
                     volume = volume * loan_mob_factor))  %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("100B", scenario)) %>%
              mutate(data = "$100B Annual Grants")) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("100B", scenario)) %>%
              mutate(volume = volume*loan_mob_factor,
                     data = "Loan-Induced Mobilization")) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("RE|\\nEnergy", scenario) & grepl("G7|China", region)) %>%
              mutate(data = "Grant-Induced Mobilization",
                     volume = volume * grant_mob_factor - (volume * loan_mob_factor)))  %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("100B", scenario)) %>%
              mutate(volume = volume * grant_mob_factor) %>%
              mutate(data = "Grant-Induced Mobilization")) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("Baseline", data)) %>%
              mutate(scenario = "2022-27 Capital\nRequirements")) %>%
  ungroup() %>%
  # filter(!grepl("Non-Energy", scenario)) %>%
  mutate(scenario = factor(scenario,
                           levels = c("G7 PGII\nRE Loans",
                                      "China BRI\nRE Loans",
                                      "G7 PGII\nEnergy Loans",
                                      "China BRI\nEnergy Loans",
                                      # "G7 & China Non-Energy Loans",
                                      "REdirect-loan",
                                      "Global $100B\nAnnual Promise",
                                      "2022-27 Capital\nRequirements"),
                           ordered = T),
         data = factor(data,
                       levels = c("Grant-Induced Mobilization",
                                  "Loan-Induced Mobilization",
                                  "$100B Annual Loans",
                                  "$100B Annual Grants",
                                  "China's 14th FYP (2021-25)",
                                  "Minilateral G7 Finance",
                                  "1.5°C", "2°C", "G20 REdirect",
                                  "Baseline Expectations"),
                       ordered = T),
         region = factor(region,
                         levels = c("GLO", "G20", "China", "G7"),
                         ordered = T)) %>%
  mutate(group.id = as.numeric(scenario)) %>%
  arrange(scenario, desc(data), desc(region))


waterfall_inv_RE_loan <- q_GLO_REdir_context_loan %>%
  filter(!grepl("Grant", data)) %>%
  filter(!grepl("Requirements|REdirect", scenario)) %>%
  filter(!grepl("100B", scenario)) %>%
  mutate(end.Bar = cumsum(volume),
         start.Bar = c(0, head(end.Bar, -1))) %>%
  group_by(scenario) %>%
  mutate(total.by.x = sum(volume)) %>%
  select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)


waterfall_inv_RE_loan <- waterfall_inv_RE_loan %>%
  bind_rows(q_GLO_REdir_context_loan %>%
              filter(grepl("REdirect", scenario)) %>% 
              filter(!grepl("Grant", data)) %>%
              mutate(end.Bar = cumsum(volume),
                     start.Bar = c(0, head(end.Bar, -1))) %>%
              group_by(scenario) %>%
              mutate(total.by.x = sum(volume)) %>%
              select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
  ) %>%
  bind_rows(q_GLO_REdir_context_loan %>%
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
  bind_rows(q_GLO_REdir_context_loan %>%
              filter(!grepl("Grant", data)) %>%
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
  geom_hline(yintercept = as.numeric(
    waterfall_inv_RE_loan %>% filter(grepl("1.5", data)) %>%
      ungroup() %>% select(total.by.x)),
    color = bluegreen[10], 
    linetype = 6) +
  geom_segment(data = waterfall_inv_RE_loan %>% 
                 filter(grepl("Mobil", data)),
               mapping = 
                 aes(x=group.id + 0.25,
                     xend = ifelse(grepl("BRI|REdirect|100B", scenario),
                                   max(waterfall_inv_RE_loan$group.id) + 0.33, 
                                   group.id + 0.75),
                     y=end.Bar,
                     yend=end.Bar),
               # color = "#440154FF",
               color = "black",
               linetype = 2)  +
  geom_text(data = . %>% filter(region!="G7" & grepl("Mobil", data)),
            mapping = aes(
              x = max(waterfall_inv_RE_loan$group.id) + 0.45,
              y = ifelse(grepl("Energy", scenario), 
                         end.Bar - 10,
                         ifelse(grepl("REdirect", scenario),
                                end.Bar + 10,
                                end.Bar)),
              alpha = scenario,
              label = paste0(round(end.Bar /
                filter(waterfall_inv_RE_loan, grepl("1.5", data))$end.Bar, 2) * 100, "%")),
            color = 'grey40',
            fontface = "italic") +
  geom_rect(data = . %>% filter(grepl("G20|G7|China|100B", data)),
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
  scale_fill_manual(values = block_cols, name = "Public Capital",
                    limits = unique(c(
                      filter(waterfall_inv_RE_loan, grepl("G20|G7|China|100B", data))$data)),
                    #   filter(waterfall_inv_RE_grant, grepl("G20|G7|China|100B", data))$data))
                    guide = guide_legend(order = 1)) +
  new_scale_fill() +
  geom_rect(data = . %>% filter(grepl("Baseline|\u00B0C", data)),
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
  scale_fill_manual(values = block_cols, guide = guide_legend(order = 2),
                    # limits = filter(waterfall_inv_RE_loan, grepl("Baseline|\u00B0C", data))$data,
                    name = "Blended Public/Private Capital") + 
  new_scale_fill() +
  geom_rect(data = . %>% filter(grepl("Mobil", data)),
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
  scale_fill_manual(values = block_cols, guide = guide_legend(order = 3),
                    # limits = unique(c(
                    #   filter(waterfall_inv_RE_loan, grepl("Mobil", data))$data,
                    #   filter(waterfall_inv_RE_grant, grepl("Mobil", data))$data)),
                    name = "Private Capital") + 
  scale_y_continuous(
    name = "5-Year Investment Sums (Billion $2019 USD)",
    expand=c(0,0)
  ) +
  coord_cartesian(ylim = c(0, 1450)) +
  geom_text(
    data = waterfall_inv_RE_loan %>% filter(grepl("Mobil|1.5|Shortfall", data)),
    mapping =
      aes(x = group.id,
          label = round(end.Bar,0),
          y = end.Bar + 20, 
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
          # alpha = scenario,
          # color = data
      ),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) + 
  # geom_text(
  #   data = . %>% filter(grepl("China|G20", region) & grepl("Mobil", data)),
  #   mapping = aes(
  #     x = 5.35,
  #     y = end.Bar,
  #     label = as.character(round(end.Bar,0)),
  #     alpha = scenario
  #   ),
  #   color = "#440154FF",
  #   fontface = "bold"
  # ) +
  scale_alpha_manual(values = c(
    "G7 PGII\nRE Loans" = 1,
    "China BRI\nRE Loans" = 1,
    "G7 PGII\nEnergy Loans" = 0.5,
    "China BRI\nEnergy Loans" = 0.5,
    "REdirect-loan" = 0.5,
    "Global $100B\nAnnual Promise" = 0.3,
    "G7 & China Non-Energy Loans" = 0.22
  ),
  guide = "none") +
  scale_color_brewer(palette = "Greys", direction = -1) +
  theme_bw() +
  scale_x_discrete(limits = 
                     c("G7 PGII\nRE Loans",
                       "China BRI\nRE Loans",
                       "G7 PGII\nEnergy Loans",
                       "China BRI\nEnergy Loans",
                       # "G7 & China Non-Energy Loans",
                       "REdirect-loan",
                       "Global $100B\nAnnual Promise",
                       "2022-27 Capital\nRequirements")
  ) +
  labs(
    # title = "Grant-Based G20 Clean Development Finance (2022-27)",
       x = "") +
  # guides() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10, face = "bold"),
        title = element_text(face = "bold"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = 'bold'),
        legend.position = c(0.1,0.8),
        legend.key.size = unit(4,"mm"),
        legend.spacing = unit(1,"mm"),
        # legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.title.align = 0.5,
        legend.background = element_blank(),
        panel.grid = element_line(color = "grey95")
  ) +
  geom_vline(xintercept = 4.5, color ="grey50", linetype = 1) +
  geom_vline(xintercept = 6.5, color ="grey50", linetype = 1) +
  geom_text(aes(x = 3.5, y = 1400, color = "grey50",
    label = "Capital Supply from\nExisting Initiatives"), color = "grey50", size = 3.3) +
  geom_text(aes(x = 5.5, y = 1400, color = "grey50",
           label = "Capital Demand for\nMultilateral Pledges"), color = "grey50", size = 3.3) +
  geom_text(aes(x = 7, y = 1400, color = "grey50",
           label = "Capital Demand for\nParis Pathways"), color = "grey50", size = 3.3)
  

 ggsave(file = "~/Coal_Finance_Exit/Plots/USD_FinEx/Loan_based_finance_waterfall.png", dpi = "retina", height = 225*0.7, width = 350*0.7, units = "mm")
ggsave(file = "~/Coal_Finance_Exit/Plots/USD_FinEx/Loan_based_finance_waterfall.pdf", dpi = "retina", height = 225, width = 350, units = "mm")


##############################################################################################################################
#### GRANT-BASED FINANCE ###
##############################################################################################################################
q_GLO_REdir_context_grant <- q_GLO_REdir_context %>% 
  filter(!grepl("Loan", data)) %>%
  mutate(scenario = ifelse(grepl("REdirect", scenario), "REdirect-grant", scenario)) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("100B", scenario)) %>%
              mutate(data = "$100B Annual Grants")) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("RE|\\nEnergy", scenario) & grepl("G7|China", region)) %>%
              mutate(data = "Grant-Induced Mobilization",
                     volume = volume * grant_mob_factor))  %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("100B", scenario)) %>%
              mutate(volume = volume * grant_mob_factor) %>%
              # filter(grepl("Goal", data)) %>%
              mutate(data = "Grant-Induced Mobilization")) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("Baseline", data)) %>%
              mutate(scenario = "2022-27 Capital\nRequirements")) %>%
  ungroup() %>%
  # filter(!grepl("Non-Energy", scenario)) %>%
  mutate(scenario = ifelse(grepl("Loans", scenario), gsub("Loans", "Grants", scenario), scenario),
         scenario = factor(scenario,
                           levels = c("G7 PGII\nRE Grants",
                                      "China BRI\nRE Grants",
                                      "G7 PGII\nEnergy Grants",
                                      "China BRI\nEnergy Grants",
                                      # "G7 & China Non-Energy Grants",
                                      "REdirect-grant",
                                      "Global $100B\nAnnual Promise",
                                      "2022-27 Capital\nRequirements"),
                           ordered = T),
         data = factor(data,
                       levels = c("Grant-Induced Mobilization",
                                  "Loan-Induced Mobilization",
                                  "$100B Annual Grants",
                                  "China's 14th FYP (2021-25)",
                                  "Minilateral G7 Finance",
                                  "1.5°C", 
                                  "2°C", 
                                  "G20 REdirect",
                                  "Baseline Expectations"),
                       ordered = T),
         region = factor(region,
                         levels = c("GLO", "G20", "China", "G7"),
                         ordered = T)) %>%
  mutate(group.id = as.numeric(scenario)) %>%
  arrange(scenario, desc(data), desc(region))


waterfall_inv_RE_grant <- q_GLO_REdir_context_grant %>%
  filter(!grepl("Loan", data)) %>%
  filter(!grepl("Requirements|REdirect", scenario)) %>%
  filter(!grepl("100B", scenario)) %>%
  mutate(end.Bar = cumsum(volume),
         start.Bar = c(0, head(end.Bar, -1))) %>%
  group_by(scenario) %>%
  mutate(total.by.x = sum(volume)) %>%
  select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)


waterfall_inv_RE_grant <- waterfall_inv_RE_grant %>%
  bind_rows(q_GLO_REdir_context_grant %>%
              filter(grepl("REdirect", scenario)) %>% 
              filter(!grepl("Loan", data)) %>%
              mutate(end.Bar = cumsum(volume),
                     start.Bar = c(0, head(end.Bar, -1))) %>%
              group_by(scenario) %>%
              mutate(total.by.x = sum(volume)) %>%
              select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
  ) %>%
  bind_rows(q_GLO_REdir_context_grant %>%
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
  bind_rows(q_GLO_REdir_context_grant %>%
              filter(!grepl("Loan", data)) %>%
              filter(grepl("100B", scenario)) %>%
              arrange(scenario, desc(data)) %>%
              mutate(end.Bar = cumsum(volume),
                     start.Bar = c(0, head(end.Bar, -1))) %>%
              group_by(scenario) %>%
              mutate(total.by.x = sum(volume)) %>%
              select(scenario, group.id, region, data, start.Bar, volume, end.Bar, total.by.x)
  ) %>% arrange(scenario, desc(data), desc(region))



### GRANT-INDUCED INVESTMENT ###
ggplot(waterfall_inv_RE_grant) +
  geom_hline(yintercept = as.numeric(
    waterfall_inv_RE_grant %>% filter(grepl("1.5", data)) %>%
      ungroup() %>% select(total.by.x)),
    color = bluegreen[10], 
    linetype = 6) +
  geom_segment(data = waterfall_inv_RE_grant %>% 
                 filter(grepl("Mobil", data)),
               mapping = 
                 aes(x=group.id + 0.25,
                     xend = ifelse(grepl("BRI|REdirect|100B", scenario),
                                   max(waterfall_inv_RE_grant$group.id) + 0.33, 
                                   group.id + 0.75),
                     y=end.Bar,
                     yend=end.Bar),
               # color = "#440154FF",
               color = "black",
               linetype = 2)  +
  geom_text(data = . %>% filter(region!="G7" & grepl("Mobil", data)),
            mapping = aes(
              x = max(waterfall_inv_RE_grant$group.id) + 0.45,
              y = ifelse(grepl("REdirect", scenario),
                                end.Bar - 10,
                                end.Bar),
              alpha = scenario,
              label = paste0(round(end.Bar /
                                     filter(waterfall_inv_RE_grant, grepl("1.5", data))$end.Bar, 2) * 100, "%")),
            color = 'grey40',
            fontface = "italic") +
  geom_rect(data = . %>% filter(grepl("G20|G7|China|100B", data)),
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
  scale_fill_manual(values = block_cols, name = "Public Capital",
                    limits = unique(c(
                      filter(waterfall_inv_RE_grant, grepl("G20|G7|China|100B", data))$data)),
                    #   filter(waterfall_inv_RE_grant, grepl("G20|G7|China|100B", data))$data))
                    guide = guide_legend(order = 1)) +
  new_scale_fill() +
  geom_rect(data = . %>% filter(grepl("Baseline|\u00B0C", data)),
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
  scale_fill_manual(values = block_cols, guide = guide_legend(order = 2),
                    # limits = filter(waterfall_inv_RE_grant, grepl("Baseline|\u00B0C", data))$data,
                    name = "Blended Public/Private Capital") + 
  new_scale_fill() +
  geom_rect(data = . %>% filter(grepl("Mobil", data)),
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
  scale_fill_manual(values = block_cols, guide = guide_legend(order = 3),
                    # limits = unique(c(
                    #   filter(waterfall_inv_RE_grant, grepl("Mobil", data))$data,
                    #   filter(waterfall_inv_RE_grant, grepl("Mobil", data))$data)),
                    name = "Private Capital") + 
  scale_y_continuous(
    name = "5-Year Investment Sums (Billion $2019 USD)",
    expand=c(0,0)
  ) +
  coord_cartesian(ylim = c(0, 1500)) +
  geom_text(
    data = waterfall_inv_RE_grant %>% filter(grepl("Mobil|1.5|Shortfall", data)),
    mapping =
      aes(x = group.id,
          label = round(end.Bar,0),
          y = end.Bar + 25, 
          alpha = scenario),
    color = "grey20",
    fontface = "bold"
  ) +
  geom_text(
    data = waterfall_inv_RE_grant,
    mapping = 
      aes(x = group.id,
          label = as.character(round(volume,0)),
          y = rowSums(cbind(start.Bar,volume/2)),
          # alpha = scenario,
          # color = data
      ),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) + 
  # geom_text(
  #   data = . %>% filter(grepl("China|G20", region) & grepl("Mobil", data)),
  #   mapping = aes(
  #     x = 5.35,
  #     y = end.Bar,
  #     label = as.character(round(end.Bar,0)),
  #     alpha = scenario
  #   ),
  #   color = "#440154FF",
  #   fontface = "bold"
  # ) +
scale_alpha_manual(values = c(
  "G7 PGII\nRE Grants" = 1,
  "China BRI\nRE Grants" = 1,
  "G7 PGII\nEnergy Grants" = 0.5,
  "China BRI\nEnergy Grants" = 0.5,
  "REdirect-grant" = 0.5,
  "Global $100B\nAnnual Promise" = 0.3,
  "G7 & China Non-Energy Grants" = 0.22
),
guide = "none") +
  scale_color_brewer(palette = "Greys", direction = -1) +
  theme_bw() +
  scale_x_discrete(limits = 
                     c("G7 PGII\nRE Grants",
                       "China BRI\nRE Grants",
                       "G7 PGII\nEnergy Grants",
                       "China BRI\nEnergy Grants",
                       # "G7 & China Non-Energy Grants",
                       "REdirect-grant",
                       "Global $100B\nAnnual Promise",
                       "2022-27 Capital\nRequirements")
  ) +
  labs(
    # title = "Grant-Based G20 Clean Development Finance (2022-27)",
    x = "") +
  geom_segment(aes(
    x = 6, xend = 6,
    y = 1400, yend = 1500),
    arrow = arrow(type = "open", length = unit(3,"mm")),
    color = 'white',
    linetype = 1
  ) +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10, face = "bold"),
        title = element_text(face = "bold"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = 'bold'),
        legend.position = c(0.1,0.8),
        legend.key.size = unit(4,"mm"),
        legend.spacing = unit(1,"mm"),
        # legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.title.align = 0.5,
        legend.background = element_blank(),
        panel.grid = element_line(color = "grey95")
  ) +
  geom_vline(xintercept = 4.5, color ="grey50", linetype = 1) +
  geom_vline(xintercept = 6.5, color ="grey50", linetype = 1) +
  geom_text(aes(x = 3, y = 1450,
                label = "Capital Supply from\nExisting Initiatives"), size = 3.3, color = "grey50") +
  geom_text(aes(x = 5, y = 1450,
                label = "Capital Demand for\nMultilateral Pledges"), size = 3.3, color = "grey50") +
  geom_text(aes(x = 7, y = 1450,
                label = "Capital Demand for\nParis Pathways"), size = 3.3, color = "grey50")


ggsave(file = "~/Coal_Finance_Exit/Plots/USD_FinEx/Grant_based_finance_waterfall.png", dpi = "retina", height = 225*0.7, width = 350*0.7, units = "mm")
ggsave(file = "~/Coal_Finance_Exit/Plots/USD_FinEx/Grant_based_finance_waterfall.pdf", dpi = "retina", height = 225, width = 350, units = "mm")

