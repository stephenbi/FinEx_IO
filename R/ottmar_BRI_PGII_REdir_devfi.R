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



## COMPARE GLOBAL REDIRECT VOLUME WITH INITATIVES AND PROMISES ##
q_GLO_REdir_context <- q_REdir_tot_host_reg %>% 
  filter(region=="GLO") %>%
  ungroup() %>%
  # G7 Partnership for Global Infrastructure and Investment
  add_row(region = "G7",
          scenario = "G7",
          data = "G20 Development\nFinance Plans\n(Loan Model)",
          value = 0.75 * 600,
          period = 2025) %>%
  # # EU Global Gateway (included within the PGII)
  # add_row(region = "GLO",
  #         scenario = "EU",
  #         data = "G7 & EU DevFi Plans\n(2021-2027)",
  #         value = 341,
  #         period = 2025) %>%
  # Estimated volume of PGII bank directed toward energy
  add_row(region = "G7",
          data = "G20 Development\nFinance Plans\n(Loan Model)",
          # scenario = "G7",
          scenario = "Energy",
          value = 0.125 * 600,
          period = 2025) %>%
  # Estimated volume of PGII bank directed toward renewables
  add_row(region = "G7",
          data = "G20 Development\nFinance Plans\n(Loan Model)",
          # scenario = "G7",
          scenario = "RE",
          value = 0.125 * 600,
          period = 2025) %>%
  # $100 billion annual finance pledge
  add_row(region = "GLO",
          data = "Global $100B\nAnnual Goal",
          scenario = "Target",
          value = 500,
          period = 2025) %>%
  # Retroactive fulfillment of pledge
  add_row(region = "GLO",
          data = "Global $100B\nAnnual Goal",
          scenario = "Retro",
          value = 900 - (80 + 78 + 71 + 58 + 58 + 61.5 + 52 + 80 + 80),
          period = 2025) %>%
  # China Belt and Road Initiative Outlook
  add_row(region = "China",
          data = "G20 Development\nFinance Plans\n(Loan Model)",
          scenario = "RE",
          value = 0.087331 * 550,
          period = 2025) %>%
  add_row(region = "China",
          data = "G20 Development\nFinance Plans\n(Loan Model)",
          scenario = "Energy",
          value = 0.375 * 550 - (0.087331 * 550),
          period = 2025) %>%
  add_row(region = "China",
          data = "G20 Development\nFinance Plans\n(Loan Model)",
          scenario = "BRI",
          value = 550 * 0.76,
          period = 2025) %>%
  group_by(scenario,region,data) %>%
  summarise(volume=sum(value),.groups="keep") %>%
  ungroup() %>%
  mutate(volume = ifelse(grepl("G7", region) & grepl("RE|Energy", scenario), volume/(1 + loan_mob_factor), volume))
  # mutate(volume = ifelse(grepl("BRI", scenario), volume * (1 + loan_mob_factor), volume))


q_GLO_REdir_context <- q_GLO_REdir_context %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("G20", data)) %>%
              mutate(data = "G20 Development\nFinance Plans\n(Grant Model)"))
#                      volume = ifelse(grepl("RE|Energy", scenario),
#                                      volume * (1+grant_mob_factor) / (1+loan_mob_factor),
#                                      volume)))

q_GLO_REdir_context <- q_GLO_REdir_context %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("G20", data)) %>%
              mutate(volume = ifelse(grepl("Grant", data), volume*grant_mob_factor, volume * loan_mob_factor)))

  # bind_rows(q_GLO_REdir_context %>% filter(grepl("Global", data)) %>% 
  #             mutate(volume = volume*1.66,
  #                    data = "Global $100B\nAnnual Goal\n(Public Only)")) %>%
  # bind_rows(q_GLO_REdir_context %>% filter(grepl("Global", data)) %>% 
  #             mutate(volume = volume*4.5,
  #                    data = "Global $100B\nAnnual Goal\n(Public Grants)")) %>%
  # add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "2022-27 REdirect") %>% mutate(data = 'Global $100B\nAnnual Goal')) %>%
  # add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "2022-27 REdirect") %>%  mutate(data = 'G20 Development\nFinance Plans\n(Loan Model)')) %>% 
  # add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "2022-27 REdirect") %>%  mutate(data = 'G20 Development\nFinance Plans\n(Grant Model)')) %>% 
  # filter(!grepl("est.", fill)) %>%
  # mutate(data = ifelse(grepl("Plan", data), "G20 Development\nFinance Plans", data)) %>%
  # rename(variable = data) %>%
  mutate(fill = factor(interaction(scenario,region),
                       levels = c("REdirect_oecdMob.GLO", "REdirect_noMob.GLO", 
                                  "Retro.GLO", "Target.GLO",
                                  "G7.G7", "BRI.China", "Energy.G7", "Energy.China", "RE.G7","RE.China", "PPCA.GLO"), 
                       # "SSP2EU-PkBudg1150", "SSP2EU-PkBudg500"),
                       labels = c("Mobilized Private Capital\n(2022-27)\n", "G20 Public REdirect\n(2022-27)\n",
                                  "$100B Shortfall\n(2013-21)\n", 
                                  "$100B Target\n(2022-25)\n",
                                  "G7 PGII\n(2022-27)\n",
                                  "China BRI\n(2021-25)\n",
                                  # "G7 Partnership for Global Infrastructure &\nInvestment (2022-27)\n",
                                  # "China Belt & Road Initiative\n(2021-25)\n",
                                  "G7 PGII (Energy est.)\n",
                                  "China BRI (Energy est.)\n",
                                  "G7 PGII (RE est.)\n",
                                  "China BRI (RE est.)\n", "Baseline Expectations\n(2022-27)\n")),
         data = as.character(data)) %>%
  filter(scenario!="EU") %>%
  ungroup()




ggplot(q_GLO_REdir_context) +
  geom_errorbar(data = mob_range %>% filter(region=="GLO"),
                mapping = aes(x = "2022-27 REdirect",
                              ymin = REdir_LO,
                              ymax = REdir_HI),
                width = 0.5,
                color = "darkgreen",
                size = 0.7) +
  geom_col(data = . %>% filter(!grepl("PkBud",scenario) & data!="PPCA_2020"),
           mapping=aes(x=data,
                       y=volume,
                       alpha = scenario,
                       group = fill,
                       fill = fill),
           width=0.9,
           position="stack") +
  # geom_hline(data = . %>% filter(grepl("PPCA_2020",data)),
  #            mapping = aes(linetype = scenario,
  #                          yintercept = volume),
  #            color = "white") + 
  geom_hline(data = . %>% filter(grepl("PkBudg",data)),
             mapping = aes(linetype = scenario,
                           yintercept = volume),
             color = "mediumblue",
             alpha = 0.7) +
  geom_text_repel(mapping = aes(
    x = 1,
    y = cumval,
    color = factor(scenario,
                   levels = c("REdirect_oecdMob", "REdirect_noMob", "PPCA"),
                   ordered = T),
    label = round(volume,0)),
    data = . %>% 
      filter(data=="2022-27 REdirect") %>% 
      group_by(region) %>%
      mutate(cumval = cumsum(volume)),
    nudge_x = -0.55,
    min.segment.length = 100,
    direction = "y",
    size = 3.75, 
    box.padding = 0.1,
    force = 0.5,
    hjust = 0.2,
    fontface = "bold"
  ) +
  # geom_text(data = . %>% filter(data == "PPCA_2020"),
  #           mapping = aes(x = 2,
  #                         y = volume),
  #           label = "2017-22 RE Investment\n(REMIND estimate)",
  #           fontface = "bold.italic",
  #           color = "white",
  #           size = 4.2) +
  geom_text(mapping = aes(
    x = 1,
    y = REdir_HI,
    label = round(REdir_HI - REdir_M,0)),
    data = mob_range %>% filter(region=="GLO"),
    nudge_x = -0.52,
    check_overlap = T,
    color = "darkgreen",
    size = 3.3, fontface = "bold"
  ) +
  scale_linetype_manual(values = c("SSP2EU-PkBudg1150" = 3, "SSP2EU-PkBudg500" = 4, "PPCA" = 2),
                        breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                        labels = c("1.5\u00B0C (2022-27)", "2\u00B0C (2022-27)"),
                        name = "REMIND Benchmarks"#,
                        # guide = "none"
  ) +
  # annotate(geom = "text",
  #          x = "2022-27 REdirect",
  #          y = mean(c(mob_range$REdir_HI[which(mob_range$region=="GLO")], 98 + mob_range$REdir[which(mob_range$region=="GLO")])),
  #          # as.numeric(select(
  #          #   filter(q_GLO_REdir_context, region == "GLO" & data == "PkBudg1150"), volume)))),
  #          # q_GLO_REdir_context$volume[which(q_GLO_REdir_context$region=="GLO")])),
  #          label = "Grant-induced\nMobilization",
  #          fontface = "bold.italic",
  #          color = "darkgreen",
  #          angle = "90",
  #          size = 3.9) +
  scale_fill_manual(values = setNames(c(accent[-c(3,7)],rep(accent[c(6,8)],2),accent[3]), 
                                      nm = levels(q_GLO_REdir_context$fill)),
                    breaks = levels(q_GLO_REdir_context$fill)[-c(7:10)],
                    name = "Source of Finance\n(Disbursal Horizon)") +
  # scale_linetype_manual(values = c("PPCA_2020" = 2,
  #                                  "2022-27 REdirect" = 1),
  #                       guide = "none") +
  scale_color_brewer(palette="Accent", direction = 1, guide = "none") +
  scale_alpha_manual(values = c("G7" = 0.15, "BRI" = 0.15, "Energy" = 0.5, "RE" = 1),
                     breaks = rev(c("G7", "Energy", "RE")),
                     name = "Estimated Allocation of \nDevelopment Finance",
                     labels = rev(c("Other", "Energy", "Renewables"))) +
  labs(x="",y="Billion USD (2019$)"
       # title="REdirect in Context"
  ) +
  theme_bw() +
  theme(text = element_text(size = 13),
        # plot.title = element_text(size=16,face="bold"),
        # strip.text = element_text(size=13),
        # axis.text = element_text(size=8),
        # axis.title = element_text(size=8),
        # legend.text = element_text(size=7),
        # legend.title = element_text(size=7,face="bold"),
        legend.background = element_blank()
        # legend.key.height = unit(25,"pt")
        # ,
        # legend.position = c(0.82,0.15)
  )

# ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_hosts_REdir_Mob_bbUSD_in_context_stackBar_largeFont_GRANT_noBase.pdf",width = 10, height = 8, units = "in", dpi="retina")  
# 
# ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_hosts_REdir_Mob_bbUSD_in_context_stackBar_largeFont_GRANT_noBase.png",width = 10, height = 8, units = "in", device = "png",dpi="retina")  

