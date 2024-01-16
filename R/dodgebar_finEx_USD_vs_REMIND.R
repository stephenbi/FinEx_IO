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

# FinEx Host Regions
regFinEx <- getItems(mag_precon_REdir_oecd_nat_mob,1)[which(mag_precon_REdir_oecd_nat_mob>0)]

regOrd <- c("GLO", "OAS", "SSA", "MEA", "NEU", "CAZ", "REF", "LAM") 
regOrder <- c("All FinEx Hosts", "S & E Asia (exc. CN, IN, JP)","Sub-Saharan Africa","Middle East & N Africa","Non-EU Europe",
              "Canada, Australia & NZ","Former Soviet Union","Latin America")
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
# ssp2eu_npi <- read.report("H:/FinEx/runs/REMIND_generic_PPCAcop26-limSE1p5x_withoutPlus.mif",as.list=F)
ssp2eu_npi <- read.report("H:/FinEx/runs/REMIND_generic_PPCAcop26-coalElTeNoCCS_withoutPlus.mif",as.list=F)
ssp2eu_ndc <- read.report("H:/FinEx/runs/REMIND_generic_SSP2EU-NDC-Neutral_withoutPlus.mif",as.list=F)

Budg500 <- read.report("H:/FinEx/runs/REMIND_generic_SSP2EU-PkBudg500_withoutPlus.mif",as.list=F)

Budg1150 <- read.report("H:/FinEx/runs/REMIND_generic_SSP2EU-PkBudg1150_withoutPlus.mif",as.list=F)

ppca_static <- read.report("H:/FinEx/runs/REMIND_generic_PPCAcop26-coalElTeNoCCS_withoutPlus.mif",as.list=F)
ppca_50p <- read.report("H:/FinEx/runs/REMIND_generic_PPCA-coalElTeNoCCS-nonoecd50p_withoutPlus.mif",as.list=F)

RE_inv_vars <- c("Energy Investments|Elec|Grid|VRE support",
                 "Energy Investments|Elec|Storage",
                 "Energy Investments|Elec|Solar",
                 "Energy Investments|Elec|Wind")
RE_inv_vars <- paste(RE_inv_vars,"(billion US$2005/yr)")

# ren21_REcap <- read_excel(path = "C:/Users/stephenb/Documents/Coal_Finance_Exit/Data/REN21_GSR2021_Data_Pack.xlsx",sheet = "Data 47",col_types = "numeric",range="B4:O14",col_names = T,)


inflation_05_to_19 <- 1.31

# npiOld_RE_inv <- ssp2eu_npi[,c(2020,2025),RE_inv_vars] * inflation_05_to_19
# 
# q_npiOld_RE_inv <- as.quitte(npiOld_RE_inv[regFinEx,,]) %>%
#   mutate(unit="billion US$2019/yr") %>%
#   mutate(scenario=ifelse(period==2020,paste0(scenario,"_2020_old"),paste0(scenario,"_2025_old")))

npi_RE_inv <- ssp2eu_npi[,c(2020,2025),RE_inv_vars] * inflation_05_to_19

ppca_static_RE_inv <- ppca_static[,c(2020,2025),RE_inv_vars] * inflation_05_to_19

q_ppca_static_RE_inv <- as.quitte(mbind(
  ppca_static_RE_inv[regFinEx,,],
  toolAggregate(ppca_static_RE_inv[regFinEx,,], rel = data.frame(from=regFinEx,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr") %>%
  mutate(scenario="PPCA") %>%
  mutate(scenario=ifelse(period==2020,paste0(scenario,"_2020"),paste0(scenario,"_2025")))


b1150_RE_inv <- Budg1150[,2025,RE_inv_vars] * inflation_05_to_19

q_b1150_RE_inv <- as.quitte(mbind(
  b1150_RE_inv[regFinEx,,],
  toolAggregate(b1150_RE_inv[regFinEx,,], rel = data.frame(from=regFinEx,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr")

b500_RE_inv <- Budg500[,2025,RE_inv_vars] * inflation_05_to_19

q_b500_RE_inv <- as.quitte(mbind(
  b500_RE_inv[regFinEx,,],
  toolAggregate(b500_RE_inv[regFinEx,,], rel = data.frame(from=regFinEx,to="GLO"),NULL))) %>%
  mutate(unit="billion US$2019/yr")



reg_REdir_mob <- as.quitte(mag_precon_REdir_oecd_nat_mob) %>%
  filter(value>0) %>%
  group_by(region) %>%
  summarise(REdir = value, .groups = "keep")

reg_REdir_HI_mob <- precon_REdir_HI_oecd_nat_mob %>%
            rename(region=Region) %>%
            group_by(region) %>%
            summarise(REdir_HI = 
                        sum(REdir_oecd_mob),.groups="keep") %>%
  filter(REdir_HI>0) %>%
  left_join(reg_REdir_mob) %>%
  summarise(REdir_HI = REdir_HI - REdir, .groups = 'keep')

reg_REdir_LO_mob <- precon_REdir_LO_oecd_nat_mob %>%
  rename(region = Region) %>%
  group_by(region) %>%
  summarise(REdir_LO = sum(REdir_oecd_mob),.groups="keep") %>%
  filter(REdir_LO>0) %>%
  left_join(reg_REdir_mob) %>%
  summarise(REdir_LO = REdir_LO - REdir, .groups = 'keep')



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
  bind_rows(q_ppca_static_RE_inv) %>%
  mutate(data = 
           ifelse(model != "REMIND" | (grepl("PPCA",scenario) & period==2025),
                  "2025 REdirect",
                  gsub("SSP2EU-","",scenario))) %>%
  bind_rows(q_b1150_RE_inv) %>%
  bind_rows(q_b500_RE_inv)


### ADD REDIRECT AND MOBILIZATION TO NPI ###
q_REdir_tot_host_reg <- q_finEx_inv_host_reg %>%
  select(-variable) %>%
  bind_rows(
    q_finEx_inv_host_reg %>%
      filter(region!="GLO",
             data == "2025 REdirect") %>%
      group_by(region,data,period) %>%
      summarise(value = sum(value),
                .groups="keep") %>%
      mutate(scenario = "Total",
             model = "All",
             unit = "billion US$2019/yr")) %>%
  group_by(region)  %>%
  left_join(reg_REdir_LO_mob, by = "region") %>% 
  mutate(REdir_LO = ifelse(scenario %in% c("Total","REdirect_oecdMob"),
                           REdir_LO,                         ## MAKE ERROR BARS FOR TOTAL COLUMN
                           0)) %>%
  left_join(reg_REdir_HI_mob, by = "region") %>% 
  mutate(REdir_HI = ifelse(scenario %in% c("Total","REdirect_oecdMob"),
                           REdir_HI,
                           0)) %>%
  mutate(model = 
           ifelse(model=="REMIND",
                  "REMIND",
                  ifelse(grepl("oecdMob",scenario),
                               "Private Mobilization",
                               "Public G20 Finance"))) %>%
  mutate(scenario =
           ifelse(grepl("PPCA",scenario),
                  gsub("_202[0-9]","",scenario),
                  scenario)) %>%
  filter(region!="GLO")


  #          ifelse(data == "2025 REdirect" & scenario == "Total",
  #                 sum(value,REdir_HI),
  #                 REdir_HI)) %>%
  # mutate(REdir_LO = 
  #          ifelse(data == "2025 REdirect" & scenario == "Total",
  #                 sum(value,REdir_LO),
  #                 REdir_LO))

# 
# mob_range <- q_REdir_tot_host_reg %>%
#   filter(data == "2025 REdirect" & !grepl("oecdMob",scenario)) %>%
#   replace(is.na(.),0) %>%
#   group_by(region, data) %>%
#   summarise(REdir_HI=sum(value,REdir_HI),
#             REdir_LO=sum(value,REdir_LO),
#             .groups="keep")
  
# q_REdir_tot_host_reg <- aggregate_map(q_REdir_tot_host_reg,
#                                       mapping = data.frame(region = unique(q_REdir_tot_host_reg$region),
#                                                            New_region = "GLO"),
#                                       by = c("region"),
#                                       subset2agg = "value",
#                                       forceAggregation = T,
#                                       only.new = FALSE,
#                                       weight = NULL)
  
  


ggplot(q_REdir_tot_host_reg %>%
         # group_by(region,data,scenario,model) %>%
         # summarise(volume=sum(value),.groups="keep") %>%
         mutate(volume = value,
                global = ifelse(region=="GLO",
                                "All FinEx Hosts",
                                "Regions"))) +
  geom_col(data = . %>% filter(data=="2025 REdirect" & region!="GLO"),
           mapping=aes(x=factor(region,levels=regOrd,labels = regOrder),
                y=volume,
                fill = factor(scenario,
                              levels = c("PPCA", "REdirect_noMob", "REdirect_oecdMob", "Total"), 
                              labels = c("Baseline Expectations\n(PPCA-Static)", "Mobilized Private Capital", "Public G20 REdirect", "2022-27 Total"))),
    width=0.9,
    position="dodge") +
  geom_point(
    data = . %>% filter(data != "2025 REdirect" & !grepl("PPCA",data) & region!="GLO"),
    mapping = aes(x = factor(region,levels=regOrd,labels = regOrder),
                  y = volume,
                  shape = scenario,
                  color = scenario),
    size = 3  ) +
  scale_shape_manual(values = c("SSP2EU-PkBudg1150" = 1, "SSP2EU-PkBudg500" = 13),
                     breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                     labels = c("1.5\u00B0C (2025)", "2\u00B0C (2025)"),
                     name = "REMIND Benchmarks") +
  scale_color_manual(values = c("SSP2EU-PkBudg500"="black",
                                "SSP2EU-PkBudg1150"="black"
                       # c("SSP2EU-PkBudg500"="#CC9900",
                       #          "SSP2EU-PkBudg1150"="#0099CC"
                                ),
                     breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                     labels = c("1.5\u00B0C (2025)", "2\u00B0C (2025)"),
                     name = "REMIND Benchmarks") +
  guides(shape = guide_legend(override.aes = list(linetype = 0))) +
  new_scale_color() +
  geom_errorbar(data = . %>% filter(data=="PPCA_2020" & region!="GLO"),
                mapping = aes(x = factor(region,levels=regOrd,labels = regOrder),
                              ymin = volume,
                              ymax = volume,
                              linetype = data,
                              color = data),
                color = "grey10") +
  geom_errorbar(data = . %>% filter(region!="GLO" & data == "2025 REdirect" & scenario %in% c("Total", "REdirect_oecdMob")),
                mapping = aes(x = factor(region,levels=regOrd,labels = regOrder),
                              ymin = volume + REdir_LO,
                              ymax = volume + REdir_HI,
                              linetype = data,
                              color = data)) +
  # facet_grid(. ~ global, scales="free",space="free_x") +
  scale_fill_brewer(palette="Accent", name="RE Investment Volume", direction = 1) +
  scale_linetype_manual(values = c("PPCA_2020" = 2,
                                   "2025 REdirect" = 1)) +
  scale_color_manual(values = c("PPCA_2020" = "grey10",
                                "2025 REdirect" = "grey40")) +
  labs(x="Region",y="Billion USD (2019$)\n"
       # ,
       # title="2025 RE Investment in REdirect vs. Mitigation Scenarios"
       ) +
  theme_minimal() +
  theme(plot.title = element_text(size=14,face="bold"),
        strip.text = element_text(size=13),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12,face="bold"),
        legend.background = element_blank()
        # ,
        # legend.position = c(0.82,0.15)
        )

ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/regi_REdir_Mob_bbUSD_vs_2025_REM_scens_stackBar.png",width = 12, height = 9, units = "in", device = "png",dpi="retina")  


## COMPARE GLOBAL REDIRECT VOLUME WITH INITATIVES AND PROMISES ##
q_GLO_REdir_context <- q_REdir_tot_host_reg %>% 
  filter(region=="GLO") %>%
  ungroup() %>%
  # G7 Partnership for Global Infrastructure and Investment
  add_row(region = "G7",
          scenario = "G7",
          data = "Development\nFinance Plans",
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
          data = "Development\nFinance Plans",
          # scenario = "G7",
          scenario = "Energy",
          value = 0.125 * 600,
          period = 2025) %>%
  # Estimated volume of PGII bank directed toward renewables
  add_row(region = "G7",
          data = "Development\nFinance Plans",
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
          data = "Development\nFinance Plans",
          scenario = "RE",
          value = 0.087331 * 550,
          period = 2025) %>%
  add_row(region = "China",
          data = "Development\nFinance Plans",
          scenario = "Energy",
          value = 0.385 * 550 - (0.087331 * 550),
          period = 2025) %>%
  add_row(region = "China",
          data = "Development\nFinance Plans",
          scenario = "BRI",
          value = 550 - 0.24 * 550,
          period = 2025) %>%
  group_by(scenario,region,data) %>%
  summarise(volume=sum(value),.groups="keep") 
  

  
q_GLO_REdir_context <- q_GLO_REdir_context %>%
  ungroup() %>%
  add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "2025 REdirect") %>% mutate(data = 'Global $100B\nAnnual Goal')) %>%
  add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "2025 REdirect") %>%  mutate(data = 'Development\nFinance Plans')) %>% 
  # filter(!grepl("est.", fill)) %>%
  # mutate(data = ifelse(grepl("Plan", data), "Development\nFinance Plans", data)) %>%
  # rename(variable = data) %>%
  mutate(fill = factor(interaction(scenario,region),
                       levels = c("REdirect_oecdMob.GLO", "REdirect_noMob.GLO", 
                                  "Retro.GLO", "Target.GLO",
                                  "G7.G7", "BRI.China", "Energy.G7", "Energy.China", "RE.G7","RE.China", "PPCA.GLO"), 
                       # "SSP2EU-PkBudg1150", "SSP2EU-PkBudg500"),
                       labels = c("Mobilized Private Capital\n(2022-27)\n", "G20 Public REdirect\n(2022-27)\n",
                                  "$100B Shortfall\n(2013-21)\n", "$100B Target\n(2022-27)\n",
                                  "G7 PGII\n(2022-27)\n",
                                  "China BRI\n(2021-25)\n",
                                  # "G7 Partnership for Global Infrastructure &\nInvestment (2022-27)\n",
                                  # "China Belt & Road Initiative\n(2021-25)\n",
                                  "G7 PGII (Energy est.)\n",
                                  "China BRI (Energy est.)\n",
                                  "G7 PGII (RE est.)\n",
                                  "China BRI (RE est.)\n", "Baseline Expectations\n(2022-27)\n"))) %>%
  filter(scenario!="EU") %>%
  ungroup()

RColorBrewer::brewer.pal(n=8,"Accent") -> accent

ggplot(q_GLO_REdir_context %>% filter(data!="PPCA_2020")) +
  geom_col(data = . %>% filter(!grepl("PkBud",scenario)),
           mapping=aes(x=factor(data,
                                levels=c("2025 REdirect", "Global $100B\nAnnual Goal", "Development\nFinance Plans")),
                       # levels=c("2025 REdirect", "Global $100B\nAnnual Goal", "Western Plans\n(2022-2027)", "Chinese Plans\n(2021-2025)")),
                       # factor(data,
                       #   levels = c("PPCA_2020", "2025 REdirect", "PkBudg1150", "PkBudg500"),
                       #   labels = c("2020\nReference", "2025\nREdirect", "2025\n2\u00B0C", "2025\n1.5\u00B0C")),
                       y=volume,
                       alpha = scenario,
                       group = fill,
                       fill = fill),
                                                # "G7 Partnership for Global\nInfrastructure & Investment",
                                                # "China's Belt & Road Initiative"))),
           # "REMIND 2\u00B0C Scenario", "REMIND 1.5\u00B0C Scenario"))),
           # fill=factor(scenario,
           #             levels = rev(c("PPCA", "PPCA_2025", "REdirect_noMob", "REdirect_oecdMob", "SSP2EU-PkBudg1150","SSP2EU-PkBudg500")),
           #             labels = rev(c("Reference", "Current Policies (pre-FinEx)", "Public G20 Finance", "Private Mobilization", "2\u00B0C", "1.5\u00B0C")))),
           width=0.9,
           position="stack") +
  geom_hline(data = . %>% filter(grepl("PkBudg",data)),
             mapping = aes(linetype = scenario,
                           yintercept = volume)) +
  # geom_point(
  #   data = . %>% filter(grepl("PkBudg",data)),
  #   mapping = aes(x = "2025 REdirect",
  #                 y = volume,
  #                 shape = scenario,
  #                 color = scenario),
  #   size = 6  ) +
  scale_linetype_manual(values = c("SSP2EU-PkBudg1150" = 3, "SSP2EU-PkBudg500" = 2),
                     breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                     labels = c("1.5\u00B0C (2022-27)", "2\u00B0C (2022-27)"),
                     name = "REMIND Benchmarks"#,
                     # guide = "none"
                     ) +
  # annotate(aes(x = 0.,
  #               y = volume,
  #               label = c("2\u00B0C", "1.5\u00B0C")),
  #           data = . %>% filter(grepl("PkBudg",data))) +
  # scale_color_manual(values = c("SSP2EU-PkBudg500"="chartreuse4",
  #                               "SSP2EU-PkBudg1150"="magenta1"),
                       # c("SSP2EU-PkBudg500"="#CC9900",
                       #          "SSP2EU-PkBudg1150"="#0099CC"),
                     # breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                     # labels = c("1.5\u00B0C (2022-27)", "2\u00B0C (2022-27)"),
                     # name = "REMIND Benchmarks") +
  # new_scale_color() +
  # geom_errorbar(data = . %>% filter(data=="PPCA_2020"),
  #               mapping = aes(x = "2025 REdirect",
  #                             ymin = volume,
  #                             ymax = volume,
  #                             linetype = data,
  #                             color = data),
  #               color = "grey10") +
  geom_errorbar(data = mob_range %>% filter(region=="GLO"),
                mapping = aes(x = "2025 REdirect",
                              ymin = REdir_LO,
                              ymax = REdir_HI,
                              color = data),
                width = 0.5,
                size = 0.7) +
  annotate(geom = "text",
           x = "2025 REdirect",
           y = 850,
           label = "Mobilization\nUncertainty",
           fontface = "italic",
           color = "forestgreen",
           angle = "90") +
  scale_fill_manual(values = setNames(c(accent[-c(3,7)],rep(accent[c(6,8)],2),accent[3]), 
                                      nm = levels(q_GLO_REdir_context$fill)),
                    breaks = levels(q_GLO_REdir_context$fill)[-c(7:10)],
                    name = "Source of Finance") +
  # scale_linetype_manual(values = c("PPCA_2020" = 2,
  #                                  "2025 REdirect" = 1),
  #                       guide = "none") +
  scale_color_manual(values = c("PPCA_2020" = "grey2",
                                "2025 REdirect" = "forestgreen"),
                     guide = "none") +
  scale_alpha_manual(values = c("G7" = 0.15, "BRI" = 0.15, "Energy" = 0.5, "RE" = 1),
                     breaks = rev(c("G7", "Energy", "RE")),
                     name = "Estimated Allocation of \nDevelopment Finance",
                     labels = rev(c("Other", "Energy", "Renewables"))) +
  labs(x="",y="Billion USD (2019$)",
       title="REdirect in Context") +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(size=16,face="bold"),
        # strip.text = element_text(size=13),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14,face="bold"),
        legend.background = element_blank()
        # legend.key.height = unit(25,"pt")
        # ,
        # legend.position = c(0.82,0.15)
  )

ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/GLO_REdir_Mob_bbUSD_in_context_stackBar_largeFont.png",width = 10, height = 8, units = "in", device = "png",dpi="retina")  

