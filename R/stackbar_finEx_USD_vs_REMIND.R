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
library(countrycode)
library(ggrepel)

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


block_cols <- c("Grant-Induced Mobilization" = blues[4],
                "Loan-Induced Mobilization" = blues[5],
                "$100B Annual Loans" = greens[3],
                "$100B Annual Grants" = greens[2],
                "China's 14th FYP (2021-25)" = greens[7],
                "Minilateral G7 Finance" = greens[8],
                "1.5°C" = bluegreen[10],
                "2°C" = bluegreen[9], 
                "PkBudg500" = bluegreen[10],
                "PkBudg1150" = bluegreen[9], 
                "G20 REdirect" = greens[5],
                "Baseline Expectations" = bluegreen[8],
                "Baseline Expectations\n(PPCA-observed scenario)" = bluegreen[8],
                'PPCA' = bluegreen[8],
                "REdirect_noMob" = greens[5],
                "REdirect_oecdMob" = blues[5],
                "Grant-Induced Private Mobilization" = blues[4],
                "Loan-Induced Private Mobilization" = blues[5])


map <- toolGetMapping("regionmappingH12.csv",type="regional")

# setConfig(mainfolder = "C:/Users/stephenb/Downloads/madrat_main/")
# path <- paste0(getConfig("sourcefolder"),"/PPCA/")
setConfig(mainfolder = "Z:/inputdata/")

# ssp2_npi <- read.report(paste0(path,"REMIND_generic_SSP2-NPi-covid-cfCHA52.mif"),as.list=F)
# ssp2eu_npi <- read.report("H:/FinEx/runs/REMIND_generic_PPCAcop26-coalElTeNoCCS_withoutPlus.mif",as.list=F)
ssp2eu_npi <- frozen[,,"PPCAcop26-jul21"]
  
Budg500 <- budget[,,"SSP2EU-PkBudg500"]

Budg1150 <- budget[,,"SSP2EU-PkBudg1150"]

ppca_static <- frozen[,,"PPCAcop26-jul21"]
# ppca_50p <- dpe_full[,,"PPCA-jul21-nonoecd"]

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


reg_REdir_M_mob <- precon_REdir_oecd_nat_mob %>%
  mutate(region = ifelse(CountryCode=="GLO", Region, RegionCode)) %>%
  group_by(region) %>%
  summarise(REdir_M = 
              sum(REdir_oecd_mob),.groups="keep")

reg_REdir_HI_mob <- precon_REdir_HI_oecd_nat_mob %>%
            rename(region=Region) %>%
            group_by(region) %>%
            summarise(REdir_HI = 
                        sum(REdir_oecd_mob),.groups="keep")

reg_REdir_LO_mob <- precon_REdir_LO_oecd_nat_mob %>%
  rename(region = Region) %>%
  group_by(region) %>%
  summarise(REdir_LO = sum(REdir_oecd_mob),.groups="keep")


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
  left_join(reg_REdir_M_mob, by = "region") %>% 
  mutate(REdir_M = ifelse(grepl("REdirect_noMob", scenario),
                          REdir_M,
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
                  "G20 REdirect",
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

# %>%
  # filter(region!="GLO") %>%
  # bind_rows(
  #   q_REdir_tot_host_reg %>%
  #     filter(region!="GLO") %>%
  #     group_by(model,scenario,data,unit,period) %>%
  #     summarise(value = sum(value),
  #               .groups="keep") %>%
  #     mutate(variable = "Total") %>%
  #     mutate(region="GLO"))


mob_range <- q_REdir_tot_host_reg %>%
  filter(data == "G20 REdirect" & !grepl("oecdMob",scenario)) %>%
  replace(is.na(.),0) %>%
  group_by(region, data) %>%
  summarise(REdir_HI=sum(value,REdir_HI),
            REdir_LO=sum(value,REdir_LO),
            REdir_M=sum(value,REdir_M),
            REdir=sum(value),
            .groups="keep")
  
#############################
### FACETED STACKBAR PLOT ###
#############################

ggplot(q_REdir_tot_host_reg %>%
         group_by(region,data,scenario,model) %>% 
         summarise(volume=sum(value),.groups="keep") %>%
         mutate(global = ifelse(region=="GLO",
                                "All FinEx Hosts",
                                "Regions")) %>%
         filter(region!="GLO")) +
  facet_wrap(. ~ factor(region,levels=regOrd,labels = regOrder), scales = "free") +
  geom_rect(data = mob_range %>% filter(region!="GLO"),
                mapping = aes(x = 1,
                              xmin = 0.7,
                              xmax = 1.3,
                              ymin = REdir_LO,
                              ymax = REdir_HI),
                color = 'black',
            linewidth = 0.2,
            fill = blues[4]
            ) +
  geom_col(data = . %>% filter(data=="G20 REdirect" & region!="GLO"),
           mapping=aes(x=1,
                       y=volume,
                       fill = factor(scenario,
                                     levels = c("REdirect_oecdMob", "REdirect_noMob", "PPCA"),
                                     labels = c("Loan-Induced Private Mobilization", "G20 REdirect", "Baseline Expectations\n(PPCA-observed scenario)"))),
           width=0.6,
           color = 'black',
           linewidth = 0.2,
           position="stack") +
  geom_point(
    data = . %>% filter(data != "G20 REdirect" & !grepl("PPCA",data) & region!="GLO"),
    mapping = aes(x = 1,
                  y = volume,
                  shape = scenario),
    size = 3,
    color = 'black') +
  scale_fill_manual(name="RE Investment Source", 
                    limits = c("Grant-Induced Private Mobilization", "Loan-Induced Private Mobilization", "G20 REdirect", "Baseline Expectations\n(PPCA-observed scenario)"),
                    values = block_cols) +
  scale_color_manual(guide = "none", 
                     values = block_cols) +
  geom_text(mapping = aes(
    x = 1,
    y = REdir_HI,
    label = round(REdir_HI - REdir_M,0)),
    data = mob_range %>% filter(region!="GLO"),
    nudge_x = 0.341,
    hjust = 0.3,
    check_overlap = T,
    color = blues[5],
    size = 3, fontface = "bold"
  ) +
  # geom_text(mapping = aes(
  #   x = 1,
  #   y = REdir_LO,
  #   label = round(REdir_LO,0)),
  #   data = mob_range %>% filter(region!="GLO"),
  #   nudge_x = 0.341,
  #   hjust = 0.3,
  #   check_overlap = T,
  #   color = "darkgreen",
  #   size = 3, fontface = "bold",
  # ) +
  scale_shape_manual(values = c("SSP2EU-PkBudg1150" = 1, "SSP2EU-PkBudg500" = 13),
                     breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                     labels = c("1.5\u00B0C", "2\u00B0C"),
                     name = "REMIND Benchmarks") +
  geom_errorbar(data = . %>% filter(data=="PPCA_2020" & region!="GLO"),
                mapping = aes(x = 1,
                              ymin = volume,
                              ymax = volume,
                              linetype = data,
                              color = data),
                color = "white",
                width=0.6,
                linewidth=0.6,
                alpha = 0.5) +
  geom_text_repel(mapping = aes(
    x = 1,
    y = cumval,
    color = factor(scenario,
                   levels = c("REdirect_oecdMob", "REdirect_noMob", "PPCA"),
                   ordered = T),
    label = round(volume,0)),
    # ifelse(grepl("PPCA",scenario), "", round(volume,0))),
    data = . %>% 
      filter(data=="G20 REdirect" & region!="GLO") %>% 
      group_by(region) %>%
      mutate(cumval = cumsum(volume)),
    nudge_x = -0.376,
    min.segment.length = 100,
    direction = "y",
    size = 3, 
    box.padding = 0.1,
    force = 0.5,
    hjust = 0.2,
    fontface = "bold"
  ) +
  # geom_text(data = mob_range %>% filter(region == "NEU") %>% ungroup(),
  #           mapping = aes(x = 1,
  #                         y = mean(c(REdir_HI, REdir_LO))),
  #          label = "Grant-induced\nMobilization",
  #          fontface = "bold.italic",
  #          color = blues[5],
  #          angle = "90",
  #          size = 3) +
  geom_text(data = . %>% filter(region == "LAM",
                                data == "PPCA_2020"),
            mapping = aes(x = 1,
                          y = volume),
            label = "2018-22 RE Investment\n(REMIND estimate)",
            fontface = "bold.italic",
            color = "white",
            # angle = "0",
            size = 2.9) +
  # scale_fill_brewer(palette="Greens", name="RE Investment Source", direction = 1, guide = guide_legend(order = 1)) +
  # scale_x_continuous(expand = c(0.99, 1.01)) +
  scale_linetype_manual(values = c("PPCA_2020" = 5,
                                   "G20 REdirect" = 1),
                        guide = "none",
                        breaks = c("PPCA_2020"
                                   # "G20 REdirect"
                                   ),
                        name = "REMIND Reference",
                        labels = c("2020 RE Investment"
                                   # "Mobilization Uncertainty"
                                   )
                        # guide = guide_legend(order = 3)
                        ) +
  labs(y="2023-27 RE Investment (Billion $2019 USD)\n") +
  expand_limits(x = c(0.62, 1.38)) +
  theme_bw() +
  theme(plot.title = element_text(size=8,face="bold"),
        text = element_text(size = 7),
        strip.text = element_text(size=9.5, face = "plain"),
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size=9),
        # axis.text.x = element_text(angle = 45,hjust = 1),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=9),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9,face="bold"),
        legend.background = element_blank(),
        legend.position = c(0.6667,0.15),
        legend.box = "horizontal"
  ) #+
  # guides(fill = )

ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_countries_FACETED_regi_REdir_Mob_bbUSD_vs_2025_REM_scens_stackBar.pdf",width = 6.7, height = 5.5, units = "in", dpi="retina")  
ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_countries_FACETED_regi_REdir_Mob_bbUSD_vs_2025_REM_scens_stackBar.png",width = 6.7, height = 5.5, units = "in", dpi="retina")  







###################
### GLOBAL ONLY ###
###################
# 
# ggplot(q_REdir_tot_host_reg %>%
#          group_by(region,data,scenario,model) %>% 
#          summarise(volume=sum(value),.groups="keep") %>%
#          mutate(global = ifelse(region=="GLO",
#                                 "All FinEx Host Countries",
#                                 "Regions")) %>%
#          filter(region=="GLO")) +
#   facet_wrap(. ~ factor(region,levels="GLO",labels = "All FinEx Host Countries"), scales = "free") +
#   geom_errorbar(data = mob_range %>% filter(region=="GLO"),
#                 mapping = aes(x = 1,
#                               ymin = REdir_M,
#                               ymax = REdir_HI,
#                               linetype = data),
#                 color = "darkgreen",
#                 width=0.6) +
#   geom_text(mapping = aes(
#     x = 1,
#     y = REdir_HI,
#     label = round(REdir_HI - REdir_M,0)),
#     data = mob_range %>% filter(region=="GLO"),
#     nudge_x = 0.37,
#     check_overlap = T,
#     color = "darkgreen",
#     size = 2.75, fontface = "bold"
#   ) +
#   geom_col(data = . %>% filter(data=="G20 REdirect" & region=="GLO"),
#            mapping=aes(x=1,
#                        y=volume,
#                        fill = factor(scenario,
#                                      levels = c("REdirect_oecdMob", "REdirect_noMob", "PPCA"), 
#                                      labels = c("Mobilized Private Capital", "G20 REdirect", "Baseline Expectations\n(PPCA-Static)"))),
#            width=0.6,
#            position="stack") +
#   geom_point(
#     data = . %>% filter(data != "G20 REdirect" & !grepl("PPCA",data) & region=="GLO"),
#     mapping = aes(x = 1,
#                   y = volume,
#                   shape = scenario),
#     size = 3,
#     color = "mediumblue") +
#   geom_text_repel(mapping = aes(
#     x = 1,
#     y = cumval,
#     color = factor(scenario,
#                    levels = c("REdirect_oecdMob", "REdirect_noMob", "PPCA"),
#                    ordered = T),
#     label = round(volume,0)),
#     # ifelse(grepl("PPCA",scenario), "", round(volume,0))),
#     data = . %>% 
#       filter(data=="G20 REdirect" & region=="GLO") %>% 
#       group_by(region) %>%
#       mutate(cumval = cumsum(volume)),
#     nudge_x = -0.375,
#     min.segment.length = 100,
#     direction = "both",
#     size = 2.75, fontface = "bold"
#   ) +
#   scale_color_brewer(palette="Accent", direction = 1, guide = "none") +
#   # geom_text(mapping = aes(
#   #   x = 1,
#   #   y = REdir_LO,
#   #   label = round(REdir_LO,0)),
#   #   data = mob_range %>% filter(region=="GLO"),
#   #   nudge_x = 0.37,
#   #   check_overlap = T,
#   #   color = "darkgreen",
#   #   size = 2.75, fontface = "bold",
#   # ) +
#   scale_shape_manual(values = c("SSP2EU-PkBudg1150" = 1, "SSP2EU-PkBudg500" = 13),
#                      breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
#                      labels = c("1.5\u00B0C (2025)", "2\u00B0C (2025)"),
#                      name = "REMIND Benchmarks") +
#   geom_errorbar(data = . %>% filter(data=="PPCA_2020" & region=="GLO"),
#                 mapping = aes(x = 1,
#                               ymin = volume,
#                               ymax = volume,
#                               linetype = data,
#                               color = data),
#                 color = "grey10",
#                 width=0.6) +
#   # guides(shape = guide_legend(override.aes = list(linetype = 0,))) +
#   # facet_grid(. ~ global, scales="free",space="free_x") +
#   scale_fill_brewer(palette="Accent", name="RE Investment Source", direction = 1, guide = guide_legend(order = 1)) +
#   scale_linetype_manual(values = c("PPCA_2020" = 5,
#                                    "G20 REdirect" = 1),
#                         breaks = c("PPCA_2020",
#                                    "G20 REdirect"),
#                         name = "",
#                         labels = c("2020 RE Investment",
#                                    "Mobilization Uncertainty"), 
#                         guide = guide_legend(order = 3)) +
#   new_scale_color() +
#   # scale_color_manual(values = c("PPCA_2020" = "grey10",
#   #                               "G20 REdirect" = "darkgreen"),
#   #                    breaks = c("PPCA_2020",
#   #                               "G20 REdirect"),
#   #                    name = "",
#   #                    labels = c("2020 RE Investment",
#   #                               "Grant-induced\nMobilization"), 
#   #                    guide = guide_legend(order = 2)) +
#   labs(y="Billion USD (2019$)\n") +
#   theme_bw() +
#   theme(plot.title = element_text(size=0.8*8,face="bold"),
#         text = element_text(size = 7),
#         strip.text = element_text(size=0.8*9.5, face = "bold"),
#         strip.background = element_rect(fill = "white", color = "white"),
#         axis.text = element_text(size=0.8*9),
#         # axis.text.x = element_text(angle = 45,hjust = 1),
#         axis.text.x = element_blank(),
#         axis.title = element_text(size=0.8*9),
#         legend.text = element_text(size=0.8*8),
#         legend.title = element_text(size=0.8*9,face="bold"),
#         legend.background = element_blank()
#         # ,
#         # legend.position = c(0.82,0.15)
#   )
# 
# ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_hosts_REdir_Mob_bbUSD_vs_2025_REM_scens_stackBar_bold_4by3.pdf",width = 4, height = 3, units = "in", dpi="retina")  
# 

## COMPARE GLOBAL REDIRECT VOLUME WITH INITATIVES AND PROMISES ##
q_GLO_REdir_context <- q_REdir_tot_host_reg %>% 
  filter(region=="GLO") %>%
  ungroup() %>%
  # G7 Partnership for Global Infrastructure and Investment
  # add_row(region = "G7",
  #         scenario = "G7",
  #         data = "G7 & China Initiatives\n(Public Loans + Private Mobil.)",
  #         value = 0.75 * 600,
  #         period = 2025) %>%
  # Estimated volume of PGII bank directed toward energy
  add_row(region = "G7",
          data = "Expected G7 & China\nEnergy Finance",
          # scenario = "G7",
          scenario = "Energy",
          value = 0.25 * 600,
          period = 2025) %>%
  # Estimated volume of PGII bank directed toward renewables
  add_row(region = "G7",
          data = "Expected G7 & China\nRenewables Finance",
          # scenario = "G7",
          scenario = "RE",
          value = 0.125 * 600,
          period = 2025) %>%
  # $100 billion annual finance pledge
  add_row(region = "GLO",
          data = "Global $100B\nAnnual Promise",
          scenario = "Target",
          value = 500,
          period = 2025) %>%
# Retroactive fulfillment of pledge
  add_row(region = "GLO",
          data = "Global $100B\nAnnual Promise",
          scenario = "Retro",
          value = 900 - (80 + 78 + 71 + 58 + 58 + 61.5 + 52 + 80 + 80),
          period = 2025) %>%
# China Belt and Road Initiative Outlook
  add_row(region = "China",
          data = "Expected G7 & China\nRenewables Finance",
          scenario = "RE",
          value = 0.08 * 550,
          period = 2025) %>%
  add_row(region = "China",
          data = "Expected G7 & China\nEnergy Finance",
          scenario = "Energy",
          value = 0.375 * 550,
          period = 2025) %>%
  # add_row(region = "China",
  #         data = "G7 & China Initiatives\n(Public Loans + Private Mobil.)",
  #         scenario = "BRI",
  #         value = 550 - 0.24 * 550,
  #         period = 2025) %>%
  group_by(scenario,region,data) %>%
  summarise(volume=sum(value),.groups="keep") %>%
  ungroup() %>%
  mutate(volume = ifelse(region == "China", volume * (1 + 0.66), volume)) 
  

q_GLO_REdir_context <- q_GLO_REdir_context %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("China", data) & grepl("RE", scenario)) %>% 
              mutate(volume = volume*4.5/1.66,
                     data = "Expected G7 & China\nRenewables Finance",
                     scenario = "RE_Grant")) %>%
  bind_rows(q_GLO_REdir_context %>% filter(grepl("China", data) & grepl("Energy", scenario)) %>% 
              mutate(volume = volume*4.5/1.66,
                     data = "Expected G7 & China\nEnergy Finance",
                     scenario = "Energy_Grant")) %>%
  # bind_rows(q_GLO_REdir_context %>% filter(grepl("Global", data)) %>% 
  #             mutate(volume = volume*1.66,
  #                    data = "Global $100B\nAnnual Promise\n(Public Only)")) %>%
  # bind_rows(q_GLO_REdir_context %>% filter(grepl("Global", data)) %>% 
  #             mutate(volume = volume*4.5,
  #                    data = "Global $100B\nAnnual Promise\n(Public Grants)")) %>%
  # add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "G20 REdirect") %>% mutate(data = 'Global $100B\nAnnual Promise')) %>%
  # add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "G20 REdirect") %>%  mutate(data = 'G7 & China Initiatives\n(Public Loans + Private Mobil.)')) %>% 
  # add_row(q_GLO_REdir_context %>% filter(scenario=="PPCA" & data == "G20 REdirect") %>%  mutate(data = 'G7 & China Initiatives\n(Public Grants + Private Mobil.)')) %>% 
  # filter(!grepl("est.", fill)) %>%
  # mutate(data = ifelse(grepl("Plan", data), "G7 & China Initiatives", data)) %>%
  # rename(variable = data) %>%
  mutate(fill = factor(interaction(scenario,region),
                       levels = c("REdirect_oecdMob.GLO", "REdirect_noMob.GLO", 
                                  "Retro.GLO", "Target.GLO",
                                  "Energy.G7", "Energy.China", "RE.G7","RE.China", "PPCA.GLO"), 
                       # "SSP2EU-PkBudg1150", "SSP2EU-PkBudg500"),
                       labels = c("Mobilized Private Capital\n(2023-27)\n", "Public REdirect\n(2023-27)\n",
                                  "$100B Shortfall\n(2013-21)\n", 
                                  "$100B Target\n(2022-25)\n",
                                  "G7 PGII Public + Private\n(2023-27)\n",
                                  "China BRI\n(2021-25)\n",
                                  # "G7 Partnership for Global Infrastructure &\nInvestment (2023-27)\n",
                                  # "China Belt & Road Initiative\n(2021-25)\n",
                                  "G7 PGII\n(2023-27)\n",
                                  "China BRI\n(2021-25)\n",
                                  "Baseline Expectations\n(2023-27)\n")),
         data = as.character(data)) %>%
  filter(scenario!="EU") %>%
  ungroup()

  


ggplot(q_GLO_REdir_context) +
  geom_errorbar(data = mob_range %>% filter(region=="GLO"),
                mapping = aes(x = "G20 REdirect",
                              ymin = REdir_LO,
                              ymax = REdir_HI),
                width = 0.5,
                color = blues[4],
                size = 0.7) +
  geom_col(data = . %>% filter(!grepl("PkBud",scenario) & data!="PPCA_2020"),
           mapping=aes(x=data,
                       y=volume,
                       alpha = scenario,
                       group = fill,
                       fill = fill),
           width=0.9,
           color = 'black',
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
      filter(data=="G20 REdirect") %>% 
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
    color = blues[4],
    size = 3.3, fontface = "bold"
  ) +
  geom_hline(data = . %>% filter(grepl("PkBudg",data)),
             mapping = aes(linetype = scenario,
                           yintercept = volume),
             color = "mediumblue",
             alpha = 0.7) +
  scale_linetype_manual(values = c("SSP2EU-PkBudg1150" = 3, "SSP2EU-PkBudg500" = 4, "PPCA" = 2),
                     breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                     labels = c("1.5\u00B0C (2023-27)", "2\u00B0C (2023-27)"),
                     name = "REMIND Benchmarks"#,
                     # guide = "none"
                     ) +
  annotate(geom = "text",
           x = "G20 REdirect",
           y = mean(c(mob_range$REdir_HI[which(mob_range$region=="GLO")], 98 + mob_range$REdir[which(mob_range$region=="GLO")])),
                      # as.numeric(select(
                      #   filter(q_GLO_REdir_context, region == "GLO" & data == "PkBudg1150"), volume)))),
                      # q_GLO_REdir_context$volume[which(q_GLO_REdir_context$region=="GLO")])),
           label = "Grant-induced\nMobilization",
           fontface = "bold.italic",
           color = blues[4],
           angle = "90",
           size = 3.9) +
  scale_fill_manual(values = setNames(c(paired[c(3,4,1,11)], rep(c("salmon","purple1"),3), paired[2]),
                      # setNames(c(accent[-c(3,7)],rep(accent[c(6,8)],2),accent[3]), 
                                      nm = levels(q_GLO_REdir_context$fill)),
                    breaks = levels(q_GLO_REdir_context$fill)[-c(7:10)],
                    name = "Source of Finance\n(Disbursal Horizon)") +
  # scale_linetype_manual(values = c("PPCA_2020" = 2,
  #                                  "G20 REdirect" = 1),
  #                       guide = "none") +
  scale_color_brewer(palette="Accent", direction = 1, guide = "none") +
  scale_alpha_manual(values = c("G7" = 0.15, "BRI" = 0.15, "Energy" = 0.5, "RE" = 1),
                     breaks = rev(c("G7", "Energy", "RE")),
                     name = "Estimated BRI & PGII\nAllocation (Opacity)",
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

ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_hosts_REdir_Mob_bbUSD_in_context_stackBar_largeFont_GRANT_noBase.pdf",width = 10, height = 8, units = "in", dpi="retina")  

ggsave(filename = "~/Coal_Finance_Exit/Plots/USD_FinEx/FNX_hosts_REdir_Mob_bbUSD_in_context_stackBar_largeFont_GRANT_noBase.png",width = 10, height = 8, units = "in", device = "png",dpi="retina")  

