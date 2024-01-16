


cap_2025_finEx <- readSource("GCPT",subtype = "G20_FinEx_2021")

cap_2025_finEx_reg <- toolAggregate(cap_2025_finEx,map,NULL)

cap2025_preFinEx <- readSource("GCPT",subtype="future2021")

cap2025_preFinEx_reg <- toolAggregate(cap2025_preFinEx,map,NULL)

diff_cap2025_finEx_reg <- cap2025_preFinEx_reg - cap_2025_finEx_reg

dimSums(diff_cap2025_finEx_reg[,,"BAU"], dim = 1)

cap_hist <- readSource("GCPT",subtype="historical2021")

cap_hist_reg <- mbind(toolAggregate(cap_hist,map,NULL),
  setItems(dimSums(cap_hist,dim=1), dim=1, value="GLO"))

## Mitigation scenario data
var_coalcap <- "Cap|Electricity|Coal (GW)"
cap_REM_scens <- mbind(Budg500[,2025,var_coalcap],
                       Budg1150[,2025,var_coalcap])

# FinEx Host Regions
regFinEx <- getItems(diff_cap2025_finEx_reg,1)[which(diff_cap2025_finEx_reg[,,"BAU"]>0)]

q_cap_2025_finEx_reg <- as.quitte(mbind(
  cap_2025_finEx_reg[regFinEx,,"BAU"],
  setItems(dimSums(cap_2025_finEx_reg[regFinEx,,"BAU"],dim=1), dim=1, value="GLO")))

q_cap2025_preFinEx_reg <- as.quitte(mbind(
  cap2025_preFinEx_reg[regFinEx,,"BAU"],
  setItems(dimSums(cap2025_preFinEx_reg[regFinEx,,"BAU"],dim=1), dim=1, value="GLO")))

q_diff_cap2025_finEx_reg <- as.quitte(mbind(
  diff_cap2025_finEx_reg[regFinEx,,"BAU"],
  setItems(dimSums(diff_cap2025_finEx_reg[regFinEx,,"BAU"],dim=1), dim=1, value="GLO")))

q_cap_hist_reg <- as.quitte(cap_hist_reg[regFinEx,,]) %>%
  bind_rows(as.quitte(
    setItems(
      dimSums(cap_hist_reg[regFinEx,,], dim = 1),
      dim = 1, value = "GLO")
    ))

q_cap_REM_scens <- as.quitte(cap_REM_scens[regFinEx,,]) %>%
  bind_rows(as.quitte(
    setItems(
      dimSums(cap_REM_scens[regFinEx,,], dim = 1),
      dim = 1, value = "GLO")
  ))

q_comp_cap2025_finEx_reg <- bind_rows(q_diff_cap2025_finEx_reg %>% mutate(scenario="Pre-FinEx Projection"),
                                      q_cap_2025_finEx_reg %>% mutate(scenario="Post-FinEx Projection"),
                                      q_cap_REM_scens,
                                      q_cap_hist_reg %>%
                                        mutate(scenario="Post-FinEx Projection"),
                                      q_cap_hist_reg %>%
                                        filter(period==2020) %>%
                                        mutate(value=0) %>%
                                        mutate(scenario="Pre-FinEx Projection"),
                                      q_cap_hist_reg %>%
                                        filter(period==2020) %>%
                                        mutate(scenario="SSP2EU-PkBudg500"),
                                      q_cap_hist_reg %>%
                                        filter(period==2020) %>%
                                        mutate(scenario="SSP2EU-PkBudg1150"))

q_comp_cap2025_finEx_reg <- q_comp_cap2025_finEx_reg %>%
  left_join(q_diff_cap2025_finEx_reg %>%
              rename(diff=value) %>%
              select(region, diff),
            by = "region") %>%
  mutate(midpt = value + diff/2) %>%
  # mutate(midpt = ifelse(scenario=="Pre-FinEx Projection",q_cap_2025_finEx_reg$value + value/2,0)) %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = ifelse(variable=="BAU","Neutral",
                           ifelse(variable=="Norm","Full Completion",variable))) %>%
  mutate(variable = factor(variable,levels=c("Neutral","Green","Brown","Full Completion"))) %>%
  mutate(scenario = as.factor(scenario))

##############################
######## PLOT CAPACITY #######
##############################
ggplot(q_comp_cap2025_finEx_reg) +
  geom_area(data = . %>% filter(!grepl("SSP",scenario)),
            aes(x=period,
               y=value,
               fill = region,
               alpha = factor(scenario,levels=(stackOrd_finEx))),
            position = "stack") +
  geom_text(data=. %>% filter(scenario=="Post-FinEx Projection" &
                                period==2025),
            mapping=aes(x=2027.1,
                        y=midpt,
                        label=paste(round(diff,1),"GW")),
            # nudge_y = ifelse(q_comp_cap2025_finEx_reg$value>10,0,q_comp_cap2025_finEx_reg$value+4),
            color="black",
            size=2.4) +
  geom_line(
    data = . %>% filter(grepl("SSP",scenario)),
    mapping = aes(x = period,
                  y = value,
                  linetype = scenario),
    color = "grey50") +
  expand_limits(x = 2028) +
  scale_fill_manual(values = regFill, guide = "none") +
  scale_linetype_manual(values = c("SSP2EU-PkBudg1150" = 2, "SSP2EU-PkBudg500" = 1),
                        name = "REMIND Mitigation Scenario",
                        labels = c("2\u00B0C","1.5\u00B0C")) +
  scale_alpha_manual(values = alpha_finEx,guide="none") +
  # fillScale_cov +
  theme_bw() +
  facet_wrap(. ~ factor(region,levels=regOrd,labels = regOrder),scales = "free") +
  labs(title = "FinEx Direct Impact on Coal-Fired Thermal Capacity",
       x = "",
       y = "Capacity (GW)") +
  scale_x_continuous(breaks = seq(2000,2025,5)) +
  theme(axis.text=element_text(size=8.5),
        plot.title=element_text(size=14,face="bold"),
        axis.title = element_text(size=13),
        strip.text = element_text(size=10))

ggsave(filename = "~/Coal_Finance_Exit/Plots/GW_FinEx/coal_cap_scens_postFinEx_2025_v1.png",width = 12, height = 6, device = "png",dpi="retina")  

##########################
#### REGION BY REGION ####
##########################

for (r in names(regFill)) {
  ymin <- as.numeric(
    q_comp_cap2025_finEx_reg %>%
    filter(region==r) %>%
    summarise(min(value[which(value > diff)]))
  )
  
  ymax <- as.numeric(
    q_comp_cap2025_finEx_reg %>%
      filter(region==r) %>%
      summarise(max(value))
  )
      
  ggplot(q_comp_cap2025_finEx_reg %>%
           filter(region==r)) +
    geom_area(data = . %>% 
                filter(!grepl("SSP",scenario)),
              aes(x=period,
                  y=value,
                  # alpha=factor(scenario,levels=stackOrd_finEx),
                  fill = region,
                  alpha = factor(scenario,levels=(stackOrd_finEx))),
              position = "stack") +
    geom_text(data=. %>% filter(scenario=="Post-FinEx Projection" &
                                  period==2025),
              mapping=aes(x=2028,
                          y=midpt,
                          label=paste(round(diff,1),"GW")),
              # nudge_y = ifelse(q_comp_cap2025_finEx_reg$value>10,0,q_comp_cap2025_finEx_reg$value+4),
              color="black",
              size=9) +
    geom_line(
      data = . %>% filter(grepl("SSP",scenario)),
      mapping = aes(x = period,
                    y = value,
                    linetype = scenario),
      color = "grey50") +
    expand_limits(x = 2030, y = c(ymin, ymax)) +
    scale_fill_manual(values = regFill, guide = "none") +
    scale_linetype_manual(values = c("SSP2EU-PkBudg1150" = 2, "SSP2EU-PkBudg500" = 1),
                          name = "REMIND Mitigation Scenario",
                          labels = c("2\u00B0C","1.5\u00B0C"),
                          guide = "none") +
    scale_alpha_manual(values = alpha_finEx,guide="none") +
    # fillScale_cov +
    # scale_fill_manual(values = color_finEx, name = "") +
    theme_bw() +
    labs(title = regOrder[r],
         x = "",
         y = "Capacity (GW)") +
    scale_x_continuous(breaks = seq(2000,2025,5)) +
    scale_y_continuous(breaks = seq.int(round(ymin,0), round(ymax,0), length.out = 3)) +
    theme(axis.text.y = element_text(size=2.5*16, face = "bold"),
          axis.text.x = element_text(size=2.5*12),
          plot.title=element_text(size=2.5*16 ,face="bold", hjust = 0.5),
          axis.title = element_text(size=2.5*14),
          strip.text = element_text(size=2.5*10),
          plot.background = element_blank(),
          panel.background = element_blank(),
          legend.background = element_blank(),
          rect = element_rect(fill = "transparent"))
  
  ggsave(filename = paste0("~/Coal_Finance_Exit/Plots/GW_FinEx/",r,"_coal_cap_scens_postFinEx_2025.png"),width = 9, height = 6, device = "png",dpi="retina")
  
}


###############################################
######## CONVERT CAPACITY TO EMISSIONS ########
###############################################
cint <- 26.1 * 44/12 #MtCO2/EJ
eta <- 0.41
GW_2_EJ <- 365*24*0.0000036

q_all_gen_emi_finEx_reg <- q_comp_cap2025_finEx_reg %>%
  mutate(gen = value * GW_2_EJ,
         gen_midpt = midpt * GW_2_EJ) %>%
  mutate(emi = gen * cint / eta, 
         emi_midpt = gen_midpt * cint/eta) %>%
  mutate(gen_diff = diff * GW_2_EJ * 2.5,
         emi_diff = diff * GW_2_EJ * 2.5 * cint / eta) %>%
  group_by(region, scenario, variable) %>%
  mutate(cumemi = cumsum(emi),
         cumgen = cumsum(gen),
         cumdiff = cumsum(diff)) %>%
  mutate(cumgen_diff = cumdiff * GW_2_EJ * 2.5,
         cumemi_diff = cumdiff * GW_2_EJ * 2.5 * cint / eta) %>%
  ungroup()

# emi_gen_diff <- 
  # interpolate_missing_periods(
  #   data = q_comp_cap2025_finEx_reg %>%
  #     filter(period>=2020 & grepl("Post-FinEx", scenario)),
  #   # periods = list(period = c(2020:2025)),
  #   method = "linear")
    

###############################
######## PLOT EMISSIONS #######
###############################
ggplot(q_all_gen_emi_finEx_reg) +
  geom_area(data = . %>% filter(!grepl("SSP",scenario)),
            aes(x=period,
                y=emi,
                # alpha=factor(scenario,levels=stackOrd_finEx),
                fill = region,
                alpha = factor(scenario,levels=(stackOrd_finEx))),
            position = "stack") +
  geom_label_repel(data=. %>% filter(scenario=="Post-FinEx Projection" &
                                period==2025),
            mapping=aes(x=2025,
                        y=emi_midpt,
                        label=paste0(round(emi_diff,1),"Mt\n(",round(diff,1),"GW)")),
            # nudge_y = ifelse(q_all_gen_emi_finEx_reg$emi>10,0,q_all_gen_emi_finEx_reg$emi+4),
            color="black",
            fill = "transparent",
            min.segment.length = 0.1,
            size=3.3,
            nudge_x = 5,
            nudge_y = 0
            ) +
  geom_line(
    data = . %>% filter(!grepl("SSP",scenario)),
    mapping = aes(x = period,
                  y = emi,
                  color = factor(scenario,levels=(stackOrd_finEx)),
                  linetype = factor(scenario,levels=(stackOrd_finEx))),
    position = "stack",
    size = 0.7) +
  geom_line(
    data = . %>% filter(grepl("SSP",scenario)),
    mapping = aes(x = period,
                  y = emi,
                  linetype = scenario,
                  color = scenario),
    size = 0.7) +
  geom_point(
    data = . %>% filter(grepl("SSP",scenario) & period==2025),
    mapping = aes(x = period,
                  y = emi,
                  shape = scenario,
                  color = scenario),
    size = 3) +
  expand_limits(x = 2035) +
  scale_fill_manual(values = regFill, guide = "none") +
  scale_linetype_manual(values = c("Pre-FinEx Projection" = 5, "Post-FinEx Projection" = 1, 
                                   "SSP2EU-PkBudg1150" = 3, "SSP2EU-PkBudg500" = 3),
                        name = "Scenario",
                        labels = c("Pre-FinEx", "Post-FinEx",
                                   "2\u00B0C","1.5\u00B0C")) +
  scale_alpha_manual(values = alpha_finEx,guide="none") +
  scale_color_manual(values = c("Pre-FinEx Projection" = "grey50", "Post-FinEx Projection" = "grey30",
                                "SSP2EU-PkBudg1150" = "magenta1", "SSP2EU-PkBudg500" = "chartreuse4"),
                     name = "Scenario",
                     labels = c("Pre-FinEx", "Post-FinEx","2\u00B0C","1.5\u00B0C")) +
  scale_shape_manual(values = c("SSP2EU-PkBudg1150" = 4, "SSP2EU-PkBudg500" = 13),
                     breaks = c("SSP2EU-PkBudg500", "SSP2EU-PkBudg1150"),
                     labels = c("1.5\u00B0C (2025)", "2\u00B0C (2025)"),
                     name = "REMIND Benchmarks",
                     guide = "none") +
  theme_bw() +
  facet_wrap(factor(region,levels=regOrd,labels = regOrder) ~ .,
             scales = "free") +
  labs(title = "FinEx Direct Impact on Coal-Powered Emissions Trajectories",
       y = "Coal Power Plant Emissions (MtCO2)") +
  guides(linetype = guide_legend(override.aes = list(shape = NA))) +
  scale_x_continuous(breaks = seq(2005,2025,length.out=3)) +
  theme(axis.text=element_text(size=8.5),
        plot.title=element_text(size=14,face="bold"),
        axis.title = element_text(size=13),
        strip.text = element_text(size=10),
        legend.key.width = unit(0.9,"cm"),
        legend.position = "bottom",
        legend.box.margin = margin(0.01,0.01,0.01,0.01),
        axis.title.x = element_blank())

ggsave(filename = "~/Coal_Finance_Exit/Plots/GW_FinEx/coal_emi_scens_postFinEx_2025_v1.png",width = 9, height = 6, device = "png",dpi="retina")  






  ########################################################################################################
#### ONLY NEUTRAL SCENARIO ####
########################################################################################################
q_cap_2025_finEx_reg_neutral <- as.quitte(cap_2025_finEx_reg[,,"BAU"])

q_cap2025_preFinEx_reg_neutral <- as.quitte(cap2025_preFinEx_reg[,,"BAU"])

q_diff_cap2025_finEx_reg_neutral <- as.quitte(diff_cap2025_finEx_reg[,,"BAU"])

q_comp_cap2025_finEx_reg_neutral <- bind_rows(q_diff_cap2025_finEx_reg_neutral %>% mutate(scenario="Pre-FinEx Projection"),
                                      q_cap_2025_finEx_reg_neutral %>% mutate(scenario="Post-FinEx Projection")) 

q_comp_cap2025_finEx_reg_neutral <- q_comp_cap2025_finEx_reg_neutral %>%
  mutate(midpt = ifelse(scenario=="Pre-FinEx Projection",q_cap_2025_finEx_reg_neutral$value + value/2,0)) %>%
  mutate(variable = as.character(variable)) %>%
  mutate(variable = ifelse(variable=="BAU","Neutral",variable))

ggplot(q_comp_cap2025_finEx_reg_neutral) +
  geom_col(aes(x=region,
               y=value,
               alpha=factor(scenario,levels=stackOrd_finEx),
               fill = region)) +
  geom_text(data=. %>% filter(scenario=="Pre-FinEx Projection" & value>0),
            mapping=aes(x=region,
                        y=midpt,
                        label=paste(round(value,1),"GW")),
            nudge_y = ifelse(q_comp_cap2025_finEx_reg_neutral$value>10,0,q_comp_cap2025_finEx_reg_neutral$value+20),
            color="gray25",
            size=3) +
  scale_fill_brewer(palette = "Paired") +
  scale_alpha_manual(values = alpha_finEx,guide="none") +
  theme_bw() +
  facet_wrap(variable ~ .,nrow=1) +
  labs(title = "FinEx Impact on 2025 Coal Capacity by Covid Recovery Scenario and Region",
       x = "Region",
       y = "Capacity (GW)")

########################################################################################################

powGDP2025 <- mbind(powerBAU1p[,2025,varGDP],powerBAU2p[,2025,varGDP],powerBAU3p[,2025,varGDP],powerBrown1p[,2025,varGDP],powerBrown2p[,2025,varGDP],powerBrown3p[,2025,varGDP],powerGreen1p[,2025,varGDP],powerGreen2p[,2025,varGDP],powerGreen3p[,2025,varGDP])
powGDP2045 <- mbind(powerBAU1p[,2045,varGDP],powerBAU2p[,2045,varGDP],powerBAU3p[,2045,varGDP],powerBrown1p[,2045,varGDP],powerBrown2p[,2045,varGDP],powerBrown3p[,2045,varGDP],powerGreen1p[,2045,varGDP],powerGreen2p[,2045,varGDP],powerGreen3p[,2045,varGDP])

demGDP2025 <- mbind(demandBAU1p[,2025,varGDP],demandBAU2p[,2025,varGDP],demandBAU3p[,2025,varGDP],demandBrown1p[,2025,varGDP],demandBrown2p[,2025,varGDP],demandBrown3p[,2025,varGDP],demandGreen1p[,2025,varGDP],demandGreen2p[,2025,varGDP],demandGreen3p[,2025,varGDP])
demGDP2045 <- mbind(demandBAU1p[,2045,varGDP],demandBAU2p[,2045,varGDP],demandBAU3p[,2045,varGDP],demandBrown1p[,2045,varGDP],demandBrown2p[,2045,varGDP],demandBrown3p[,2045,varGDP],demandGreen1p[,2045,varGDP],demandGreen2p[,2045,varGDP],demandGreen3p[,2045,varGDP])


(base[getItems(base,1)[which(getItems(base,1)!="GLO")],2025,varGDP] -powGDP2025[getItems(base,1)[which(getItems(base,1)!="GLO")],,])/base[getItems(base,1)[which(getItems(base,1)!="GLO")],2025,varGDP]*100
(base[getItems(base,1)[which(getItems(base,1)!="GLO")],2045,varGDP] -powGDP2045[getItems(base,1)[which(getItems(base,1)!="GLO")],,])/base[getItems(base,1)[which(getItems(base,1)!="GLO")],2045,varGDP]*100

(base[getItems(base,1)[which(getItems(base,1)!="GLO")],2025,varGDP] -demGDP2025[getItems(base,1)[which(getItems(base,1)!="GLO")],,])/base[getItems(base,1)[which(getItems(base,1)!="GLO")],2025,varGDP]*100
(base[getItems(base,1)[which(getItems(base,1)!="GLO")],2045,varGDP] -demGDP2045[getItems(base,1)[which(getItems(base,1)!="GLO")],,])/base[getItems(base,1)[which(getItems(base,1)!="GLO")],2045,varGDP]*100
