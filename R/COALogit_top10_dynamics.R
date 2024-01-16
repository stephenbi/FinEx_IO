
glo_coalgen <- q_ppca_ref %>% filter(region=="GLO", variable == v_seel_coal_noccs, period %in% c(2025, 2040, 2045))

glo_coalgen_2019 <- as.numeric(dimSums(dim = 1, dimSums(dim = 3,
  x = data[,2019,c("BITCOAL.ELOUTPUT","LIGNITE.ELOUTPUT", "HARDCOAL.ELOUTPUT", "SUBCOAL.ELOUTPUT", "COKCOAL.ELOUTPUT")])))

oecd10 <- read.csv(
  "P:/REMIND_3p0_dev/remind/output/PPCA-jul21-oecd_2023-08-31_05.36.16/oecd_top10_status.csv"
  ) %>% mutate(scenario = "PPCA-growth") %>%
  bind_rows(
      read.csv("P:/REMIND_3p0_dev/remind/output/FinEx-noLObd-jul21-50p-oecd_2023-09-07_02.17.26/oecd_top10_status.csv") %>% 
      mutate(scenario = "FinEx")
  ) %>%
  # bind_rows(
  #   read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_nomob-jul21-50p-oecd_2023-08-31_10.58.52/oecd_top10_status.csv") %>% 
  #     mutate(scenario = "REdirect")
  # ) %>%
  bind_rows(
    read.csv("P:/REMIND_3p0_dev/remind/output/REdirect-loan-jul21-50p-oecd_2023-08-31_13.01.40/oecd_top10_status.csv") %>% 
      mutate(scenario = "REdirect-loan")
  ) %>%
  bind_rows(
    read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_coopex-2030-jul21-50p-oecd_2023-09-22_17.30.29/oecd_top10_status.csv") %>%
      mutate(scenario = "REdirect-grant")
  ) %>%
  mutate(
    glo_share = gen_2025 / filter(glo_coalgen, period==2025)$value
  ) %>%
  bind_rows(
    read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd-2045_2023-08-31_10.17.19/current_oecd_top10.csv") %>%
      mutate(scenario = "Current (2019)",
             glo_share = gen / glo_coalgen_2019) 
    # %>%
    #   mutate(elgen_2025 = gen / share,
    #          share_2025 = share,
    #          # accession_prob_2025 = accession_prob,
    #          gen_2025 = gen,
    #          glo_share = gen / glo_coalgen_2019)
  ) %>%
  left_join(as.quitte(totalgen_2019_c) %>%
              select(region, value) %>%
              rename(country = region,
                     elgen = value),
            by = "country") %>%
  mutate(
    # elgen = gen_2025/share_2025,
    glo_elshare = elgen / as.numeric(dimSums(totalgen_2019_c, 1)),
    glo_elshare_2025 = elgen_2025 / 
      filter(q_ppca_growth, region=="GLO", variable==v_seel, period == 2025)$value,
    oecd = "By 2027 (OECD)",
         country = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                       paste0(countrycode(sourcevar = country, origin = "iso3c", destination = "country.name"),"*"), 
                       countrycode(sourcevar = country, origin = "iso3c", destination = "country.name")),
         scenario = factor(x = scenario,
                           levels = c("Current (2019)", "PPCA-growth", "FinEx", 
                                      "REdirect-loan", "REdirect-grant"))
    )

### NON-OECD 2045 (ONLY) ###
nonoecd10 <-   bind_rows(
  read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd_2023-09-13_12.19.33/nonoecd_top10_status.csv") %>%
    mutate(scenario = "PPCA-growth"),
  read.csv("P:/REMIND_3p0_dev/remind/output/FinEx-noLObd-jul21-50p-nonoecd_2023-09-13_12.19.35/nonoecd_top10_status.csv") %>%
    mutate(scenario = "FinEx"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect-loan-jul21-50p-nonoecd_2023-09-13_12.19.37/nonoecd_top10_status.csv") %>%
    mutate(scenario = "REdirect-loan"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_coopex-2030-jul21-50p-nonoecd_2023-09-24_23.39.20/nonoecd_top10_status.csv") %>%
    mutate(scenario = "REdirect-grant")
) %>%
  mutate(
    glo_share = gen_2045 / filter(glo_coalgen, period==2045)$value
  ) %>%
  bind_rows(
    read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd_2023-09-13_14.54.48/current_nonoecd.csv") %>%
      mutate(scenario = "Current (2019)") %>%
      mutate(elgen_2045 = replace_na(gen / share, 0),
             share_2045 = share,
             glo_share = gen / glo_coalgen_2019,
             # accession_prob_2045 = accession_prob,
             gen_2045 = gen)
  ) %>%
  left_join(as.quitte(totalgen_2019_c) %>%
              select(region, value) %>%
              rename(country = region,
                     elgen = value),
            by = "country") %>%
  mutate(
    oecd = "By 2047 (Non-OECD)",
    # elgen = gen_2045/share_2045,
    glo_elshare = elgen / as.numeric(dimSums(totalgen_2019_c, 1)),
    glo_elshare_2045 = elgen_2045 / 
      filter(q_ppca_growth, region=="GLO", variable==v_seel, period == 2045)$value,
    country = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                     paste0(countrycode(sourcevar = country, origin = "iso3c", destination = "country.name"),"*"),
                     countrycode(sourcevar = country, origin = "iso3c", destination = "country.name")),
    finex_host = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                        "Non-OECD FinEx Hosts", "Non-FinEx Non-OECD Nations"),
    scenario = factor(x = scenario,
                      levels = c("Current (2019)", "PPCA-growth", "FinEx", "REdirect-loan", "REdirect-grant")))

### NON-OECD 2040 - 1st stage ###
nonoecd10_2040 <-   bind_rows(
  read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd-2045_2023-09-14_12.03.40/nonoecd_top10_status.csv") %>% 
    mutate(scenario = "PPCA-growth"),
  read.csv("P:/REMIND_3p0_dev/remind/output/FinEx-noLObd-jul21-50p-nonoecd-2045_2023-09-14_12.03.41/nonoecd_top10_status.csv") %>%
    mutate(scenario = "FinEx"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect-loan-jul21-50p-nonoecd-2045_2023-09-14_12.03.44/nonoecd_top10_status.csv") %>% 
    mutate(scenario = "REdirect-loan"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_coopex-2030-jul21-50p-nonoecd-2045_2023-09-24_18.30.54/nonoecd_top10_status.csv") %>%
    mutate(scenario = "REdirect-grant")
) %>% 
  mutate(
    glo_share = gen_2040 / filter(glo_coalgen, period==2040)$value
  ) %>%
  # bind_rows(
  #   read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd-2045_2023-09-14_10.54.39/current_nonoecd.csv") %>%
  #     mutate(scenario = "Current (2019)")
  #   # %>%
  #   #   mutate(elgen_2040 = replace_na(gen / share, 0),
  #   #          share_2040 = share,
  #   #          glo_share = gen / glo_coalgen_2019,
  #   #          accession_prob_2040 = accession_prob,
  #   #          gen_2040 = gen)
  # ) %>%
  left_join(as.quitte(totalgen_2019_c) %>%
              select(region, value) %>%
              rename(country = region,
                     elgen = value),
            by = "country") %>%
  mutate(
    glo_elshare = elgen / as.numeric(dimSums(totalgen_2019_c, 1)),
    glo_elshare_2040 = elgen_2040 / 
      filter(q_ppca_growth, region=="GLO", variable==v_seel, period == 2040)$value,
    oecd = ifelse(oecd == "OECD", "By 2027 (OECD)", "By 2047 (Non-OECD)"),
    # elgen = gen_2040/share_2040,
    country = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                     paste0(countrycode(sourcevar = country, origin = "iso3c", destination = "country.name"),"*"), 
                     countrycode(sourcevar = country, origin = "iso3c", destination = "country.name")),
    finex_host = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                        "Non-OECD FinEx Hosts", "Non-FinEx Non-OECD Nations"),
    scenario = factor(x = scenario,
                      levels = c("Current (2019)", "PPCA-growth", "FinEx", "REdirect-loan", "REdirect-grant"))) 


### NON-OECD 2045 - 2nd stage ###
nonoecd10_2045 <-   bind_rows(
  read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd-2050_2023-09-11_23.23.16/nonoecd_top10_status.csv") %>%
    mutate(scenario = "PPCA-growth"),
  read.csv("P:/REMIND_3p0_dev/remind/output/FinEx-noLObd-jul21-50p-nonoecd-2050_2023-09-11_23.23.17/nonoecd_top10_status.csv") %>%
    mutate(scenario = "FinEx"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect-loan-jul21-50p-nonoecd-2050_2023-09-11_23.23.20/nonoecd_top10_status.csv") %>%
    mutate(scenario = "REdirect-loan"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_coopex-2030-jul21-50p-nonoecd-2050_2023-09-11_23.23.21/nonoecd_top10_status.csv") %>%
    mutate(scenario = "REdirect-grant")
) %>%
  mutate(
    glo_share = gen_2045 / filter(glo_coalgen, period==2045)$value
  ) %>%
  bind_rows(
    read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_coopex-2030-jul21-50p-nonoecd-2045_2023-08-31_14.48.27/current_nonoecd.csv") %>%
      mutate(scenario = "Current (2019)")
    # %>%
    #   mutate(elgen_2045 = gen / share,
    #          share_2045 = share,
    #          glo_share = gen / glo_coalgen_2019,
    #          # accession_prob_2045 = accession_prob,
    #          gen_2045 = gen)
  ) %>%
  left_join(as.quitte(totalgen_2019_c) %>%
              select(region, value) %>%
              rename(country = region,
                     elgen = value),
            by = "country") %>%
  mutate(
    glo_elshare = elgen / as.numeric(dimSums(totalgen_2019_c, 1)),
    glo_elshare_2045 = elgen_2045 / 
      filter(q_ppca_growth, region=="GLO", variable==v_seel, period == 2045)$value,
    oecd = "By 2047 (Non-OECD)",
    # elgen = gen_2045/share_2045,
    country = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                     paste0(countrycode(sourcevar = country, origin = "iso3c", destination = "country.name"),"*"),
                     countrycode(sourcevar = country, origin = "iso3c", destination = "country.name")),
    finex_host = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                        "Non-OECD FinEx Hosts", "Non-FinEx Non-OECD Nations"),
    scenario = factor(x = scenario,
                      levels = c("Current (2019)", "PPCA-growth", "FinEx", "REdirect-loan", "REdirect-grant"))) %>%
  # left_join(nonoecd10_2040, by = c("country", "scenario"), unmatched = 'error', keep = TRUE) %>%
  # , 'oecd', 'ppca', 'gen', 'gdp', 'share', 'accession_prob', 'Region', 'share_2025', 'accession_prob_2025', 'gen_2025', 'glo_share', 'finex_host'), keep = FALSE)
  # %>%
  bind_cols(nonoecd10_2040 %>% filter(grepl("2047", oecd)) %>%
              select(gen_2040, share_2040, elgen_2040, accession_prob_2040))


###########################################################################################################################
###################################### 2050 ONLY ##########################################################################
###########################################################################################################################
bubble_line_2025_2045 <- bind_rows(oecd10, nonoecd10, nonoecd10_2040) %>% 
  mutate(scenario = factor(scenario, ordered = T),
         country = ifelse(country == "Turkey*", 
                          "Türkiye*", country)) %>%
  # filter(grepl("*", country, fixed = T)) %>%
  filter((oecd == "By 2027 (OECD)" &
            (grepl("*", country, fixed = T) |
               country %in% country[which(accession_prob_2025 <= 0.5 |
                                            accession_prob < 0.2)])) |
           (oecd == "By 2047 (Non-OECD)" &
              (country %in%
                 c("China", "India",
                   country[which(grepl("*", country, fixed = T) & gen > 0 &
                                   (accession_prob < 0.01 | accession_prob_2045 < 0.5))]
                 )))) %>%
  arrange(desc(gen)) %>%
  mutate(country = factor(country, levels = unique(country), ordered = T))


ggplot(data = bubble_line_2025_2045,
       mapping = aes(
         x = country,
         # x = as.numeric(country), 
         ymin = accession_prob,
         color = scenario
       )) +
  geom_linerange(data = . %>% filter(!grepl("Current", scenario) & grepl("2027", oecd) & is.na(accession_prob_2040)),
                 mapping = aes(ymax = accession_prob_2025),
                 position = position_dodge(0.6),
                 linewidth = 1) +
  geom_linerange(data = . %>% filter(!grepl("Current", scenario) & grepl("2047", oecd) & is.na(share_2040)),
                 mapping = aes(ymax = accession_prob_2045),
                 position = position_dodge(0.6),
                 linewidth = 1) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & grepl("2027", oecd) & is.na(accession_prob_2040)),
             mapping = aes(
               y = accession_prob_2025,
               alpha = share_2025,
               size = glo_elshare_2025),
             position = position_dodge(0.6)) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & grepl("2047", oecd) & is.na(share_2040)),
             mapping = aes(
               y = accession_prob_2045,
               alpha = share_2045,
               size = glo_elshare_2045),
             position = position_dodge(0.6)) +
  geom_point(data = . %>% filter(grepl("Current", scenario)),
             mapping = aes(
               y = accession_prob,
               alpha = share,
               size = glo_elshare)) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & grepl("2027", oecd) & is.na(accession_prob_2040)),
             mapping = aes(
               y = accession_prob_2025,
               size = glo_elshare_2025),
             shape = 1, position =  position_dodge(0.6)) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & grepl("2047", oecd) & is.na(share_2040)),
             mapping = aes(
               y = accession_prob_2045,
               size = glo_elshare_2045),
             shape = 1, position =  position_dodge(0.6)) +
  geom_point(data = . %>% filter(grepl("Current", scenario)),
             mapping = aes(
               y = accession_prob,
               size = glo_elshare),
             shape = 1) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & !is.na(accession_prob_2040)),
             mapping = aes(x = country,
                           y = accession_prob_2040,
                           # alpha = glo_elshare_2040,
                           shape = "X"),
             position = position_dodge(0.6),
             shape = 4,
             alpha = 0.9,
             size = 2.5) +
  # scale_fill_manual(values = scen_clrs,
  #                    breaks = levels(oecd10$scenario),
  #                    labels = c("Current (2019)", "PPCA-growth", "FinEx", 
  #                               "REdirect-loan", "REdirect-grant"),
  #                    name = "Scenario") +
  scale_color_manual(values = scen_clrs,
                     breaks = levels(oecd10$scenario),
                     labels = c("Current (2019)", "PPCA-growth", "FinEx", 
                                "REdirect-loan", "REdirect-grant"),
                     name = "Scenario") +
  # scale_shape_discrete(name = "Probability in 2040",
  #                    # values = c("By 2047 (Non-OECD)" = 4),
  #                    #            "By 2027 (OECD)" = 4),
  #                    labels = c("=")) +
  geom_text(data = . %>% filter(grepl("2047", oecd) & scenario=="FinEx"),
            mapping = aes(
              x = "Laos*",
              y = 0.925,
              label = "= Probability at decision deadline"),
            color = "grey20",
            size = 3,
            nudge_x = 0.6) +
  geom_point(data = . %>% filter(grepl("2047", oecd) & scenario=="FinEx"),
             mapping = aes(
               x = "Indonesia*",
               y = 0.925),
             color = "grey20",
             size = 3,
             shape = 16) +
  geom_text(data = . %>% filter(grepl("2047", oecd) & scenario=="FinEx"),
            mapping = aes(
              x = "Laos*",
              y = 0.89,
              label = "= Probability in 2040"),
            color = "grey20",
            size = 3,
            nudge_x = 0.1) +
  geom_point(data = . %>% filter(grepl("2047", oecd) & scenario=="FinEx"),
            mapping = aes(
              x = "Indonesia*",
              y = 0.89),
            color = "grey20",
            size = 3,
            shape = 4) +
  guides(color = guide_legend(order = 1, 
                              override.aes = list(size = 2), 
                              title.position="top", title.hjust = 0.5),
         size = guide_legend(order = 2, title.position="top", title.hjust = 0.5),
         alpha = guide_legend(order = 3, title.position="top", title.hjust = 0.5, 
                              override.aes = list(size = 5))
         ) +
  geom_hline(yintercept = 0.5,
             color = "black",
             alpha = 0.7,
             linetype = 2) +
  ylab("PPCA Accession Probability") +
  theme_bw() +
  scale_y_continuous(name = "PPCA Accession Probability", 
                     breaks = seq(0,1,0.1), 
                     labels = scales::percent,
                     limits = c(0,1),
                     expand = expansion(mult = c(0.05,0))) +
  # scale_x_continuous(name = "Country",
  #                    breaks = c(1:18),
  #                    n.breaks = length(levels(bubble_line_2025_2045$country)),
  #                  # breaks = as.numeric(unique(bubble_line_2025_2045$country)),
  #                  labels = levels(bubble_line_2025_2045$country)) +
  facet_grid(. ~ oecd, scales = 'free', space = "free_x") +
  scale_alpha_continuous(range = c(0, 1),
                         limits = c(0,1),
                         # trans="sqrt",
                         breaks = c(0.05, 0.15, 0.4),
                         labels = paste0(100 * c(0.1, 0.5,0.99),"%"),
                        # limits = c(0.01, max(bubble_line_2025_2045$gen_2025)),
                        # trans = "log10",
                        name = "% Coal in Domestic\nPower Supply") +
  scale_size_continuous(range = c(1.5,10),
                        limits = c(0, 0.3),
                        # trans = "sqrt",
                        breaks = c(0.01, 0.1, 0.25),
                        labels = paste0(100 * c(0.01, 0.1, 0.25),"%"),
                        name = "% of Global\nPower Demand") +
  theme(panel.grid = element_line(color = "grey94"),
        text = element_text(size = 1.3 * 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 1.3 * 9),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 1.3 * 9),
        legend.box.spacing = unit(0, units = "mm"),
        # legend.box.just = "left",
        legend.justification = "left",
        # legend.key.size = 1.3 * unit(20,"mm"),
        # legend.spacing = unit(0, units = "mm"),
        legend.position = "bottom",
        # legend.position = c(0.62, 0.93),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.width = unit(3,"mm"),
        legend.box = "horizontal",
        axis.title.x = element_blank(),
        legend.title = element_text(size = 1.3 * 9),
        legend.text = element_text(size = 1.25 * 8)
  )  

ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/linerange_ALL_jul21_2050_only.pdf", dpi = "retina", width = 11, height = 6, units = 'in')
ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/linerange_ALL_jul21_2050_only.jpg", dpi = "retina", width = 11, height = 6, units = 'in')






##################################################################################################################
##################################################################################################################
##################################################################################################################

ggplot(data = bind_rows(oecd10, nonoecd10_2045) %>% 
         mutate(scenario = factor(scenario, ordered = T)) %>%
         filter((oecd == "By 2027 (OECD)" & 
                   (grepl("*", country, fixed = T) | 
                      country %in% country[which(accession_prob_2025 < 0.5 | 
                                                   accession_prob < 0.2)])) |
                  (oecd == "By 2047 (Non-OECD)" & 
                     (country %in% 
                        c("China", "India",
                          country[which(grepl("*", country, fixed = T) &
                                          (accession_prob < 0.01 | accession_prob_2045 < 0.5))] 
                          # (accession_prob_2045 < 0.5 & gen_2045 > 0.01))])
                        )))),
       mapping = aes(
         x = country, 
         ymin = accession_prob,
         color = scenario
       )) +
  geom_linerange(data = . %>% filter(!grepl("Current", scenario) & grepl("2027", oecd)),
                 mapping = aes(ymax = accession_prob_2025),
                 position = position_dodge(0.6),
                 linewidth = 1) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & grepl("2027", oecd)),
             mapping = aes(
               y = accession_prob_2025,
               size = share_2025),
             position = position_dodge(0.6)) +
  geom_linerange(data = . %>% filter(!grepl("Current", scenario) & grepl("2047", oecd)),
                 mapping = aes(ymax = accession_prob_2045),
                 position = position_dodge(0.6),
                 linewidth = 1) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & grepl("2047", oecd)),
             mapping = aes(
               y = accession_prob_2045,
               size = share_2045),
             position = position_dodge(0.6)) +
  geom_point(data = . %>% filter(grepl("Current", scenario)),
             mapping = aes(
               y = accession_prob,
               size = share),
             shape = 21) +
  geom_point(data = . %>% filter(!grepl("Current", scenario) & !is.na(accession_prob_2040)),
             mapping = aes(x = country,
                           y = accession_prob_2040,
                           shape = oecd),
             position = position_dodge(0.6),
             size = 2.5) +
  scale_color_manual(values = scen_clrs,
                     breaks = levels(oecd10$scenario),
                     labels = c("Current (2019)", "PPCA-growth", "FinEx", 
                                "REdirect-loan", "REdirect-grant"),
                     name = "Scenario") +
  scale_shape_manual(name = "Probability in 2040",
                     values = c("By 2047 (Non-OECD)" = 4),
                     labels = c("=")) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 2), title.position="top", title.hjust = 0.5),
         size = guide_legend(order = 2, title.position="top", title.hjust = 0.5),
         shape = guide_legend(order = 3, title.position="right", title.hjust = 0.5, label.hjust = 0)) +
  geom_hline(yintercept = 0.5,
             color = "black",
             alpha = 0.7,
             linetype = 3) +
  ylab("PPCA Accession Probability") +
  theme_bw() +
  scale_y_continuous(name = "PPCA Accession Probability", 
                     breaks = seq(0,1,0.1), 
                     limits = c(0,1),
                     expand = expansion(mult = c(0.05,0))) +
  scale_x_discrete(name = "Country") +
  facet_grid(. ~ oecd, scales = 'free', space = "free_x") +
  scale_size_continuous(range = c(0,10),
                        breaks = c(0, 33, 66, 99) / 100,
                        labels = paste0(c(0, 33, 66, 99),"%"),
                        name = "% Coal in Power Supply") +
  theme(panel.grid = element_line(color = "grey94"),
        text = element_text(size = 9),
        axis.text = element_text(angle = 45, hjust = 1, size = 9),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 9),
        legend.box.spacing = unit(0, units = "mm"),
        # legend.spacing = unit(0, units = "mm"),
        legend.position = "bottom",
        # legend.position = c(0.68, 0.92),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        axis.title.x = element_blank(),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
  )  

ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/linerange_ALL_jul21_nonoecd_2stage.pdf", dpi = "retina", width = 11, height = 5.5, units = 'in')
ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/linerange_ALL_jul21_nonoecd_2stage.jpg", dpi = "retina", width = 11, height = 5.5, units = 'in')



# ggplot(oecd10 %>% 
#          filter(grepl("*", country, fixed = T) |
#                   country %in% country[which(accession_prob_2025 < 0.5 |
#                                                accession_prob < 0.2)]) %>%
#          mutate(accession_prob_2025 = ifelse(grepl("Current", scenario), 
#                                              accession_prob, accession_prob_2025))) +
#   geom_point(data = . %>% 
#                group_by(country) %>%
#                filter((grepl("REdirect", scenario) | grepl("growth", scenario)) & 
#                         abs(accession_prob_2025 - accession_prob_2025[which(grepl("FinEx", scenario))]) < 0.05) %>%
#                ungroup(),
#              mapping = 
#                aes(x = country,
#                    y = accession_prob_2025,
#                    color = scenario,
#                    shape = oecd,
#                    size = gen_2025),
#              alpha = 0.6,
#              # position = position_dodge2(width = 1, preserve = "total", padding = 0.1)
#              position = position_jitterdodge(jitter.height = 0, jitter.width = 0.75, dodge.width = 0.75, seed = 1)
#   ) +
#   geom_point(data = . %>% 
#                group_by(country) %>%
#                filter(grepl("FinEx", scenario) | grepl("Current", scenario) |
#                         abs(accession_prob_2025 - accession_prob_2025[which(grepl("FinEx", scenario))]) > 0.05) %>%
#                ungroup(),
#              mapping = 
#                aes(x = country,
#                    y = accession_prob_2025,
#                    color = scenario,
#                    shape = oecd,
#                    size = gen_2025),
#              alpha = 0.6
#   ) +
#   geom_point(data = . %>% 
#                group_by(country) %>%
#                filter((grepl("REdirect", scenario) | grepl("growth", scenario)) & 
#                         abs(accession_prob_2025 - accession_prob_2025[which(grepl("FinEx", scenario))]) < 0.05) %>%
#                ungroup(),
#              mapping = 
#                aes(x = country,
#                    y = accession_prob_2025,
#                    color = scenario,
#                    shape = ppca,
#                    size = elgen_2025),
#              position = position_jitterdodge(jitter.height = 0, jitter.width = 0.75, dodge.width = 0.75, seed = 1)
#              # position = position_jitter(height = 0, width = 0.3, seed = 2)
#              # position = position_dodge2(width = 1, preserve = "total", padding = 0.1)
#   ) +
#   geom_point(data = . %>% 
#                group_by(country) %>%
#                filter(grepl("FinEx", scenario) | grepl("Current", scenario) |
#                         abs(accession_prob_2025 - accession_prob_2025[which(grepl("FinEx", scenario))]) > 0.05) %>%
#                ungroup(),
#              mapping = 
#                aes(x = country,
#                    y = accession_prob_2025,
#                    color = scenario,
#                    shape = ppca,
#                    size = elgen_2025),
#   ) +
#   scale_color_manual(values = scen_clrs,
#                      breaks = levels(oecd10$scenario),
#                      labels = c("Current (2019)", "PPCA-growth", "FinEx", 
#                                                                     "REdirect-loan", "REdirect-grant"),
#                      name = "Scenario\n(analysis year)") +
#   guides(color = guide_legend(order = 1, override.aes = list(size = 2)),
#          size = guide_legend(order = 2),
#          shape = guide_legend(order = 3)) +
#   geom_hline(yintercept = 0.5,
#              color = "black",
#              alpha = 0.7,
#              linetype = 3) +
#   ylab("PPCA Accession Probability") +
#   theme_bw() +
#   scale_y_continuous(name = "PPCA Accession Probability", breaks = seq(0,1,0.1), limits = c(0,1)) +
#   scale_x_discrete(name = "Country") +
#   facet_grid(. ~ oecd) +
#   scale_shape_manual(values = c("OECD" = 16, "Free" = 1),
#                      name = "Coal Share in\nPower Supply", 
#                      labels = c("Total", "Coal")) +
#   scale_size_continuous(range = c(0, 16), limits = c(0, 40), name = "Electricity\nGeneration\n(EJ/yr)", breaks = c(0,1,10,20,40)) +
#   geom_segment(mapping = aes(x = country,
#                              xend = country,
#                              y = accession_prob,
#                              yend = accession_prob_2025),
#                data = . %>% filter(grepl("FinEx", scenario)),
#                arrow = arrow(length = unit(0.1, 'inches'), type = 'open'),
#                alpha = 0.35,
#                linetype = 1,
#                linewidth = 0.25) +
#   theme(panel.grid = element_line(color = "grey94"),
#         text = element_text(size = 7),
#         axis.text = element_text(angle = 45, hjust = 1),
#         strip.background = element_rect(fill = "white"),
#         legend.spacing = unit(0, units = "mm"),
#         legend.box = "horizontal",
#         axis.title.x = element_blank(),
#         legend.title = element_text(size = 7)
#         )

# ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/bubbles_oecd_jul21.pdf", dpi = "retina", width = 6, height = 3.5)
# ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/bubbles_oecd_jul21.jpg", dpi = "retina", width = 6, height = 3.5)









# FinEx Hosts #
ggplot(nonoecd10 %>% 
         filter(finex_host == "Non-OECD FinEx Hosts" & 
                  country %in% country[which(accession_prob_2045 < 0.5 & 
                                               gen_2045 > median(gen_2045, na.rm = T))]) %>%
         mutate(accession_prob_2045 = ifelse(grepl("Current", scenario), 
                                             accession_prob, accession_prob_2045))) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("REdirect", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > 0.03) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("REdirect", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < 0.03) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6,
             position = position_dodge(width = 0.5, preserve = "single")
             # position = position_jitter(height = 0, width = 0.3, seed = 1)
             ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("REdirect", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > 0.03) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("REdirect", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < 0.03) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
             position = position_dodge(width = 0.5, preserve = "single")
             # position = position_jitter(height = 0, width = 0.3, seed = 1)
  ) +
  scale_color_manual(values = scen_clrs,
    # values = setNames(brewer.pal(n = 8, name = "Set2")[-c(3:5)],
    #                                    nm = rev(levels(nonoecd10$scenario))),
                     breaks = levels(nonoecd10$scenario),
                     labels = levels(nonoecd10$scenario),
                     name = "Scenario\n(analysis year)",
                     guide = "none") +
  # scale_color_brewer(palette = "Set2", name = "Scenario (analysis year)", direction = -1,
  #                    breaks = levels(nonoecd10$scenario),
  #                    labels = levels(nonoecd10$scenario),
  #                    guide = "none") +
  # guides(color = guide_legend(order = 1, override.aes = list(size = 4)),
  #        size = guide_legend(order = 2),
  #        shape = guide_legend(order = 3)) +
  geom_hline(yintercept = 0.5,
             color = brewer.pal(n=6, "Set2")[6],
             linetype = 2) +
  geom_hline(yintercept = 0.4,
             color = brewer.pal(n=3, "Set2")[1],
             linetype = 2,
             alpha = 0.6) +
  facet_grid(. ~ finex_host) +
  ylab("PPCA Accession Probability") +
  theme_bw() +
  scale_y_continuous(name = "PPCA Accession Probability", breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete(name = "Country", 
                   labels = ~ ifelse(grepl("HKG", .), "Hong Kong",
                                     ifelse(grepl("BIH", .), "Bosnia",
                                     countrycode(sourcevar = ., origin = "iso3c", destination = "country.name")))) +
  # scale_alpha_continuous(breaks = c(1, 0.9),
  scale_shape_manual(values = c("Non-OECD" = 16, "Free" = 1),
                     name = "Coal Share in\nPower Supply", 
                     labels = c("Total", "Coal"),
                     guide = "none") +
  scale_size_continuous(range = c(0, 16), limits = c(0, 40), name = "Electricity\nGeneration (EJ/yr)", breaks = seq(0,3,1),
                        guide = "none") +
  geom_segment(mapping = aes(x = country,
                           xend = country,
                           y = accession_prob,
                           yend = accession_prob_2045),
               data = . %>% filter(grepl("FinEx", scenario)),
               arrow = arrow(length = unit(0.1, 'inches'), type = 'open'),
               alpha = 0.35,
               linetype = 1,
               linewidth = 0.25) +
  theme(panel.grid = element_line(color = "grey94"),
        axis.text = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        text = element_text(size = 8))

ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_finex_hosts_nonoecd_jul21.pdf", dpi = "retina", width = 4, height = 3.5)
ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_finex_hosts_nonoecd_jul21.jpg", dpi = "retina", width = 4, height = 3.5)


#Non-FinEx Hosts#
ggplot(nonoecd10 %>% 
         filter(finex_host != "Non-OECD FinEx Hosts" & 
                  (country %in% c("CHN", "IND") |
                     country %in% country[which(accession_prob_2045 < 0.5 &
                                               gen_2045 > mean(gen_2045, na.rm = T))])) %>%
         mutate(accession_prob_2045 = ifelse(grepl("Current", scenario), 
                                             accession_prob, accession_prob_2045))) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("FinEx", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6,
             # position = position_nudge(x = ifelse(grepl("loan", scenario), -0.3, 0.3), y = 0)
             # position = position_jitterdodge(jitter.height = 0, jitter.width = 0.3, seed = 2)
             position = position_dodge(width = 1, preserve = "total")
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("FinEx", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("FinEx", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
             # position = position_nudge(x = ifelse(grepl("loan", scenario), -0.3, 0.3), y = 0)
             position = position_dodge(width = 1, preserve = "total")
             # position = position_jitterdodge(jitter.height = 0, jitter.width = 0.3, seed = 2)
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("FinEx", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
  ) +
  scale_color_manual(values = scen_clrs,
                     # breaks = levels(nonoecd10$scenario),
                     # labels = levels(nonoecd10$scenario),
                     name = "Scenario\n(analysis year)",
                     guide = "none") +
  # guides(color = guide_legend(order = 1, override.aes = list(size = 4)),
  #        size = guide_legend(order = 2),
  #        shape = guide_legend(order = 3)) +
  geom_hline(yintercept = 0.5,
             color = brewer.pal(n=6, "Set2")[6],
             linetype = 2) +
  geom_hline(yintercept = 0.4,
             color = brewer.pal(n=3, "Set2")[1],
             linetype = 2,
             alpha = 0.6) +
  facet_grid(. ~ finex_host) +
  # ylab("PPCA Accession Probability") +
  theme_bw() +
  scale_y_continuous(name = NULL, breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete(name = "Country", 
                   labels = ~ ifelse(grepl("HKG", .),
                                     "Hong Kong",
                                     countrycode(sourcevar = ., origin = "iso3c", destination = "country.name"))) +
  # scale_alpha_continuous(breaks = c(1, 0.9),
  scale_shape_manual(values = c("Non-OECD" = 16, "Free" = 1),
                     name = "Coal Share in\nPower Supply", 
                     labels = c("Total", "Coal"),
                     guide = "none") +
  scale_size_continuous(range = c(0, 16), limits = c(0, 41), name = "Electricity\nGeneration (EJ/yr)", breaks = c(1,5,20,40),
                        guide = "none") +
  geom_segment(mapping = aes(x = country,
                             xend = country,
                             y = accession_prob,
                             yend = accession_prob_2045),
               data = . %>% filter(grepl("FinEx", scenario)),
               arrow = arrow(length = unit(0.1, 'inches'), type = 'open'),
               alpha = 0.35,
               linetype = 1,
               linewidth = 0.25) +
  theme(legend.title = element_text(size = 8),
        panel.grid = element_line(color = "grey94"),
        axis.text = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        text = element_text(size = 8))


ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_nonfinex_nonoecd_jul21.pdf", dpi = "retina", width = 2.5, height = 3.5)
ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_nonfinex_nonoecd_jul21.jpg", dpi = "retina", width = 2.5, height = 3.5)





### NON-OECD ONLY 2050 ###
nonoecd10 <-   bind_rows(
  # read.csv(list.files(path = Sys.glob(paste0("P:/REMIND_3p0_dev/remind/output/", getItems(dpe_full,3.1)[!grepl("PPCA", getItems(dpe_full,3.1))], "*")), 
  #            pattern = "oecd_top10_status.csv", full.names = TRUE)) %>%
  #   mutate(scenario = factor(scenario,
  #                            levels = levels(q_dpe_full$scenario),
  #                            ),
  read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd-2050_2023-08-31_14.24.41/nonoecd_top10_status.csv") %>% 
    mutate(scenario = "PPCA-growth"),
  read.csv("P:/REMIND_3p0_dev/remind/output/FinEx-jul21-50p-nonoecd-2050_2023-08-31_21.27.03/nonoecd_top10_status.csv") %>%
    mutate(scenario = "FinEx"),
  # read.csv("P:/REMIND_3p0_dev/remind/output/FinEx-noLObd-jul21-50p-nonoecd-2050_2023-09-06_11.13.04/nonoecd_top10_status.csv") %>%
  #   mutate(scenario = "FinEx"),
  # read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_nomob-jul21-50p-nonoecd-2050_2023-08-31_20.04.31/nonoecd_top10_status.csv") %>% 
  #   mutate(scenario = "REdirect"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect-loan-jul21-50p-nonoecd-2050_2023-08-31_23.35.15/nonoecd_top10_status.csv") %>% 
    mutate(scenario = "REdirect-loan"),
  # read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_HI-cond-2025-only-ann_redir_oct-nonoecd2_2023-07-28_05.02.45/nonoecd_top10_status.csv") %>%
  #   mutate(scenario = "REdirect-grant"),
  read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_coopex-2030-jul21-50p-nonoecd-2050_2023-08-31_18.42.26/nonoecd_top10_status.csv") %>%
    mutate(scenario = "REdirect-grant"),
  # read.csv("P:/REMIND_3p0_dev/remind/output/REdirect_HI-cond-2025-only-ann_redir_oct-nonoecd1_2023-07-28_01.00.46/nonoecd_top10_status.csv") %>%
  #   rename(share_2045 = share_2040,
  #          gen_2045 = gen_2040,
  #          elgen_2045 = elgen_2040,
  #          accession_prob_2045 = accession_prob_2040) %>%
  #   mutate(scenario = "REdirect-grant"),
) %>% 
  # select(-c(share, accession_prob)) %>%
  # mutate(share = share_2045,
  #        accession_prob = accession_prob_2045)  %>%
  bind_rows(
    read.csv("P:/REMIND_3p0_dev/remind/output/PPCA-jul21-nonoecd-2045_2023-08-31_10.17.19/current_nonoecd.csv") %>%
      mutate(scenario = "Current (2019)") %>%
      mutate(elgen_2045 = gen / share,
             share_2045 = share,
             # accession_prob_2045 = accession_prob,
             gen_2045 = gen)
  ) %>%
  mutate(
    # elgen = gen_2045/share_2045,
    finex_host = ifelse(country %in% filter(finEx_by_host, PreCon.PubG20.bbUSD>0)$Country,
                        "Non-OECD FinEx Hosts", "Non-FinEx Non-OECD Nations"),
    scenario = factor(x = scenario,
                      levels = c("Current (2019)", "PPCA-growth", "FinEx", "REdirect-loan", "REdirect-grant"),
                      ordered = TRUE)) 

#FinEx Hosts#
ggplot(nonoecd10 %>% 
         filter(finex_host == "Non-OECD FinEx Hosts" & 
                  country %in% country[which(accession_prob_2045[which(scenario=="PPCA-growth")] < 0.5)]) %>%
                  # & 
                  #                              gen_2045 > median(gen_2045, na.rm = T))]) %>%
         mutate(accession_prob_2045 = ifelse(grepl("Current", scenario), 
                                             accession_prob, accession_prob_2045))) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("REdirect", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > .018) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("REdirect", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < .018) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6,
             position = position_dodge(width = 0.5, preserve = "single")
             # position = position_jitter(height = 0, width = 0.3, seed = 1)
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("REdirect", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > .018) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("REdirect", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < .018) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
             position = position_dodge(width = 0.5, preserve = "single")
             # position = position_jitter(height = 0, width = 0.3, seed = 1)
  ) +
  scale_color_manual(values = scen_clrs,
                     # values = setNames(brewer.pal(n = 8, name = "Set2")[-c(3:5)],
                     #                                    nm = rev(levels(nonoecd10$scenario))),
                     breaks = levels(nonoecd10$scenario),
                     labels = levels(nonoecd10$scenario),
                     name = "Scenario\n(analysis year)",
                     guide = "none") +
  # scale_color_brewer(palette = "Set2", name = "Scenario (analysis year)", direction = -1,
  #                    breaks = levels(nonoecd10$scenario),
  #                    labels = levels(nonoecd10$scenario),
  #                    guide = "none") +
  # guides(color = guide_legend(order = 1, override.aes = list(size = 4)),
  #        size = guide_legend(order = 2),
  #        shape = guide_legend(order = 3)) +
  geom_hline(yintercept = 0.5,
             color = "black",
             alpha = 0.7,
             linetype = 3) +
  facet_grid(. ~ finex_host) +
  ylab("PPCA Accession Probability") +
  theme_bw() +
  scale_y_continuous(name = "PPCA Accession Probability", breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete(name = "Country", 
                   labels = ~ ifelse(grepl("HKG", .), "Hong Kong",
                                     ifelse(grepl("BIH", .), "Bosnia",
                                            countrycode(sourcevar = ., origin = "iso3c", destination = "country.name")))) +
  # scale_alpha_continuous(breaks = c(1, 0.9),
  scale_shape_manual(values = c("Non-OECD" = 16, "Free" = 1),
                     name = "Coal Share in\nPower Supply", 
                     labels = c("Total", "Coal"),
                     guide = "none") +
  scale_size_continuous(range = c(0, 16), limits = c(0, 40), name = "Electricity\nGeneration (EJ/yr)", breaks = seq(0,3,1),
                        guide = "none") +
  geom_segment(mapping = aes(x = country,
                             xend = country,
                             y = accession_prob,
                             yend = accession_prob_2045),
               data = . %>% filter(grepl("FinEx", scenario)),
               arrow = arrow(length = unit(0.1, 'inches'), type = 'open'),
               alpha = 0.35,
               color = "grey50",
               linetype = 1,
               linewidth = 0.25) +
  theme(panel.grid = element_line(color = "grey94"),
        axis.text = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        text = element_text(size = 8))

ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_finex_hosts_nonoecd_2050_only_jul21.pdf", dpi = "retina", width = 4, height = 3.5)
ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_finex_hosts_nonoecd_2050_only_jul21.jpg", dpi = "retina", width = 4, height = 3.5)


# Non-FinEx Hosts 2050 only #
ggplot(nonoecd10 %>% 
         filter(finex_host != "Non-OECD FinEx Hosts" & 
                  (country %in% c("CHN", "IND") |
                     country %in% country[which(accession_prob_2045 < 0.5 &
                                                  gen_2045 > mean(gen_2045, na.rm = T))])) %>%
         mutate(accession_prob_2045 = ifelse(grepl("Current", scenario), 
                                             accession_prob, accession_prob_2045))) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("FinEx", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6,
             # position = position_nudge(x = ifelse(grepl("loan", scenario), -0.3, 0.3), y = 0)
             # position = position_jitterdodge(jitter.height = 0, jitter.width = 0.3, seed = 2)
             position = position_dodge(width = 1, preserve = "total")
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("FinEx", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = oecd,
                   size = gen_2045),
             alpha = 0.6
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(!grepl("FinEx", scenario) & 
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) < 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
             # position = position_nudge(x = ifelse(grepl("loan", scenario), -0.3, 0.3), y = 0)
             position = position_dodge(width = 1, preserve = "total")
             # position = position_jitterdodge(jitter.height = 0, jitter.width = 0.3, seed = 2)
  ) +
  geom_point(data = . %>% 
               group_by(country) %>%
               filter(grepl("FinEx", scenario) |
                        abs(accession_prob_2045 - accession_prob_2045[which(scenario=="FinEx")]) > 0.1) %>%
               ungroup(),
             mapping = 
               aes(x = country,
                   y = accession_prob_2045,
                   color = scenario,
                   shape = ppca,
                   size = elgen_2045),
  ) +
  scale_color_manual(values = scen_clrs,
                     # breaks = levels(nonoecd10$scenario),
                     # labels = levels(nonoecd10$scenario),
                     name = "Scenario\n(analysis year)",
                     guide = "none") +
  # guides(color = guide_legend(order = 1, override.aes = list(size = 4)),
  #        size = guide_legend(order = 2),
  #        shape = guide_legend(order = 3)) +
  geom_hline(yintercept = 0.5,
             color = "black",
             alpha = 0.7,
             linetype = 3) +
  facet_grid(. ~ finex_host) +
  # ylab("PPCA Accession Probability") +
  theme_bw() +
  scale_y_continuous(name = NULL, breaks = seq(0,1,0.1), limits = c(0,1)) +
  scale_x_discrete(name = "Country", 
                   labels = ~ ifelse(grepl("HKG", .),
                                     "Hong Kong",
                                     countrycode(sourcevar = ., origin = "iso3c", destination = "country.name"))) +
  # scale_alpha_continuous(breaks = c(1, 0.9),
  scale_shape_manual(values = c("Non-OECD" = 16, "Free" = 1),
                     name = "Coal Share in\nPower Supply", 
                     labels = c("Total", "Coal"),
                     guide = "none") +
  scale_size_continuous(range = c(0, 16), limits = c(0, 41), name = "Electricity\nGeneration (EJ/yr)", breaks = c(1,5,20,40),
                        guide = "none") +
  geom_segment(mapping = aes(x = country,
                             xend = country,
                             y = accession_prob,
                             yend = accession_prob_2045),
               data = . %>% filter(grepl("FinEx", scenario)),
               arrow = arrow(length = unit(0.1, 'inches'), type = 'open'),
               alpha = 0.35,
               linetype = 1,
               linewidth = 0.25) +
  theme(legend.title = element_text(size = 8),
        panel.grid = element_line(color = "grey94"),
        axis.text = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        text = element_text(size = 8))


ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_nonfinex_nonoecd_2050_only_jul21.pdf", dpi = "retina", width = 2.5, height = 3.5)
ggsave(filename = "~/FinEx/output/plots/COALogit_bubbles/concentric_bubbles_nonfinex_nonoecd_2050_only_jul21.jpg", dpi = "retina", width = 2.5, height = 3.5)



