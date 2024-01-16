coalcap_jul23 <- read.magpie("~/FinEx/output/gcpt_stats/nat_coalcap_hist.csv")
lifespans2023 <- read.csv("~/FinEx/output/gcpt_stats/nat_lifespans2023.csv")
comp_rates2023 <- read.csv("~/FinEx/output/gcpt_stats/nat_comp_rates2023.csv")
meanAge2023 <- read.csv("~/FinEx/output/gcpt_stats/nat_meanAge2023.csv")
finex_cap_2021 <- read.csv("~/FinEx/output/gcpt_stats/nat_postFinEx_cap_2021.csv")


mobilization_summary <- merge_nm_gcpt2021 %>% 
  filter(PreCon.PubG20.GW > 0) %>%
  mutate(`Remaining Coal Projects (GW)` = sum(Con.Tot.GW, PreCon.NonPubG20.GW)) %>%
  select(Country.name, Country, PreCon.PubG20.GW, PreCon.PubG20.bbUSD, `Remaining Coal Projects (GW)`) %>%
  rename(`FinEx (GW)` = PreCon.PubG20.GW,
         `REdirect ($bn)` = PreCon.PubG20.bbUSD,
         ISO = Country,
         Country = Country.name) %>%
  left_join(mob_rate_oecd_RE %>% 
              filter(!grepl("Cote|Tanzania,", Recipient)) %>%
              rename(`Loan Mobilization Factor` = Mobilization_rate) %>%
              select(Country, `Loan Mobilization Factor`), by = c("ISO" = "Country")) %>%
  left_join(mob_rate_oecd_Energy_no_guar_grant %>% 
              ungroup() %>%
              filter(!grepl("Cote|Tanzania,", Recipient)) %>%
              # mutate(Mobilization_rate = ifelse(
              #   is.na(Mobilization_rate), 
              # )) %>%
              rename(`Grant Mobilization Factor` = Mobilization_rate) %>%
              select(Country, `Grant Mobilization Factor`), by = c("ISO" = "Country")) %>%
  mutate(`Loan Mobilization Factor` = ifelse(is.na(`Loan Mobilization Factor`), 0, `Loan Mobilization Factor`)) %>%
  mutate(`Grant Mobilization Factor` = ifelse(is.na(`Grant Mobilization Factor`), 0, `Grant Mobilization Factor`)) %>%
  mutate(`REdirect-loan` = (1 + `Loan Mobilization Factor`) * `REdirect ($bn)`,
         `REdirect-grant` = (1 + `Grant Mobilization Factor`) * `REdirect ($bn)`) %>%
  left_join(as.quitte(coalcap_jul23) %>%
              filter(period == 2023) %>%
              rename(ISO = region,
                     `Active Fleet (GW)` = value) %>%
              select(ISO, `Active Fleet (GW)`)) %>%
  left_join(meanAge2023 %>% 
              rename(ISO = dummy,
                     `Mean Plant Age (yrs)` = meanAge) %>% 
              select(ISO, `Mean Plant Age (yrs)`)) %>%
  left_join(lifespans2023 %>% 
              mutate(ISO = dummy,
                     `Mean Lifespan (yrs)` = Avg_Ret_Age) %>%
              select(ISO, `Mean Lifespan (yrs)`)) %>%
  left_join(comp_rates2023 %>% 
              mutate(ISO = dummy.1,
                     `Completion Rate` = ifelse(Announced==0, Pre.permit, Announced)) %>%
              select(ISO, `Completion Rate`)) %>%
  left_join(finex_cap_2021 %>% 
              mutate(ISO = dummy.1,
                     `post-FinEx Upper Bound (GW)` = Neutral) %>%
              select(ISO, `post-FinEx Upper Bound (GW)`))


write.csv(mobilization_summary, "~/FinEx/output/gcpt_stats/SI_Table1_mobilization_stats_extra.csv")

  