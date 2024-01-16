
### Project Count ###
# Number of public G20 overseas pre-construction projects
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Status %in% c("announced","pre-permit","permitted","shelved") & Country!=Financing.Country) %>% 
  summarise(length(unique(Plant)))

# Number of public G20 overseas shelved projects
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Status %in% c("shelved") & Country!=Financing.Country) %>% 
  summarise(length(unique(Plant)))

# Number of public G20 overseas projects under construction
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Status=="construction" & Country!=Financing.Country) %>% 
  summarise(length(unique(Project.Name)))

# Number of G20 publicly financed overseas operating plants
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Status=="operating" & Country!=Financing.Country) %>% 
  summarise(length(unique(Project.Name)))


### MW Capacities ###
# # Public G20 overseas pre-construction plants
pub_g20_plants_precon <- 
  filter(finCoal,
         ownership=="state-owned" & 
           Financing.Country %in% g20 & 
           Status %in% c("announced","pre-permit","permitted","shelved") &
           Country!=Financing.Country)$Plant

# # Public G20 overseas pre-construction units
pub_g20_units_precon <- 
  finCoal %>% filter(Status %in% c("announced","pre-permit","permitted","shelved") &
                       (ownership=="state-owned" & 
                           Financing.Country %in% g20 & 
                           Country!=Financing.Country) 
                        # |
                        #   ((CONSTRUCT_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_1_Country[i]))) |
                        #      (CONSTRUCT_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_2_Country[i]))) |
                        #      (CONSTRUCT_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_3_Country[i]))) |
                        #      (CONSTRUCT_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],CONSTRUCT_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],CONSTRUCT_4_Country[i]))) |
                        #      (Sponsor_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_1_Country[i]))) |
                        #      (Sponsor_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_2_Country[i]))) |
                        #      (Sponsor_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_3_Country[i]))) |
                        #      (Sponsor_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Sponsor_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Sponsor_4_Country[i]))) |
                        #      (Parent_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_1_Country[i]))) |
                        #      (Parent_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_2_Country[i]))) |
                        #      (Parent_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_3_Country[i]))) |
                        #      (Parent_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_4_Country[i]))) |
                        #      (Parent_5_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_5_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_5_Country[i]))) |
                        #      (Parent_6_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_6_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_6_Country[i]))) |
                        #      (TURBMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],TURBMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],TURBMFR_1_Country[i]))) |
                        #      (TURBMFR_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],TURBMFR_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],TURBMFR_2_Country[i]))) |
                        #      (GENMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],GENMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],GENMFR_1_Country[i]))) |
                        #      (GENMFR_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],GENMFR_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],GENMFR_2_Country[i]))) |
                        #      (NOXMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],NOXMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],NOXMFR_1_Country[i]))) |
                        #      (PARTMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],PARTMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],PARTMFR_1_Country[i]))) |
                        #      (FGDMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],FGDMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],FGDMFR_1_Country[i]))) |
                        #      (SSSMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],SSSMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],SSSMFR_1_Country[i]))) |
                        #      (Insurer_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_1_Country[i]))) |
                        #      (Insurer_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_2_Country[i])))))
  ) %>% summarise(Unit=unique(Unit))

tot_g20_finEx_cap_by_unit <- finCoal %>% 
  filter(Unit %in% pub_g20_units_precon$Unit) %>%
  # filter(Status %in% c("construction")) %>%
  # filter(Status %in% c("announced","pre-permit","permitted","shelved")) %>%
  summarise(sum(capacity_weighted_by_funding_share_MW))

### by tracker ID ###
pub_g20_ID_precon <- 
  finCoal %>% filter(Status %in% c("announced","pre-permit","permitted","shelved") &
                       ((ownership=="state-owned" & 
                           Financing.Country %in% g20 & 
                           Country!=Financing.Country) |
                          ((CONSTRUCT_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_1_Country[i]))) |
                             (CONSTRUCT_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_2_Country[i]))) |
                             (CONSTRUCT_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_3_Country[i]))) |
                             (CONSTRUCT_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],CONSTRUCT_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],CONSTRUCT_4_Country[i]))) |
                             (Sponsor_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_1_Country[i]))) |
                             (Sponsor_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_2_Country[i]))) |
                             (Sponsor_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_3_Country[i]))) |
                             (Sponsor_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Sponsor_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Sponsor_4_Country[i]))) |
                             (Parent_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_1_Country[i]))) |
                             (Parent_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_2_Country[i]))) |
                             (Parent_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_3_Country[i]))) |
                             (Parent_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_4_Country[i]))) |
                             (Parent_5_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_5_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_5_Country[i]))) |
                             (Parent_6_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_6_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_6_Country[i]))) |
                             (TURBMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],TURBMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],TURBMFR_1_Country[i]))) |
                             (TURBMFR_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],TURBMFR_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],TURBMFR_2_Country[i]))) |
                             (GENMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],GENMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],GENMFR_1_Country[i]))) |
                             (GENMFR_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],GENMFR_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],GENMFR_2_Country[i]))) |
                             (NOXMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],NOXMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],NOXMFR_1_Country[i]))) |
                             (PARTMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],PARTMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],PARTMFR_1_Country[i]))) |
                             (FGDMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],FGDMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],FGDMFR_1_Country[i]))) |
                             (SSSMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],SSSMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],SSSMFR_1_Country[i]))) |
                             (Insurer_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_1_Country[i]))) |
                             (Insurer_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_2_Country[i])))))
  ) %>% summarise(ID=unique(TrackerID))

tot_g20_finEx_cap_by_ID <- finCoal %>% 
  filter(TrackerID %in% pub_g20_ID_precon$ID) %>%
  # filter(Status %in% c("construction")) %>%
  # filter(Status %in% c("announced","pre-permit","permitted","shelved")) %>%
  summarise(sum(capacity_weighted_by_funding_share_MW))


# Public G20 overseas projects under construction
pub_g20_units_con <- 
  filter(finCoal,
         ownership=="state-owned" & 
           Financing.Country %in% g20 & 
           Status=="construction" & 
           Country!=Financing.Country)$Unit

pub_g20_units_con <- 
  finCoal %>% filter(Status=="construction" &
                       ((ownership=="state-owned" & 
                           Financing.Country %in% g20 & 
                           Country!=Financing.Country) |
                          ((CONSTRUCT_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_1_Country[i]))) |
                             (CONSTRUCT_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_2_Country[i]))) |
                             (CONSTRUCT_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],CONSTRUCT_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],CONSTRUCT_3_Country[i]))) |
                             (CONSTRUCT_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],CONSTRUCT_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],CONSTRUCT_4_Country[i]))) |
                             (Sponsor_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_1_Country[i]))) |
                             (Sponsor_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_2_Country[i]))) |
                             (Sponsor_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Sponsor_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Sponsor_3_Country[i]))) |
                             (Sponsor_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Sponsor_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Sponsor_4_Country[i]))) |
                             (Parent_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_1_Country[i]))) |
                             (Parent_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_2_Country[i]))) |
                             (Parent_3_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Parent_3_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Parent_3_Country[i]))) |
                             (Parent_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_4_Country[i]))) |
                             (Parent_5_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_5_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_5_Country[i]))) |
                             (Parent_6_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_6_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_6_Country[i]))) |
                             (TURBMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],TURBMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],TURBMFR_1_Country[i]))) |
                             (TURBMFR_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],TURBMFR_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],TURBMFR_2_Country[i]))) |
                             (GENMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],GENMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],GENMFR_1_Country[i]))) |
                             (GENMFR_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],GENMFR_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],GENMFR_2_Country[i]))) |
                             (NOXMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],NOXMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],NOXMFR_1_Country[i]))) |
                             (PARTMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],PARTMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],PARTMFR_1_Country[i]))) |
                             (FGDMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],FGDMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],FGDMFR_1_Country[i]))) |
                             (SSSMFR_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],SSSMFR_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],SSSMFR_1_Country[i]))) |
                             (Insurer_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_1_Country[i]))) |
                             (Insurer_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_2_Country[i])))))
  ) %>% summarise(Unit=unique(Unit))

tot_g20_con_cap_by_unit <- finCoal %>% 
  filter(Status=="construction" & Unit %in% pub_g20_units_con$Unit) %>%
  summarise(sum(capacity_weighted_by_funding_share_MW))

pub_g20_plants_con <- 
  filter(finCoal,
         ownership=="state-owned" & 
           Financing.Country %in% g20 & 
           Status=="construction" & 
           Country!=Financing.Country)$Plant

tot_g20_con_cap_by_plant_unit1 <- finCoal %>% 
  filter(Status=="construction" & Unit %in% pub_g20_units_con$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_con)) %>%
  summarise(sum(capacity_weighted_by_funding_share_MW))


# G20 publicly financed overseas operating plants
pub_g20_units_oper <- 
  filter(finCoal,
         ownership=="state-owned" & 
           Financing.Country %in% g20 & 
           Status %in% "operating" & 
           Country!=Financing.Country)$Unit

pub_g20_units_oper <- 
  finCoal %>% filter(Status=="operating" &
                       ((ownership=="state-owned" & 
                           Financing.Country %in% g20 & 
                           Country!=Financing.Country) |
                          ((Insurer_1_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_1_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_1_Country[i]))) |
                             (Insurer_2_Country %in% g20_full & sapply(1:nrow(.),function(i) !grepl(Financing.Country.name[i],Insurer_2_Country[i])) & sapply(1:nrow(.),function(i) !grepl(Country.name[i],Insurer_2_Country[i])))))
  ) %>% summarise(Unit=unique(Unit))


tot_g20_oper_cap_by_unit <- finCoal %>% 
  filter(Unit %in% pub_g20_units_oper$Unit) %>%
  filter(Status=="operating") %>%
  summarise(sum(capacity_weighted_by_funding_share_MW))

pub_g20_plants_oper <- 
  filter(finCoal,
         ownership=="state-owned" & 
           Financing.Country %in% g20 & 
           Status %in% "operating" & 
           Country!=Financing.Country)$Plant

tot_g20_oper_cap_by_plant_unit1 <- finCoal %>% 
  filter(Unit %in% pub_g20_units_oper) %>%
  filter(Status=="operating") %>%
  summarise(sum(capacity_weighted_by_funding_share_MW))


# Total operating plants
tot_oper_mw <- finCoal %>% 
  filter(Status=="operating") %>%
  summarise(sum(capacity_weighted_by_funding_share_MW))

############################################
### Finance Volumes (transaction amount) ###
############################################

# Public G20 overseas pre-construction projects
# G20 public contribution
pub_g20_finVol_precon <- finCoal %>% 
  filter((Unit %in% pub_g20_units_precon$Unit) &
           ownership=="state-owned" & Financing.Country %in% g20 & Status %in% c("announced","pre-permit","permitted","shelved")
         & Country!=Financing.Country) %>%
  summarise(sum(transaction_estim))

# private co-finance
pvt_g20_finVol_precon <- finCoal %>% 
  filter((TrackerID %in% pub_g20_ID_precon$ID) &
           ownership=="private" & Status %in% c("announced","pre-permit","permitted","shelved")) %>%
  group_by(Financing.Country) %>%
  summarise(sum(transaction_estim))

finCoal %>% 
  filter((TrackerID %in% pub_g20_ID_precon$ID) &
           ownership=="private" & Status %in% c("announced","pre-permit","permitted","shelved")) %>%
  # group_by(Financing.Country) %>%
  summarise(sum(transaction_estim))

finCoal %>% 
  filter(Financing.Country %in% g20 |
           !(Status %in% c("cancelled"))) %>%
  group_by(Financing.Country) %>%
  summarise(sum(transaction_estim))

# Pre-con sans shelved projects
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Status %in% c("announced","pre-permit","permitted") & Country!=Financing.Country) %>% 
  summarise(sum(transaction_estim))

# Public G20 overseas projects under construction
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Status=="construction" & Country!=Financing.Country) %>% 
  summarise(sum(transaction_estim))

# G20 publicly financed overseas operating plants
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Status=="operating" & Country!=Financing.Country) %>% 
  summarise(sum(transaction_estim))

# Overseas operating plants by owner
finCoal %>% 
  filter(Status=="operating" & 
           (Financing.Country %in% g20 & Country!=Financing.Country) |
           (Sponsor_1_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Sponsor_1_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Sponsor_1_Country[i]))) |
           (Sponsor_2_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Sponsor_2_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Sponsor_2_Country[i]))) |
           (Sponsor_3_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Sponsor_3_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Sponsor_3_Country[i]))) |
           (Sponsor_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Sponsor_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Sponsor_4_Country[i]))) |
           (Parent_1_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_1_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_1_Country[i]))) |
           (Parent_2_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_2_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_2_Country[i]))) |
           (Parent_3_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_3_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_3_Country[i]))) |
           (Parent_4_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_4_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_4_Country[i]))) |
           (Parent_5_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_5_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_5_Country[i]))) |
           (Parent_6_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Financing.Country.name[i],Parent_6_Country[i])) & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Parent_6_Country[i])))) %>% 
  group_by(Financing.Country.name) %>%
  summarise(sum(transaction_estim))


##############################################
### Insurance Volumes (transaction amount) ###
##############################################

# Public G20 overseas pre-construction projects
pub_g20_insure_precon <- finCoal %>% 
  filter(Status %in% c("announced","pre-permit","permitted","shelved") & 
           ((Insurer_1_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_1_Country[i]))) |
           (Insurer_2_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_2_Country[i]))))) %>%
  group_by(Insurer_1,Insurer_2) %>%
  summarise(sum(transaction_estim),.groups="keep")

# Public G20 overseas under-construction projects
pub_g20_insure_con <- finCoal %>% 
  filter(Status=="construction" & 
           ((Insurer_1_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_1_Country[i]))) |
              (Insurer_2_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_2_Country[i]))))) %>%
  # group_by(Insurer_1,Insurer_2) %>%
  summarise(sum(transaction_estim),.groups="keep")

# Public G20 overseas operating coal plants
pub_g20_insure_oper <- finCoal %>% 
  filter(Status=="operating" & 
           ((Insurer_1_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_1_Country[i]))) |
              (Insurer_2_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_2_Country[i]))))) %>%
  # group_by(Insurer_1,Insurer_2) %>%
  summarise(sum(transaction_estim),.groups="keep")



 ### Total 
View(finCoal %>% 
       filter((Financing.Country %in% g20 | Insurer_1_Country %in% g20_full  | Insurer_2_Country %in% g20_full) &
                !(Status %in% c("cancelled"))) %>%
       group_by(Financing.Country) %>%
       summarise(sum(transaction_estim)))


### Country pair finance flows ###
View(finCoal %>% 
     filter((Financing.Country %in% g20 & Financing.Country!=Country) | 
              (Insurer_1_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_1_Country[i]))) | 
              (Insurer_2_Country %in% g20_full & !sapply(1:nrow(.),function(i) grepl(Country.name[i],Insurer_2_Country[i])))) %>%
     group_by(Financing.Country.name,Country.name,Insurer_1_Country,Insurer_2_Country,ownership) %>%
     summarise(sum(transaction_estim),.groups="keep"))

### MISC ###

# % of G20 overseas projects with missing transaction data
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & weighted_funding_amount_estimated_mio==0 & Country!=Financing.Country) %>% 
  summarise(n()) / 
  finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country) %>% 
  summarise(n()) 


#BU database China coverage
finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country=="CHN" & (Institution %in% c("China Development Bank","Export Import Bank of China (Chexim)")) & !(Status %in% c("announced","cancelled")) & Country!=Financing.Country) %>% 
  summarise(sum(transaction_estim)) /
  finCoal %>% 
  filter(ownership=="state-owned" & Financing.Country=="CHN" & !(Status %in% c("announced","cancelled")) & Country!=Financing.Country) %>% 
  summarise(sum(transaction_estim))


### TABLE AGGREGATE ### 
# write_xlsx(
  finCoal %>% 
    filter(ownership=="state-owned" & Financing.Country %in% g20 & Status %in% c("announced","pre-permit","permitted","shelved") & Country!=Financing.Country) %>%
    select(c(Financing.Country,Country,Status,capacity_weighted_by_funding_share_MW,transaction_estim)) %>%
    mutate(Status="precon") %>%
    full_join(finCoal %>%
                filter(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country & Status %in% c("construction","operating")) %>% 
                select(c(Financing.Country,Country,Status,capacity_weighted_by_funding_share_MW,transaction_estim))) %>%  
    group_by(Status) %>%
    summarise(MW=sum(capacity_weighted_by_funding_share_MW*1e-3),
              Reported_Mio_USD=sum(transaction_estim*1e-3),.groups="keep")
  # ,
  # "~/Coal_Finance_Exit/Summary_FULL_g20_overseas_proj_by_status.xlsx")


# write_xlsx(
  finCoal %>% 
    filter(Financing.Country %in% g20 & Status %in% c("announced","pre-permit","permitted","shelved")) %>%
    select(c(Financing.Country,Country,Status,capacity_weighted_by_funding_share_MW,transaction_estim)) %>%
    mutate(Status="precon") %>%  
  full_join(finCoal %>%
              filter(Financing.Country %in% g20 & Status %in% c("construction","operating")) %>% 
              select(c(Financing.Country,Country,Status,capacity_weighted_by_funding_share_MW,transaction_estim))) %>%
    group_by(Status) %>%
    summarise(MW=sum(capacity_weighted_by_funding_share_MW)*1e-3,
              Reported_Mio_USD=sum(transaction_estim)*1e-3,.groups="keep")
  # ,
  # "~/Coal_Finance_Exit/Summary_FULL_g20_all_proj_by_status.xlsx")

# write_xlsx(
  finCoal %>%
    filter(Status!="cancelled") %>% 
    select(c(Financing.Country,Country,Status,capacity_weighted_by_funding_share_MW,transaction_estim)) %>%
    full_join(finCoal %>% 
                filter(Status %in% c("announced","pre-permit","permitted","shelved")) %>%
                select(c(Financing.Country,Country,Status,capacity_weighted_by_funding_share_MW,transaction_estim)) %>%
                mutate(Status="precon")) %>%  
    group_by(Status) %>%
    summarise(GW=sum(capacity_weighted_by_funding_share_MW*1e-3),
              Reported_Mio_USD=sum(transaction_estim*1e-3),.groups="keep")
  # ,
  # "~/Coal_Finance_Exit/Summary_FULL_glo_all_proj_by_status.xlsx")


  
### TABLE BY HOST COUNTRY ### 
finEx_by_host <- finCoal %>%
  filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
  filter(Status %in% c("announced","pre-permit","permitted","shelved")) %>%
  mutate(Status="precon") %>%
  group_by(Country,Country.name,.add=T) %>%
  summarise(PreCon.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep") %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
      filter(Status=="announced") %>%
      mutate(Status="ann") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Ann.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
      filter(Status=="pre-permit") %>%
      mutate(Status="pre") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Pre.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
      filter(Status=="permitted") %>%
      mutate(Status="perm") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Perm.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
      filter(Status=="shelved") %>%
      mutate(Status="she") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(She.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_con | (!grepl("1",Unit) & Plant %in% pub_g20_plants_con) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
      filter(Status=="construction") %>%
      mutate(Status="con") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Con.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_oper | (!grepl("1",Unit) & Plant %in% pub_g20_plants_oper)) %>%
      filter(Status=="operating") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Oper.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved") & ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country) %>%
     group_by(Country,Country.name,.add=T) %>%
     summarise(PreCon.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(transaction_estim)) %>%
      filter(Status=="announced") %>%
      mutate(Status="ann") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Ann.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(transaction_estim)) %>%
      filter(Status=="pre-permit") %>%
      mutate(Status="pre") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Pre.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(transaction_estim)) %>%
      filter(Status=="permitted") %>%
      mutate(Status="perm") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(Perm.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(transaction_estim)) %>%
      filter(Status=="shelved") %>%
      mutate(Status="she") %>%
      group_by(Country,Country.name,.add=T) %>%
      summarise(She.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(Status=="construction" & Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon)) %>%
     group_by(Country,Country.name,.add=T) %>%
     summarise(Con.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep"))  %>%
  full_join(
   finCoal %>%
     filter(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country & Status=="operating") %>%
     group_by(Country,Country.name,.add=T) %>%
     summarise(Oper.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep"))  %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved") & ownership=="private" & Country %in% g20 & Country!=Financing.Country) %>%
     group_by(Country,Country.name,.add=T) %>%
     summarise(PreCon.Pvt.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(Status=="construction" & ownership=="private" & Country %in% g20 & Country!=Financing.Country) %>%
     group_by(Country,Country.name,.add=T) %>%
     summarise(Con.Pvt.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(ownership=="private" & Country %in% g20 & Country!=Financing.Country & Status=="operating") %>%
     group_by(Country,Country.name,.add=T) %>%
     summarise(Oper.Pvt.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  mutate(across(everything(),~ifelse(is.na(.x),0,.x)))

# write_xlsx(finEx_by_host,"~/Coal_Finance_Exit/Summary_pub_priv_g20_overseas_by_host.xlsx")

mag_nat_finEx_by_host <- toolCountryFill(as.magpie(finEx_by_host %>% ungroup() %>% select(-c(Country.name))), fill = 0)
mag_finEx_by_host <- toolAggregate(mag_nat_finEx_by_host,map,NULL)


### TABLE BY FINANCIER COUNTRY ### 
finEx_by_creditor <- finCoal %>%
  filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
  filter(Status %in% c("announced","pre-permit","permitted","shelved")) %>%
  mutate(Status="precon") %>%
  group_by(Financing.Country,Financing.Country.name,Country.name,Financing.Country.name,.add=T) %>%
  summarise(PreCon.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep") %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_con | (!grepl("1",Unit) & Plant %in% pub_g20_plants_con)) %>%
      filter(Status=="construction") %>%
      mutate(Status="con") %>%
      group_by(Financing.Country,Financing.Country.name,Country.name,Financing.Country.name,.add=T) %>%
      summarise(PreOp.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_oper | (!grepl("1",Unit) & Plant %in% pub_g20_plants_oper)) %>%
      filter(Status=="operating") %>%
      group_by(Financing.Country,Financing.Country.name,Country.name,Financing.Country.name,.add=T) %>%
      summarise(Oper.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Status %in% c("announced","pre-permit","permitted","shelved") & !(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country)) %>%
      group_by(Financing.Country,Financing.Country.name,Country.name,Financing.Country.name,.add=T) %>%
      summarise(PreCon.NonPubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Status %in% c("announced","pre-permit","permitted","shelved","construction") & !(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country)) %>%
      group_by(Financing.Country,Financing.Country.name,Country.name,Financing.Country.name,.add=T) %>%
      summarise(PreOp.NonPubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep"))  %>%
  full_join(
    finCoal %>%
      filter(!(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country) & Status=="operating") %>%
      group_by(Financing.Country,Financing.Country.name,Country.name,Financing.Country.name,.add=T) %>%
      summarise(Oper.NonPubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")
  )
# write_xlsx(finEx_by_creditor,"~/Coal_Finance_Exit/Summary_FULL_g20_overseas_proj_by_financier.xlsx")


### Finance & Capacity Flows by Country Pairs ###
country_pair_finEx <- finCoal %>%
  filter(Unit %in% pub_g20_units_precon$Unit | (!grepl("1",Unit) & Plant %in% pub_g20_plants_precon) & !is.na(capacity_weighted_by_funding_share_MW)) %>%
  mutate(Status="precon") %>%
  group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
  summarise(PreCon.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep") %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_con | (!grepl("1",Unit) & Plant %in% pub_g20_plants_con)) %>%
      mutate(Status="preop") %>%
      group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
      summarise(PreOp.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
    finCoal %>%
      filter(Unit %in% pub_g20_units_oper | (!grepl("1",Unit) & Plant %in% pub_g20_plants_oper)) %>%
      group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
      summarise(Oper.PubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved") & !(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country)) %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(PreCon.NonPubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved","construction") & !(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country)) %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(PreOp.NonPubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep"))  %>%
  full_join(
   finCoal %>%
     filter(!(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country) & Status=="operating") %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(Oper.NonPubG20.GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep"))  %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved") & ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country) %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(PreCon.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved","construction") & ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country) %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(PreOp.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep"))  %>%
  full_join(
   finCoal %>%
     filter(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country & Status=="operating") %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(Oper.PubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep"))  %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved") & !(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country)) %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(PreCon.NonPubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep")) %>%
  full_join(
   finCoal %>%
     filter(Status %in% c("announced","pre-permit","permitted","shelved","construction") & !(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country)) %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(PreOp.NonPubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep"))  %>%
  full_join(
   finCoal %>%
     filter(!(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country) & Status=="operating") %>%
     group_by(Financing.Country,Financing.Country.name,Country.name,Country,.add=T) %>%
     summarise(Oper.NonPubG20.bbUSD=sum(transaction_estim*1e-3),.groups="keep"))

# write_xlsx(country_pair_finEx,"~/Coal_Finance_Exit/Summary_FULL_Paired_g20_overseas_proj.xlsx")
# 
# country_pair_finEx <- country_pair_finEx %>%
#   mutate(across(everything(),~ifelse(is.na(.x),0,.x))) %>%
#   mutate(unit=)



#### GCPT 2022 ####
gcpt2021 <- read_excel("C:/Users/stephenb/Downloads/madrat_main/sources/GCPT/GCPT_data_July2021.xlsx",skip=3)

gcpt21_by_finEx_host <- gcpt2021 %>%
  filter(!grepl("Total|Percent|World|Global", Country)) %>%
  mutate(Country=toolCountry2isocode(Country)) %>%
  mutate(PreCon.Tot.GW = (`Announced + Pre-permit + Permitted`+Shelved)*1e-3) %>%
  mutate(Ann.Tot.GW = Announced*1e-3) %>%
  mutate(Pre.Tot.GW = `Pre-permit`*1e-3) %>%
  mutate(Perm.Tot.GW = Permitted*1e-3) %>%
  mutate(She.Tot.GW = Shelved*1e-3) %>%
  mutate(Con.Tot.GW = Construction*1e-3) %>%
  mutate(Oper.Tot.GW = Operating*1e-3) %>%
  select(Country,PreCon.Tot.GW,Con.Tot.GW,Oper.Tot.GW,Ann.Tot.GW,Pre.Tot.GW,Perm.Tot.GW,She.Tot.GW)

merge_nm_gcpt2021 <- finEx_by_host %>%
  full_join(gcpt21_by_finEx_host,by = c("Country")) %>%
  mutate(across(everything(),~replace_non_finite(.x,replace  =  0))) %>%
  mutate(PreCon.NonPubG20.GW = PreCon.Tot.GW-PreCon.PubG20.GW) %>%
  mutate(Ann.NonPubG20.GW = Ann.Tot.GW-Ann.PubG20.GW) %>%
  mutate(Pre.NonPubG20.GW = Pre.Tot.GW-Pre.PubG20.GW) %>%
  mutate(Perm.NonPubG20.GW = Perm.Tot.GW-Perm.PubG20.GW) %>%
  mutate(She.NonPubG20.GW = She.Tot.GW-She.PubG20.GW) %>%
  mutate(Con.NonPubG20.GW = Con.Tot.GW-Con.PubG20.GW) %>%
  mutate(Oper.NonPubG20.GW = Oper.Tot.GW - Oper.PubG20.GW) %>%
  mutate(across(everything(), ~ ifelse(. < 0, 0, .)))

mag_nm_gcpt21 <- as.magpie(merge_nm_gcpt2021[,-2],spatial=1,temporal=0)
reg_mag_nm_gcpt21 <- toolAggregate(toolCountryFill(mag_nm_gcpt21,fill=0),rel=map,weight = NULL)

# # write_xlsx(merge_nm_gcpt2021,
  # "~/Coal_Finance_Exit/G20_overseas_vs_all_proj_by_host.xlsx")
  
gcpt2020 <- read_excel("C:/Users/stephenb/Downloads/madrat_main/sources/GCPT/GCPT_data_Jan2021.xlsx",sheet="Summary",range="A4:K112")

gcpt20_by_finEx_host <- gcpt2020 %>%
  # filter(Country %in% unique(finEx_by_host$Country.name)) %>%
  mutate(Country=toolCountry2isocode(Country)) %>%
  mutate(PreCon.Tot.GW=(`Announced + Pre-permit + Permitted`+Shelved)*1e-3) %>%
  mutate(Ann.Tot.GW=Announced*1e-3) %>%
  mutate(Pre.Tot.GW=`Pre-permit`*1e-3) %>%
  mutate(Perm.Tot.GW=Permitted*1e-3) %>%
  mutate(She.Tot.GW=Shelved*1e-3) %>%
  mutate(Con.Tot.GW=Construction*1e-3) %>%
  mutate(Oper.Tot.GW=Operating*1e-3) %>%
  select(Country,PreCon.Tot.GW,Con.Tot.GW,Oper.Tot.GW,Ann.Tot.GW,Pre.Tot.GW,Perm.Tot.GW,She.Tot.GW)

merge_nm_gcpt2020 <- finEx_by_host %>%
  full_join(gcpt20_by_finEx_host,by="Country") %>%
  mutate(across(everything(),~replace_non_finite(.x,replace = 0))) %>%
  mutate(PreCon.NonPubG20.GW=PreCon.Tot.GW-PreCon.PubG20.GW) %>%
  mutate(Ann.NonPubG20.GW=Ann.Tot.GW-Ann.PubG20.GW) %>%
  mutate(Pre.NonPubG20.GW=Pre.Tot.GW-Pre.PubG20.GW) %>%
  mutate(Perm.NonPubG20.GW=Perm.Tot.GW-Perm.PubG20.GW) %>%
  mutate(She.NonPubG20.GW=She.Tot.GW-She.PubG20.GW) %>%
  mutate(Con.NonPubG20.GW=Con.Tot.GW-Con.PubG20.GW) %>%
  mutate(Oper.NonPubG20.GW=Oper.Tot.GW - Oper.PubG20.GW) %>%
  mutate(across(everything(), ~ ifelse(. < 0, 0, .)))

# %>%
#   mutate(`preCon%`=replace_non_finite(PreCon.PubG20.GW/preCon.Tot.GW,replace = 0)) %>%
#   mutate(`preOp%`=replace_non_finite(PreOp.PubG20.GW/preOp.Tot.GW,replace = 0)) %>%
#   mutate(`Oper%`=replace_non_finite(Oper.PubG20.GW/Oper.Tot.GW,replace = 0))

mag_nm_gcpt20 <- as.magpie(merge_nm_gcpt2020[,-2],spatial=1,temporal=0)
reg_mag_nm_gcpt20 <- toolAggregate(toolCountryFill(mag_nm_gcpt20,fill=0),rel=map,weight = NULL)

# write.magpie(reg_mag_nm_gcpt20,
           # "~/Coal_Finance_Exit/G20_overseas_vs_gcpt2020_by_host.csv")
