### OECD Public Energy outlay ###
oecd_pub_Energy_disb <- 
  bind_rows(oecd_mob_data$`Public ODA disburse Energy`,oecd_mob_data$`Public OOF disburse Energy`) %>%
  replace(is.na(.),0) %>% 
  group_by(Recipient) %>%
  mutate(`Tot 2018-19` = sum(`2018`,`2019`)) %>%
  summarise(across(everything(),sum)) %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  filter(Country %in% map$CountryCode) %>%
  rename(CountryCode=Country) %>%
  left_join(map, by = "CountryCode") %>%
  select(-X) %>%
  rename(Region = RegionCode)


### OECD Public Energy Finance Commitments ###
oecd_pub_Energy_com <- 
  bind_rows(oecd_mob_data$`Public ODA commit Energy`,oecd_mob_data$`Public OOF commit Energy`) %>%
  replace(is.na(.),0) %>% 
  group_by(Recipient) %>%
  mutate(`Tot 2018-19` = sum(`2018`,`2019`)) %>% 
  summarise(across(everything(),sum)) %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  filter(Country %in% map$CountryCode) %>%
  rename(CountryCode=Country) %>%
  left_join(map, by = "CountryCode") %>%
  select(-X) %>%
  rename(Region = RegionCode)


# OECD Public Grant-equivalent development finance
oecd_pub_Energy_grant <- 
  oecd_mob_data$`Public Grant Eq Energy` %>%
  replace(is.na(.),0) %>% 
  group_by(Recipient) %>%
  mutate(`Tot 2018-19` = sum(`2018`,`2019`)) %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  filter(Country %in% map$CountryCode) %>%
  rename(CountryCode=Country) %>%
  left_join(map, by = "CountryCode") %>%
  select(-X) %>%
  rename(Region = RegionCode)


# Private Capital Mobilized by OECD public spending

### REDIRECT-HI SCENARIO ###
oecd_pvt_Energy_mob <- 
  full_join(
    oecd_mob_data$`Private mob all Energy` %>% 
      replace(is.na(.),0) %>% 
      group_by(Recipient) %>%
      mutate(`Tot 2018-19` = sum(`2018`,`2019`)) %>%
      select(Recipient, `Tot 2012-19`, `Tot 2018-19`),
    oecd_mob_data$`Private mob guar Energy` %>%
      replace(is.na(.),0) %>% 
      group_by(Recipient) %>%
      rename(`Guar 2012-19` = `Tot 2012-19`) %>%
      mutate(`Guar 2018-19` = sum(`2018`,`2019`)) %>%
      select(Recipient, `Guar 2012-19`, `Guar 2018-19`),
    by ="Recipient") %>%
  replace(is.na(.),0) %>% 
  mutate(`NoGuar 2018-19` = `Tot 2018-19` - `Guar 2018-19`) %>%
  mutate(`NoGuar 2012-19` = `Tot 2012-19` - `Guar 2012-19`) %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(CountryCode=toolCountry2isocode(Recipient)) %>%
  filter(CountryCode %in% map$CountryCode) %>%
  left_join(map, by = "CountryCode") %>%
  select(-X) %>%
  rename(Region = RegionCode)


## Private Mobilization rates due to grant-equivalent portions of public finance ##
mob_rate_oecd_Energy_grant <- 
  full_join(oecd_pub_Energy_grant %>% rename(Public=`Tot 2018-19`) %>% select(Recipient,`Public`,Region),
            oecd_pvt_Energy_mob %>% rename(Private=`Tot 2018-19`) %>% select(Recipient,`Private`,Region),
            by = c("Recipient","Region")
  ) %>%
  replace(is.na(.),0) %>% 
  mutate(Mobilization_rate = Private/Public)

regi_oecd_mob_rate_Energy_grant <- mob_rate_oecd_Energy_grant %>%
  group_by(Region) %>%
  summarise(sum(Private)/sum(Public))

aggregate_oecd_mob_rate_Energy_grant <- mob_rate_oecd_Energy_grant %>%
  ungroup() %>%
  summarise(sum(Private)/sum(Public))


### OECD mobilized private Energy finance WITHOUT guarantees ###
# oecd_pvt_Energy_mob_no_guar <- 
  # bind_rows(
  #   filter(oecd_pvt_Energy_mob,
  #          !(Recipient %in% oecd_mob_data$`Private mob guar Energy`$Recipient)),
  #   bind_cols(
  #     select(oecd_mob_data$`Private mob guar Energy`,Recipient),
  #     select(filter(oecd_mob_data$`Private mob all Energy`,
  #                   Recipient %in% oecd_mob_data$`Private mob guar Energy`$Recipient),-Recipient) - 
  #       select(oecd_mob_data$`Private mob guar Energy`,-Recipient)
  #   )
  # ) %>%
  # replace(is.na(.),0) %>% 
  # group_by(Recipient) %>%
  # replace(.=="China (People's Republic of)","China") %>% 
  # replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  # filter(!grepl("unspecified",Recipient)) %>%
  # mutate(Country=toolCountry2isocode(Recipient)) %>%
  # filter(Country %in% map$CountryCode) %>%
  # rename(CountryCode=Country) %>%
  # left_join(map, by = "CountryCode") %>%
  # select(-X) %>%
  # rename(Region = RegionCode)

oecd_pvt_Energy_mob_no_guar <- 
  oecd_pvt_Energy_mob %>%
  select(Recipient, Region, `NoGuar 2018-19`, `NoGuar 2012-19`) %>%
  replace(is.na(.),0) 


### Time-aggregate Energy OECD Mobilization rates (Default WITH Guarantees) ###
mob_rate_oecd_Energy <- full_join(
  oecd_pvt_Energy_mob %>% rename(Private=`Tot 2012-19`) %>% select(Recipient,Region,`Private`),
  oecd_pub_Energy_disb %>% rename(Public=`Tot 2012-19`) %>% select(Recipient,Region,`Public`),
  by = c("Recipient","Region")
) %>%
  replace(is.na(.),0) %>% 
  mutate(Mobilization_rate = Private/Public)

regi_oecd_mob_rate_Energy_grant <- mob_rate_oecd_Energy_grant %>%
  group_by(Region) %>%
  summarise(sum(Private)/sum(Public))

aggregate_oecd_mob_rate_Energy <- mob_rate_oecd_Energy %>% 
  ungroup() %>%
  summarise(sum(Private)/sum(Public))


## Non-guarantees ##
mob_rate_oecd_Energy_no_guar <- full_join(
  oecd_pvt_Energy_mob_no_guar %>% rename(Private=`NoGuar 2012-19`) %>% select(Recipient,Region,`Private`),
  oecd_pub_Energy_disb %>% rename(Public=`Tot 2012-19`) %>% select(Recipient,Region,`Public`),
  by = c("Recipient","Region")
) %>%
  replace(is.na(.),0) %>% 
  mutate(Mobilization_rate = Private/Public)

regi_oecd_mob_rate_Energy_grant <- mob_rate_oecd_Energy_grant %>%
  group_by(Region) %>%
  summarise(sum(Private)/sum(Public))

aggregate_oecd_mob_rate_Energy_no_guar <- mob_rate_oecd_Energy_no_guar %>% ungroup() %>% summarise(weighted.mean(Mobilization_rate,Public))



#####################################################################################################################
############# FIRST CHOICE FOR REDIRECT-HI SCENARIOS #############
##################################################################

## Grant-equiv rate WITHOUT guarantees ##
mob_rate_oecd_Energy_no_guar_grant <- full_join(
  oecd_pvt_Energy_mob_no_guar %>% 
    rename(Private=`NoGuar 2018-19`) %>% 
    select(Recipient,Region,`Private`),
  oecd_pub_Energy_grant %>% 
    rename(Public=`Tot 2018-19`) %>% 
    select(Recipient,Region,`Public`),
  by = c("Recipient","Region")
) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  # mutate(Recipient = countrycode(Recipient, "country.name", "iso3c")) %>%
  filter(!is.na(Country)) %>%
  replace(is.na(.),0) %>% 
  full_join(map, by = c("Country"="CountryCode", "Region" = "RegionCode", "Recipient" = "X")) %>%
  mutate(Mobilization_rate = Private/Public)

regi_oecd_mob_rate_Energy_no_guar_grant <- mob_rate_oecd_Energy_no_guar_grant %>%
  filter(is.finite(Mobilization_rate)) %>%
  group_by(Region) %>%
  summarise(Regi_mob_rate = sum(Private)/sum(Public))

mob_rate_oecd_Energy_no_guar_grant <- mob_rate_oecd_Energy_no_guar_grant %>%
  left_join(regi_oecd_mob_rate_Energy_no_guar_grant, by = "Region") %>%
  replace(is.na(.),0) %>% 
  mutate(Mobilization_rate = 
           ifelse(is.finite(Mobilization_rate) && Mobilization_rate > 0,
                  Mobilization_rate,
                  Regi_mob_rate))

aggregate_oecd_mob_rate_Energy_no_guar_grant <- mob_rate_oecd_Energy_no_guar_grant %>% 
  ungroup() %>% 
  summarise(weighted.mean(Mobilization_rate,Public))

mob_rate_oecd_Energy_no_guar_grant %>% 
  ungroup() %>% 
  summarise(sum(Private) / sum(Public))

#####################################################################################################################



## OAS ##
filter(mob_rate_oecd_Energy,Recipient %in% map$X[which(map$RegionCode=="OAS")]) -> oas_mob_rate_Energy
oas_mob_rate_Energy %>% summarise(weighted.mean(Mobilization_rate,Public))

filter(mob_rate_oecd_Energy_no_guar,Recipient %in% map$X[which(map$RegionCode=="OAS")]) -> oas_mob_rate_Energy_no_guar
oas_mob_rate_Energy_no_guar %>% summarise(weighted.mean(Mobilization_rate,Public))


### DEFAULT MOBILIZATION RATES BY COUNTRY (DISBURSED, WITH GUARANTEES) ###
## ALL FINEX HOSTS ##
## All Financiers (inc China) ##
mob_rate_finEx_hosts_Energy <- mob_rate_oecd_Energy %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(finEx_by_host,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  replace(is.na(.),0) %>%
  full_join(finEx_by_host %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD) %>%
              replace(is.na(.),0),
            by=c("Recipient"="Country.name"))

mob_rate_finEx_hosts_Energy <- mob_rate_finEx_hosts_Energy %>%
  ungroup() %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(mob_rate_finEx_hosts_Energy$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(mob_rate_finEx_hosts_Energy$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(mob_rate_finEx_hosts_Energy$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_Energy), Mobilization_rate)) %>%
  mutate(Energydir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

write.csv(mob_rate_finEx_hosts_Energy,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/AllFinEx_OECDmobRates_disb_incGuar_byHost.csv")

# No Guarantees #
mob_rate_finEx_hosts_no_guar <- mob_rate_oecd_Energy_no_guar %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(finEx_by_host,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  replace(is.na(.),0) %>%
  full_join(finEx_by_host %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD) %>%
              replace(is.na(.),0),
            by=c("Recipient"="Country.name"))

mob_rate_finEx_hosts_no_guar <- mob_rate_finEx_hosts_no_guar %>%
  ungroup() %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(mob_rate_finEx_hosts_no_guar$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(mob_rate_finEx_hosts_no_guar$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(mob_rate_finEx_hosts_no_guar$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_Energy_no_guar), Mobilization_rate)) %>%
  mutate(Energydir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

write.csv(mob_rate_finEx_hosts_no_guar,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/AllFinEx_OECDmobRates_disb_noGuar_byHost.csv")


## Only OECD financiers ##
OECDmap <- toolGetMapping("C:/Users/stephenb/Downloads/madrat_main/mappings/regional/regionmappingOECD.csv")

oecd_preCon_finFlows <- preCon_finFlows %>% 
  filter(Financing.Country.name %in% OECDmap$X[which(OECDmap$RegionCode=="OECD")]) %>%
  group_by(Country.name) %>%
  summarise(PreCon.PubG20.bbUSD=sum(PreCon.PubG20.bbUSD))

oecd_mob_rate_finEx_hosts <- mob_rate_oecd_Energy %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(oecd_preCon_finFlows,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  full_join(oecd_preCon_finFlows %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD),
            by=c("Recipient"="Country.name")) %>%
  replace(is.na(.),0)

oecd_mob_rate_finEx_hosts <- oecd_mob_rate_finEx_hosts %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(oecd_mob_rate_finEx_hosts$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(oecd_mob_rate_finEx_hosts$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(oecd_mob_rate_finEx_hosts$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_Energy), Mobilization_rate)) %>%
  mutate(Energydir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

write.csv(oecd_mob_rate_finEx_hosts,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/OECDFinEx_OECDmobRates_disb_incGuar_byHost.csv")

# No Guarantees #
oecd_mob_rate_finEx_hosts_no_guar <- mob_rate_oecd_Energy_no_guar %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(oecd_preCon_finFlows,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  full_join(oecd_preCon_finFlows %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD),
            by=c("Recipient"="Country.name")) %>%
  replace(is.na(.),0)

oecd_mob_rate_finEx_hosts_no_guar <- oecd_mob_rate_finEx_hosts_no_guar %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(oecd_mob_rate_finEx_hosts_no_guar$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(oecd_mob_rate_finEx_hosts_no_guar$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(oecd_mob_rate_finEx_hosts_no_guar$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_Energy_no_guar), Mobilization_rate)) %>%
  mutate(Energydir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

write.csv(oecd_mob_rate_finEx_hosts_no_guar,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/OECDFinEx_OECDmobRates_disb_noGuar_byHost.csv")

## Non-China Financiers ##
noCHN_preCon_finFlows <- preCon_finFlows %>% 
  filter(Financing.Country.name!="China") %>%
  group_by(Country.name) %>%
  summarise(PreCon.PubG20.bbUSD=sum(PreCon.PubG20.bbUSD))

noCHN_mob_rate_finEx_hosts <- mob_rate_oecd_Energy %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(noCHN_preCon_finFlows,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  full_join(noCHN_preCon_finFlows %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD),
            by=c("Recipient"="Country.name")) %>%
  replace(is.na(.),0)

noCHN_mob_rate_finEx_hosts <- noCHN_mob_rate_finEx_hosts %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(noCHN_mob_rate_finEx_hosts$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(noCHN_mob_rate_finEx_hosts$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(noCHN_mob_rate_finEx_hosts$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_Energy), Mobilization_rate)) %>%
  mutate(Energydir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

write.csv(noCHN_mob_rate_finEx_hosts,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/nonCHN_FinEx_OECDmobRates_disb_incGuar_byHost.csv")

# No Guarantees #
noCHN_mob_rate_finEx_hosts_no_guar <- mob_rate_oecd_Energy_no_guar %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(noCHN_preCon_finFlows,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  full_join(noCHN_preCon_finFlows %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD),
            by=c("Recipient"="Country.name")) %>%
  replace(is.na(.),0)

noCHN_mob_rate_finEx_hosts_no_guar <- noCHN_mob_rate_finEx_hosts_no_guar %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(noCHN_mob_rate_finEx_hosts_no_guar$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(noCHN_mob_rate_finEx_hosts_no_guar$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(noCHN_mob_rate_finEx_hosts_no_guar$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_Energy_no_guar), Mobilization_rate)) %>%
  mutate(Energydir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

# write.csv(noCHN_mob_rate_finEx_hosts_no_guar,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/nonCHN_FinEx_OECDmobRates_disb_noGuar_byHost.csv")


#######################################
### REdirect-HI PreCon Nat OECD mob ###
#######################################
mob_rate_oecd_Energy_no_guar_grant <- mob_rate_oecd_Energy_no_guar_grant %>%
  filter(Country %in% map$CountryCode) %>% 
  ungroup()

mag_mob_rate_oecd_Energy_no_guar_grant <- as.magpie(mob_rate_oecd_Energy_no_guar_grant %>% select(-Recipient,-Region))

mag_mob_rate_oecd_Energy_no_guar_grant <- toolCountryFill(mag_mob_rate_oecd_Energy_no_guar_grant,fill = 0)

mag_mob_rate_oecd_Energy_no_guar_grant[,,"Mobilization_rate"] <-
  toolNAreplace(
    toolFillWithRegionAvg(
      x = mag_mob_rate_oecd_Energy_no_guar_grant[,,"Mobilization_rate"],
      valueToReplace = 0,
      weight = mag_mob_rate_oecd_Energy_no_guar_grant[,,"Public"],
      regionmapping = map),
    replaceby = 0)$x
  

reg_mag_mob_rate_oecd_Energy_no_guar_grant <- toolAggregate(mag_mob_rate_oecd_Energy_no_guar_grant[,,"Mobilization_rate"], map, mag_mob_rate_oecd_Energy_no_guar_grant[,,"Public"])

regi_oecd_mob_rate_REdirect_HI <- regi_oecd_mob_rate_Energy_no_guar_grant

#######################################
### REdirect-HI PreCon Reg OECD mob ###
#######################################
write.magpie(setItems(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"]*(1+reg_mag_mob_rate_oecd_Energy_no_guar_grant),dim=3,value = "REdir_HI"),"~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_HI_oecd_reg_mob.cs4r")

precon_REdir_HI_oecd_nat_mob <- finEx_by_host %>% 
  rename(CountryCode=Country) %>%
  select(CountryCode, Country.name, PreCon.PubG20.bbUSD) %>%
  left_join(mob_rate_oecd_Energy_no_guar_grant %>% 
              rename(CountryCode=Country) %>%
              filter(!(Recipient %in% c("Cote d Ivoire","Tanzania, United Republic of"))) %>%
              select(-Region,-Recipient),
            by = "CountryCode") %>%
  mutate(
    Mobilization_rate = ifelse(is.na(Public) | is.na(Private),
                                    Regi_mob_rate,
                                    Mobilization_rate)
    ) %>%
  mutate(REdir_oecd_mob = PreCon.PubG20.bbUSD * (Mobilization_rate)) %>%
  rename(Country = Country.name) %>%
  left_join(map, by = "CountryCode") %>%
  select(-X) %>%
  rename(Region = RegionCode) %>%
  ungroup() %>%
  replace(is.na(.),0)

glo_REdir_HI_finEx_mob <- precon_REdir_HI_oecd_nat_mob %>%
  summarise(Mobilization_rate = weighted.mean(Mobilization_rate,Public),
            PreCon.PubG20.bbUSD = sum(PreCon.PubG20.bbUSD),
            REdir_oecd_mob = sum(REdir_oecd_mob),
            .groups="keep") %>%
  mutate(Region="GLO",
         CountryCode="GLO",
         Country="All FinEx Hosts")

precon_REdir_HI_oecd_nat_mob <- precon_REdir_HI_oecd_nat_mob %>%
  bind_rows(glo_REdir_HI_finEx_mob)
  

nat_mag_precon_REdir_HI_oecd_nat_mob <- setNames(
  toolCountryFill(
    as.magpie(precon_REdir_HI_oecd_nat_mob %>% 
                select(CountryCode,REdir_oecd_mob)),
    fill = 0,
    verbosity = 2),
  nm = NULL)

mag_precon_REdir_HI_oecd_nat_mob <- 
  toolAggregate(
    nat_mag_precon_REdir_HI_oecd_nat_mob,
    rel=map, 
    partrel = T,
    weight = NULL)

write.magpie(setNames(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + mag_precon_REdir_HI_oecd_nat_mob, nm = NULL),
             "~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_hi_oecd_nat_mob.cs4r")
write.magpie(setNames(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + mag_precon_REdir_HI_oecd_nat_mob, nm = NULL),
             "P:/REMIND_3p0_dev/remind/modules/47_regipol/PPCAcoalExit/input/p47_FinEx_REdirect_hi_oecd_nat_mob.cs4r")
write.magpie(setNames(mag_nat_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + nat_mag_precon_REdir_HI_oecd_nat_mob, nm = NULL),
             "~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_hi_oecd_nat_mob_country.cs4r")
write.magpie(setNames(mag_nat_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + nat_mag_precon_REdir_HI_oecd_nat_mob, nm = NULL),
             "P:/REMIND_3p0_dev/remind/modules/47_regipol/PPCAcoalExit/input/p47_FinEx_REdirect_hi_oecd_nat_mob_country.cs4r")
