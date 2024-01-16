library(writexl)

map_glo <- map %>% add_row(X = "All FinEx Hosts",
                CountryCode = "GLO",
                RegionCode = "GLO")

oecd_filename <- "C:/Users/stephenb/Documents/Coal_Finance_Exit/OECD mobilization data/Summaries/OECD_Bilateral_finance_public_commit_disburse_private_RE_nonRE_jpn_kor.xlsx"

oecd_mob_data <- lapply(excel_sheets(oecd_filename),
                        function(x) read_excel(oecd_filename,sheet=x,range="A1:K191",na="..",col_types=c("text",rep("numeric",10))))

names(oecd_mob_data) <- excel_sheets(oecd_filename)

oecd_mob_data <- 
  lapply(1:length(oecd_mob_data),
       function(i) 
         oecd_mob_data[[i]] %>%
         select(!contains(c("...","2011"))) %>%
         filter(!(is.na(Recipient) | grepl("Total",Recipient) | grepl("regional",Recipient) | grepl("unspecified", Recipient))) 
       # %>%
       #   filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD))
       )

names(oecd_mob_data) <- excel_sheets(oecd_filename)

### OECD Public RE outlay ###
oecd_pub_RE_disb <- 
  bind_rows(oecd_mob_data$`Public ODA disburse RE`,oecd_mob_data$`Public OOF disburse RE`) %>%
  # filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD)) %>% 
  group_by(Recipient) %>%
  summarise(across(everything(),sum))

oecd_pub_RE_commit <- 
  bind_rows(oecd_mob_data$`Public ODA commit RE`,oecd_mob_data$`Public OOF commit RE`) %>%
  # filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD)) %>% 
  group_by(Recipient) %>% 
  summarise(across(everything(),sum))


### OECD mobilized private RE finance ###
oecd_pvt_RE_mob_no_guar <- 
  bind_rows(
    filter(oecd_mob_data$`Private mob all RE`,
           !(Recipient %in% oecd_mob_data$`Private mob guar RE`$Recipient)),
    bind_cols(
      select(oecd_mob_data$`Private mob guar RE`,Recipient),
      select(filter(oecd_mob_data$`Private mob all RE`,
                    Recipient %in% oecd_mob_data$`Private mob guar RE`$Recipient),-Recipient) - 
      select(oecd_mob_data$`Private mob guar RE`,-Recipient)
      )
    )

### Time-aggregate RE OECD Mobilization rates ###
# Disbursals #
mob_rate_oecd_RE <- full_join(
  oecd_mob_data$`Private mob all RE` %>% rename(Private=`Tot 2012-19`) %>% select(Recipient,`Private`),
  oecd_pub_RE_disb %>% rename(Public=`Tot 2012-19`) %>% select(Recipient,`Public`),
  by = "Recipient") %>%
  mutate(Private = replace_non_finite(Private, replace=0)) %>%
  mutate(Public = replace_non_finite(Public, replace=0)) %>%
  mutate(Mobilization_rate = replace_non_finite(Private/Public, replace=0)) %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  filter(Country %in% map$CountryCode) %>%
  rename(CountryCode=Country) %>%
  left_join(map, by = "CountryCode") %>%
  select(-CountryCode) %>%
  rename(Region = RegionCode)

reg_oecd_mob_rate_RE <- mob_rate_oecd_RE %>% group_by(Region) %>% summarise(weighted.mean(Mobilization_rate,Public))
aggregate_oecd_mob_rate_RE <- mob_rate_oecd_RE %>% summarise(weighted.mean(Mobilization_rate,Public))
mob_rate_oecd_RE %>% summarise(sum(Private) / sum(Public))
mob_rate_oecd_RE %>% summarise(sum(Private))
mob_rate_oecd_RE %>% summarise(sum(Public))

# Commitments #
mob_rate_oecd_RE_commit <- full_join(
  oecd_mob_data$`Private mob all RE` %>% rename(Private=`Tot 2012-19`) %>% select(Recipient,`Private`),
  oecd_pub_RE_commit %>% rename(Public=`Tot 2012-19`) %>% select(Recipient,`Public`),
  by = "Recipient") %>%
  mutate(Private = replace_non_finite(Private, replace=0)) %>%
  mutate(Public = replace_non_finite(Public, replace=0)) %>%
  mutate(Mobilization_rate = replace_non_finite(Private/Public, replace=0)) %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  filter(Country %in% map$CountryCode) %>%
  rename(CountryCode=Country) %>%
  left_join(map, by = "CountryCode") %>%
  select(-CountryCode) %>%
  rename(Region = RegionCode)

mob_rate_oecd_RE_commit %>% summarise(sum(Private) / sum(Public))


## REDIRECT-LO ##
## Disbursals w/o guarantees ##
mob_rate_oecd_RE_no_guar_disb <- full_join(
  oecd_pvt_RE_mob_no_guar %>% rename(Private=`Tot 2012-19`) %>% select(Recipient,`Private`),
  oecd_pub_RE_disb %>% rename(Public=`Tot 2012-19`) %>% select(Recipient,`Public`),
  by = "Recipient"
) %>%
  mutate(Private = replace_non_finite(Private, replace=0)) %>%
  mutate(Public = replace_non_finite(Public, replace=0)) %>%
  mutate(Mobilization_rate = replace_non_finite(Private/Public, replace=0))

aggregate_oecd_mob_rate_RE_no_guar <- mob_rate_oecd_RE_no_guar_disb %>% summarise(weighted.mean(Mobilization_rate,Public))
mob_rate_oecd_RE_no_guar_disb %>% summarise(sum(Public))
mob_rate_oecd_RE_no_guar_disb %>% summarise(sum(Private))
mob_rate_oecd_RE_no_guar_disb %>% summarise(sum(Private)/sum(Public))

## OAS ##
filter(mob_rate_oecd_RE,Recipient %in% map$X[which(map$RegionCode=="OAS")]) -> oas_mob_rate_RE
oas_mob_rate_RE %>% summarise(weighted.mean(Mobilization_rate,Public))

filter(mob_rate_oecd_RE_no_guar_disb,Recipient %in% map$X[which(map$RegionCode=="OAS")]) -> oas_mob_rate_RE_no_guar
oas_mob_rate_RE_no_guar %>% summarise(weighted.mean(Mobilization_rate,Public))


########################################################################################################
#### FIRST CHOICE FOR REDIRECT-LO SCENARIO ###
##############################################
## Commitments w/o Guarantees ##
mob_rate_oecd_RE_no_guar_commit <- full_join(
  oecd_pvt_RE_mob_no_guar %>% rename(Private=`Tot 2012-19`) %>% select(Recipient,`Private`),
  oecd_pub_RE_commit %>% rename(Public=`Tot 2012-19`) %>% select(Recipient,`Public`),
  by = "Recipient"
) %>%
  mutate(Private = replace_non_finite(Private, replace=0)) %>%
  mutate(Public = replace_non_finite(Public, replace=0)) %>%
  mutate(Mobilization_rate = replace_non_finite(Private/Public, replace=0))

aggregate_oecd_mob_rate_RE_no_guar_commit <- mob_rate_oecd_RE_no_guar_commit %>% 
  summarise(weighted.mean(Mobilization_rate,Public))
mob_rate_oecd_RE_no_guar_commit %>% summarise(sum(Public))
mob_rate_oecd_RE_no_guar_commit %>% summarise(sum(Private))
mob_rate_oecd_RE_no_guar_commit %>% summarise(sum(Private)/sum(Public))

########################################################################################################





### DEFAULT MOBILIZATION RATES BY COUNTRY (DISBURSED, WITH GUARANTEES) ###
## ALL FINEX HOSTS ##

## All Financiers (inc China) ##
mob_rate_finEx_hosts <- mob_rate_oecd_RE %>%
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(finEx_by_host,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  full_join(finEx_by_host %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD),
            by=c("Recipient"="Country.name")) %>%
  filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD))

mob_rate_finEx_hosts <- mob_rate_finEx_hosts %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(mob_rate_finEx_hosts$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(mob_rate_finEx_hosts$Private_mob_mUSD),
          Mobilization_rate=replace_non_finite(Private_mob_mUSD/Public_OECD_mUSD,replace=0),
          PreCon.PubG20.bbUSD=sum(mob_rate_finEx_hosts$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_RE), Mobilization_rate)) %>%
  mutate(REdir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

# # write.csv(mob_rate_finEx_hosts,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/AllFinEx_OECDmobRates_disb_incGuar_byHost.csv")


## REDIRECT-LO NATIONAL MOBILIZATION RATES ##
# Disbursals w/o Guarantees #
mob_rate_finEx_hosts_no_guar_disb <- mob_rate_oecd_RE_no_guar_disb %>%
  replace(.=="Viet Nam","Vietnam") %>% 
  rename(Public_OECD_mUSD=Public) %>%
  rename(Private_mob_mUSD=Private) %>%
  filter(Recipient %in% unique(filter(finEx_by_host,PreCon.PubG20.bbUSD>0)$Country.name)) %>%
  full_join(finEx_by_host %>% 
              ungroup() %>%
              filter(PreCon.PubG20.bbUSD>0) %>%
              select(Country.name, PreCon.PubG20.bbUSD) %>%
              rename(PreCon.PubG20.bbUSD=PreCon.PubG20.bbUSD),
            by=c("Recipient"="Country.name")) %>%
  filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD))

mob_rate_finEx_hosts_no_guar_disb <- mob_rate_finEx_hosts_no_guar_disb %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(mob_rate_finEx_hosts_no_guar_disb$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(mob_rate_finEx_hosts_no_guar_disb$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(mob_rate_finEx_hosts_no_guar_disb$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_RE_no_guar), Mobilization_rate)) %>%
  mutate(REdir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

# # write.csv(mob_rate_finEx_hosts_no_guar_disb,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/AllFinEx_OECDmobRates_disb_noGuar_byHost.csv")


## Only OECD financiers ##
OECDmap <- toolGetMapping("C:/Users/stephenb/Downloads/madrat_main/mappings/regional/regionmappingOECD.csv")

oecd_preCon_finFlows <- preCon_finFlows %>% 
  filter(Financing.Country.name %in% OECDmap$X[which(OECDmap$RegionCode=="OECD")]) %>%
  group_by(Country.name) %>%
  summarise(PreCon.PubG20.bbUSD=sum(PreCon.PubG20.bbUSD))

oecd_mob_rate_finEx_hosts <- mob_rate_oecd_RE %>%
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
  filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD))

oecd_mob_rate_finEx_hosts <- oecd_mob_rate_finEx_hosts %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(oecd_mob_rate_finEx_hosts$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(oecd_mob_rate_finEx_hosts$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(oecd_mob_rate_finEx_hosts$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_RE), Mobilization_rate)) %>%
  mutate(REdir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

# # write.csv(oecd_mob_rate_finEx_hosts,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/OECDFinEx_OECDmobRates_disb_incGuar_byHost.csv")

# No Guarantees #
oecd_mob_rate_finEx_hosts_no_guar_disb <- mob_rate_oecd_RE_no_guar_disb %>%
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
  filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD))

oecd_mob_rate_finEx_hosts_no_guar_disb <- oecd_mob_rate_finEx_hosts_no_guar_disb %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(oecd_mob_rate_finEx_hosts_no_guar_disb$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(oecd_mob_rate_finEx_hosts_no_guar_disb$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(oecd_mob_rate_finEx_hosts_no_guar_disb$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_RE_no_guar), Mobilization_rate)) %>%
  mutate(REdir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

# # write.csv(oecd_mob_rate_finEx_hosts_no_guar_disb,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/OECDFinEx_OECDmobRates_disb_noGuar_byHost.csv")

## Non-China Financiers ##
noCHN_preCon_finFlows <- preCon_finFlows %>% 
  filter(Financing.Country.name!="China") %>%
  group_by(Country.name) %>%
  summarise(PreCon.PubG20.bbUSD=sum(PreCon.PubG20.bbUSD))

noCHN_mob_rate_finEx_hosts <- mob_rate_oecd_RE %>%
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
  filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD))

noCHN_mob_rate_finEx_hosts <- noCHN_mob_rate_finEx_hosts %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(noCHN_mob_rate_finEx_hosts$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(noCHN_mob_rate_finEx_hosts$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(noCHN_mob_rate_finEx_hosts$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_RE), Mobilization_rate)) %>%
  mutate(REdir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

# # write.csv(noCHN_mob_rate_finEx_hosts,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/nonCHN_FinEx_OECDmobRates_disb_incGuar_byHost.csv")

# No Guarantees #
noCHN_mob_rate_finEx_hosts_no_guar_disb <- mob_rate_oecd_RE_no_guar_disb %>%
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
  filter(!is.na(Private_mob_mUSD) & !is.na(Public_OECD_mUSD))

noCHN_mob_rate_finEx_hosts_no_guar_disb <- noCHN_mob_rate_finEx_hosts_no_guar_disb %>%
  add_row(Recipient="FinEx Hosts", 
          Public_OECD_mUSD=sum(noCHN_mob_rate_finEx_hosts_no_guar_disb$Public_OECD_mUSD), 
          Private_mob_mUSD=sum(noCHN_mob_rate_finEx_hosts_no_guar_disb$Private_mob_mUSD),
          Mobilization_rate=Private_mob_mUSD/Public_OECD_mUSD,
          PreCon.PubG20.bbUSD=sum(noCHN_mob_rate_finEx_hosts_no_guar_disb$PreCon.PubG20.bbUSD)) %>%
  mutate(Mobilization_rate=ifelse(Mobilization_rate==0, as.numeric(aggregate_oecd_mob_rate_RE_no_guar), Mobilization_rate)) %>%
  mutate(REdir_total_bbUSD = PreCon.PubG20.bbUSD + Mobilization_rate * PreCon.PubG20.bbUSD)

# # write.csv(noCHN_mob_rate_finEx_hosts_no_guar_disb,"~/Coal_Finance_Exit/OECD mobilization data/Summaries/nonCHN_FinEx_OECDmobRates_disb_noGuar_byHost.csv")


######################################################################################################
###### REMIND INPUT FILES ######
######################################################################################################
### DEFAULT REDIRECT (DISBURSALS WITH GUARANTEES) ###
#####################################################
mob_rate_oecd_RE <- mob_rate_oecd_RE %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  filter(Country %in% map$CountryCode)

mag_mob_rate_oecd_RE <- as.magpie(mob_rate_oecd_RE %>% select(-c(Region,Recipient,X)))

mag_mob_rate_oecd_RE <- toolCountryFill(mag_mob_rate_oecd_RE,fill = 0)

reg_mag_mob_rate_oecd_RE <- toolAggregate(mag_mob_rate_oecd_RE[,,"Mobilization_rate"], map, mag_mob_rate_oecd_RE[,,"Public"])

# # write.magpie(reg_mag_mob_rate_oecd_RE,"~/Coal_Finance_Exit/OECD mobilization data/REMIND_input/p47_mob")
  
###################################
### REdirect PreCon Public Only ###
###################################
# write.magpie(setItems(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"],dim=3,value = "REdir_pubOnly"),"~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_pubOnly.cs3r")
# write.magpie(setItems(mag_nat_finEx_by_host[,,"PreCon.PubG20.bbUSD"],dim=3,value = "REdir_pubOnly"),"~/Coal_Finance_Exit/REMIND_input/p47_nat_FinEx_REdirect_pubOnly.cs3r")

####################################
### REdirect PreCon Reg OECD mob ###
####################################
# write.magpie(setItems(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"]*(1+reg_mag_mob_rate_oecd_RE),dim=3,value = "REdir_pubOnly"),"~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_oecd_reg_mob.cs4r")

####################################
### REdirect PreCon Nat OECD mob ###
####################################
precon_REdir_oecd_nat_mob <- finEx_by_host %>% 
  rename(CountryCode=Country) %>%
  select(CountryCode, Country.name, PreCon.PubG20.bbUSD) %>%
  left_join(mob_rate_oecd_RE %>% rename(CountryCode=Country) %>%
              select(-Recipient),
            by = c("CountryCode")) %>%
  mutate(Mobilization_rate = ifelse(is.na(Public) | is.na(Private),
                                    reg_mag_mob_rate_oecd_RE[map$RegionCode[which(CountryCode==map$CountryCode)],,],
                                    Mobilization_rate)) %>%
  mutate(REdir_oecd_mob = PreCon.PubG20.bbUSD * (Mobilization_rate)) %>%
  ungroup() %>%
  select(-X, -Region) %>%
  rename(Country = Country.name) %>%
  replace(is.na(.),0) %>%
  left_join(map, by = "CountryCode")


glo_REdir_finEx_mob <- precon_REdir_oecd_nat_mob %>%
  summarise(Mobilization_rate = weighted.mean(Mobilization_rate,Public),
            PreCon.PubG20.bbUSD = sum(PreCon.PubG20.bbUSD),
            REdir_oecd_mob = sum(REdir_oecd_mob),
            .groups="keep") %>%
  mutate(Region="GLO",
         CountryCode="GLO",
         Country="All FinEx Hosts")

precon_REdir_oecd_nat_mob <- precon_REdir_oecd_nat_mob %>%
  bind_rows(glo_REdir_finEx_mob)


nat_mag_precon_REdir_oecd_nat_mob <- toolCountryFill(
  as.magpie(precon_REdir_oecd_nat_mob %>% select(CountryCode,REdir_oecd_mob) %>% filter(CountryCode!="GLO")),
  fill = 0,
  verbosity = 2)

mag_precon_REdir_oecd_nat_mob <- mbind(
  toolAggregate(
    nat_mag_precon_REdir_oecd_nat_mob,
    rel=map_glo, 
    partrel = T,
    weight = NULL),
  setItems(dimSums(nat_mag_precon_REdir_oecd_nat_mob, dim = 1), dim = 1, value = "GLO")
)
  
# write.magpie(setNames(mag_precon_REdir_oecd_nat_mob,nm=NULL),"~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_oecd_nat_mob.cs4r")
# write.magpie(setNames(mag_precon_REdir_oecd_nat_mob,nm=NULL),"P:/REMIND_3p0_dev/remind/modules/47_regipol/PPCAcoalExit/input/p47_FinEx_REdirect_oecd_nat_mob_country.cs4r")
# write.magpie(setNames(nat_mag_precon_REdir_oecd_nat_mob,nm=NULL),"~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_oecd_nat_mob_country.cs4r")
# write.magpie(setNames(nat_mag_precon_REdir_oecd_nat_mob,nm=NULL),"P:/REMIND_3p0_dev/remind/modules/47_regipol/PPCAcoalExit/input/p47_FinEx_REdirect_oecd_nat_mob_country.cs4r")


#######################################
### REdirect-LO PreCon Nat OECD mob ###
#######################################
mob_rate_oecd_RE_no_guar_commit <- mob_rate_oecd_RE_no_guar_commit %>%
  replace(.=="China (People's Republic of)","China") %>% 
  replace(.=="West Bank and Gaza Strip", "Palestine") %>%
  filter(!grepl("unspecified",Recipient)) %>%
  mutate(Country=toolCountry2isocode(Recipient)) %>%
  filter(Country %in% map$CountryCode)

mag_mob_rate_oecd_RE_no_guar_commit <- as.magpie(mob_rate_oecd_RE_no_guar_commit %>% select(!Recipient))

mag_mob_rate_oecd_RE_no_guar_commit <- toolCountryFill(mag_mob_rate_oecd_RE_no_guar_commit,fill = 0)

reg_mag_mob_rate_oecd_RE_no_guar_commit <- toolAggregate(mag_mob_rate_oecd_RE_no_guar_commit[,,"Mobilization_rate"], map, mag_mob_rate_oecd_RE_no_guar_commit[,,"Public"])


#######################################
### REdirect-LO PreCon Reg OECD mob ###
#######################################
# write.magpie(setItems(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"]*(1+reg_mag_mob_rate_oecd_RE_no_guar_commit),dim=3,value = "REdir_LO"),"~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_LO_oecd_reg_mob.cs4r")

precon_REdir_LO_oecd_nat_mob <- finEx_by_host %>% 
  rename(CountryCode=Country) %>%
  select(CountryCode, Country.name, PreCon.PubG20.bbUSD) %>%
  left_join(mob_rate_oecd_RE_no_guar_commit %>% 
              rename(CountryCode=Country) %>%
              select(-Recipient),
            by = c("CountryCode")) %>%
  mutate(Mobilization_rate = ifelse(is.na(Public) | is.na(Private),
                                    0,
                                    # reg_mag_mob_rate_oecd_RE_no_guar_commit[map$RegionCode[which(CountryCode==map$CountryCode)],,],
                                    Mobilization_rate)) %>%
  mutate(REdir_oecd_mob = PreCon.PubG20.bbUSD * (Mobilization_rate)) %>%
  ungroup() %>%
  replace(is.na(.),0) %>%
  rename(Country = Country.name) %>%
  left_join(map, by = "CountryCode") %>%
  select(-X) %>%
  rename(Region = RegionCode) 

glo_REdir_LO_finEx_mob <- precon_REdir_LO_oecd_nat_mob %>%
  summarise(Mobilization_rate = weighted.mean(Mobilization_rate,Public),
            PreCon.PubG20.bbUSD = sum(PreCon.PubG20.bbUSD),
            REdir_oecd_mob = sum(REdir_oecd_mob),
            .groups="keep") %>%
  mutate(Region="GLO",
         CountryCode="GLO",
         Country="All FinEx Hosts")

precon_REdir_LO_oecd_nat_mob <- precon_REdir_LO_oecd_nat_mob %>%
  bind_rows(glo_REdir_LO_finEx_mob)


nat_mag_precon_REdir_LO_oecd_nat_mob <- setNames(
  toolCountryFill(
  as.magpie(precon_REdir_LO_oecd_nat_mob %>% 
              select(CountryCode,REdir_oecd_mob)),
  fill = 0,
  verbosity = 2),
  nm = NULL)
  
mag_precon_REdir_LO_oecd_nat_mob <- 
  toolAggregate(
    nat_mag_precon_REdir_LO_oecd_nat_mob,
    rel=map, 
    partrel = T,
    weight = NULL)

write.magpie(setNames(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + mag_precon_REdir_LO_oecd_nat_mob, nm = NULL),
             "~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_lo_oecd_nat_mob.cs4r")
write.magpie(setNames(mag_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + mag_precon_REdir_LO_oecd_nat_mob, nm = NULL),
             "P:/REMIND_3p0_dev/remind/modules/47_regipol/PPCAcoalExit/input/p47_FinEx_REdirect_lo_oecd_nat_mob.cs4r")
write.magpie(setNames(mag_nat_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + nat_mag_precon_REdir_LO_oecd_nat_mob, nm = NULL),
             "~/Coal_Finance_Exit/REMIND_input/p47_FinEx_REdirect_lo_oecd_nat_mob_country.cs4r")
write.magpie(setNames(mag_nat_finEx_by_host[,,"PreCon.PubG20.bbUSD"] + nat_mag_precon_REdir_LO_oecd_nat_mob, nm = NULL),
             "P:/REMIND_3p0_dev/remind/modules/47_regipol/PPCAcoalExit/input/p47_FinEx_REdirect_lo_oecd_nat_mob_country.cs4r")
