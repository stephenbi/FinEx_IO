require(readxl)
require(dplyr)
require(mrremind)

map <- toolGetMapping("regionmappingH12.csv",type="regional")
g7 <- unique(c("United States","United Kingdom","Germany","France","Italy","Canada","Japan", map$X[which(map$RegionCode=="EUR")]))
g7_iso <- toolCountry2isocode(g7)

g20_full <- c("Australia","Canada","Saudi Arabia","United States","India","Russia","South Africa","Turkey","Argentina","Brazil","China","Indonesia","Japan","South Korea","Mexico",map$X[which(map$RegionCode=="EUR")])
g20 <- toolCountry2isocode(g20_full)

g18 <- c("Australia","Canada","United States","India","South Africa","Turkey","Argentina","Brazil","China","Indonesia","Japan","South Korea","Mexico",map$X[which(map$RegionCode=="EUR")])
g18_iso <- toolCountry2isocode(g18)

ggpt <- read_excel("C:/Users/stephenb/Downloads/madrat_main/sources/GGPT/Global Gas Plant Tracker (GGPT) completed 2021-10-01.xlsx",sheet=3)

ggpt <- ggpt %>%
  select(-c(`Last Updated`,Region)) %>%
  mutate(Country=ifelse(grepl("Republic of the Congo",Country),"Congo, the Democratic Republic of the",Country)) %>%
  mutate(CountryCode = toolCountry2isocode(Country)) %>%
  left_join(map %>% select(-X),by="CountryCode") %>%
  mutate(MW = as.numeric(`Capacity elec. (MW)`)) %>%
  filter(!is.na(MW)) 
# %>%
#   mutate(Financiers=strsplit(`Parent HQ country`,"; "))

# G7 Cross-border gas plant capacity

cap_G7_XB_gas_precon <- ggpt %>%
  filter(any(unlist(strsplit(Financiers,"; ")) %in% g7) & 
           any(unlist(strsplit(Financiers,"; ")) != rep(Country,length(unlist(strsplit(Financiers,"; "))))) &
           # (CountryCode %in% g7_iso) &
           Status %in% c("proposed")) %>%
  group_by(Country) %>%
  summarise(sum(MW),.groups="keep")

glo_cap_G7_XB_gas_precon <- ggpt %>%
  filter(any(strsplit(`Parent HQ country`,"; ") %in% g7) & 
           !all(grepl(Country,g7)) & 
           Status %in% c("proposed")) %>%
  summarise(sum(MW))

# G18 Cross-border gas plant capacity
cap_G18_XB_gas_precon <- ggpt %>%
  filter(any(unlist(strsplit(Financiers,"; ")) %in% g18) & 
           any(unlist(strsplit(Financiers,"; ")) != rep(Country,length(unlist(strsplit(Financiers,"; "))))) &
           Status %in% c("proposed")) %>%
  group_by(Country) %>%
  summarise(sum(MW),.groups="keep")

glo_cap_G18_XB_gas_precon <- ggpt %>%
  filter(length(intersect(x=Financiers,y=g18)) > 0 & 
           length(setdiff(x=Financiers,y=Country)) > 0 &
           # (CountryCode %in% g18_iso) &
           Status %in% c("proposed")) %>%
  summarise(sum(MW))

ggpt %>%
  filter("China" %in% strsplit(Financiers,"; ")[[1]] & 
           Country!="China" &
           Status %in% c("proposed")) %>%
  summarise(sum(MW))

# Historical public-private split of finance by host
pubFin_nonRE_ODA <- read_excel("C:/Users/stephenb/Documents/Coal_Finance_Exit/OECD mobilization data/Source_data/public_oecd_disbursed_nonRE_ODA.xlsx")

pubFin_nonRE_OOF <- read_excel("C:/Users/stephenb/Documents/Coal_Finance_Exit/OECD mobilization data/Source_data/public_oecd_disbursed_nonRE_OOF.xlsx")

priv_mob_nonRE <- read_excel("C:/Users/stephenb/Documents/Coal_Finance_Exit/OECD mobilization data/Source_data/private_mob_oecd_nonRE.xlsx")

pubFin_nonRE_tot <- pubFin_nonRE_ODA %>%
  full_join(pubFin_nonRE_OOF,by="Recipient") %>%
  # filter(!grepl("Total",Recipient,ignore.case = T) & 
  #          !grepl("regional",Recipient,ignore.case = T) &
  #          !grepl("unspecified",Recipient,ignore.case = T)) %>%
  mutate(across(starts_with('2'),~replace_non_finite(suppressWarnings(as.numeric(.x)),replace = 0))) %>%
  group_by(Recipient) %>%
  summarise(pubFin = rowSums(across(where(is.numeric))),.groups = "keep")
  
privMob_nonRE_tot <- priv_mob_nonRE %>%
  # filter(!grepl("Total",Recipient,ignore.case = T) & 
  #          !grepl("regional",Recipient,ignore.case = T) &
  #          !grepl("unspecified",Recipient,ignore.case = T)) %>%
  mutate(across(starts_with('2'),~replace_non_finite(suppressWarnings(as.numeric(.x)),replace = 0))) %>%
  group_by(Recipient) %>%
  summarise(privMob = rowSums(across(where(is.numeric))),.groups = "keep")


mobilization_rate_nonRE <- pubFin_nonRE_tot %>%
  full_join(privMob_nonRE_tot,by="Recipient") %>%
  group_by(Recipient) %>%
  mutate(across(everything(),~replace_non_finite(.x,replace = 0))) %>%
  mutate(mobRate_nonRE = privMob/pubFin)

