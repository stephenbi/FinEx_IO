finCoal <- read.csv(file = "~/Coal_Finance_Exit/df_order_unit_transactions_merged_GCPT_2021_01_GCFT_2020_12_urg_2021_02_platts_2019_20210729.csv",sep=";",dec=",")

finCoal <- finCoal %>%
  # mutate(Status=ifelse(Status=="construction" & Year < 2022,"operating",Status)) %>%
  mutate(Country.name=Country) %>%
  mutate(Financing.Country.name=Financing.Country) %>%
  mutate(Country=toolCountry2isocode(Country)) %>%
  mutate(Financing.Country=toolCountry2isocode(Financing.Country))

g20_full <- c("Australia","Canada","Saudi Arabia","United States","India","Russia","South Africa","Turkey","Argentina","Brazil","China","Indonesia","Japan","South Korea","Mexico",map$X[which(map$RegionCode=="EUR")])
g20 <- toolCountry2isocode(g20_full)

g20_finEx <- finCoal %>%
  filter(ownership=="state-owned" & Financing.Country %in% g20 & Country!=Financing.Country & Status %in% c("announced","pre-permit","permitted","shelved") & !is.na(capacity_weighted_by_funding_share_MW)) %>%
  mutate(Status=stringr::str_to_sentence(Status)) %>%
  rename(variable=Status) %>%
  group_by(Country,variable,.add=T) %>%
  summarise(FinEx_GW=sum(capacity_weighted_by_funding_share_MW*1e-3),.groups="keep") %>%
  mutate(across(everything(),~ifelse(is.na(.x),0,.x)))

m_g20_finEx <- toolCountryFill(x=toolNAreplace(as.magpie(g20_finEx),replaceby=0)[[1]],fill = 0,verbosity = 1)
