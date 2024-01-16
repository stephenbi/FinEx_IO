require(stringr)
require(readxl)
require(dplyr)
require(mrremind)
require(quitte)
require(writexl)


map <- toolGetMapping("regionmappingH12.csv",type="regional")
g20_full <- c("Australia","Canada","Saudi Arabia","United States","India","Russia","South Africa","Turkey","Argentina","Brazil","China","Indonesia","Japan","South Korea","Mexico",map$X[which(map$RegionCode=="EUR")])
g20 <- toolCountry2isocode(g20_full)

### $M/MW per technology ###
inco <- readSource(type = "IEA_WEO", subtype = "Invest_Costs")
coal_inco0 <- inco[,2020,grepl("Coal",getItems(inco, dim=3))]

diff_inco <- calcOutput(type = "DiffInvestCosts", subtype = "Invest_Costs")
diff_inco_coal <- diff_inco[,2020,"pc"]

for (r in map$RegionCode) {
  for(tech in getItems(coal_inco0, dim = 3.2)) {
    coal_inco0[map$CountryCode[which(map$RegionCode==r)],2020,tech] <- 
      coal_inco0[map$CountryCode[which(map$RegionCode==r)],2020,tech][which(coal_inco0[map$CountryCode[which(map$RegionCode==r)],2020,tech]!=0)][1]
  }
}

finCoal <- read.csv(file = "~/Coal_Finance_Exit/df_order_unit_transactions_merged_GCPT_2021_01_GCFT_2020_12_urg_2021_02_platts_2019_20210729.csv",sep=";",dec=",")

finCoal <- finCoal %>%
  mutate(Status=ifelse(Status=="construction" & (!is.na(Year) && Year <= 2022),"operating",Status)) %>%
  mutate(Country.name=Country) %>%
  mutate(Financing.Country.name=Financing.Country) %>%
  mutate(Country=toolCountry2isocode(Country)) %>%
  mutate(Financing.Country=toolCountry2isocode(Financing.Country)) 

bbUSD_GW <- as.data.frame(coal_inco0*1e-3) %>% 
  filter(Data1!="CCS") %>%
  select(!c(Data1,Cell,Year)) %>%
  mutate(Data2=ifelse(grepl("-",Data2),str_to_title(gsub("Steam Coal - ","",Data2)),as.character(Data2))) %>%
  mutate(Data2=ifelse(grepl("Ultra",Data2),"Ultra-super",Data2)) %>%
  rename(Country=Region) 

# Fill in unknown values with average cost of plants expected in the region
bbUSD_GW <- bbUSD_GW %>%
  full_join(data.frame(
    Country=unique(bbUSD_GW$Country),
    Data2="Unknown",
    Value=as.numeric(toolAggregate(diff_inco_coal,map,NULL)*1e-3)
  )) %>%
  full_join(data.frame(
    Country=unique(bbUSD_GW$Country),
    Data2="unknown",
    Value=as.numeric(toolAggregate(diff_inco_coal,map,NULL)*1e-3)
  )) %>%
  full_join(data.frame(
    Country=unique(bbUSD_GW$Country),
    Data2="CFB",
    Value=bbUSD_GW$Value[which(bbUSD_GW$Data2=="Ultra-super")])
  ) %>%
  rename(Combustion.technology=Data2) %>%
  rename(MWcost=Value)


### FILL IN MISSING FINANCE VOLUMES ###
finCoal <- finCoal %>%
  mutate(across(everything(),~ifelse(is.na(.x),0,.x))) %>%
  left_join(bbUSD_GW,by=c("Country","Combustion.technology")) %>%
  mutate(transaction_estim=ifelse(weighted_transaction_amount_mio==0,
                                  capacity_weighted_by_funding_share_MW*MWcost,
                                  weighted_transaction_amount_mio))





###########################################################################################################



teMWcost <- finCoal %>%
  filter(weighted_transaction_amount_mio >0 & capacity_weighted_by_funding_share_MW>0) %>%
  group_by(Combustion.technology) %>%
  summarise(mean(total_funding_amount_estimated_mio/Capacity..MW.))

teMWcost_XB_host <- finCoal %>%
  filter(Financing.Country=="CHN" & Country!=Financing.Country & weighted_transaction_amount_mio >0 & capacity_weighted_by_funding_share_MW>0) %>%
  group_by(Combustion.technology,Country) %>%
  summarise(mean(total_funding_amount_estimated_mio/Capacity..MW.),.group="keep")


dfTech <- finCoal %>%
  filter(weighted_transaction_amount_mio>0 & capacity_weighted_by_funding_share_MW>0) %>%
  group_by(Combustion.technology) %>%
  do(fitTech=augment(lm(total_funding_amount_estimated_mio ~ log(Capacity..MW.),data=.))) %>%
  unnest(fitTech)

