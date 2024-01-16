china_mob <- read.csv("~/Coal_Finance_Exit/Mobilization data/China/chinese_privInv_mobilization_byCountry.csv")

china_mob_fnx <- as.numeric(china_mob %>% mutate(Country = toolCountry2isocode(Country)) %>% filter(Country %in% hosts & Chinese_investment > 0) %>% 
  summarise(weighted.mean(private_mobilization_factor, w = Chinese_investment, na.rm = T))
)
