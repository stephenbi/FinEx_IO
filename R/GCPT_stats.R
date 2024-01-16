coalcap_jul23 <- readSource("GCPT", subtype = "historical")

finex_cap_jan22 <- readSource("GCPT", subtype = "G20_FinEx_2022")

gcpt_valid_nat <- coalcap_jul23[,2023,] - finex_cap_jan22[,,"Neutral"]

gcpt_valid_reg <- toolAggregate(coalcap_jul23[,2023,], map, NULL) - toolAggregate(finex_cap_jan22[,,"Neutral"], map, NULL)

max(gcpt_valid_nat)
max(gcpt_valid_reg)

toolAggregate(finex_cap_jul23[,,"Neutral"] - finex_cap_jan22[,,"Neutral"], map, NULL)


lifespans2023 <- readSource("GCPT", subtype = "lifespans")

future2023 <- readSource("GCPT", subtype = "future")
reg_future2023 <- toolAggregate(future2023,map,NULL)

comp_rates2023 <- readSource("GCPT", subtype = "comp_rates")

reg_comp_rates2023 <- readSource("GCPT", subtype = "reg_comp_rates")

glo_comp_rates2023 <- readSource("GCPT", subtype = "glo_comp_rates")

meanAge2023 <- readSource("GCPT", subtype = "meanAge")

status2023 <- readSource("GCPT", subtype = "status")
reg_status2023 <- toolAggregate(status2023,map,NULL)

# emissions2023 <- readSource("GCPT", subtype = "emissions")

reg_lifespans2023 <- toolAggregate(lifespans, map, weight = coalcap_jul23[,2023,])

reg_meanAge2023 <- toolAggregate(meanAge2023, map, weight = coalcap_jul23[,2023,])

write.magpie(coalcap_jul23, "~/FinEx/output/gcpt_stats/nat_coalcap_hist.csv")
write.magpie(lifespans2023, "~/FinEx/output/gcpt_stats/nat_lifespans2023.csv")
write.magpie(reg_lifespans2023, "~/FinEx/output/gcpt_stats/reg_lifespans2023.csv")
write.magpie(comp_rates2023, "~/FinEx/output/gcpt_stats/nat_comp_rates2023.csv")
write.magpie(meanAge2023, "~/FinEx/output/gcpt_stats/nat_meanAge2023.csv")
write.magpie(reg_comp_rates2023, "~/FinEx/output/gcpt_stats/reg_comp_rates2023.csv")
write.magpie(reg_meanAge2023, "~/FinEx/output/gcpt_stats/reg_meanAge2023.csv")
write.magpie(glo_comp_rates2023, "~/FinEx/output/gcpt_stats/glo_comp_rates2023.csv")



finex_cap_jan22 <- readSource("GCPT", subtype = "G20_FinEx_2022")
reg_finex_cap_2022 <- toolAggregate(finex_cap_jan22,map,NULL)

future2022 <- readSource("GCPT", subtype = "future2022")
reg_future2022 <- toolAggregate(future2022,map,NULL)

lifespans2022 <- readSource("GCPT", subtype = "lifespans2022")

comp_rates2022 <- readSource("GCPT", subtype = "comp_rates2022")

reg_comp_rates2022 <- readSource("GCPT", subtype = "reg_comp_rates2022")

glo_comp_rates2022 <- readSource("GCPT", subtype = "glo_comp_rates2022")

meanAge2022 <- readSource("GCPT", subtype = "meanAge2022")

status2022 <- readSource("GCPT", subtype = "status2022")
reg_status2022 <- toolAggregate(status2022,map,NULL)

# emissions2022 <- readSource("GCPT", subtype = "emissions2022")

reg_lifespans2022 <- toolAggregate(lifespans, map, weight = coalcap_jul23[,2022,])

reg_meanAge2022 <- toolAggregate(meanAge, map, weight = coalcap_jul23[,2022,])

write.magpie(lifespans2022, "~/FinEx/output/gcpt_stats/nat_lifespans2022.csv")
write.magpie(reg_lifespans2022, "~/FinEx/output/gcpt_stats/reg_lifespans2022.csv")
write.magpie(comp_rates2022, "~/FinEx/output/gcpt_stats/nat_comp_rates2022.csv")
write.magpie(meanAge2022, "~/FinEx/output/gcpt_stats/nat_meanAge2022.csv")
write.magpie(reg_comp_rates2022, "~/FinEx/output/gcpt_stats/reg_comp_rates2022.csv")
write.magpie(reg_meanAge2022, "~/FinEx/output/gcpt_stats/reg_meanAge2022.csv")
write.magpie(glo_comp_rates2022, "~/FinEx/output/gcpt_stats/glo_comp_rates2022.csv")





finex_cap_2021 <- readSource("GCPT", subtype = "G20_FinEx_2021")
reg_finex_cap_2021 <- toolAggregate(finex_cap_2021,map,NULL)

future2021 <- readSource("GCPT", subtype = "future2021")
reg_future2021 <- toolAggregate(future2021,map,NULL)

lifespans2021 <- readSource("GCPT", subtype = "lifespans2021")

comp_rates2021 <- readSource("GCPT", subtype = "comp_rates2021")

reg_comp_rates2021 <- readSource("GCPT", subtype = "reg_comp_rates2021")

glo_comp_rates2021 <- readSource("GCPT", subtype = "glo_comp_rates2021")

meanAge2021 <- readSource("GCPT", subtype = "meanAge2021")

status2021 <- readSource("GCPT", subtype = "status2021")
reg_status2021 <- toolAggregate(status2021,map,NULL)

# emissions2021 <- readSource("GCPT", subtype = "emissions2021")

reg_lifespans2021 <- toolAggregate(lifespans, map, weight = coalcap_jul23[,2021,])

reg_meanAge2021 <- toolAggregate(meanAge, map, weight = coalcap_jul23[,2021,])

write.magpie(finex_cap_2021, "~/FinEx/output/gcpt_stats/nat_postFinEx_cap_2021.csv")
write.magpie(future2021, "~/FinEx/output/gcpt_stats/nat_preFinEx_exp_cap.csv")
write.magpie(lifespans2021, "~/FinEx/output/gcpt_stats/nat_lifespans2021.csv")
write.magpie(reg_lifespans2021, "~/FinEx/output/gcpt_stats/reg_lifespans2021.csv")
write.magpie(comp_rates2021, "~/FinEx/output/gcpt_stats/nat_comp_rates2021.csv")
write.magpie(meanAge2021, "~/FinEx/output/gcpt_stats/nat_meanAge2021.csv")
write.magpie(reg_comp_rates2021, "~/FinEx/output/gcpt_stats/reg_comp_rates2021.csv")
write.magpie(reg_meanAge2021, "~/FinEx/output/gcpt_stats/reg_meanAge2021.csv")
write.magpie(glo_comp_rates2021, "~/FinEx/output/gcpt_stats/glo_comp_rates2021.csv")
