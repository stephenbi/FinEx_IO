x <- readSource("Ember")
x <- x[, , "%", invert = TRUE]
x <- collapseDim(x, dim = 3.2)

gen_nat <- x[,,grepl("Electricity generation|Fuel",getItems(x,3),fixed=T)]
cap_nat <- x[,,grepl("Capacity|Fuel",getItems(x,3),fixed=T)]

map <- toolGetMapping("regionmappingH12.csv",type="regional")

gen_reg <- toolAggregate(setItems(gen_nat,3,gsub("Electricity generation|Fuel|","",getItems(gen_nat,3),fixed=T)), map, NULL)
cap_reg <- toolAggregate(setItems(cap_nat,3,gsub("Capacity|Fuel|","",getItems(cap_nat,3),fixed=T)), map, NULL) * 8.76

cf_reg <- gen_reg / cap_reg

write.magpie(cfreg$x,"~/ember_cf_reg.csv")



genreg <- calcOutput("Ember",subtype="generation")
capreg <- calcOutput("Ember",subtype="capacity")

getItems(genreg,dim=3) <- gsub("SE|Electricity|+|","", gsub(" (EJ/yr)","",getItems(genreg,dim = 3),fixed=T), fixed = T)

getItems(capreg,dim=3) <- gsub("Cap|Electricity|","", gsub(" (GW)","", getItems(capreg,dim = 3), fixed = T), fixed = T)

cfreg <- toolNAreplace((genreg[,,getItems(genreg,dim=3)!="SE|Electricity"]) / (capreg * 365 * 24 * 3.6e-6), replaceby = 0)

write.magpie(cfreg$x,"~/ember_cf_reg.csv")
