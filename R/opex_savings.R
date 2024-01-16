opex_savings <- 0.0289412619407873+0.00274807181400299+0.00727776528687514

fuel_savings <- 0.199744018518622+0.0162094153012969+0.0421584420387468

fuel_savings2040 <- 0.195323864848239 + 0.015827503319655 + 0.0411828244469414

redir_opex <- 0.0114771530829968 + 0.00570072264701902 + 0.00461425979622794

redir_opex2040 <- 0.011590359590367 + 0.00577473723725373 + 0.00195151066482425

opex_savings + fuel_savings - redir_opex

opex_savings + fuel_savings2040 - redir_opex2040

read.gdx("P:/REMIND_3p0_dev/remind/output/REdirect_HI-cond-gcpt23_HiLoFix-nonoecd50p_2023-04-15_01.08.05/fulldata.gdx",
         requestList.name = c("v47_REdir_opex", "v47_ref_coal_opex", "v47_ref_coal_fuelcost"),
         fields = "l")
