require(ggplot2)
require(ggnewscale)
require(ggrepel)
require(stringr)
require(dplyr)
require(scales)
require(countrycode)
require(data.table)
require(stats)
require(mrremind)
require(mip)

#Relevant regional mappings and country classifications
#Current members of PPCA
PPCAmap <- toolGetMapping("regionmappingPPCA_cop26.csv",type = "regional")
# PPCAmap <- toolGetMapping("regionmappingPPCA.csv",type = "regional")
ppca_map <- as.magpie(PPCAmap[,-1])
#Current OECD members
OECDmap <- toolGetMapping("regionmappingOECD.csv",type = "regional")
oecd_map <- as.magpie(OECDmap[,-1])
#REMIND 12 region mapping
map <- toolGetMapping("regionmappingH12.csv",type="regional")
map <- setorder(map,cols=CountryCode)

new_mem_july21 <- c("DEU","GRC",'HRV','HUN','MKD','MNE','PRT','ALB','PER','SVK','URY')

newest_mem_cop26 <- c('UKR','CHL','SGP','MUS','AZE','SVN','EST')

# PPCAmap <- PPCAmap %>% 
#   mutate(RegionCode = ifelse(CountryCode %in% c(new_mem_july21, newest_mem_cop26),
#                              "PPCA", RegionCode))
# write.csv(PPCAmap, file = paste0(getConfig("mappingfolder"),"/regional/regionmappingPPCA_cop26.csv"))

TWyr_2_EJ <- 31.536

##Probability levels and line types
p <- c(0.05, 0.5, 0.95)
lts <- c("0.5" = 2, "0.05" = 3, "0.95" = 4)
# p <- c(0.95, 0.75, 0.5, 0.25, 0.05)
# lts <- c("0.25" = 0, "0.5" = 2, "0.75" = 0, "0.05" = 3, "0.95" = 3) 

##Countries with subnational PPCA members
# subnat <- c("ES", "GB", "US", "KR", "AU", "NL", "CA")

##########
## 2019 ##
##########
##Read in data
# setConfig(mainfolder = "Z:/inputdata/")
# setConfig(mainfolder = "C:/Users/stephenb/Downloads/madrat_main/")
setConfig(forcecache=T)
# setConfig(cachefolder = "P:/cache/SB_GCPT/")
# olddata <- readSource("IEA",subtype="EnergyBalances") * 0.0000418680000
# coalvars <- getItems(olddata,dim=3.1)[1:18]

setConfig(cachefolder = "P:/cache/IEA_2021/IEA/")
data <- readSource("IEA",subtype="EnergyBalances") * 0.0000418680000
coalvars <- c("BITCOAL", "ANTCOAL", "SUBCOAL", "COKCOAL", "LIGNITE")
# 
# old_raw_data <- data.frame(Country=map$CountryCode,
#                        Country.Name=map$X,
#                  Region=map$RegionCode,
#                  PPCA=PPCAmap$RegionCode,
#                  OECD=OECDmap$RegionCode,
#                  Pop=as.numeric(calcOutput("Population",aggregate=F,years=2019)[,,"pop_SSP2"]),
#                  Coal.Share=as.numeric(toolNAreplace(dimSums(olddata[,2019,paste0(coalvars,".ELOUTPUT")],dim=3)/
#                                          olddata[,2019,"TOTAL.ELOUTPUT"],replaceby = 0)[[1]]),
#                  GDP.PC=as.numeric(calcOutput("GDPpc",aggregate=F,years=2019)[,,"SSP2"])*1e-3,
#                  GW=as.numeric(calcOutput("Capacity",subtype="capacityByPE",aggregate=F,years=2019)[,,"pecoal"])*1e3)

pop <- calcOutput("PopulationPast",aggregate=F)
gdpPC <- calcOutput("GDPpcPast",aggregate=F)
if (!exists("cap_hist"))  cap_hist <- readSource("GCPT",subtype="historical")
coalgen <- dimSums(data[,, paste0(coalvars,".ELOUTPUT")], dim=3)
totgen <- data[,, "TOTAL.ELOUTPUT"]
  
raw_data <- data.frame(Country=map$CountryCode,
                       Country.Name=map$X,
                       Region=map$RegionCode,
                       PPCA=PPCAmap$RegionCode,
                       OECD=OECDmap$RegionCode,
                       Pop16 = as.numeric(pop[,2016,]),
                       Pop19 = as.numeric(pop[,2019,]),
                       gdpPC16 = as.numeric(gdpPC[,2016,]),
                       gdpPC19 = as.numeric(gdpPC[,2019,]),
                       GW16 = as.numeric(cap_hist[,2016,]),
                       GW19 = as.numeric(cap_hist[,2019,]),
                       coalgen16 = as.numeric(coalgen[,2016,]),
                       coalgen19 = as.numeric(coalgen[,2019,]),
                       totgen16 = as.numeric(totgen[,2016,]),
                       totgen19 = as.numeric(totgen[,2019,])
                       )
                       
raw_data <- raw_data %>%
  mutate(Year = ifelse(PPCA=="Free",
                       2019,
                       ifelse(Country %in% c(new_mem_july21, newest_mem_cop26),
                              2019,
                              2016)))
# 
# raw_data <- raw_data %>%
# 
# raw_data <- raw_data %>%
#   mutate(Coal.Share = toolNAreplace(Coal.Gen / Tot.Gen, replaceby = 0)$x) 
# %>%
#   mutate(across(everything(), ~ifelse(is.magpie(.x),
#                                      as.numeric(.x),
#                                      .x)))

# write.csv(raw_data,file=paste0(getConfig("sourcefolder"),"/PPCA/All_PPCA_status_SSP2_COP26.csv"))
# setConfig(forcecache = F)
# raw_data <- read.csv("C:/Users/stephenb/Downloads/madrat_main/sources/PPCA/PPCA_status_2020.csv", 
#                stringsAsFactors = F,sep = ";")
# raw_data$Country[44] <- "NA"
# 
# raw_data <- raw_data %>% mutate(Country=countrycode(Country,origin="iso2c",destination="iso3c")) %>% 
#   arrange(Country) %>%
#   mutate(raw_data,OECD=OECDmap$RegionCode[which(OECDmap$CountryCode %in% Country)])


#Country - 2-letter ISO country code
#PPCA - PPCA membershp
#PPCA.Bin (binary) - PPCA membership (1/0)
#PPCA.Date (binary) - PPCA membership with a pledged date (Mexico doesn't have one)
#GDP.PC - GDP per capita (1000 USD)
#Prod.PC - Coal production per capita (toe)
#Coal.Share - Share of coal in electricity supply
#Coal.Indep - Independence of coal supply (0 - all coal is imported, 1 - all coal is produced domestically, and maybe exported)
#FOG - Functioning of Government index (0-12)
#Age - Average age of coal power plant fleet
#GW - total installed capacity of coal-fired fleet
#Coal22 (binary) - 1 marks the world's 22 largest coal producers (> 90% of global coal production)

##Bold font for Coal16 countries (Coal22 countries that are not PPCA members)
##Asterisk for countries with subnational PPCA members 
num_data_16_19 <- raw_data %>% 
  mutate(Pop = ifelse(Year==2016, Pop16, Pop19),
         GDP.PC = 1e-3 * ifelse(Year==2016, gdpPC16, gdpPC19),
         GW = ifelse(Year==2016, GW16, GW19),
         Coal.Gen = ifelse(Year==2016, coalgen16, coalgen19),
         Tot.Gen = ifelse(Year==2016, totgen16, totgen19))%>%
  mutate(Coal.Share = toolNAreplace(Coal.Gen / Tot.Gen, replaceby = 0)$x) %>%
  mutate(PPCA.Bin=ifelse(PPCA=="PPCA",1,0)) %>%
  filter(is.finite(Coal.Share)) %>% 
  filter((Coal.Share>0.01 & Pop>=2) | PPCA=="PPCA") %>%
  mutate(GW=ifelse(GW==0,0.1,GW)) %>%
  select(!contains("1"))
# %>%
#   mutate(OECD = ifelse(Region=="EUR","OECD",OECD))

# write.csv(num_data_16_19,file=paste0(getConfig("sourcefolder"),"/PPCA/PPCA_status_SSP2_2019.csv"))
write.csv(num_data_16_19,file=paste0(getConfig("sourcefolder"),"/PPCA/PPCA_status_COP26_combo_16_19.csv"))

#Image for GDP.PC vs. share of coal in electricity supply
##Logit regression of PPCA membership on variables on the axes 
logit_16_19 <- glm(formula = PPCA.Bin ~  Coal.Share + GDP.PC, family = "binomial", data = num_data_16_19, y = 1)
ic <-  (log(1/p -1) -  summary(logit_16_19)$coef[1])/ summary(logit_16_19)$coef[3]
slope <- -(summary(logit_16_19)$coef[2]/summary(logit_16_19)$coef[3])
# All countries lying above this line are members of the given coalition scenario
ln <- data.frame(Ic = ic, Slope = slope, Prob = as.character(p))


################################
#### PLOT 2019 & 2016 COMBO ####
################################

plot_data_16_19 <- num_data_16_19 %>%
  mutate(Face = ifelse(PPCA=="PPCA", "bold.italic", "plain")) %>%
  mutate(Country=ifelse(Country %in% new_mem_july21,
                        paste0(Country,"*"),
                        ifelse(Country %in% newest_mem_cop26,
                               paste0(Country,"^"),
                               Country)))
###Colors
clrs <- c("OECD"="#E41A1C", "Non-OECD"="#789FC6","PPCA" = "goldenrod3", "Free" = "#000000")

plot_data_16_19 <- filter(plot_data_16_19,Coal.Share>=1e-2) %>%
  mutate(nudge = ifelse(Country %in% c("BIH","BWA","MKD*"),-0.03,
                        ifelse(Country %in% c("SRB","CHN"), 0.065,
                               ifelse(Country %in% c("DEU*","KOR","RUS","UKR^","KAZ"), 0.035,
                                      0.025+GW/1.5e4))))
# mutate(nudge = ifelse(GW>=170 | Country=="SRB" | Country=="KAZ",0.065,0.01))

### No Shading ###

## Building the image
ggplot(plot_data_16_19) +
  #Shading
  # geom_abline(intercept = seq(ln$Ic[1]+0.1,90,0.1),
  #             slope = ln$Slope[1],
  #             color="green",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[2]+0.1,(ln$Ic[1]-0.1),0.1),
  #             slope = ln$Slope[2],
  #             color="blue",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[3]+0.1,(ln$Ic[2]-0.1),0.1),
  #             slope = ln$Slope[3],
  #             color="#CC3333",alpha=0.06) +
  
  geom_point(aes(x = Coal.Share, y = GDP.PC, color = OECD, size = GW),shape=16) + 
  scale_size_continuous(range = 2.9*c(0.15, 13), limits = c(0,1050),breaks = c(10, 50, 250, 1000),name="2019 Capacity (GW)") +
  scale_color_manual(values = clrs, name="OECD Status (2021)",breaks=c("OECD","Non-OECD"),labels=c("OECD","Non-OECD"),guide=guide_legend(override.aes = list(size=5))) +
  new_scale_color() +
  #Black contours for Coal 16
  # geom_point(data = plot_data_16_19,aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, stroke = 0.5,color = "black") +
  ##Special treatment of countries 'overshadowed' by bigger ones
  # geom_point(data = filter(plot_data_16_19, Country %in% c("KZ")), 
  #            aes(x = Coal.Share, y = GDP.PC, size = GW, color = PPCA,)) +
  geom_point(data = filter(plot_data_16_19, Country %in% c("KAZ","MAR","BWA")),
             aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, color = "black", stroke = 0.2) +
  
  #Labels
  geom_text_repel(data = plot_data_16_19,
                  aes(x = Coal.Share, y = GDP.PC, label = Country, fontface = Face, color = PPCA), 
                  size=5.7, segment.size = 0.3,force=5,nudge_x = plot_data_16_19$nudge,segment.alpha = 0.35, max.overlaps=80) +
  # geom_text(guide_legend(override.aes = list(label = "O"))) +
  scale_color_manual(values = clrs, name="PPCA Status (July 2021)",breaks=c("PPCA","Free"),labels=c("PPCA","Freerider"),guide=guide_legend(override.aes = list(size=7,label="A",face="bold"))) +
  guides(size=guide_legend(order=1),color=guide_legend(order=2,override.aes = list(size=1))) +
  new_scale_color() +
  #Probability lines
  geom_abline(data = ln, aes(intercept = Ic, slope = Slope, color = Prob),size = 0.4,alpha=0.4) +
  scale_color_manual(values=c("gold3","#CC3333","blue"), 
                     name = "Coalition Threshold", 
                     labels=c("\u2265 95% likely","\u2265 50% likely","\u2265 5% likely"),
                     guide=guide_legend(override.aes = list(size=1.5))) +
  ##Axis labels (and limits, if necessary)
  scale_x_continuous("% of coal in electricity supply", labels = percent, limits = c(0, 1)) +
  scale_y_continuous("GDP p.c. ($1000)",limits=c(0,70)) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=3.5*8.5),
        plot.title = element_text(face="bold",size=3.5*10,hjust=0.5),
        legend.text = element_text(size=3.5*6),
        legend.title = element_text(size=3.5*7)) +
  labs(title = "PPCA Dynamic Feasibility Space (2019)")

ggsave("~/Coal_Finance_Exit/Plots/Bubble Chart/DFS_2019_2016_combo.png",device="png",width=6.3*3,height=4.63*3,units="in",dpi="retina")



###################
#### PLOT 2019 ####
###################

num_data_19 <- raw_data %>% 
  mutate(Pop = ifelse(Year==2016, Pop16, Pop19),
         GDP.PC = 1e-3 * ifelse(Year==2016, gdpPC16, gdpPC19),
         GW = ifelse(Year==2016, GW16, GW19),
         Coal.Gen = ifelse(Year==2016, coalgen16, coalgen19),
         Tot.Gen = ifelse(Year==2016, totgen16, totgen19))%>%
  mutate(Coal.Share = toolNAreplace(Coal.Gen / Tot.Gen, replaceby = 0)$x) %>%
  mutate(PPCA.Bin=ifelse(PPCA=="PPCA",1,0)) %>%
  filter(is.finite(Coal.Share)) %>% 
  filter((Coal.Share>0.01 & Pop>=2) | PPCA=="PPCA") %>%
  mutate(GW=ifelse(GW==0,0.1,GW)) %>%
  select(!contains("1"))
# %>%
#   mutate(OECD = ifelse(Region=="EUR","OECD",OECD))

# write.csv(num_data_19,file=paste0(getConfig("sourcefolder"),"/PPCA/PPCA_status_SSP2_2019.csv"))
write.csv(num_data_19,file=paste0(getConfig("sourcefolder"),"/PPCA/PPCA_status_SSP2_COP26.csv"))

#Image for GDP.PC vs. share of coal in electricity supply
##Logit regression of PPCA membership on variables on the axes 
logit_19 <- glm(formula = PPCA.Bin ~  Coal.Share + GDP.PC, family = "binomial", data = num_data_19, y = 1)
ic <-  (log(1/p -1) -  summary(logit_19)$coef[1])/ summary(logit_19)$coef[3]
slope <- -(summary(logit_19)$coef[2]/summary(logit_19)$coef[3])
# All countries lying above this line are members of the given coalition scenario
ln <- data.frame(Ic = ic, Slope = slope, Prob = as.character(p))

ic <-  (log(1/p -1) -  summary(logit_16_19)$coef[1])/ summary(logit_16_19)$coef[3]
slope <- -(summary(logit_16_19)$coef[2]/summary(logit_16_19)$coef[3])
# All countries lying above this line are members of the given coalition scenario
ln <- data.frame(Ic = ic, Slope = slope, Prob = as.character(p))

plot_data_19 <- num_data_19 %>%
  mutate(Face = ifelse(PPCA=="PPCA", "bold.italic", "plain")) %>%
  mutate(Country=ifelse(Country %in% new_mem_july21,
                        paste0(Country,"*"),
                        ifelse(Country %in% newest_mem_cop26,
                               paste0(Country,"^"),
                               Country)))
###Colors
clrs <- c("OECD"="#E41A1C", "Non-OECD"="#789FC6","PPCA" = "goldenrod3", "Free" = "#000000")

plot_data_19 <- filter(plot_data_19,Coal.Share>=1e-2) %>%
  mutate(nudge = ifelse(Country %in% c("BIH","BWA","MKD*"),-0.03,
                        ifelse(Country %in% c("SRB","CHN"), 0.065,
                               ifelse(Country %in% c("DEU*","KOR","RUS","UKR^","KAZ"), 0.035,
                                      0.025+GW/1.5e4))))
# mutate(nudge = ifelse(GW>=170 | Country=="SRB" | Country=="KAZ",0.065,0.01))

### No Shading ###

## Building the image
ggplot(plot_data_19) +
  #Shading
  # geom_abline(intercept = seq(ln$Ic[1]+0.1,90,0.1),
  #             slope = ln$Slope[1],
  #             color="green",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[2]+0.1,(ln$Ic[1]-0.1),0.1),
  #             slope = ln$Slope[2],
  #             color="blue",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[3]+0.1,(ln$Ic[2]-0.1),0.1),
  #             slope = ln$Slope[3],
  #             color="#CC3333",alpha=0.06) +
  
geom_point(aes(x = Coal.Share, y = GDP.PC, color = OECD, size = GW),shape=16) + 
  scale_size_continuous(range = 2.9*c(0.15, 13), limits = c(0,1050),breaks = c(10, 50, 250, 1000),name="2019 Capacity (GW)") +
  scale_color_manual(values = clrs, name="OECD Status (2021)",breaks=c("OECD","Non-OECD"),labels=c("OECD","Non-OECD"),guide=guide_legend(override.aes = list(size=5))) +
  new_scale_color() +
  #Black contours for Coal 16
  # geom_point(data = plot_data_19,aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, stroke = 0.5,color = "black") +
  ##Special treatment of countries 'overshadowed' by bigger ones
  # geom_point(data = filter(plot_data_19, Country %in% c("KZ")), 
  #            aes(x = Coal.Share, y = GDP.PC, size = GW, color = PPCA,)) +
  geom_point(data = filter(plot_data_19, Country %in% c("KAZ","MAR","BWA")),
             aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, color = "black", stroke = 0.2) +
  
  #Labels
  geom_text_repel(data = plot_data_19,
                  aes(x = Coal.Share, y = GDP.PC, label = Country, fontface = Face, color = PPCA), 
                  size=5.7, segment.size = 0.3,force=5,nudge_x = plot_data_19$nudge,segment.alpha = 0.35, max.overlaps=80) +
  # geom_text(guide_legend(override.aes = list(label = "O"))) +
  scale_color_manual(values = clrs, name="PPCA Status (July 2021)",breaks=c("PPCA","Free"),labels=c("PPCA","Freerider"),guide=guide_legend(override.aes = list(size=7,label="A",face="bold"))) +
  guides(size=guide_legend(order=1),color=guide_legend(order=2,override.aes = list(size=1))) +
  new_scale_color() +
  #Probability lines
  geom_abline(data = ln, aes(intercept = Ic, slope = Slope, color = Prob),size = 0.4,alpha=0.4) +
  scale_color_manual(values=c("gold3","#CC3333","blue"), 
                     name = "Coalition Threshold", 
                     labels=c("\u2265 95% likely","\u2265 50% likely","\u2265 5% likely"),
                     guide=guide_legend(override.aes = list(size=1.5))) +
  ##Axis labels (and limits, if necessary)
  scale_x_continuous("% of coal in electricity supply", labels = percent, limits = c(0, 1)) +
  scale_y_continuous("GDP p.c. ($1000)",limits=c(0,70)) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=3.5*8.5),
        plot.title = element_text(face="bold",size=3.5*10,hjust=0.5),
        legend.text = element_text(size=3.5*6),
        legend.title = element_text(size=3.5*7)) +
  labs(title = "PPCA Dynamic Feasibility Space (2019)")

ggsave("~/Coal_Finance_Exit/Plots/Bubble Chart/DFS_2019_combo.pdf",device="pdf",width=6.3*3,height=4.63*3,units="in",dpi="retina")




############################################################################################################################################

##########
## 2020 ##
##########

popC_2020 <- calcOutput("PopulationFuture",aggregate=F,years = c(2019,2020))[,,"pop_SSP2"]
popR_2020 <- toolAggregate(popC_2020,map,NULL)

popGrowth <- popC_2020[,"y2020",] - popC_2020[,2019,]
getYears(popGrowth) <- gsub(".y2019","",getYears(popGrowth))

weight <- new.magpie(getRegions(popC_2020),getYears(popGrowth),fill=0)
# Aggregation weights for downscaling regional model output (and re-aggregating)
for (yr in getYears(weight)) {
  weight[,yr,] <- data[,2019,"TOTAL.ELOUTPUT"] +
    ifelse(data[,2019,"TOTAL.ELOUTPUT"] + (data[,2019,"TOTAL.ELOUTPUT"]/popC_2020[,2019,]) * popGrowth[,yr,] >= 0,
           (data[,2019,"TOTAL.ELOUTPUT"]/popC_2020[,2019,]) * popGrowth[,yr,], 0)
}
getYears(weight) <- getYears(popGrowth)

##Read in data2020
data2020 <- read.report("C:/Users/stephenb/Downloads/madrat_main/sources/PPCA/REMIND_generic_SSP2-NPi-covid-cfCHA52.mif",as.list=F)
raw_data2020 <- data.frame(Country=map$CountryCode,
                           Region=map$RegionCode,
                           PPCA=PPCAmap$RegionCode,
                           OECD=OECDmap$RegionCode,
                           Pop=as.numeric(calcOutput("Population",aggregate=F,years=2020)[,,"pop_SSP2"]),
                           Coal.Share=toolNAreplace(as.numeric(
                             calcOutput("Capacity",subtype="capacityByPE",aggregate=F,years=2020)[,,"pecoal"] *
                               calcOutput("CapacityFactor",aggregate=F,years=2020)[,,"pc"] * TWyr_2_EJ /
                               toolAggregate(
                                 dimSums(data2020[-which(getItems(data2020,1)=="GLO"),2020,"SE|Electricity (EJ/yr)"],dim=3),
                                 map,weight)),replaceby = 0)[[1]],
                           GDP.PC=as.numeric(calcOutput("GDPpc",aggregate=F,years=2020)[,,"SSP2"])*1e-3,
                           GW=as.numeric(calcOutput("Capacity",subtype="capacityByPE",aggregate=F,years=2020)[,,"pecoal"])*1e3)

raw_data2020 <- raw_data2020 %>%
  mutate(
    Probability=predict(logit2,newdata = data.frame(Coal.Share=raw_data2020$Coal.Share,GDP.PC=raw_data2020$GDP.PC),type = "response"))

write.csv(raw_data2020,file=paste0(getConfig("sourcefolder"),"/PPCA/All_PPCA_status_SSP2_2020.csv"))

num_data2020 <- raw_data2020 %>% 
  mutate(PPCA.Bin=ifelse(PPCA=="PPCA",1,0)) %>%
  filter(is.finite(Coal.Share)) %>% 
  filter((Coal.Share>0.01 & Pop>=2) | PPCA=="PPCA") %>%
  mutate(GW=ifelse(GW==0,0.1,GW)) %>%
  mutate(Face = "bold.italic") 
# %>%
#   mutate(OECD = ifelse(Region=="EUR","OECD",OECD))


###################
#### PLOT 2020 ####
###################

plot_data2020 <- num_data2020 %>%
  mutate(Face = ifelse(PPCA=="PPCA", "bold.italic", "plain")) %>%
  mutate(Country=ifelse(Country %in% new_mem_july21,
                        paste0(Country,"*"),
                        ifelse(Country %in% newest_mem_cop26,
                               paste0(Country,"^"),
                               Country)))
###Colors
clrs <- c("OECD"="#E41A1C", "Non-OECD"="#789FC6","PPCA" = "goldenrod", "Free" = "#000000")

plot_data2020 <- filter(plot_data2020,Coal.Share>=1e-2) %>%
  mutate(nudge = ifelse(Country %in% c("SRB","BIH","BWA","KAZ","MKD*"),-0.03,
                                       ifelse(Country %in% c("CHN"), 0.065,
                                              ifelse(Country %in% c("DEU*","KOR","UKR^"), 0.04,
                                                     0.025+GW/1.5e4))))
# mutate(nudge = ifelse(GW>=170 | Country=="SRB" | Country=="KAZ",0.065,0.01))

### No Shading ###

## Building the image
ggplot(plot_data2020) +
  #Shading
  # geom_abline(intercept = seq(ln$Ic[1]+0.1,90,0.1),
  #             slope = ln$Slope[1],
  #             color="green",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[2]+0.1,(ln$Ic[1]-0.1),0.1),
  #             slope = ln$Slope[2],
  #             color="blue",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[3]+0.1,(ln$Ic[2]-0.1),0.1),
  #             slope = ln$Slope[3],
  #             color="#CC3333",alpha=0.06) +
  
geom_point(aes(x = Coal.Share, y = GDP.PC, color = OECD, size = GW),shape=16) + 
  scale_size_continuous(range = 2.9*c(0.15, 13), limits = c(0,1010),breaks = c(10, 50, 250, 900),name="2020 Capacity (GW)") +
  scale_color_manual(values = clrs, name="OECD Status (2021)",breaks=c("OECD","Non-OECD"),labels=c("OECD","Non-OECD"),guide=guide_legend(override.aes = list(size=5))) +
  new_scale_color() +
  #Black contours for Coal 16
  # geom_point(data = plot_data2020,aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, stroke = 0.5,color = "black") +
  ##Special treatment of countries 'overshadowed' by bigger ones
  # geom_point(data = filter(plot_data2020, Country %in% c("KZ")), 
  #            aes(x = Coal.Share, y = GDP.PC, size = GW, color = PPCA,)) +
  geom_point(data = filter(plot_data2020, Country %in% c("KAZ","MAR","BWA","SRB")),
             aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, color = "black", stroke = 0.2) +
  
  #Labels
  geom_text_repel(data = plot_data2020,
                  aes(x = Coal.Share, y = GDP.PC, label = Country, fontface = Face, color = PPCA), 
                  size=5.7, segment.size = 0.3,force=5,nudge_x = plot_data2020$nudge,segment.alpha = 0.35, max.overlaps=80) +
  # geom_text(guide_legend(override.aes = list(label = "O"))) +
  scale_color_manual(values = clrs, name="PPCA Status (July 2021)",breaks=c("PPCA","Free"),labels=c("PPCA","Freerider"),guide=guide_legend(override.aes = list(size=7,label="A",face="bold"))) +
  guides(size=guide_legend(order=1),color=guide_legend(order=2,override.aes = list(size=1))) +
  new_scale_color() +
  #Probability lines
  geom_abline(data = ln, aes(intercept = Ic, slope = Slope, color = Prob),size = 0.4,alpha=0.4) +
  scale_color_manual(values=c("gold3","#CC3333","blue"), 
                     name = "Coalition Threshold", 
                     labels=c("\u2265 95% likely","\u2265 50% likely","\u2265 5% likely"),
                     guide=guide_legend(override.aes = list(size=1.5))) +
  ##Axis labels (and limits, if necessary)
  scale_x_continuous("% of coal in electricity supply", labels = percent, limits = c(0, 1)) +
  scale_y_continuous("GDP p.c. ($1000)",limits=c(0,70)) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=3.5*8.5),
        plot.title = element_text(face="bold",size=3.5*10,hjust=0.5),
        legend.text = element_text(size=3.5*6),
        legend.title = element_text(size=3.5*7)) +
  labs(title = "PPCA Dynamic Feasibility Space (2020)")

ggsave("~/PPCA Paper/Bubble Chart/DFS_2020_noShade_colorblind_noEU_rev_rd2.pdf",device="pdf",width=6.3*3,height=4.63*3,units="in",dpi="retina")



####################
#### LOGIT 2020 ####
####################

##Logit regression of PPCA membership on variables on the axes 
logit2020 <- glm(PPCA.Bin ~  Coal.Share + GDP.PC , data = num_data2020, family = "binomial")
ic2020 <-  (log(1/p -1) -  summary(logit2020)$coef[1])/ summary(logit2020)$coef[3]
slope2020 <- -(summary(logit2020)$coef[2]/summary(logit2020)$coef[3])
# All countries lying above this line are members of the given coalition scenario
ln2020 <- data.frame(Ic = ic, Slope = slope, Prob = as.character(p))

########################
#### RE-FITTED 2020 ####
########################

## Building the image
ggplot(plot_data2020) +
  #Shading
  # geom_abline(intercept = seq(ln$Ic[1]+0.1,90,0.1),
  #             slope = ln$Slope[1],
  #             color="green",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[2]+0.1,(ln$Ic[1]-0.1),0.1),
  #             slope = ln$Slope[2],
  #             color="blue",alpha=0.06) +
  # geom_abline(intercept = seq(ln$Ic[3]+0.1,(ln$Ic[2]-0.1),0.1),
  #             slope = ln$Slope[3],
  #             color="#CC3333",alpha=0.06) +
  
geom_point(aes(x = Coal.Share, y = GDP.PC, color = OECD, size = GW),shape=16) + 
  scale_size_continuous(range = 2.9*c(0.15, 13), limits = c(0,1010),breaks = c(10, 50, 250, 900),name="2020 Capacity (GW)") +
  scale_color_manual(values = clrs, name="OECD Status (2021)",breaks=c("OECD","Non-OECD"),labels=c("OECD","Non-OECD"),guide=guide_legend(override.aes = list(size=5))) +
  new_scale_color() +
  #Black contours for Coal 16
  # geom_point(data = plot_data2020,aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, stroke = 0.5,color = "black") +
  ##Special treatment of countries 'overshadowed' by bigger ones
  # geom_point(data = filter(plot_data2020, Country %in% c("KZ")), 
  #            aes(x = Coal.Share, y = GDP.PC, size = GW, color = PPCA,)) +
  geom_point(data = filter(plot_data2020, Country %in% c("KAZ","MAR","BWA","SRB")),
             aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, color = "black", stroke = 0.2) +
  
  #Labels
  geom_text_repel(data = plot_data2020,
                  aes(x = Coal.Share, y = GDP.PC, label = Country, fontface = Face, color = PPCA), 
                  size=5.7, segment.size = 0.3,force=5,nudge_x = plot_data2020$nudge,segment.alpha = 0.35, max.overlaps=80) +
  # geom_text(guide_legend(override.aes = list(label = "O"))) +
  scale_color_manual(values = clrs, name="PPCA Status (July 2021)",breaks=c("PPCA","Free"),labels=c("PPCA","Freerider"),guide=guide_legend(override.aes = list(size=7,label="A",face="bold"))) +
  guides(size=guide_legend(order=1),color=guide_legend(order=2,override.aes = list(size=1))) +
  new_scale_color() +
  #Probability lines
  geom_abline(data = ln2020, aes(intercept = Ic, slope = Slope, color = Prob),size = 0.4,alpha=0.4) +
  scale_color_manual(values=c("gold3","#CC3333","blue"), 
                     name = "Coalition Threshold", 
                     labels=c("\u2265 95% likely","\u2265 50% likely","\u2265 5% likely"),
                     guide=guide_legend(override.aes = list(size=1.5))) +
  ##Axis labels (and limits, if necessary)
  scale_x_continuous("% of coal in electricity supply", labels = percent, limits = c(0, 1)) +
  scale_y_continuous("GDP p.c. ($1000)",limits=c(0,70)) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=3.5*8.5),
        plot.title = element_text(face="bold",size=3.5*10,hjust=0.5),
        legend.text = element_text(size=3.5*6),
        legend.title = element_text(size=3.5*7)) +
  labs(title = "PPCA Dynamic Feasibility Space (2020)")

ggsave("~/PPCA Paper/Bubble Chart/DFS_2020_REFIT_noShade_colorblind_noEU_rev_rd2.pdf",device="pdf",width=6.3*3,height=4.63*3,units="in",dpi="retina")

#### With Shading ###
# ## Building the image
# ggplot(plot_data) +
#   #Shading
#   geom_abline(intercept = seq(ln$Ic[1]+0.1,90,0.1),
#               slope = ln$Slope[1],
#               color="green",alpha=0.06) +
#   geom_abline(intercept = seq(ln$Ic[2]+0.1,(ln$Ic[1]-0.1),0.1),
#               slope = ln$Slope[2],
#               color="blue",alpha=0.06) +
#   geom_abline(intercept = seq(ln$Ic[3]+0.1,(ln$Ic[2]-0.1),0.1),
#               slope = ln$Slope[3],
#               color="#CC3333",alpha=0.06) +
#   
#   geom_point(aes(x = Coal.Share, y = GDP.PC, color = OECD, size = GW),shape=16) + 
#   scale_size_continuous(range = c(0.15, 13), limits = c(0,900),breaks = c(10, 50, 250, 900),name="Capacity (GW)") +
#   scale_color_manual(values = clrs, name="OECD Status",breaks=c("OECD","Non-OECD"),labels=c("OECD","Non-OECD")) +
#   new_scale_color() +
#   #Black contours for Coal 16
#   # geom_point(data = plot_data,aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, stroke = 0.5,color = "black") +
#   ##Special treatment of countries 'overshadowed' by bigger ones
#   # geom_point(data = filter(plot_data, Country %in% c("KZ")), 
#   #            aes(x = Coal.Share, y = GDP.PC, size = GW, color = PPCA,)) +
#   # geom_point(data = filter(plot_data, Country %in% c("KZ")), 
#   #            aes(x = Coal.Share, y = GDP.PC, size = GW), shape = 1, color = "black", stroke = 0.5) +
#   
#   #Labels
#   geom_text_repel(data = plot_data,
#                   aes(x = Coal.Share, y = GDP.PC, label = Country, fontface = Face, color = PPCA), 
#                   size = 2.5, segment.size = 0.3,force=5,nudge_x = plot_data$nudge,nudge_y = plot_data$nudge,segment.alpha = 0.35, max.overlaps=80) +
#   # geom_text(guide_legend(override.aes = list(label = "O"))) +
#   scale_color_manual(values = clrs, name="PPCA Status",breaks=c("PPCA","Free"),labels=c("PPCA","Freerider"),guide=guide_legend(override.aes = list(size=3,label="O",face="bold"))) +
#   guides(size=guide_legend(order=1),color=guide_legend(order=2,override.aes = list(size=2))) +
#   new_scale_color() +
#   #Probability lines
#   geom_abline(data = ln, aes(intercept = Ic, slope = Slope, color = Prob),size = 0.4,alpha=0.4) +
#   scale_color_manual(values=c("green3","blue","#CC3333"), name = "Coalition Scenario", labels=c("1p (95%)","2p (50%)","3p (5%)")) +
#   
#   ##Axis labels (and limits, if necessary)
#   scale_x_continuous("% of coal in electricity supply", labels = percent, limits = c(0, 1)) +
#   scale_y_continuous("GDP p.c. ($1000)",limits=c(0,70)) + 
#   theme_bw() +
#   theme(panel.grid.minor = element_blank(),
#         plot.title = element_text(face="bold",size=12,hjust=0.5)) +
#   labs(title = "PPCA Dynamic Feasibility Space (2019)")
# 
# ggsave("~/PPCA Paper/Bubble Chart/DFS_Current.png",device="png",width=65/2.5,height=45/2.5,units="cm",dpi="retina")


#########################
### MODEL DIAGNOSTICS ###
#########################
library(performance)
library(see)
binned_residuals(logit2)
performance_accuracy(logit2,method="boot",n=51)
performance_accuracy(logit2,method="cv",n=51)
performance_hosmer(logit2,n_bins = 3)
performance_logloss(logit2)
performance_pcp(logit2)
performance_pcp(logit2,method="Gelman-Hill")
performance_rse(logit2)
performance_score(logit2)
r2(logit2)
r2_coxsnell(logit2)
r2_nagelkerke(logit2)
r2_mcfadden(logit2)
library(rms)
residuals(logit2)
val.prob(logit2)
