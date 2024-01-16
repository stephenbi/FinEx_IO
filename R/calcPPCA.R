#' Powering Past Coal Alliance (PPCA) policy-coalition scenarios
#' @description  Derive coalition size scenarios (proven, probable and proven) for PPCA accession
#' @param phase Specify the stage of analysis (current, OECD, or Non-OECD)
#' @param policy Specify the policy scenario (power,demand,export)
#' @param recovery BAU, Green or Brown COVID recovery 
#' @param run REMIND run on which to perform the logistic regression analysis
#' @param subtype Specify whether to return REMIND input data (inputdata), logistic regression inputs (bubble) or PPCA membership (coalition)
#' @author Stephen Bi
#' @importFrom dplyr %>% mutate select filter
#' @importFrom stats glm

calcPPCA <- function(phase,policy,subpolicy="none",recovery=NULL,size=NULL,run,subtype="inputdata") {
  
  if (grepl(".mif",run) & !grepl("/",run,fixed=TRUE)) {
    run <- paste0(getConfig("sourcefolder"),"/PPCA/",run)
  }
  #Read output from preceding REMIND run in the DPE process
  rundata <- read.report(run,as.list=F)
  rundata <- rundata[,getYears(rundata)<="y2100",]
  #Relevant regional mappings and country classifications
  #Current members of PPCA
  PPCAmap <- toolGetMapping("regionmappingPPCA.csv",type = "regional")
  ppca_map <- new.magpie(PPCAmap[,2],years=NULL,names=NULL,PPCAmap[,3])
  #Current OECD members
  OECDmap <- toolGetMapping("regionmappingOECD.csv",type = "regional")
  oecd_map <- new.magpie(OECDmap[,2],years=NULL,names=NULL,OECDmap[,3])
  #REMIND 12 region mapping
  map <- toolGetMapping(getConfig("regionmapping"),type="regional")
  
  pol_threshold <- 0.2     # REMIND regions only enforce the policy if its member nations comprise over a threshold share of regional energy and electricity demand
  leakage_allowance <- 1.5  # Freeriders in multi-national regions can increase coal consumption by 50% above reference
  
  setConfig(forcecache=T)
  
  # Read historical energy demand
  io <- calcOutput("IO",subtype="input",aggregate=F)[,2015,]
  # io <- calcOutput("IO",subtype="input",aggregate=F,years=seq(2000,2015,5))
  
  #Read historical electricity generation
  data <- readSource("IEA",subtype="EnergyBalances")[,2015,] * 0.0000418680000
  # data <- readSource("IEA",subtype="EnergyBalances")[,seq(2000,2015,5),] * 0.0000418680000
  # setConfig(forcecache = F)
  totalgen_c_hist <- data[,2015,"TOTAL.ELOUTPUT"]
  # totalgen_c_hist <- data[,seq(2000,2015,5),"TOTAL.ELOUTPUT"]
  totalgen_R_hist <- toolAggregate(totalgen_c_hist[,"y2015",],map,NULL)
  
  totalgen_2015_c <- totalgen_c_hist[,2015,]
  totalgen_2015_R <- toolAggregate(totalgen_R_hist[,2015,],map,NULL)
  
  #Read historical coal power generation
  coalvars <- fulldim(data)[[2]]$PRODUCT[1:18]
  coalgen <- data[,2015,paste0(coalvars,".ELOUTPUT")]
  # coalgen <- data[,seq(2000,2015,5),paste0(coalvars,".ELOUTPUT")]
  coalgen <- dimSums(coalgen,dim=3)
  coalgen_2015_c <- coalgen[,2015,]
  coalgen_2015_R <- toolAggregate(toolAggregate(coalgen_2015_c,map,NULL),map,NULL)
  
  #Assign regional load factors to countries with no historical capacity
  # for (y in getYears(loadfactor_c)) {
  #   loadfactor_c[,y,][which(hist_cap_c[,y,]==0 & cap_2025_c[,,"Green"]==0)] <- 0
  #   
  #   loadfactor_c[,y,][which(hist_cap_c[,y,]==0 & cap_2025_c[,,"Brown"]!=0)] <-
  #     loadfactor_R[,y,][map$RegionCode[which(map$CountryCode %in% getRegions(loadfactor_c[which(hist_cap_c[,y,]==0 & cap_2025_c[,,"Brown"]!=0)]))],,]
  # }
  
  if (is.null(recovery)) {
    recovery <- gsub(".mif","",strsplit(run,"-")[[1]][length(strsplit(run,"-")[[1]])])
    if (!(recovery %in% c("BAU","Brown","Green"))) {
      if (grepl("withoutPlus",recovery)) { 
        recovery <- gsub("_withoutPlus","",recovery)
      }else {
        recovery <- "none"
      }
    }
  }
  
  #Read future load factors
  setConfig(forcecache=T)
  loadfactor_c <- calcOutput("CapacityFactor",aggregate=F)[,c(2025,2045),"pc"]
  loadfactor_R <- calcOutput("CapacityFactor")[,c(2025,2045),"pc"]

  #SSP2 GDP per capita
  setConfig(forcecache=T)
  gdppc <- 1e-3 *
    calcOutput("GDP",
               GDPCalib = "past_transition",
               GDPPast = "IHME_USD05_PPP_pc-MI",
               GDPFuture = "SSPs-MI",
               extension2150 = "constant",
               average2020 = FALSE,
               aggregate = FALSE,
               FiveYearSteps = TRUE,
               naming = "scenario") /
    calcOutput("Population",
               PopulationCalib = "past_transition",
               PopulationPast = "WDI-MI",
               PopulationFuture = "SSPs_old-MI",
               FiveYearSteps = TRUE,
               aggregate = FALSE,
               extension2150 = "constant", 
               naming = "scenario")
  gdppc <- gdppc[,c(2015,2025,2045),"SSP2"]
  
  #SSP2 Population
  popC <- calcOutput("Population",
                     PopulationCalib = "past_transition",
                     PopulationPast = "WDI-MI",
                     PopulationFuture = "SSPs_old-MI",
                     FiveYearSteps = TRUE,
                     aggregate = FALSE,
                     extension2150 = "constant", 
                     naming = "scenario"
                     ,years = getYears(rundata)[which(getYears(rundata)>="y2015")])[,,"SSP2"]
  popR <- toolAggregate(popC,map,NULL)
  
  popGrowth <- popC[,getYears(popC)>"y2015",] - popC[,2015,]
  getYears(popGrowth) <- gsub(".y2015","",getYears(popGrowth))
  
  weight <- new.magpie(getRegions(popC),getYears(popGrowth),fill=0)
  # Aggregation weights for downscaling regional model output (and re-aggregating)
  for (yr in getYears(weight)) {
    weight[,yr,] <- data[,2015,"TOTAL.ELOUTPUT"] +
      ifelse(data[,2015,"TOTAL.ELOUTPUT"] + (data[,2015,"TOTAL.ELOUTPUT"]/popC[,2015,]) * popGrowth[,yr,] >= 0,
             (data[,2015,"TOTAL.ELOUTPUT"]/popC[,2015,]) * popGrowth[,yr,], 0)
  }
  getYears(weight) <- getYears(popGrowth)
  
  # Aggregation weights for downscaling coal variables are the same as for energy/electricity demand 
  # except countries with no historical or planned coal demand are given zero weights.
  coalweight <- weight
  pipeline <- readSource("GCPT_PPCApaper",subtype="status",convert=F)
  zero_pipe_reg <- getRegions(pipeline)[which(dimSums(pipeline[,,c("Announced","Pre-permit","Permitted","Construction","Shelved","Operating")],dim=3)==0)]
  coalweight[zero_pipe_reg,getYears(coalweight)<"y2100",] <- 0
  
  # Coal share downscaling function
  downscale_coal <- function(coal_c_t1,total_c_t1,coal_R_t1,total_R_t1,coal_R_t2,total_R_t2) {
    if (length(getRegions(coal_R_t1))<length(getRegions(coal_c_t1)))  
      coal_R_t1 <- toolAggregate(coal_R_t1[-which(getRegions(coal_R_t1)=="GLO"),,],map,NULL)
    if (length(getRegions(total_R_t1))<length(getRegions(coal_c_t1)))  
      total_R_t1 <- toolAggregate(total_R_t1[-which(getRegions(total_R_t1)=="GLO"),,],map,NULL)
    if (length(getRegions(coal_R_t2))<length(getRegions(coal_c_t1)))  
      coal_R_t2 <- toolAggregate(coal_R_t2[-which(getRegions(coal_R_t2)=="GLO"),,],map,NULL)
    if (length(getRegions(total_R_t2))<length(getRegions(coal_c_t1)))  
      total_R_t2 <- toolAggregate(total_R_t2[-which(getRegions(total_R_t2)=="GLO"),,],map,NULL)
    share_c_t1 <- replace_non_finite(coal_c_t1/total_c_t1,replace=0)
    share_R_t1 <- replace_non_finite(coal_R_t1/total_R_t1,replace=0)
    share_R_t2 <- replace_non_finite(coal_R_t2/total_R_t2,replace=0)
    share_c_t2 <- new.magpie(getRegions(coal_c_t1),getYears(coal_R_t2),getNames(coal_c_t1),fill=0)
    for (reg in unique(map$RegionCode)) {
      # reg_oecd_members <- map$CountryCode[which(map$CountryCode %in% oecd_members & map$RegionCode==reg)]
      reg_all <- map$CountryCode[which(map$RegionCode==reg)]
      if (length(reg_all)>1) {
        reg_nonzero <- reg_all[which(share_c_t1[reg_all,,]>0)]
        for (ts in getYears(total_R_t2)) {
          if (length(reg_nonzero)) {
            if (share_R_t2[reg_nonzero[1],ts,]>=share_R_t1[reg_nonzero[1],,]) {
              dist <- (share_c_t1[reg_nonzero,,]-share_R_t1[reg_nonzero,,])/(1-share_R_t1[reg_nonzero,,])
              share_c_t2[reg_nonzero,ts,] <- share_R_t2[reg_nonzero,ts,] + dist*(1-share_R_t2[reg_nonzero,ts,])
            }else {
              dist <- (share_c_t1[reg_nonzero,,]-share_R_t1[reg_nonzero,,])/(0-share_R_t1[reg_nonzero,,])
              share_c_t2[reg_nonzero,ts,] <- share_R_t2[reg_nonzero,ts,] + dist*(0-share_R_t2[reg_nonzero,ts,])
            }
          }
        }
      }else  share_c_t2[reg_all,ts,] <- share_R_t2[reg_all,ts,]
    }
    getYears(share_c_t2) <- getYears(coal_R_t2)
    return(share_c_t2)
  }
  
  #Read coal capacity data from GCPT for the given COVID recovery scenario
  hist_cap_c <- readSource("GCPT_PPCApaper",subtype="historical",convert=F)
  
  # Case with no COVID capacity constraint in 2025
  if (recovery=="none") {
    coalgen_2025_R <- rundata[,"y2025","SE|Electricity|Coal (EJ/yr)"]
    coalgen_2025_R <- toolAggregate(coalgen_2025_R[-which(getRegions(coalgen_2025_R)=="GLO"),,],map,NULL)
    # Proportional country-region relationship from 2015
    Kc <- toolNAreplace(coalgen_2015_c/coalgen_2015_R,replaceby=0)[[1]]
    coalgen_2025_c <- Kc * coalgen_2025_R
    
    # Derive 2025 capacity from rundata and load factor assumptions
    cap_2025_c <- coalgen_2025_c / (loadfactor_c[,2025,]*365*24/277777.77778)
    getYears(cap_2025_c) <- "y2025"
    cap_2025_c[which(hist_cap_c[,2015,]==0 & coalgen_2025_c==0)] <- 0
    
    # Cases with fixed 2025 capacity due to COVID recovery scenario
  }else {
    cap_2025_c <- readSource("GCPT_PPCApaper",subtype="future",convert=F)[,,recovery]
    cap_R_2025 <- toolAggregate(cap_2025_c,rel=map,weight=NULL)
    hist_cap_R <- toolAggregate(hist_cap_c,rel=map,weight=NULL)
    
    print(cap_R_2025)
    #Derive 2025 coal generation in exajoules from coal capacity scenarios
    coalgen_2025_c <- cap_2025_c * loadfactor_c[,2025,] *365*24/277777.77778
    coalgen_2025_c[which(hist_cap_c[,2015,]==0 & cap_2025_c==0)] <- 0
    #Aggregate results to regional level
    coalgen_2025_R <- toolAggregate(toolAggregate(coalgen_2025_c,map,NULL),map,NULL)
  }
  getYears(coalgen_2025_c) <- "y2025"
  #Alternate method from historical IEA data
  # coalgen_2025_R <- rundata[,"y2025","SE|Electricity|Coal|w/o CCS (EJ/yr)"]
  # if (grepl("ccs",policy)) {
  #   coalgen_2025_R <- coalgen_2025_R + rundata[,"y2025","SE|Electricity|Coal|w/ CCS (EJ/yr)"]
  # }
  # coalgen_2025_R <- toolAggregate(coalgen_2025_R[-which(getRegions(coalgen_2025_R)=="GLO"),,],map,NULL)
  # #Apply downscaling formula
  # coalgen_2025_c2 <- coalgen_2015_c + coalgen_2015_c * ((coalgen_2025_R - coalgen_2025_R) / coalgen_2025_R)
  
  
  ### Total Electricity generation 2025 ###
  totalgen_2025_R <- rundata[,"y2025","SE|Electricity (EJ/yr)"]
  # Downscale regional REMIND results to national level using disaggregation weight defined above
  totalgen_2025_c <- toolAggregate(totalgen_2025_R[-which(getRegions(totalgen_2025_R)=="GLO"),,],map,weight[,2025,])
  
  for (reg in getRegions(totalgen_2025_R)) {
    reg_all <- map$CountryCode[which(map$RegionCode==reg)]
    reg_excess <- reg_all[which(coalgen_2025_c[reg_all,,]/totalgen_2025_c[reg_all,,] > 1)]
    reg_nonzero <- reg_all[which(totalgen_2025_c[reg_all,,] > 0)]
    
    if (length(reg_excess)) {
      reg_normalized <- totalgen_2025_c[reg_nonzero,,] /
        dimSums(totalgen_2025_c[reg_nonzero,,],dim=1)
      
      excess <- dimSums(coalgen_2025_c[reg_excess,,] - totalgen_2025_c[reg_excess,,] / 
                          max(coalgen_2015_c[reg_all,,]/totalgen_2015_c[reg_all,,],na.rm=TRUE), dim=1)
      
      totalgen_2025_c[reg_excess,,] <- coalgen_2025_c[reg_excess,,] / 
        max(coalgen_2015_c[reg_all,,]/totalgen_2015_c[reg_all,,],na.rm=TRUE)
      
      totalgen_2025_c[reg_nonzero,,] <- totalgen_2025_c[reg_nonzero,,] - 
        excess * reg_normalized
    }
  }
  getYears(totalgen_2025_c) <- "y2025"
  
  #Derive country-level coal shares
  coalShare_2025_c <- replace_non_finite(coalgen_2025_c/totalgen_2025_c,replace = 0)
  
  # Assume electricity demand remains constant until 2025 in OECD members and countries with declining population or GDP
  # This is to roughly account for uneven growth rates in our regional downscaling, especially for growing economies with large coal pipelines 
  # flat_countries <- map$CountryCode[which(map$RegionCode %in% c("SSA","OAS","LAM") & 
  #                                           (popGrowth2025<=0 | gdpGrowth2025<=0 | OECDmap$RegionCode=="OECD"))]
  # totalgen_2015_c[flat_countries,,] <- 0
  # totalgen_2015_R <- toolAggregate(toolAggregate(totalgen_2015_c,map,NULL),map,NULL)
  # eGrowth_2025_R <- (totalgen_2025_R - totalgen_2015_R) / totalgen_2015_R
  # eGrowth_2025_R[flat_countries,,] <- 0
  
  # Linear proportional country-region relationship downscaling method #
  # totalgen_2025_R <- toolAggregate(totalgen_2025_R[-which(getRegions(totalgen_2025_R)=="GLO"),,],map,NULL)
  # totalgen_2025_c <- totalgen_c_hist[,2015,] + totalgen_c_hist[,2015,] * (totalgen_2025_R - totalgen_2015_R) / totalgen_2015_R
  # Alternative method...
  #totalgen_2025_c2 <-  totalgen_2025_R * totalgen_c_hist[,2015,]/totalgen_2015_R
  
  #########################################
  ########## DEFINE CURRENT PPCA ##########
  #########################################
  if (grepl("current",size)) {
    ppca <- PPCAmap$CountryCode[which(PPCAmap$RegionCode=="PPCA")]
    oecd <- OECDmap$CountryCode[which(OECDmap$RegionCode=="OECD")]
    EU27 <- map$CountryCode[which(map$RegionCode=="EUR")]
    nonoecd <- OECDmap$CountryCode[which(OECDmap$RegionCode=="NON")]
    #The PPCA stipulates that OECD and EU members must phase out coal power by 2030
    oecd_members <- ppca[which(ppca %in% oecd | ppca %in% EU27)]
    #While non-OECD members must phase out coal power by 2050
    nonoecd_members <- ppca[which(!(ppca %in% oecd_members))]
  }else {
    #################################################
    ################## LOGIT MODEL ##################
    #################################################
    # Read file tracking the current status of the PPCA which contains GDPpc, % coal in electricity, 
    # standing coal power capacity, among other data
    # s3 <- read.csv(paste0(getConfig("sourcefolder"),"/PPCA/PPCA_status_SSP2_2015.csv"),stringsAsFactors = F,sep = ",")
    s3 <- read.csv(paste0(getConfig("sourcefolder"),"/PPCA/PPCA_status_SSP2_2015.csv"),stringsAsFactors = F,sep = ",")
    # s3 <- s3 %>% mutate(PPCA.Bin=ifelse(PPCA=="PPCA",1,0))
    p <- c(0.95, 0.75, 0.5, 0.25, 0.05)
    
    #Logit regression of existing PPCA membership 
    logit2 <- glm(data = s3, PPCA.Bin ~  Coal.Share + GDP.PC, family = "binomial")
    ic <-  (log(1/p -1) -  summary(logit2)$coef[1])/ summary(logit2)$coef[3]
    slope <- -(summary(logit2)$coef[2]/summary(logit2)$coef[3])
    # All countries lying above this line are members of the given coalition scenario
    ln <- data.frame(Ic = ic, Slope = slope, Prob = as.character(p))
    
    # coalShare_2025_c[which(coalShare_2025_c>1)] <- max(coalShare_2025_c[which(coalShare_2025_c<1)])
    # coalShare_2025_c[which(coalShare_2025_c>1)] <- 0.8
    getNames(coalShare_2025_c) <- recovery
    getYears(coalShare_2025_c) <- "y2025"
    
    OECD <- data.frame(country=sort(getRegions(coalShare_2025_c)),oecd=as.character(oecd_map),ppca=as.character(ppca_map),gdp=as.numeric(gdppc[,2025,]),
                       share=as.numeric(coalShare_2025_c))
    
    #Determine PPCA coalition scenarios
    OECD <- OECD %>% mutate(coalition=ifelse(share<(gdp-ln[1,"Ic"])/ln[1,"Slope"] & 
                                               gdp>(ln[1,"Ic"]+ln[1,"Slope"]*share) & oecd=="OECD","3p","free"))
    OECD <- OECD %>% mutate(coalition=ifelse(share<(gdp-ln[3,"Ic"])/ln[3,"Slope"] & 
                                               gdp>(ln[3,"Ic"]+ln[3,"Slope"]*share) & oecd=="OECD","2p",coalition))
    OECD <- OECD %>% mutate(coalition=ifelse((share<(gdp-ln[5,"Ic"])/ln[5,"Slope"] & 
                                                gdp>(ln[5,"Ic"]+ln[5,"Slope"]*share) | ppca=="PPCA") & oecd=="OECD","1p",coalition))
    
    #OECD coalition
    oecd_1p <- OECD %>% filter(coalition=="1p") %>% select(country)
    oecd_2p <- OECD %>% filter(coalition %in% c("1p","2p")) %>% select(country)
    oecd_3p <- OECD %>% filter(coalition %in% c("1p","2p","3p")) %>% select(country)
    
    if (grepl("coalitions",subtype) & !grepl("Non",phase,ignore.case=TRUE)) {
      return(list(oecd1p=oecd_1p,oecd2p=oecd_2p,oecd3p=oecd_3p))
    }
    
    if (grepl("1p",run) | grepl("1p",size)) {
      oecd_members <- as.character(oecd_1p[,1])
    }else if (grepl("2p",run) | grepl("2p",size)) {
      oecd_members <- as.character(oecd_2p[,1])
    }else if (grepl("3p",run) | grepl("3p",size)) {
      oecd_members <- as.character(oecd_3p[,1])
    }
  }
  
  ################################################
  ########### POWER EXIT REMIND DATA #############
  ################################################
  
  # Assign zero weights to OECD PPCA members for coal variables in 2030 and later (Not in use)
  # coalweight[oecd_members,getYears(coalweight)>="y2030",] <- numTe * 1e-6
  
  # Use regional downscaling of REMIND results to derive country-level coal and total electricity generation in 2030
  coalgen_2030_R <- rundata[,getYears(rundata)>="y2030","SE|Electricity|Coal (EJ/yr)"]
  coalgen_2030_R <- toolAggregate(coalgen_2030_R[-which(getRegions(coalgen_2030_R)=="GLO"),,],map,NULL)
  # coalgen_2030_c <- toolAggregate(coalgen_2030_R[-which(getRegions(coalgen_2030_R)=="GLO"),,],map,coalweight[,2030,])
  
  totalgen_2030_R <- rundata[,getYears(rundata)>="y2030","SE|Electricity (EJ/yr)"]
  totalgen_2030_c <- toolAggregate(totalgen_2030_R[-which(getRegions(totalgen_2030_R)=="GLO"),,],map,weight[,2030,])
  # totalgen_2030_c <- totalgen_2025_c + totalgen_2025_c * ((totalgen_2030_R - totalgen_2025_R) / totalgen_2025_R)
  # getYears(totalgen_2030_c) <- "y2030"
  
  # Apply downscale formula to derive 2030 coal generation by country
  coalshare_2030_c <- downscale_coal(coalgen_2025_c,totalgen_2025_c,coalgen_2025_R,totalgen_2025_R,coalgen_2030_R,totalgen_2030_R)
  coalgen_2030_c <- coalshare_2030_c * totalgen_2030_c
  
  # Set OECD PPCA members to zero
  # tmp_coalgen_2025_c <- coalgen_2025_c
  # tmp_coalgen_2025_c[oecd_members,,] <- 0
  
  # Kc <- replace_non_finite(coalgen_2025_c/coalgen_2025_R,replace = 0)
  # coalgen_2030_c <- coalgen_2030_R * Kc
  
  #Read 2045 total electricity generation from appropriate OECD phase-out REMIND scenario
  totalgen_2045_R <- rundata[,"y2045","SE|Electricity (EJ/yr)"]
  totalgen_2045_c <- toolAggregate(totalgen_2045_R[-which(getRegions(totalgen_2045_R)=="GLO"),,],map,weight[,2045,])
  
  coalgen_2045_R <- rundata[,"y2045","SE|Electricity|Coal (EJ/yr)"]
  # coalgen_2045_c <- toolAggregate(coalgen_2045_R[-which(getRegions(coalgen_2045_R)=="GLO"),,],map,coalweight[,2045,])
  coalgen_2045_R <- toolAggregate(coalgen_2045_R[-which(getRegions(coalgen_2045_R)=="GLO"),,],map,NULL)
  
  #Read 2050 total electricity generation from appropriate OECD phase-out REMIND scenario
  totalgen_2050_R <- rundata[,getYears(rundata)>="y2050","SE|Electricity (EJ/yr)"]
  totalgen_2050_c <- toolAggregate(totalgen_2050_R[-which(getRegions(totalgen_2050_R)=="GLO"),,],map,weight[,2050,])
  
  coalgen_2050_R <- rundata[,getYears(rundata)>="y2050","SE|Electricity|Coal (EJ/yr)"]
  # coalgen_2050_c <- toolAggregate(coalgen_2050_R[-which(getRegions(coalgen_2050_R)=="GLO"),,],map,coalweight[,2050,])
  coalgen_2050_R <- toolAggregate(coalgen_2050_R[-which(getRegions(coalgen_2050_R)=="GLO"),,],map,NULL)
  
  
  ##################################################
  ### 2045 COAL SHARE IN ELECTRICITY CALCULATION ###
  ##################################################
  logit_coalgen_2030_c <- coalgen_2030_c[,"y2030",]
  for (reg in unique(map$RegionCode)) {
    reg_oecd_members <- map$CountryCode[which(map$CountryCode %in% oecd_members & map$RegionCode==reg)]
    reg_all <- map$CountryCode[which(map$RegionCode==reg)]
    reg_freeriders <- setdiff(reg_all,reg_oecd_members)
    reg_nonzero_freeriders <- getRegions(logit_coalgen_2030_c[reg_freeriders,,])[which(logit_coalgen_2030_c[reg_freeriders,,]>0)]
    
    # Set OECD PPCA members' 2030 coal share to 0
    logit_coalgen_2030_c[reg_oecd_members,,] <- 0
    
    ## If there are both OECD PPCA members and freeriders in the region, distribute the PPCA members' coal to the freeriders
    if (length(reg_oecd_members) & length(reg_nonzero_freeriders)) {
      reg_normalized_pow_dem <- totalgen_2030_c[reg_nonzero_freeriders,"y2030",]/dimSums(totalgen_2030_c[reg_nonzero_freeriders,"y2030",],dim=1)
      
      logit_coalgen_2030_c[reg_nonzero_freeriders,,] <- logit_coalgen_2030_c[reg_nonzero_freeriders,,] + 
        dimSums(coalgen_2030_c[reg_oecd_members,"y2030",]-logit_coalgen_2030_c[reg_oecd_members,,],dim=1) * reg_normalized_pow_dem
      
      reg_excess <- reg_nonzero_freeriders[which(logit_coalgen_2030_c[reg_nonzero_freeriders,,]/totalgen_2030_c[reg_nonzero_freeriders,"y2030",] > 1)]
      
      ## If the redistribution of coal results in other countries with >100% share in coal...
      while (length(reg_excess) & length(reg_nonzero_freeriders)) {
        excess <- dimSums(logit_coalgen_2030_c[reg_excess,,] - (leakage_allowance * coalshare_2030_c[reg_excess,"y2030",]), dim=1)
        
        # Set them to the maximum allowed leakage
        logit_coalgen_2030_c[reg_excess,,] <- leakage_allowance * coalgen_2030_c[reg_excess,"y2030",]
        
        reg_nonzero_freeriders <- reg_nonzero_freeriders[which(!(reg_nonzero_freeriders %in% reg_excess))]
        
        # And redistribute the excess to other freeriding nations (if any)
        if (length(reg_nonzero_freeriders)) {
          reg_normalized_pow_dem <- totalgen_2030_c[reg_nonzero_freeriders,"y2030",]/dimSums(totalgen_2030_c[reg_nonzero_freeriders,"y2030",],dim=1)
          logit_coalgen_2030_c[reg_nonzero_freeriders,,] <- logit_coalgen_2030_c[reg_nonzero_freeriders,,] + excess * reg_normalized_pow_dem
          
          # Check again if this causes any countries to exceed 100%
          reg_excess <- reg_nonzero_freeriders[which(logit_coalgen_2030_c[reg_nonzero_freeriders,,]/totalgen_2030_c[reg_nonzero_freeriders,"y2030",] > 1)]
        }
      }
    }
  }
  
  ###############################
  ######### DEMAND EXIT #########
  ###############################
  if (grepl("demand",policy)) {
    for (yr in getYears(weight)) {
      weight[,yr,] <- data[,2015,"TOTAL.TFC"] + 
        ifelse(data[,2015,"TOTAL.TFC"] + (data[,2015,"TOTAL.TFC"]/popC[,2015,]) * popGrowth[,yr,] > 0,
               (data[,2015,"TOTAL.TFC"]/popC[,2015,]) * popGrowth[,yr,], 0)
    }
    zero_coal_dem <- getRegions(data)[which(dimSums(data[,2015,paste0(coalvars,".TFC")],dim=3)==0)]
    coalweight[intersect(zero_coal_dem,zero_pipe_reg),getYears(coalweight)<"y2100",] <- 0
    
    setConfig(forcecache=T)
    # Steel sector is permitted to continue using coal for 20 years after other demand sectors
    if (grepl("steel",subpolicy)) {
      # Coal demand from the steel sector
      coaldem_c <- dimSums(data[,2015,paste0(coalvars,".IRONSTL")],dim=3)
      
      # 2050 steel sector coal demand from REMIND run (using coalemi_2030 for convenience)
      coalemi_2030_R <- rundata[,getYears(rundata)>="y2040",]
      coalemi_2030_R <- coalemi_2030_R[-which(getRegions(coalemi_2030_R)=="GLO"),,"Emi|CO2|FFaI|Industry|Steel|Fuel|Coal (Mt CO2/yr)"]
      totalemi_2030_R <- dimSums(rundata[,getYears(rundata)>="y2040","Emi|CO2|w/ Bunkers (Mt CO2/yr)"],dim=3)
      
    }else if (grepl("solids",subpolicy)) {
      # Historical coal solids demand
      coaldem_c <- io[,2015,"pecoal.sesofos.coaltr"]
      
      # Coal solids emissions except from steel sector in 2030
      coalemi_2030_R <- rundata[,getYears(rundata)>="y2030",]
      coalemi_2030_R <- coalemi_2030_R[-which(getRegions(coalemi_2030_R)=="GLO"),,"Emi|CO2|Energy|Demand|Solids|After IndustryCCS (Mt CO2/yr)"] - 
        coalemi_2030_R[-which(getRegions(coalemi_2030_R)=="GLO"),,"Emi|CO2|FFaI|Industry|Steel|Fuel|Coal (Mt CO2/yr)"]
      totalemi_2030_R <- dimSums(rundata[,getYears(rundata)>="y2030","Emi|CO2|w/ Bunkers (Mt CO2/yr)"],dim=3)
      
    }else {
      # Coal demand (except solids) for 2015
      coaldem_c <- dimSums(io[,2015,"pecoal"],dim=3) - io[,2015,"pecoal.sesofos.coaltr"]
      
      # 2030 Coal demand from REMIND run
      coalemi_2030_R <- rundata[,getYears(rundata)>="y2030",]
      coalemi_2030_R <- coalemi_2030_R[-which(getRegions(coalemi_2030_R)=="GLO"),,"Emi|CO2|Fossil Fuels and Industry|Coal|Before IndustryCCS (Mt CO2/yr)"] - 
        coalemi_2030_R[-which(getRegions(coalemi_2030_R)=="GLO"),,"Emi|CO2|Energy|Demand|Solids|After IndustryCCS (Mt CO2/yr)"]
      totalemi_2030_R <- dimSums(rundata[,getYears(rundata)>="y2030","Emi|CO2|w/ Bunkers (Mt CO2/yr)"],dim=3)
    }
    
    # Convert historical coal demand to emissions 
    # 26.1 GtC/ZJ to MtCO2/EJ
    emifac_coal <- 26.1 * 3.67 * 1e-3 * 1e3
    coalemi_c <- coaldem_c * emifac_coal
    coalemi_R <- toolAggregate(toolAggregate(coalemi_c,map,NULL),map,NULL)
    setConfig(forcecache=T)
    
    # Read in total 2015 emissions by country
    totalemi_c <- dimSums(calcOutput("HistEmissions",subtype="sector",aggregate=F,years=2015)[,,"co2"],dim=3)
    # totalemi_c <- dimSums(calcOutput("HistEmissions",subtype="sector",aggregate=F,years=seq(2000,2015,5)),dim=2)
    totalemi_R <- toolAggregate(toolAggregate(totalemi_c,map,NULL),map,NULL)
    
    ## Downscale total emissions from REMIND energy demand and population disaggregation weight
    totalemi_2030_c <- toolAggregate(totalemi_2030_R[-which(getRegions(totalemi_2030_R)=="GLO"),,],map,weight[,getYears(totalemi_2030_R),])
    coalemi_2030_R <- toolAggregate(coalemi_2030_R,map,NULL)
    
    # Extrapolate country-level coal emissions from historical 2015 data
    coalshare_2030_c <- downscale_coal(coalemi_c,totalemi_c,coalemi_R,totalemi_R,coalemi_2030_R,totalemi_2030_R)
    coalemi_2030_c <- coalshare_2030_c * totalemi_2030_c
    
    getNames(coalemi_2030_c) <- recovery
    
    # tmp_coalemi_c <- coalemi_c
    # tmp_coalemi_c[oecd_members,,] <- 1e-9
    # Kc <- replace_non_finite(tmp_coalemi_c/coalemi_R,replace=0)
    # coalemi_2030_c <- coalemi_2030_R * Kc 
    
    #Derive coal and total country-level emissions from REMIND output weighted by population growth and base year energy demand 
    # coalemi_2030_c <- toolAggregate(coalemi_2030_R,map,coalweight[,getYears(coalemi_2030_R),])
    
    # totalemi_2030_c <- totalemi_2030_R * replace_non_finite(totalemi_c/totalemi_R,replace=0)
    
    # coalemi_2030_R <- toolAggregate(toolAggregate(coalemi_2030_c,map,NULL),map,NULL)
    # totalemi_2030_R <- toolAggregate(toolAggregate(totalemi_2030_c,map,NULL),map,NULL)
  }
  
  
  # Proportional country-region relationship from 2030 to 2045
  # Kc <- toolNAreplace(logit_coalgen_2030_c[,"y2030",]/coalgen_2030_R[,"y2030",],replaceby=0)[[1]]
  # coalgen_2045_c <- Kc * coalgen_2045_R
  # Derive 2045 coal share in electricity for use in logistic regression
  logit_coalShare_2045_c <- downscale_coal(logit_coalgen_2030_c,totalgen_2030_c[,"y2030",],coalgen_2030_R[,"y2030",],totalgen_2030_R[,"y2030",],coalgen_2045_R[,"y2045",],totalgen_2045_R[,"y2045",])
  logit_coalgen_2045_c <- logit_coalShare_2045_c * totalgen_2045_c[,"y2045",]
  getNames(logit_coalShare_2045_c) <- recovery
  
  # 2045 Coal share in electricity for use in REMIND policy formulation
  coalShare_2045_c <- downscale_coal(coalgen_2030_c[,"y2030",],totalgen_2030_c[,"y2030",],coalgen_2030_R[,"y2030",],totalgen_2030_R[,"y2030",],coalgen_2045_R,totalgen_2045_R)
  coalgen_2045_c <- coalShare_2045_c * totalgen_2045_c
  
  # coalShare_2045_c <- replace_non_finite(coalgen_2045_c/totalgen_2045_c,replace = 0)
  getNames(coalShare_2045_c) <- recovery
  getYears(coalShare_2045_c) <- "y2045"
  
  #################################################
  ################## LOGIT MODEL ##################
  #################################################
  if (!grepl("current",size)) {
    nonOECD <- data.frame(country=sort(getRegions(logit_coalShare_2045_c)),oecd=as.character(oecd_map),ppca=as.character(ppca_map),
                          share_2045=as.numeric(logit_coalShare_2045_c),gdp=as.numeric(gdppc[,2045,]))
    #Determine PPCA coalition scenarios
    nonOECD <- nonOECD %>% mutate(nonOECDcoalition=ifelse(share_2045<(gdp-ln[1,"Ic"])/ln[1,"Slope"] & 
                                                            gdp>(ln[1,"Ic"]+ln[1,"Slope"]*share_2045) & oecd=="Non-OECD","3p","free"))
    nonOECD <- nonOECD %>% mutate(nonOECDcoalition=ifelse(share_2045<(gdp-ln[3,"Ic"])/ln[3,"Slope"] & 
                                                            gdp>(ln[3,"Ic"]+ln[3,"Slope"]*share_2045) & oecd=="Non-OECD","2p",nonOECDcoalition))
    nonOECD <- nonOECD %>% mutate(nonOECDcoalition=ifelse(((share_2045<(gdp-ln[5,"Ic"])/ln[5,"Slope"] & 
                                                              gdp>(ln[5,"Ic"]+ln[5,"Slope"]*share_2045)) | ppca=="PPCA") & oecd=="Non-OECD","1p",nonOECDcoalition))
    
    #Non-OECD coalitions
    non_oecd_1p <- nonOECD %>% filter(nonOECDcoalition=="1p") %>% select(country)
    non_oecd_2p <- nonOECD %>% filter(nonOECDcoalition %in% c("1p","2p")) %>% select(country)
    non_oecd_3p <- nonOECD %>% filter(nonOECDcoalition %in% c("1p","2p","3p")) %>% select(country)
    
    if (grepl("1p",run) | grepl("1p",size)) {
      nonoecd_members <- as.character(non_oecd_1p[,1])
    }else if (grepl("2p",run) | grepl("2p",size)) {
      nonoecd_members <- as.character(non_oecd_2p[,1])
    }else if (grepl("3p",run) | grepl("3p",size)) {
      nonoecd_members <- as.character(non_oecd_3p[,1])
    }
    
    if (grepl("coalitions",subtype)) {
      for (yr in getYears(weight)) {
        weight[,yr,] <- data[,2015,"TOTAL.TFC"] + 
          ifelse(data[,2015,"TOTAL.TFC"] + (data[,2015,"TOTAL.TFC"]/popC[,2015,]) * popGrowth[,yr,] > 0,
                 (data[,2015,"TOTAL.TFC"]/popC[,2015,]) * popGrowth[,yr,], 0)
      }
      getYears(weight) <- getYears(popGrowth)
      return(list(non_oecd=unique(c(oecd_members,nonoecd_members)),weight=weight))
    }
    # browser()
    if (grepl("validation",subtype)) {
      country_probs <- predict(logit2,data.frame(GDP.PC=as.numeric(gdppc[,2045,]),Coal.Share=as.numeric(logit_coalShare_2045_c)),type="response")
      yprobs <- ifelse(country_probs>=0.5,1,0)
      return(list(Fit=val.prob(country_probs,yprobs),Values=data.frame(Country=map$CountryCode,Probability=country_probs)))
    }
  }
  
  
  if (grepl("power",policy)) {
    # Proportional country-region relationship from 2045 (after OECD PPCA implementation)
    # Kc <- toolNAreplace(coalgen_2045_c/coalgen_2045_R,replaceby=0)[[1]]
    # coalgen_2050_c <- Kc * coalgen_2050_R
    # totalgen_2050_c <- Kt * totalgen_2050_R
    
    # Extrapolate 2050 national coal shares from 2045
    coalshare_2050_c <- downscale_coal(coalgen_2045_c,totalgen_2045_c,coalgen_2045_R,totalgen_2045_R,coalgen_2050_R,totalgen_2050_R)
    coalgen_2050_c <- coalshare_2050_c * totalgen_2050_c
    getYears(coalgen_2050_c) <- getYears(coalgen_2050_R)
    
  }else if (grepl("demand",policy)) {
    if (grepl("steel",subpolicy)) {
      # Coal solids emissions from steel sector in 2060 (using 2050 vars for convenience)
      coalemi_2050_R <- rundata[,getYears(rundata)>="y2060",]
      coalemi_2050_R <- coalemi_2050_R[-which(getRegions(coalemi_2050_R)=="GLO"),,"Emi|CO2|FFaI|Industry|Steel|Fuel|Coal (Mt CO2/yr)"]
      totalemi_2050_R <- dimSums(rundata[,getYears(rundata)>="y2060","Emi|CO2|w/ Bunkers (Mt CO2/yr)"],dim=3)
    }else if (grepl("solids",subpolicy)) {
      # Coal solids emissions except from steel sector in 2050
      coalemi_2050_R <- rundata[,getYears(rundata)>="y2050",]
      coalemi_2050_R <- coalemi_2050_R[-which(getRegions(coalemi_2050_R)=="GLO"),,"Emi|CO2|Energy|Demand|Solids|After IndustryCCS (Mt CO2/yr)"] - 
        coalemi_2050_R[-which(getRegions(coalemi_2050_R)=="GLO"),,"Emi|CO2|FFaI|Industry|Steel|Fuel|Coal (Mt CO2/yr)"]
      totalemi_2050_R <- dimSums(rundata[,getYears(rundata)>="y2050","Emi|CO2|w/ Bunkers (Mt CO2/yr)"],dim=3)
    }else {
      # Coal emissions (except from solids) from 2050 time step of the reference scenario
      coalemi_2050_R <- rundata[,getYears(rundata)>="y2050",]
      coalemi_2050_R <- coalemi_2050_R[-which(getRegions(coalemi_2050_R)=="GLO"),,"Emi|CO2|Fossil Fuels and Industry|Coal|Before IndustryCCS (Mt CO2/yr)"] - 
        coalemi_2050_R[-which(getRegions(coalemi_2050_R)=="GLO"),,"Emi|CO2|Energy|Demand|Solids|After IndustryCCS (Mt CO2/yr)"]
      totalemi_2050_R <- dimSums(rundata[,getYears(rundata)>="y2050","Emi|CO2|w/ Bunkers (Mt CO2/yr)"],dim=3)
    }
    # Proportional country-region relationship from 2030 (after OECD PPCA implementation) 
    # coalemi_2030_R <- toolAggregate(toolAggregate(coalemi_2030_c,map,NULL),map,NULL)
    # Kc <- toolNAreplace(coalemi_2030_c[,"y2030",]/coalemi_2030_R[,"y2030",],replaceby=0)[[1]]
    # coalemi_2050_c <- Kc * coalemi_2050_R
    
    # coalemi_2050_c <- toolAggregate(coalemi_2050_R[,,],map,coalweight[,getYears(coalemi_2050_R),])
    
    totalemi_2050_c <- toolAggregate(totalemi_2050_R[-which(getRegions(totalemi_2050_R)=="GLO"),,],map,weight[,getYears(totalemi_2050_R),])
    coalemi_2050_R <- toolAggregate(coalemi_2050_R,map,NULL)
    
    # Extrapolate 2050 national coal shares from 2030
    coalshare_2050_c <- downscale_coal(coalemi_2030_c[,getYears(coalemi_2030_c)[1],],totalemi_2030_c[,getYears(totalemi_2030_c)[1],],coalemi_2030_R[,getYears(coalemi_2030_R)[1],],totalemi_2030_R[,getYears(totalemi_2030_R)[1],],coalemi_2050_R,totalemi_2050_R)
    coalemi_2050_c <- coalshare_2050_c * totalemi_2050_c
    getYears(coalemi_2050_c) <- getYears(coalemi_2050_R)
    getNames(coalemi_2050_c) <- recovery
    getNames(totalemi_2050_c) <- recovery
    
  }
  
  # Unmanipulated reference scenario variables
  coalgen_ttot_R <- rundata[-which(getRegions(rundata)=="GLO"),getYears(coalweight),"SE|Electricity|Coal (EJ/yr)"]
  coalgen_ttot_c <- toolAggregate(coalgen_ttot_R,map,coalweight)
  
  totalgen_ttot_R <- rundata[-which(getRegions(rundata)=="GLO"),getYears(weight),"SE|Electricity (EJ/yr)"]
  totalgen_ttot_c <- toolAggregate(totalgen_ttot_R,map,weight)
  
  coalemi_ttot_R <- rundata[-which(getRegions(rundata)=="GLO"),getYears(coalweight),"Emi|CO2|Fossil Fuels and Industry|Coal|Before IndustryCCS (Mt CO2/yr)"]
  coalemi_ttot_c <- toolAggregate(coalemi_ttot_R,map,coalweight)
  
  totalemi_ttot_R <- rundata[-which(getRegions(rundata)=="GLO"),getYears(weight),"Emi|CO2 (Mt CO2/yr)"]
  totalemi_ttot_c <- toolAggregate(totalemi_ttot_R,map,weight)
  
  endem_ttot_R <- rundata[-which(getRegions(rundata)=="GLO"),getYears(weight),"PE (EJ/yr)"]
  endem_ttot_c <- toolAggregate(endem_ttot_R,map,weight)
  
  
  ##### Derive output for implementation of policy coalition in REMIND #####
  coalshare_2030 <- new.magpie(map$CountryCode,years=NULL,names=NULL,fill=NA)
  coalshare_2050 <- new.magpie(map$CountryCode,years=NULL,names=NULL,fill=NA)
  OECDexit <- new.magpie(unique(map$RegionCode),years=NULL,names=NULL,fill=0)
  
  # print(dimSums(toolAggregate(coalgen_2050_c[,getYears(coalgen_2050_c)>="y2050",],map,NULL),dim=2) /
  #         dimSums(toolAggregate(totalgen_2050_c[,getYears(coalgen_2050_c)>="y2050",],map,NULL),dim=2))
  
  for (reg in unique(map$RegionCode)) {
    reg_oecd_members <- map$CountryCode[which(map$CountryCode %in% oecd_members & map$RegionCode==reg)]
    reg_members <- map$CountryCode[which(map$CountryCode %in% c(oecd_members,nonoecd_members) & map$RegionCode==reg)]
    reg_all <- map$CountryCode[which(map$RegionCode==reg)]
    
    # Apply coal phase-out constraint to a region if its PPCA members constitute >20% of total regional coal power demand and energy demand 
    if (length(reg_members)>0) {
      if (grepl("power",policy)) {
        # OECD exit
        if (!grepl("Non",phase,ignore.case=TRUE) && length(reg_oecd_members>0) && 
            (length(reg_oecd_members)>1 | length(reg_all)==1) &&
            dimSums(dimSums(endem_ttot_c[reg_oecd_members,getYears(endem_ttot_c)>="y2030",],dim=1),dim=2) > 
            (pol_threshold * dimSums(dimSums(endem_ttot_c[reg_all,getYears(endem_ttot_c)>="y2030",],dim=1),dim=2)) &&
            dimSums(dimSums(coalgen_2030_c[reg_oecd_members,getYears(coalgen_2030_c)>="y2030",],dim=1),dim=2) > 
            (pol_threshold * dimSums(dimSums(coalgen_2030_c[reg_all,getYears(coalgen_2030_c)>="y2030",],dim=1),dim=2))) {
          #Set 2030 coal power generation in coalition members to zero
          coalgen_2030_c[reg_oecd_members,getYears(coalgen_2030_c)>="y2030",] <- 1e-9
          # Calculate 2030 coal share in electricity or energy demand as output for REMIND policy implementation 
          # Set the policy stringency based on 2050 to allow room for the leakage
          coalshare_2030[reg_all,,] <- ifelse(length(reg_oecd_members)==length(reg_all),1,leakage_allowance) *
            toolNAreplace(dimSums(coalgen_2030_c[reg_all,getYears(coalgen_2030_c)>="y2030",],dim=2) /
                            dimSums(totalgen_2030_c[reg_all,getYears(totalgen_2030_c)>="y2030",],dim=2),replaceby=0)[[1]]
          OECDexit[reg,,] <- 1
        }else {
          coalshare_2030[reg_all,,] <- 0
        }
        # Non-OECD exit 
        if (OECDexit[reg,,]==1 | ((length(reg_members)>1 | length(reg_all)==1) &&
                                  dimSums(dimSums(endem_ttot_c[reg_members,getYears(endem_ttot_c)>="y2050",],dim=1),dim=2) > 
                                  (pol_threshold * dimSums(dimSums(endem_ttot_c[reg_all,getYears(endem_ttot_c)>="y2050",],dim=1),dim=2)) &&
                                  dimSums(dimSums(coalgen_2050_c[reg_members,getYears(coalgen_2050_c)>="y2050",],dim=1),dim=2) > 
                                  (pol_threshold * dimSums(dimSums(coalgen_2050_c[reg_all,getYears(coalgen_2050_c)>="y2050",],dim=1),dim=2)))) {
          #Set 2050 coal power generation in coalition members to zero
          coalgen_2050_c[reg_members,getYears(coalgen_2050_c)>="y2050",] <- 1e-9
          # Calculate 2050 coal share in electricity or energy demand as output for REMIND policy implementation 
          coalshare_2050[reg_all,,] <- ifelse(length(reg_members)==length(reg_all),1,leakage_allowance) *
            toolNAreplace(dimSums(coalgen_2050_c[reg_all,getYears(coalgen_2050_c)>="y2050",],dim=2) / 
                            dimSums(totalgen_2050_c[reg_all,getYears(totalgen_2050_c)>="y2050",],dim=2),replaceby=0)[[1]]
        }else {
          coalshare_2050[reg_all,,] <- 0
        }
      }else if (grepl("demand",policy)) {
        # OECD exit
        if (!grepl("Non",phase,ignore.case=TRUE) && length(reg_oecd_members>0) && 
            (length(reg_oecd_members)>1 | length(reg_all)==1) &&
            dimSums(dimSums(coalemi_2030_c[reg_oecd_members,,],dim=1),dim=2) > 
            (pol_threshold * dimSums(dimSums(coalemi_2030_c[reg_all,,],dim=1),dim=2)) &&
            dimSums(dimSums(endem_ttot_c[reg_oecd_members,getYears(coalemi_2030_c),],dim=1),dim=2) > 
            (pol_threshold * dimSums(dimSums(endem_ttot_c[reg_all,getYears(coalemi_2030_c),],dim=1),dim=2))) {
          #Set 2030 coal emissions in coalition members to zero
          coalemi_2030_c[reg_oecd_members,,] <- 1e-9
          # Calculate 2030 coal share in electricity or energy demand as output for REMIND policy implementation 
          coalshare_2030[reg_all,,] <- ifelse(length(reg_oecd_members)==length(reg_all),1,leakage_allowance) *
            toolNAreplace(dimSums(coalemi_2030_c[reg_all,,],dim=2) / 
                            dimSums(totalemi_2030_c[reg_all,,],dim=2),replaceby=0)[[1]]
          OECDexit[reg,,] <- 1
        }else {
          coalshare_2030[reg_all,,] <- 0
        }
        # Non-OECD exit 
        if (OECDexit[reg,,]==1 | ((length(reg_members)>1 | length(reg_all)==1) &&
                                  dimSums(dimSums(coalemi_2050_c[reg_members,,],dim=1),dim=2) > 
                                  (pol_threshold * dimSums(dimSums(coalemi_2050_c[reg_all,,],dim=1),dim=2)) &&
                                  dimSums(dimSums(endem_ttot_c[reg_members,getYears(coalemi_2050_c),],dim=1),dim=2) > 
                                  (pol_threshold * dimSums(dimSums(endem_ttot_c[reg_all,getYears(coalemi_2050_c),],dim=1),dim=2)))) {
          #Set 2050 coal emissions in coalition members to zero
          coalemi_2050_c[reg_members,,] <- 1e-9
          # Calculate 2050 coal share in electricity or energy demand as output for REMIND policy implementation 
          coalshare_2050[reg_all,,] <- ifelse(length(reg_members)==length(reg_all),1,leakage_allowance) *
            toolNAreplace(dimSums(coalemi_2050_c[reg_all,,],dim=2) / 
                            dimSums(totalemi_2050_c[reg_all,,],dim=2),replaceby=0)[[1]]
        }else {
          coalshare_2050[reg_all,,] <- 0
        }
      }
    }else {
      coalshare_2030[reg_all,,] <- 0
      coalshare_2050[reg_all,,] <- 0
    }
  }
  # 
  # totdem_2015_c <- data[,2015,"TOTAL.TPES"]
  # totdem_2015_R <- toolAggregate(toolAggregate(totdem_2015_c,map,NULL),map,NULL)
  # totdem_2050_R <- rundata[,2050,"PE (EJ/yr)"]
  # totdem_2050_c <- toolAggregate(totdem_2050_R[-which(getRegions(totdem_2050_R)=="GLO"),,],map,NULL) * totdem_2015_c/totdem_2015_R
  # 
  if (grepl("Non",phase,ignore.case=TRUE)) {
    if (grepl("bubble",subtype)) {
      cap_2045_c <- logit_coalgen_2045_c / (loadfactor_c[,2045,]*365*24/277777.77778)
      getNames(cap_2045_c) <- recovery
      getNames(logit_coalgen_2045_c) <- recovery
      getYears(logit_coalShare_2045_c) <- "y2045"
      getYears(cap_2045_c) <- "y2045"
      getSets(logit_coalShare_2045_c) <- c("region","year","scenario")
      getSets(cap_2045_c) <- c("region","year","scenario")
      getSets(gdppc) <- c("region","year","scenario")
      out <- mbind(gdppc[,2045,],logit_coalShare_2045_c,cap_2045_c)
      getNames(out) <- c("GDPpc","Share","Capacity")
      weight <- NULL
    }else {
      out <- coalshare_2050
      getYears(out) <- NULL
      getNames(out) <- NULL
      # out[which(out>1)] <- max(out[which(out<1)])
      # out[which(out>1)] <- 0.9
      weight <- dimSums(weight[,getYears(weight)>="y2050",],dim=2)
    }
  }else {
    if (grepl("bubble",subtype)) {
      getYears(coalShare_2025_c) <- "y2025"
      getYears(cap_2025_c) <- "y2025"
      getSets(coalShare_2025_c) <- c("region","year","scenario")
      getSets(cap_2025_c) <- c("region","year","scenario")
      getSets(gdppc) <- c("region","year","scenario")
      out <- mbind(gdppc[,2025,],coalShare_2025_c,cap_2025_c)
      getNames(out) <- c("GDPpc","Share","Capacity")
      weight <- NULL
    }else {
      out <- coalshare_2030
      for (country in map$CountryCode[which(!(map$CountryCode %in% c(oecd_members,nonoecd_members)))]) {
        if (coalshare_2050[country,,] > coalshare_2030[country,,] & coalshare_2030[country,,] > 0) {
          out[country,,] <- coalshare_2050[country,,]
        }
      }
      # out[which(out>1)] <- max(out[which(out<1)])
      # out[which(out>1)] <- 0.9
      getYears(out) <- NULL
      getNames(out) <- NULL
      weight <- dimSums(weight[,getYears(weight)>="y2030",],dim=2)
    }
  }
  
  # Assign phase-out stringency based on coal demand type (for compatibility with REMIND bounds)
  if (!grepl("bubble",subtype)) {
    if (grepl("power",policy)) {
      numTe <- 3
    }else if (grepl("steel",subpolicy)) {
      getNames(out) <- "steel"
      numTe <- 1
    }else if (grepl("solids",subpolicy)) {
      getNames(out) <- "solids"
      numTe <- 10
    }else if (grepl("demand",policy)) {
      getNames(out) <- "demand"
      numTe <- 15
    }
    out[which(out>0 & out < 1e-6)] <- 1e-6
    
    # out[which(out>0 & out < (numTe * 1e-6))] <- numTe * 1e-6
    # out[which(out==0.9)] <- 0
  }
  
  setConfig(forcecache = F)
  
  # if (grepl("Non",phase,ignore.case=TRUE)) {
  #   # if (grepl("power",policy))  weight <- totalgen_2050_c
  #   # else if (grepl("demand",policy))  weight <- totalemi_2050_c
  # }else {
  #   # if (grepl("power",policy))  weight <- totalgen_2030_c
  #   # else if (grepl("demand",policy))  weight <- totalemi_2030_c
  # }
  
  
  return(list(x=out,weight=weight,unit="share",
              description="Maximum share of coal in regional electricity generation from 2030 and 2050 due to PPCA national coal phase-outs"))
}
