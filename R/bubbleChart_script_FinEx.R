
bubblePPCA <- function(subtype,phase,size,policy,run,recovery,file=NULL,inData=F) {
  setConfig(forcecache = T)
  
  require(ggplot2)
  require(ggnewscale)
  require(ggrepel)
  require(stringr)
  require(dplyr)
  require(scales)
  require(readxl)
  require(countrycode)
  
  setConfig(mainfolder = "C:/Users/stephenb/Downloads/madrat_main/")
  #Relevant regional mappings and country classifications
  #Current members of PPCA
  PPCAmap <- toolGetMapping("regionmappingPPCA.csv",type = "regional")
  ppca_map <- as.magpie(PPCAmap[,-1])
  #Current OECD members
  OECDmap <- toolGetMapping("regionmappingOECD.csv",type = "regional")
  oecd_map <- as.magpie(OECDmap[,-1])
  #REMIND 12 region mapping
  map <- toolGetMapping("regionmappingH12.csv",type="regional")
  
  ppca <- PPCAmap$CountryCode[which(PPCAmap$RegionCode=="PPCA")]
  oecd <- OECDmap$CountryCode[which(OECDmap$RegionCode=="OECD")]
  EU27 <- map$CountryCode[which(map$RegionCode=="EUR")]
  nonoecd <- OECDmap$CountryCode[which(OECDmap$RegionCode=="NON")]
  
  new_mem_july21 <- c("DEU","FJI","GRC",'HRV','HUN','ISR','MEX','MKD','MNE','PRT','VUT')
  
  newest_mem_cop26 <- c('UKR','CHL','SGP','MUS','AZE','SVN','EST')
  
  if (recovery=="Neutral")  recovery <- "BAU"
  
  ##Probability levels and line types
  if (!is.null(file)) {
    if (grepl("50CI",file))  p <- c(.25, 0.5, 0.75)
    else if (grepl("33CI",file))  p <- c(.33, 0.5, 0.67)
    else if (grepl("60CI",file))  p <- c(0.2, 0.5, 0.8)
    else if (grepl("70CI",file))  p <- c(0.15, 0.5, 0.85)
    else if (grepl("90CI",file))  p <- c(0.05, 0.5, 0.95)
    else if (uncertainty=="10step")      p <- seq(10,90,10)
    else  p <- c(0.05, 0.5, 0.95)
  }
  
  lts <- c("0.5" = 2, "0.05" = 3, "0.95" = 4)
  # p <- c(0.95, 0.75, 0.5, 0.25, 0.05)
  # lts <- c("0.25" = 0, "0.5" = 2, "0.75" = 0, "0.05" = 3, "0.95" = 3)
  
  # Read file tracking the current status of the PPCA which contains GDPpc, % coal in electricity, 
  # standing coal power capacity, among other data
  #Country - 3-letter ISO country code
  #PPCA - PPCA membershp
  #PPCA.Bin (binary) - PPCA membership (1/0)
  #GDP.PC - GDP per capita (1000 USD)
  #Coal.Share - Share of coal in electricity supply
  #GW Capacity - total installed capacity of coal-fired fleet
  histData <- read.csv(paste0(getConfig("sourcefolder"),"/PPCA/PPCA_status_SSP2_COP26.csv"),stringsAsFactors = F,sep = ",")
  # browser()
  if (inData) {
    
    ## Re-creating logit model fit from before GDP update
    logit_model <- makeglm(PPCA.Bin ~ Coal.Share + GDP.PC, data = histData, family = "binomial", -0.896, Coal.Share = -5.747, GDP.PC = 0.095)
    
    # Read in file with DFS results pre-GDP update
    inFile <- paste("~/PPCA Paper/Bubble Chart/DFS_data_Zotero/DFS_data",ifelse(phase=="OECD", "2025_OECD", "2045_Non-OECD"), size, recovery, sep = "_")
    testData <- read.csv(paste0(inFile,".csv"))
    if (grepl(";",testData)) {
      testData <- read.csv(paste0(inFile,"_v2.csv"),sep=";")
    }
    
    ## Remove OECD designation from non-OECD EU members (revisions round 2 bugfix)
    testData <- testData %>% 
      mutate(OECD=ifelse(country %in% setdiff(EU27,oecd), "Non-OECD", OECD))
    
  }else {
  
    #Logit regression of existing PPCA membership 
    logit_model <- glm(data = histData, PPCA.Bin ~  Coal.Share + GDP.PC , family = "binomial")
    
    ## Run COALogit subfunction to retrieve national coal shares, capacities, GDPpc
    testData <- mrremind:::calcPPCA(phase=phase,policy=policy,subpolicy="none",recovery=recovery,size=size,run=run,subtype="bubble") 

    testData <- data.frame(country=getRegions(testData$x),share=as.numeric(testData$x[,,2]),gdp=as.numeric(testData$x[,,1]),
                     Capacity=as.numeric(testData$x[,,3]),OECD=as.character(oecd_map),PPCA=PPCAmap$RegionCode,Region=map$RegionCode)
    
    testData <- testData %>% 
      ## Run logit model on test data to return probabilities
      mutate(Probability=predict(logit_model,newdata = data.frame(Coal.Share=share,GDP.PC=gdp),type = "response")) %>%
      mutate(Face = ifelse(PPCA=="PPCA", "bold.italic", "plain"))
  }
  
  #Logit regression of existing PPCA membership 
  logit_model <- glm(data = histData, PPCA.Bin ~  Coal.Share + GDP.PC , family = "binomial")

  ## Sanity check test data 
  testData$share[which(testData$share>1)] <- max(testData$share[which(testData$share<1)])
  
  if (phase=="OECD" & recovery=="BAU") 
    testData <- testData %>% mutate(nudge = ifelse(country=="MAR",-0.02,ifelse(country=="CHN",0.065,0.025+Capacity/1.5e4)))
  else if (phase=="Non-OECD")
    testData <- testData %>% mutate(nudge = ifelse(country=="CHN",0.065,0.025+Capacity/1.5e4))
  
  # Derive intercept and slope of probability thresholds 
  ic <-  (log(1/p -1) -  logit_model$coefficients[1])/ logit_model$coefficients[3]
  slope <- -(logit_model$coefficients[2]/logit_model$coefficients[3])
  # All countries lying above this line are members of the given coalition scenario
  ln <- data.frame(Ic = ic, Slope = slope, Prob = as.character(p))
  
  
  plotData <- filter(testData,share>=1e-2)
  
  if (grepl("non",phase,ignore.case=T)) {
    plotData <- filter(plotData,!(PPCA=="PPCA" & OECD=="OECD"))
  }  
  
  ################################################################################################
  if (subtype == "data_2020") { 
    # browser()
    testData <- testData %>% mutate(country_name=countrycode(country,"iso3c","country.name"))
    write.csv(testData,paste0("~/PPCA Paper/Bubble Chart/",paste("DFS_data",ifelse(phase=="OECD","2025","2045"),phase,size,recovery,sep="_"),".csv"))
    # phase,policy,size,recovery,sep="_"),".csv"))
    ################################################################################################
  }else if (subtype == "data_full") { 
    # browser()
    testData <- testData %>% mutate(country_name=countrycode(country,"iso3c","country.name"))
    write.csv(testData,paste0("~/PPCA Paper/Bubble Chart/",paste("DFS_data",ifelse(phase=="OECD","2025","2045"),phase,size,recovery,sep="_"),".csv"))
    # phase,policy,size,recovery,sep="_"),".csv"))
    ################################################################################################
  }else if (subtype == "data") { 
    # browser()
    plotData <- plotData %>% mutate(country_name=countrycode(country,"iso3c","country.name"))
    write.csv(plotData,paste0("~/PPCA Paper/Bubble Chart/",paste("DFS_data",ifelse(phase=="OECD","2025","2045"),phase,size,recovery,sep="_"),".csv"))
                                                           # phase,policy,size,recovery,sep="_"),".csv"))
  ################################################################################################
  }else if (subtype == "plot") {
    browser()
    if (recovery=="BAU")  recovery <- "Neutral"
    plotData <- plotData %>% 
      mutate(country=ifelse(country %in% new_mem_july21,
                            paste0(country,"*"),
                            ifelse(country %in% newest_mem_cop26,
                                   paste0(country,"^"),
                                   country)))
    
    clrs <- c("OECD"="#E41A1C", "Non-OECD"="#789FC6","PPCA" = "goldenrod3", "Free" = "#000000")
    # colnames(plotData)[which(grepl("cap_",colnames(plotData)))] <- paste0("GW Capacity (",gsub("cap_","",colnames(plotData)[which(grepl("cap_",colnames(plotData)))]),")")
    
    year <- ifelse(phase=="OECD","2025","2045")
    
    size <- ifelse(size=="1p", "95p",
                   ifelse(size=="2p", "50p",
                          ifelse(size=="3p", "5p", "")))
    

    ggplot(plotData) + 
      #Probability lines
      geom_abline(intercept = ln$Ic[which(ln$Prob==0.5)],
                  slope = ln$Slope,
                  color="red",alpha=0.16) +
      geom_abline(intercept = ln$Ic,
                  slope = ln$Slope,
                  color="grey50",alpha=0.06) +
      geom_point(data = plotData,aes(x = share, y = gdp, color = OECD, size = Capacity),shape=16) +
      scale_color_manual(values = clrs, name="OECD Status (2021)",breaks=c("OECD","Non-OECD"),labels=c("OECD","Non-OECD"),guide=guide_legend(override.aes = list(size=5), order=3)) +
      new_scale_color() +
      
      #Labels
      geom_text_repel(data = plotData, 
                    aes(x = share, y = gdp, label = country, fontface = Face, color = PPCA), 
                    size = 5.2, segment.size = 0.3, segment.alpha = 0.5, force = 5,nudge_x = plotData$nudge,max.overlaps = 80) +
      
      scale_color_manual(values = clrs, name="PPCA Status (July 2021)",breaks=c("PPCA","Free"),labels=c("PPCA","Freerider"),guide=guide_legend(override.aes = list(size=7,label="A"),order=4)) +
      guides(size=guide_legend(order=1),color=guide_legend(order=2,override.aes = list(size=2))) +
      
      #Probability lines
      new_scale_color() +
      geom_abline(data = ln, aes(intercept = Ic, slope = Slope, color = Prob),size = 0.4,alpha=0.4) +
      
      ##Special treatment of countries 'overshadowed' by bigger ones
      geom_point(data = filter(plotData, ifelse(phase=="OECD" & recovery=="Neutral", country %in% c("VNM"),
                               country %in% "")),
                 aes(x = share, y = gdp, size = Capacity), shape = 1, color = "black", stroke = 0.2) +

      ##Axis labels (and limits, if necessary)
      scale_x_continuous("% of coal in electricity supply", labels = percent, limits = c(0, 1)) +
      scale_y_continuous("GDP p.c. ($1000)",limits = c(0,80)) + 
      scale_color_manual(values=c("gold","#CC3333","blue"), 
                         name = "Coalition Scenario", 
                         labels=c(paste0("\u2265 ",p[3]*100,"% likely"),paste0("\u2265 ",p[2]*100,"% likely"),paste0("\u2265 ",p[1]*100,"% likely")),
                         guide=guide_legend(override.aes = list(size=5),order=2)) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            text = element_text(size=3.5*8.5),
            plot.title = element_text(face="bold",size=3.5*10,hjust=0.5),
            legend.text = element_text(size=3.5*6),
            legend.title = element_text(size=3.5*7)) +
      scale_size_continuous(range = 2.9*c(0.15, 13), breaks = c(10, 50, 250, 900),name=paste(ifelse(grepl("non",phase,ignore.case = T),"2045","2025"),"Capacity (GW)"),guide=guide_legend(order=1)) +
      labs(title = ifelse(grepl("REdir",run,ignore.case = T),
                          ifelse(grepl("non",phase,ignore.case = T),
                                 paste0("2045 PPCA Feasibility Space (G20", ifelse(grepl("oilgas",run)," + G7 "," "), "FinEx REdirect", ifelse(grepl("noMob",run),")"," + Mobilization)")),
                                 "2025 PPCA Feasibility Space (G20 Pledged FinEx)"),
                          ifelse(grepl("non",phase,ignore.case = T),
                                 paste("PPCA Feasibility Space",paste0("(",year),recovery,size,paste0(toupper(substr(policy,1,1)),substr(policy,2,nchar(policy))),"Exit)"),
                                 paste("PPCA Feasibility Space",paste0("(",year),recovery,paste0(toupper(substr(policy,1,1)),substr(policy,2,nchar(policy))),"Exit)"))))
    
    if (!is.null(file))  ggsave(file,width=6.3*3,height=4.63*3,units="in",dpi="retina")
    else  ggsave(paste0("~/PPCA Paper/Bubble Chart/",paste0("DFS_clrblind_rev_rd2_newGDP_",run),".png"),device="png",width=6.3*3,height=4.63*3,units="in",dpi="retina")
    
    
################################################################################################
################################################################################################
  }else if (subtype=="calc") {
    
    # Calculations
    plotData <- testData
    # Determine PPCA coalition scenarios
    plotData <- plotData %>% mutate(coalition=ifelse(share<(gdp-ln[3,"Ic"])/ln[3,"Slope"] & 
                                           gdp>(ln[3,"Ic"]+ln[3,"Slope"]*share) & OECD==phase,"3p","free"))
    plotData <- plotData %>% mutate(coalition=ifelse(share<(gdp-ln[2,"Ic"])/ln[2,"Slope"] & 
                                           gdp>(ln[2,"Ic"]+ln[2,"Slope"]*share) & OECD==phase,"2p",coalition))
    plotData <- plotData %>% mutate(coalition=ifelse((share<(gdp-ln[1,"Ic"])/ln[1,"Slope"] & 
                                            gdp>(ln[1,"Ic"]+ln[1,"Slope"]*share) | PPCA=="PPCA") & OECD==phase,"1p",coalition))
    
    # Add population and GDP ppp
    setConfig(forcecache = T)
    plotData <- plotData %>% mutate(GDPppp=mrdrivers:::calcGDPFuture()$x[,2045,"gdp_SSP2"])
    plotData <- plotData %>% mutate(population=calcOutput("Population",aggregate=F)[,2045,"pop_SSP2"])
    # Add load factor
    plotData <- plotData %>% mutate(loadfactor=calcOutput("CapacityFactor",aggregate=F)[,2045,"pc"])
    # setConfig(forcecache = F)
    # Add EU column
    plotData <- plotData %>% mutate(EU=ifelse(country %in% EU27,1,0))
    
    #Global capacity 
    plotData %>% summarise(global_capacity=sum(Capacity))
    
    #Sum population
    # 1p
    # View(plotData %>% filter(coalition=="1p" & (OECD=="OECD" | EU==1)))
    # View(plotData %>% filter(coalition=="1p" & OECD=="Non-OECD" & EU==0))
    
    plotData %>% summarise(global_pop=sum(population))
    plotData %>% filter(coalition=="1p") %>% summarise(PPCA_pop=sum(population))
    plotData %>% filter(coalition=="1p") %>% summarise(PPCA_pop=sum(population)) / plotData %>% summarise(global_pop=sum(population))
    
    # 2p
    # View(plotData %>% filter(coalition=="2p" & (OECD=="OECD" | EU==1)))
    # View(plotData %>% filter(coalition=="2p" & OECD=="Non-OECD" & EU==0))
    
    plotData %>% summarise(global_pop=sum(population))
    plotData %>% filter(coalition=="2p") %>% summarise(PPCA_pop=sum(population))
    plotData %>% filter(coalition=="2p") %>% summarise(PPCA_pop=sum(population)) / plotData %>% summarise(global_pop=sum(population))
    
    # Sum GDP
    plotData %>% summarise(global_GDPppp=sum(GDPppp))
    
    plotData %>% filter(coalition=="2p") %>% summarise(PPCA_GDPppp=sum(GDPppp))
    
    plotData %>% filter(coalition=="2p") %>% summarise(PPCA_GDPppp=sum(GDPppp)) / plotData %>% summarise(global_GDPppp=sum(GDPppp))
    
    # PPCA Non-OECD capacity
    plotData %>% filter(coalition=="2p" & OECD=="Non-OECD") %>% summarise(PPCA_capacity=sum(Capacity))
    #PPCA cap
    plotData %>% filter(coalition=="2p") %>% summarise(PPCA_capacity=sum(Capacity))
    
    # Coal gen PPCA
    plotData %>% filter(coalition=="2p") %>% summarise(PPCA_generation=sum(Capacity)) *0.5*365*24/277777.77778
    plotData %>% summarise(global_generation=sum(Capacity)) *0.5*365*24/277777.77778
    
    
    # 3p
    # View(plotData %>% filter(coalition=="3p" & (OECD=="OECD" | EU==1)))
    # View(plotData %>% filter(coalition=="3p" & OECD=="Non-OECD" & EU==0))
    
    plotData %>% summarise(global_pop=sum(population))
    plotData %>% filter(coalition=="3p") %>% summarise(PPCA_pop=sum(population))
    plotData %>% filter(coalition=="3p") %>% summarise(PPCA_pop=sum(population)) / plotData %>% summarise(global_pop=sum(population))
    
    # Sum GDP
    plotData %>% summarise(global_GDPppp=sum(GDPppp))
    
    plotData %>% filter(coalition=="3p") %>% summarise(PPCA_GDPppp=sum(GDPppp))
    
    plotData %>% filter(coalition=="3p") %>% summarise(PPCA_GDPppp=sum(GDPppp)) / plotData %>% summarise(global_GDPppp=sum(GDPppp))
    
    # PPCA Non-OECD capacity
    plotData %>% filter(coalition=="3p" & OECD=="Non-OECD") %>% summarise(PPCA_capacity=sum(Capacity))
    #PPCA cap
    plotData %>% filter(coalition=="3p") %>% summarise(PPCA_capacity=sum(Capacity))
    
    # Coal gen PPCA
    plotData %>% filter(coalition=="3p") %>% summarise(PPCA_generation=sum(Capacity)) *0.5*365*24/277777.77778
    plotData %>% summarise(global_generation=sum(Capacity)) *0.5*365*24/277777.77778
  }
  
  return(plotData)
  
}
  

