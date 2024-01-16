finEx_reg_mag_nm_gcpt21 <- mselect(reg_mag_nm_gcpt21,Country=getItems(reg_mag_nm_gcpt21,1)[which(reg_mag_nm_gcpt21[,,"PreCon.PubG20.GW"]>0)])
finEx_glo_mag_nm_gcpt21 <- setItems(dimSums(finEx_reg_mag_nm_gcpt21,dim=1),dim=1,value="GLO")
  
q_finEx_by_host_reg <- as.quitte(mbind(finEx_reg_mag_nm_gcpt21,finEx_glo_mag_nm_gcpt21)) 

q_finEx_cap_by_host_reg_precon <- q_finEx_by_host_reg %>%
  filter(!(variable %in% c("Oper","oper","Con","con")) & data1=="GW")

# stackOrdFill <- c("oper"="gray20",
#                   "Oper"="gray29",
#                   "con"="tan4",
#                   "Con"="tan3",
#                   "preCon"="darkgreen",
#                   "PreCon"="mediumseagreen")
# stackOrd <- c("oper","Oper","con","Con","preCon","PreCon")
# stackOrder <- c("Operating (Other)","Operating (G20)","Under Construction (Other)","Under Construction (G20)","Pre-Construction (Other)","Pre-Construction (G20)")

stackOrd <- c("PreCon","Perm","Pre","Ann","She")
stackOrder <- c("All","Permitted","Pre-Permit","Announced","Shelved")
stackOrdAlpha <- c("PubG20" = 0.4,
                   "NonPubG20" = 1)
# stackOrdFill <- c("Ann" = "",
#                   "Pre",
#                   "Perm",
#                   "She")

# fillScale <- scale_fill_manual(values = stackOrdFill,
#                                labels=stackOrder,
#                                breaks = levels(stackOrdFill),
#                                name="Status")

regOrd <- c("GLO", "OAS", "NEU", "SSA", "MEA", "LAM", "CAZ", "REF") 
regOrder <- c("All FinEx Hosts","S & E Asia ex. CN, IN, JP","Non-EU Europe","Sub-Saharan Africa","Middle East & N Africa",
              "Latin America","Canada, Australia & NZ","Former Soviet Union")

q_finEx_cap_by_host_reg_precon$data <- factor(q_finEx_cap_by_host_reg_precon$data,
                                              levels=c("PubG20","NonPubG20","Pvt","Tot"),
                                              ordered = TRUE)

q_finEx_cap_by_host_reg_precon %>% 
  filter(data %in% c("PubG20","NonPubG20") & data1=="GW" & region %in% c("GLO")) %>%
  group_by(region,variable) %>%
  arrange(data) %>%
  mutate(cum_val = cumsum(value)) %>%
  ggplot(aes(x=data,
             y=value,
             fill=factor(variable,levels=rev(stackOrd),labels=rev(stackOrder)),
             alpha=data)) +
  geom_col(
    data= . %>% filter(variable != "PreCon"),
           position="stack",
           # alpha=1
           ) +
  # geom_col(data= . %>% filter(data=="PubG20"),
  #          position=position_dodge(width = 0.9),
  #          alpha=0.6) +
  facet_wrap(. ~ factor(region,levels=regOrd,labels = regOrder), scales="free") +
  # suppressWarnings(geom_tile(aes(y=NA_integer_, alpha = factor(data)))) +
  labs(x="",y="Capacity (GW)",
       title="FinEx Impact on Regional Coal Pipelines by Pre-Construction Planning Phase (2020)") +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2",
                    labels = rev(stackOrder),
                    name = "Planning Phase",
                    guide = guide_legend(override.aes = list(size=5),order=2)) +
  scale_alpha_manual(values=stackOrdAlpha, 
                     labels = c("Halted", "Active"), 
                     name = "Finance Status\n(Transparency)") +
  scale_x_discrete(labels = c("G20", "Other"), name = "Financier") +
  theme(
    title = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    # title = element_text(size=9, face="bold"),
        axis.title = element_text(size = 2 * 9, face="plain"),
    axis.text.x = element_text(size = 2 * 9),
    # axis.text.x = element_blank(),
        axis.text.y = element_text(size = 2 * 9),
        strip.text = element_text(size = 2 * 10),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(size = 2 * 9),
        legend.title = element_text(size = 2 * 9, face="bold"),
        # legend.position = c(0.87,0.2),
    legend.position = "right",
    legend.direction = "vertical",
        legend.margin = margin(0,0,0,0)#,
        # strip.background = element_rect(fill = regFill)
        ) +
  guides(alpha = guide_legend(title.position = "top",
                              direction = "horizontal",
                              # position = "bottom",
                              title.hjust = 0))
    # fill = guide_legend(title.position = "top",
  #                            title.hjust = 0,
  #                            order=1,
  #                            keywidth = 0.5,
  #                            # keyheight = 0.5
  #                            ),
         
  
  # scale_fill_manual(values = rev(stackOrdFill),labels=rev(stackOrder),name="Status (Finance Source)")

ggsave(filename = "~/Coal_Finance_Exit/Plots/GW_FinEx/GLO_preCon_stackbar_facet_regi_cap_status_2020.png",width = 6.5, height = 5, units = "in", device = "png",dpi="retina")  
ggsave(filename = "~/Coal_Finance_Exit/Plots/GW_FinEx/GLO_preCon_stackbar_facet_regi_cap_status_2020.pdf",width = 6.5, height = 5, units = "in", device = "pdf",dpi="retina")  




for (reg in sort(host_regi)) {
  ggplot(data = q_finEx_cap_by_host_reg_precon %>% 
         filter(data %in% c("PubG20","NonPubG20") & data1=="GW" & region == reg) %>%
         group_by(region,variable) %>%
         arrange(data) %>%
         mutate(cum_val = cumsum(value)),
       mapping = aes(x=data,
             y=value,
             fill=factor(variable,levels=rev(stackOrd),labels=rev(stackOrder)),
             alpha=data)) +
  geom_col(
    data= . %>% filter(variable != "PreCon"),
    position="stack",
    # alpha=1
  ) +
  # geom_col(data= . %>% filter(data=="PubG20"),
  #          position=position_dodge(width = 0.9),
  #          alpha=0.6) +
  facet_wrap(. ~ factor(region,levels=regOrd,labels = regOrder), scales="free_y") +
  # suppressWarnings(geom_tile(aes(y=NA_integer_, alpha = factor(data)))) +
  labs(x="Finance Status",y="Capacity (GW)",
       title="FinEx Impact on Regional Coal Pipelines (2020)") +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2",
                    labels = rev(stackOrder),
                    name = "Planning Phase",
                    guide = "none"
                    # guide = guide_legend(override.aes = list(size=5),order=2)
                    ) +
  scale_x_discrete(labels = c("G20", "Other")) +
  scale_alpha_manual(values=stackOrdAlpha, labels = c("G20 FinEx\n(Halted)", "Other\n(Active)"), name = "Finance Status", guide = "none") +
  theme(title = element_blank(),
    # title = element_text(size=10, face="bold"),
        # axis.title = element_text(size = 2 * 9, face="plain"),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 2 * 9),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(size = 2 * 0.75 * 12),
        strip.text = element_text(size = 2 * 10), 
    strip.background = element_rect(fill = regFill[reg]),
        legend.text = element_text(size = 2 * 9),
        legend.title = element_text(size = 2 * 9, face="bold"),
        legend.position = "bottom",
        legend.direction = "horizontal"
    #,
        # legend.margin = margin(0,60,0,0)#,
        # strip.background = element_rect(fill = regFill)
  ) 
  # +
  # guides(fill = guide_legend(title.position = "top", 
  #                            title.hjust = 0,
  #                            order=1,
  #                            keywidth = 0.5,
  #                            keyheight = 0.5),
  #        alpha = guide_legend(title.position = "top", 
  #                             title.hjust = 0,
  #                             keywidth = 0.5,
  #                             keyheight = 0.5)) 
  # 
  ggsave(filename = paste0("~/Coal_Finance_Exit/Plots/GW_FinEx/",reg,"_preCon_stackbar_facet_regi_cap_status_2020.png"),width = 4, height = 4, units = "in", dpi="retina")  
  ggsave(filename = paste0("~/Coal_Finance_Exit/Plots/GW_FinEx/",reg,"_preCon_stackbar_facet_regi_cap_status_2020.pdf"),width = 4, height = 4, units = "in", dpi="retina")  
  
}

# g <- ggplot_gtable(ggplot_build(p))
# striprt <- which( grepl('strip-r', g$layout$name) | grepl('strip-t', g$layout$name) )
# fills <- c(rev(brewer.pal(n = 12, name = "Set3"))[which(sort(unique(map$RegionCode)) %in% host_regi)], "white")
# k <- 1
# for (i in striprt) {
#   j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
#   if(length(j) == 0) next
#   g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
#   k <- k+1
# }
# grid::grid.draw(g)
# 
# ggsave(filename = "~/Coal_Finance_Exit/Plots/GW_FinEx/preCon_stackbar_facet_regi_cap_status_2020_OAS_World.jpg", height = 5.6, width = 5.3, units = "in", device = "png",dpi="retina")  
