require(readxl)
require(mrremind)
require(quitte)
require(dplyr)
require(stringr)
require(ggplot2)
library(sf)
library(s2)
library(grid)
library(gridExtra)
library(rnaturalearth)
library(hrbrthemes)
library(tidyverse)
library(rgeocodio)
library(albersusa)
library(ggthemes)
library(ggrepel)

world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(name != "Antarctica") %>%
  left_join(map %>% select(-X),
            by = c("iso_a3" = "CountryCode"))

sf_use_s2(FALSE)

preCon_finFlows <- country_pair_finEx %>%
  filter(!is.na(PreCon.PubG20.bbUSD)) %>%
  left_join(rename_with(world,~ paste0("dest_",.x)),by=c("Country"="dest_iso_a3")) %>%
  left_join(rename_with(world,~ paste0("origin_",.x)),by=c("Financing.Country"="origin_iso_a3")) %>%
  mutate(to = suppressWarnings(st_centroid(dest_geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>%
  mutate(from = suppressWarnings(st_centroid(origin_geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>%
  select(Financing.Country,Country,PreCon.PubG20.bbUSD,from,to)

tib_preCon_finFlows <-
  st_coordinates(preCon_finFlows$from) %>%
  as_tibble() %>%
  set_names(c("from_x","from_y")) %>%
  bind_cols(
    st_coordinates(preCon_finFlows$to) %>%
      as_tibble() %>%
      set_names(c("to_x","to_y")),
    select(preCon_finFlows,Financing.Country,Country,PreCon.PubG20.bbUSD)
  ) %>%
  mutate(reg_creditor = ifelse(Financing.Country == "CHN", "CHA",
                               ifelse(Financing.Country == "KOR", "OAS",
                                      ifelse(Financing.Country == "ZAF", "SSA",
                                             Financing.Country)))) 
           # filter(map, CountryCode == Financing.Country)$RegionCode)


################################################################################################################
ggplot() +
  geom_sf(data = st_crop(world,xmin=-90,xmax=170,ymin=-60,ymax=170), 
          aes(fill = RegionCode),
          linewidth = 0.125, 
          # fill = "#3B454A", 
          color = "grey50") +
  coord_sf(crs = "+proj=eqearth +wktext") +
  # theme_ft_rc(grid="") +
  theme_minimal() +
  geom_curve(data=tib_preCon_finFlows %>% filter(PreCon.PubG20.bbUSD>=.001) %>% 
               filter(!grepl("IND|JPN|ZAF", Financing.Country)),
             mapping=aes(x = from_x, 
                         y = from_y, xend = to_x, 
                         yend = to_y,
                         alpha = PreCon.PubG20.bbUSD,
                         # color = reg_creditor
                         # size=PreCon.PubG20.bbUSD,
                         # color=PreCon.PubG20.bbUSD
                         ), 
    curvature = 0.3, 
    size=1.2,
    # alpha = 0.5,
    color = "darkgreen",
    arrow = arrow(angle = 30,
                  length = unit(4, "pt"),
                  type = "closed")
  ) +
  geom_curve(data=tib_preCon_finFlows %>% filter(PreCon.PubG20.bbUSD>=.001) %>% 
               filter(grepl("IND|JPN|ZAF", Financing.Country)),
             mapping=aes(x = from_x, 
                         y = from_y, xend = to_x, 
                         yend = to_y,
                         alpha = PreCon.PubG20.bbUSD,
                         # color = reg_creditor
                         # size=PreCon.PubG20.bbUSD,
                         # color=PreCon.PubG20.bbUSD
             ), 
             curvature = -0.3, 
             size=1.2,
             # alpha = 0.5,
             color = "darkgreen",
             arrow = arrow(angle = 30,
                           length = unit(4, "pt"),
                           type = "closed")
  ) +
  scale_size_continuous(
    name = "Billion $2019\nSupplied or\nDemanded",
    # trans = "log10",
    range = c(2.5,10),
    breaks = c(0.1,1,10,100), 
    labels = c(0.1,1,10,100), 
    limits = c(.001,130),
  ) +
  scale_alpha_continuous(range = c(0.2, 1),
                         breaks = c(0.05, 5, 15, 30),
                         # trans = "log10",
                         name = "",
                         labels = c(0.05, 5, 15, 30),
                         limits = c(min(tib_preCon_finFlows$PreCon.PubG20.bbUSD),
                                    30)
                                    # max(tib_preCon_finFlows$PreCon.PubG20.bbUSD))
                         ) +
  geom_text_repel(data=tib_preCon_finFlows %>% 
                    group_by(Financing.Country) %>% 
                    filter(PreCon.PubG20.bbUSD>=.001) %>% 
                    mutate(total=sum(PreCon.PubG20.bbUSD)) %>% 
                    distinct(Financing.Country,.keep_all=T),
                  mapping = aes(x=from_x,y=from_y, label=Financing.Country, size = total),
                  alpha=0.9,
                  fontface="bold",
                  # size=3.7,
                  position=position_nudge_repel(450000,450000),
                  direction="both",
                  colour="grey40",
                  min.segment.length = 0.1,
                  ) +
  geom_text_repel(data=tib_preCon_finFlows %>% 
                    group_by(Country) %>% 
                    filter(PreCon.PubG20.bbUSD>=.001) %>% 
                    mutate(total=sum(PreCon.PubG20.bbUSD)) %>% 
                    distinct(Country,.keep_all=T),
             mapping = aes(x=to_x,y=to_y, label=Country, size = total),
             # alpha=0.9,
             # size=3.7,
             fontface="bold",
             color="black",
             position=position_nudge_repel(-300000,-300000),
             min.segment.length = 0.1,
             max.overlaps = 30,
             # nudge_y = -1,
             direction = "both") +
  # scale_color_manual(values = regFill, guide = "none") +
  scale_fill_manual(values = regFill, guide = "none") +
  guides(
    alpha = guide_legend(order = 2, reverse = TRUE, title = NULL, title.position = "bottom", title.theme = element_blank()),
    size = guide_legend(order = 1, reverse = TRUE, override.aes = list(label = "A", fontface="bold"))
  ) +
  # scale_alpha_continuous(guide=F) +
  labs(
    x = NULL, y = NULL
    # title = "Bilateral G20 Public Coal Finance Volumes Cancelled by FinEx",
  ) +
  theme(legend.key.size = unit(20,"pt"),
        legend.position = c(0.95, 0.75),
        legend.title.align = 0,
        legend.text.align = 1,
        legend.box.spacing = unit(0,'mm'),
        # legend.box.just = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 20),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "aliceblue"))

ggsave(filename = "~/Coal_Finance_Exit/Plots/finFlow_map/Coal2020_G20_finEx_map_regiColor_labelsize_colorblind.png",device = "png",dpi="retina",height=8,width=12)  
    
ggsave(filename = "~/Coal_Finance_Exit/Plots/finFlow_map/Coal2020_G20_finEx_map_regiColor_labelsize_colorblind.pdf",dpi="retina",height=8,width=12)  


# country_pair_finEx %>%
#   filter(!is.na(PreOp.PubG20.bbUSD)) %>%
#   left_join(rename_with(world,~ paste0("dest_",.x)),by=c("Country"="dest_iso_a3")) %>%
#   





##### UNDER CONSTRUCTION ####

con_finFlows <- country_pair_finEx %>%
  mutate(PreOp.PubG20.bbUSD=ifelse(is.na(PreOp.PubG20.bbUSD),0,PreOp.PubG20.bbUSD)) %>%
  mutate(PreCon.PubG20.bbUSD=ifelse(is.na(PreCon.PubG20.bbUSD),0,PreCon.PubG20.bbUSD)) %>%
  mutate(Con.PubG20.bbUSD=PreOp.PubG20.bbUSD - PreCon.PubG20.bbUSD) %>%
  left_join(rename_with(world,~ paste0("dest_",.x)),by=c("Country"="dest_iso_a3")) %>%
  left_join(rename_with(world,~ paste0("origin_",.x)),by=c("Financing.Country"="origin_iso_a3")) %>%
  mutate(to = suppressWarnings(st_centroid(dest_geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>%
  mutate(from = suppressWarnings(st_centroid(origin_geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>%
  select(Financing.Country,Country,Con.PubG20.bbUSD,from,to)

st_coordinates(con_finFlows$from) %>%
  as_tibble() %>%
  set_names(c("from_x","from_y")) %>%
  bind_cols(
    st_coordinates(con_finFlows$to) %>%
      as_tibble() %>%
      set_names(c("to_x","to_y")),
    select(con_finFlows,Financing.Country,Country,Con.PubG20.bbUSD)
  ) -> tib_con_finFlows

ggplot() +
  geom_sf(data = st_crop(world,xmin=-70,xmax=180,ymin=-50,ymax=180), size = 0.125, fill = "#3B454A", color = "Black") +
  geom_curve(data=tib_con_finFlows %>% filter(Con.PubG20.bbUSD>=.001),mapping=aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color=Con.PubG20.bbUSD, size=0.25), 
             curvature = 0.2, arrow = arrow(length = unit(10, "pt"), type = "closed")
  ) +
  coord_sf(crs = "+proj=eqearth +wktext") +
  scale_color_distiller(
    palette = "RdYlBu", trans="log10",name = "Billion USD", label = scales::comma,
    breaks = seq(.001,max(tib_con_finFlows$Con.PubG20.bbUSD),length.out = 4), limits = c(.001,max(tib_con_finFlows$Con.PubG20.bbUSD))
  ) +
  geom_point(data=tib_con_finFlows %>% group_by(Financing.Country) %>% mutate(total=sum(Con.PubG20.bbUSD)) %>% filter(Con.PubG20.bbUSD>=.001) %>% distinct(Financing.Country,.keep_all=T),
             mapping = aes(x=from_x,y=from_y, size=total, color=max(tib_con_finFlows$Con.PubG20.bbUSD)),alpha=0.8) +
  geom_text_repel(data=tib_con_finFlows %>% filter(Con.PubG20.bbUSD>=.001) %>% distinct(Financing.Country,.keep_all=T),
                  mapping = aes(x=from_x,y=from_y, label=Financing.Country,fontface="bold"),
                  alpha=0.8,size=3,position=position_nudge_repel(500000,500000),
                  direction="both",colour="orangered"
  ) +
  geom_point(data=tib_con_finFlows %>% group_by(Country) %>% mutate(total=sum(Con.PubG20.bbUSD)) %>% filter(Con.PubG20.bbUSD>=.001) %>% distinct(Country,.keep_all=T),
             mapping = aes(x=to_x,y=to_y, size=total),color="White",alpha=0.5) +
  geom_text_repel(data=tib_con_finFlows %>% filter(Con.PubG20.bbUSD>=.001) %>% distinct(Country,.keep_all=T),
                  mapping = aes(x=to_x,y=to_y, label=Country, fontface="bold"),
                  alpha=0.8,size=3,color="White",position=position_nudge_repel(-400000,-550000)) +
  scale_size_continuous(
    range = c(1,10),
    guide = FALSE
  ) +
  theme_ft_rc(grid="") +
  guides(
    color = guide_legend(reverse = TRUE)
  ) +
  # scale_alpha_continuous(guide=F) +
  labs(
    x = NULL, y = NULL,
    title = "G20 Public Finance Flows to Coal Plants Under Construction",
  ) +
  # theme_ft_rc(grid="") +
  theme(legend.key.height = unit(2.8, "lines")) +
  theme(legend.position = c(0.97, 0.8))

ggsave(filename = "~/Coal_Finance_Exit/Plots/finFlow_map/con_Coal2020_G20_finEx_map_log10.png",device = "png",dpi="retina",height=8.14,width=16)  

    



### OPERATIONAL PLANTS ###
oper_finFlows <- country_pair_finEx %>%
  filter(!is.na(Oper.PubG20.bbUSD)) %>%
  left_join(rename_with(world,~ paste0("dest_",.x)),by=c("Country"="dest_iso_a3")) %>%
  left_join(rename_with(world,~ paste0("origin_",.x)),by=c("Financing.Country"="origin_iso_a3")) %>%
  mutate(to = suppressWarnings(st_centroid(dest_geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>%
  mutate(from = suppressWarnings(st_centroid(origin_geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>%
  select(Financing.Country,Country,Oper.PubG20.bbUSD,from,to)

st_coordinates(oper_finFlows$from) %>%
  as_tibble() %>%
  set_names(c("from_x","from_y")) %>%
  bind_cols(
    st_coordinates(oper_finFlows$to) %>%
      as_tibble() %>%
      set_names(c("to_x","to_y")),
    select(oper_finFlows,Financing.Country,Country,Oper.PubG20.bbUSD)
  ) -> tib_oper_finFlows

ggplot() +
  geom_sf(data = st_crop(world,xmin=-70,xmax=180,ymin=-50,ymax=180), size = 0.125, fill = "#3B454A", color = "Black") +
  geom_curve(data=tib_oper_finFlows %>% filter(Oper.PubG20.bbUSD>=.001),mapping=aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color=Oper.PubG20.bbUSD, size=0.25), 
             curvature = 0.2, arrow = arrow(length = unit(10, "pt"), type = "closed")
  ) +
  coord_sf(crs = "+proj=eqearth +wktext") +
  scale_color_distiller(
    palette = "RdYlBu", trans="log10",name = "Billion USD", label = scales::comma,
    breaks = seq(.001,max(tib_oper_finFlows$Oper.PubG20.bbUSD),length.out = 4), limits = c(.001,max(tib_oper_finFlows$Oper.PubG20.bbUSD))
  ) +
  # geom_point(data=tib_oper_finFlows %>% group_by(Financing.Country) %>% mutate(total=sum(Oper.PubG20.bbUSD)) %>% filter(Oper.PubG20.bbUSD>=.001) %>% distinct(Financing.Country,.keep_all=T),
  #            mapping = aes(x=from_x,y=from_y, size=total, color=max(tib_oper_finFlows$Oper.PubG20.bbUSD)),alpha=0.8) +
  geom_text_repel(data=tib_oper_finFlows %>% filter(Oper.PubG20.bbUSD>=.001) %>% distinct(Financing.Country,.keep_all=T),
                  mapping = aes(x=from_x,y=from_y, label=Financing.Country, size = total),
                  alpha=0.8,
                  size=3,
                  position=position_nudge_repel(500000,500000),
                  direction="both",colour="orangered"
  ) +
  geom_point(data=tib_oper_finFlows %>% group_by(Country) %>% mutate(total=sum(Oper.PubG20.bbUSD)) %>% filter(Oper.PubG20.bbUSD>=.001) %>% distinct(Country,.keep_all=T),
             mapping = aes(x=to_x,y=to_y, size=total),alpha=0.5, color="white") +
  geom_text_repel(data=tib_oper_finFlows %>% filter(Oper.PubG20.bbUSD>=.001) %>% distinct(Country,.keep_all=T),
                  mapping = aes(x=to_x,y=to_y, label=Country, fontface="bold"),
                  alpha=0.8,size=3,color="White",position=position_nudge_repel(-400000,-550000)) +
  scale_size_continuous(
    range = c(1,10),
    guide = FALSE
  ) +
  theme_ft_rc(grid="") +
  guides(
    color = guide_legend(reverse = TRUE)
  ) +
  # scale_alpha_continuous(guide=F) +
  labs(
    x = NULL, y = NULL,
    title = "G20 Public Overseas Finance in Operating Coal Plants",
  ) +
  # theme_ft_rc(grid="") +
  theme(legend.key.height = unit(2.8, "lines")) +
  theme(legend.position = c(0.97, 0.8))

ggsave(filename = "~/Coal_Finance_Exit/Plots/finFlow_map/oper_Coal2020_G20_finEx_map_log10.png",device = "png",dpi="retina",height=8.14,width=16)  


