#Maps of mongu district
if(!exists("ward_shape", envir = globalenv())){
  source("set_up.R")
  source("read_data.R")
  
}




#Clean data for this district (Mongu) --------------------------------------------
my_wards = ward_shape %>%
  filter(NAME == "Mongu")

names_wards <- cbind(my_wards, st_coordinates(st_centroid(my_wards))) #names for map


#get soil type of the district
my_district = district_shape_soil %>%
  filter(NAME == "Mongu") %>%
  tibble() %>%
  select(labels_soil, NAME)


data_map = my_wards %>%
  left_join(my_district, by = c("NAME"))


#crop polygons of water riskm rivers, and damns to fit this district
my_water_logged = water_logged_shape %>%
  st_crop(st_bbox(data_map))

my_river = shape_rivers %>%
  st_crop(st_bbox(data_map))

my_dam = shape_dams %>%
  st_crop(st_bbox(data_map))


my_hf = shape_hfs %>%
  filter(`Name of District` == "Mongu") %>%
  st_crop(st_bbox(data_map))


# xmin = st_bbox(data_map)[1]
# xmax =st_bbox(data_map)[3]
# ymin =st_bbox(data_map)[2]
# ymax= st_bbox(data_map)[4]



#MAP -----------------------
#define labels to map (so they do not overlao)
wards_text = c( "Lumbo", "Katongo", "Namushekende", "Lealui", "Mabumbu", "Kaande")
wards_text_nudge = c("Yeta", "Mulambwa")


#define position of artifitial layer
key_start = 22.55
key_end = 22.6



anot_pos_x = 22.605
anot_pos_y_borders = -15.4
anot_pos_y_rivers = -15.45
anot_pos_y_water_risk = -15.5


#produce the map 
ggplot(data = data_map) +
  
  #soil type ------------------------------------------------------
  geom_sf(  color = color_borders,
            aes(fill = "Largely sandy soils",)
          ) +
  #water logged --------------------------------------------------
  geom_sf(data = my_water_logged,
          fill = alpha(color_water_risk, .6),
          color = alpha(color_water_risk, .6)
          )+
  #rivers -----------------------------------------------
   geom_sf(data = my_river,
           fill = color_water,
           color = color_water,
           size = 1) +
  geom_sf(data = my_dam,
          fill = color_water,
          color = color_water)+
  
  #Health facilities ----------------------------
  stat_sf_coordinates(
    data = my_hf,
    geom = "point",
    size = 4,
    #color = color_hf,
    aes(color = "Rural Health facilities")
    ) +
  
  #names wards
  geom_text_repel(data = filter(names_wards,!NAME1_ %in% c(wards_text, wards_text_nudge)),
                  aes(X, Y, label = NAME1_),
                  box.padding = 1, max.overlaps = Inf,
                  size = 8)+
  geom_text(data = filter(names_wards,NAME1_ %in% c(wards_text)), 
            aes(X, Y, label = NAME1_),
            size = 8,
            nudge_y = .02) +
  geom_text(data = filter(names_wards,NAME1_ %in% c(wards_text_nudge)), 
            aes(X, Y, label = NAME1_),
            size = 8,
            nudge_y = -.04) +
  #legend ----------------------------------------------------
  guides(color = guide_legend(title = element_blank(),
                              override.aes = list(size = 7)
                              ),
         fill = guide_legend(title = element_blank())
         ) +
  
  #colors of soil ------------------------------------
  
scale_fill_manual(values = color_sandy) +
  xlim(xmin-.3, xmax+.1)+
  ylim(ymin-.1, ymax +.1)  +
  
  #legend floded = ---------------------------------
annotate("text",
         x = anot_pos_x,
         y = anot_pos_y_water_risk,
         label= "Prone to high water",
         size = size_legend,
         family = font_legend,
         hjust = 0
) +
  annotate("segment",
           x = key_start, xend = key_end,
           y =anot_pos_y_water_risk, yend = anot_pos_y_water_risk,
           color = alpha(color_water_risk, .6),
           size = 13
  ) +
  
  
  
  #legend rivers -----------------------------------------------------------------
  annotate("text",
           x = anot_pos_x,
           y = anot_pos_y_rivers,
           label= "Rivers/Lakes",
           size = size_legend,
           family = font_legend,
           hjust = 0
  ) +
  annotate("segment",
           x = key_start, xend = key_end,
           y =anot_pos_y_rivers, yend = anot_pos_y_rivers,
           color = color_water,
           size = 3
  ) +
  
  #legend wards ---------------------------------------------------------
annotate("text",
         x = anot_pos_x,
         y = anot_pos_y_borders,
         label= "Wards",
         size = size_legend,
         family = font_legend,
         hjust = 0
) +
  annotate("segment",
           x = key_start, xend = key_end,
           y =anot_pos_y_borders, yend = anot_pos_y_borders,
           color = color_borders,
           size = 3
  ) +
  
#labs --------------------------------------------------------------------
labs(title = "Mongu - Soil Map For Sanitation Technologies",
     caption = "Oxford Policy Management, 2021") +
  #theme ---------------------------
theme(
  
  #plot
  plot.background = element_rect(color_panel),
  
  #panel
  panel.background = element_rect(fill = color_panel),
  panel.grid = element_blank(),
  #axis    
  axis.text = element_blank(),
  #axis.text = element_text(size = 20),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  #legend
  #.226
  legend.position = c(.2,.1),
  legend.background = element_blank(),
  legend.key =  element_rect(fill = NA, size = 10),
  legend.text = element_text(size = 32, margin = margin(b=20), family = "Calibri Light"),
  legend.key.width = unit(3, 'cm'),
  #title
  plot.title = element_text(size = 46, hjust = .5, family = "Calibri", color = color_OPM, margin = margin(t = 20)),
  plot.caption = element_text(size = 24, hjust = 1, family = "Roboto", color = color_OPM),
)


##export ------------------------------------------------------------------------------

#exfile = file.path("maps_final", "Mongu.png")


ggsave(exfile, last_plot(), 
       width = 50,
       height = 50,
       units = 'cm',
       dev = 'png') 

