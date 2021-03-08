
if(!exists("district_soils_key", globalenv())) {
  
  source("read_data.R")
  
}

library(ggspatial)



color_soil = c(color_stable, color_mixed, color_unstable, color_sandy)
color_logged = alpha(color_risk_water, .8)
color_borders_unicef = "#E31F27"


boundaries = st_bbox(district_shape_soil)
xmin = boundaries['xmin']
xmax = boundaries['xmax']
ymin = boundaries['ymin']
ymax = boundaries['ymax']





# define keys for artifitial layer ---------------------------------
 key_start = xmin
 key_end = xmin + .87

 anot_pos_x = xmin + .9
 
 anot_pos_y_borders = -6.73
 anot_pos_y_rivers = -7.13
 anot_pos_unicef = anot_pos_y_borders +.4
 
anot_pos_y_borders
 

 anot_pos_y_water_risk = -10.35


 size_legend = 11.8

 
 
#map 
ggplot() +
  #Soil type districts -----------------------------------------
geom_sf(data = district_shape_soil,
        aes(fill = labels_soil),
        color = NA
) +
  scale_fill_manual(values = c(color_soil, "blue"))+
  #rivers and dams -----------------------------------------------
geom_sf(data = shape_rivers,
        fill = color_water,
        color = color_water,
        size = 1) +
  geom_sf(data = shape_dams,
          fill = color_water
  ) +
  #health facilities ---------------------------------------------
stat_sf_coordinates(
  data = shape_hfs,
  geom = "point",
  size = 2,
  fill = color_hf,
  aes(color = "Rural Health Facilities")
) +
  ##prone to high water
  geom_sf(data= water_logged_shape, 
          fill = color_logged
  ) +
  
  #district borders  --------------------------------------------------------
  geom_sf(data = district_shape_soil,
          fill = NA,
          color = color_borders,
          size = 1.2) +
  #pioroity districts unicef --------------------------------------
  geom_sf(data = shape_unicef,
          fill = NA,
          color = color_borders_unicef,
          size = 1.4) +  
  
  
  ##legend ---------------------------------------------------
  guides(color = guide_legend(title = element_blank(),
                              override.aes = list(size = 7),
                              label.vjust = 0
                              ),
         fill = guide_legend(title = element_blank())
        ) +
  
  ##legend districts unicef ---------------------------------------------------
annotate("text",
         x = anot_pos_x,
         y = anot_pos_unicef,
         label= "Unicef priority districts",
         size = size_legend,
         family = font_legend,
         hjust = 0
) +
  annotate("segment",
           x = key_start, xend = key_end,
           y =anot_pos_unicef, yend = anot_pos_unicef,
           color = color_borders_unicef,
           size = 3
  ) +
  #legend districts -----------------------------------------------------------
  
annotate("text",
         x = anot_pos_x,
         y = anot_pos_y_borders,
         label= "Non-priority districts",
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
  
  #legend rivers -------------------------------------------------------------
  
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
  
  #legend floded  -------------------------------------------------------------

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
           color = color_logged,
           size = 13
  ) +
  
 
  #labs --------------------------------------------------------------------
labs(title = "Zambia - Soil Map For Sanitation Technologies",
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
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #legend
    #.226
    legend.position = c(.25,.75),
    legend.background = element_blank(),
    legend.key =  element_rect(fill = NA, size = 10),
    legend.text = element_text(size = 32, margin = margin(b=20), family = "Calibri Light"),
    legend.key.width = unit(3, 'cm'),
    #title
    plot.title = element_text(size = 46, hjust = .5, family = "Calibri", color = color_OPM, margin = margin(t = 20)),
    plot.caption = element_text(size = 24, hjust = 1, family = "Roboto", color = color_OPM),
  )


exfile = file.path("maps_final", "Zambia_soils_unicef_borders.png")
ggsave(exfile, last_plot(), 
       width = 50,
       height = 50,
       units = 'cm',
       dev = 'png') 



