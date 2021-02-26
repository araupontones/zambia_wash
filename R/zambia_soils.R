
if(!exists("district_soils_key", globalenv())) {
  
  source("read_data.R")
  
}

library(ggspatial)




# define keys for artifitial layer ---------------------------------
key_start = 21.95
key_end = 22.84

anot_pos_x = 22.89
anot_pos_y_borders = -6.73
anot_pos_y_rivers = -7.13
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
          fill = alpha(color_water_risk, .6)
  ) +
  
  #district borders 
  geom_sf(data = district_shape_soil,
          fill = NA,
          color = color_borders,
          size = 1.2)+
          
  
  ##legend ---------------------------------------------------
  guides(color = guide_legend(title = element_blank(),
                              override.aes = list(size = 7),
                              label.vjust = 0
                              ),
         fill = guide_legend(title = element_blank())
        ) +
  
  #legend districts -----------------------------------------------------------
  
annotate("text",
         x = anot_pos_x,
         y = anot_pos_y_borders,
         label= "Districts",
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
           color = alpha(color_water_risk, .6),
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


# export -------------------------------------------------------------------------

if(color_stable == '#F1F3F4'){
  name_map = "white"
  
} else {
  
  name_map = paste0("brown_alpha_", alpha_stable)
}

exfile = file.path("maps_final", paste0("zambia_soil_", name_map,".png"))


ggsave(exfile, last_plot(), 
       width = 50,
       height = 50,
       units = 'cm',
       dev = 'png') 

