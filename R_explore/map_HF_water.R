 if(!exists("basemap", envir = globalenv())){
   source("set_up.R")
   source("read_data.R")
   
 }


hf_colors = c("#340203", "#208040", "#860102", "#A00001", "#ED001C", "#208040", "#BD6324" )
 
#dams ----
 


dams_map  = ggplot() +
   geom_sf(data = shape_dams,
           fill = color_water,
           color = NA)

#rivers and health facilities ----- 
 map_hf_water = dams_map +
  #rivers --
   geom_sf(data = shape_rivers,
           fill = color_water,
           color = color_water)+
  #health facilities -- 
  stat_sf_coordinates(
     data = shape_hfs,
     geom = "point",
     size = 1,
     alpha = .5,
     aes(color = `Type of Facility`)
   ) +
  #provinces ---
  geom_sf(data = province_shape,
          fill = NA,
          size = .9) +
  #colors --
   scale_color_manual(values = hf_colors) +
  #labs --
   labs(title = "Rivers, lakes, boundaries, and health facilites") +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())



exfile = file.path("maps_exploration", "HF_water.png")

ggsave(exfile, map_hf_water, 
       width = 40,
       height = 30,
       units = 'cm',
       dev = 'png') 
 
   