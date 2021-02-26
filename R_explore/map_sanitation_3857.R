#dir.create("R_explore")
#Explore shapfiles
source("set_up.R")
files = list.files(shapefiles)
files[str_detect(files, "shp")]


#unzip shapefiles zambia
list.files(health_facilities_shape)


if(!exists("basemap", envir = globalenv())){
  source("set_up.R")
  source("read_data.R")
  
}





#wash maps ---------------------------------------------------------------


map_3857 = ggplot() +
  geom_sf(data = sanitation3857,
          aes(fill = code)) +
  labs(title = "Soil sustainability sanitation 3857") +
  theme(panel.background = element_blank())



exfile = file.path("maps_exploration", "sanitation3857.png")
ggsave(exfile, map_3857)
