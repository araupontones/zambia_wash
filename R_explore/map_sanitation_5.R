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


map_sanitation5  =  ggplot() +
  geom_sf(data = sanitation5,
          color = NA,
          aes(fill = code)) +
  scale_fill_manual(values = color_soil,
                    name = "") +
  labs(title = "Soil sustainability sanitation 5") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


exfile = file.path("maps_exploration", "sanitation5.png")

ggsave(exfile, map_sanitation5, 
       width = 40,
       height = 30,
       units = 'cm',
       dev = 'png') 



