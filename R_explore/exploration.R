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


ggplot() +
  geom_sf(data = sanitation5,
          aes(fill = code)) +
  labs(title = "Soil sustainability sanitation 5") +
  theme(panel.background = element_blank())



wash = read_sf(file.path(shapefiles,"20141210_SoilSuitabilitySanitation_noLakes_epsg3857.shp"))


ggplot() +
  geom_sf(data = wash,
          aes(fill = code)) +
  labs(title = "Soil sustainability")
names(wash)
table(wash$code) 
