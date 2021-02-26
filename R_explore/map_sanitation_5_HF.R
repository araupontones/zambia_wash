

if(!exists("shape_soil_type", envir = globalenv())){
  source("set_up.R")
  source("read_data.R")
  
}


#breaks for legend
soils = shape_soil_type %>%
  filter(code != "Firm soil") %>%
  .$code %>%
  unique()




#keep relevant observations 

#remoce non rural from health facilties
shape_rural_hf = shape_hfs %>%
  filter(str_detect(`Type of Facility`, "Rural Health Centre"))

districts_3857 <- sf::st_transform(district_shape, 4326 )

soil_3857 <- sf::st_transform(shape_soil_type, 4326)
rivers_3857 <- sf::st_transform(shape_rivers, 4326)

st_crs(district_shape)
st_crs(districts_3857)
xmin = st_bbox(districts_3857)[1]
xmin

st_bbox(districts_3857)
xmax = st_bbox(districts_3857)[3]
ymin = st_bbox(districts_3857)[2]
ymax = st_bbox(districts_3857)[4]
xrange = xmax - xmin
yrange = ymax - ymin
xmin
xmax
xmax

st_crs(district_shape)
st_crs(districts_3857)

#legend 


size_borders = 1.2
color_borders = '#8DA2B4'
color_panel = "#D7E7EF"
color_OPM = "#0B1F51"
#wash maps ---------------------------------------------------------------



map_sanitation5_hf =
  ggplot() +
  
  #soil type
  geom_sf(data = soil_3857,
          color = color_panel,
          aes(fill = code)) +
  #rivers ------------------------------------
  geom_sf(data = rivers_3857,
          fill = color_water_natural,
          color = color_water_natural,
          size = 2
          ) +
  #health facilities -- -----------------------
  stat_sf_coordinates(
    data = shape_rural_hf,
    geom = "point",
    size = 2,
    fill = alpha("#A50014",.5),
    aes(color = alpha("#ED001C",.5))
    ) +
  #districts ---------------------------------------------
  geom_sf(data = districts_3857,
          fill = NA,
          color = color_borders,
          size = size_borders
          )+
    #colors soilf 
  scale_fill_manual(values = color_soil,
                    name = "",
                    breaks = soils
                    ) +
    #legend HF
    scale_color_manual(values =c(alpha("#A50014", .5)),
                       name = "Health Facilities",
                       labels = c("")
                       ) +
  annotate("segment", x = xmin,xend = xmin+3, y = ymin, yend = ymin, colour = "red", size = 4) +
  guides(
         color = guide_legend(title.hjust = 0,
                              title.position = "right",
                              override.aes = list(size = 5),
                              title = "Rural Health Facilities",
                              title.theme = element_text(size = 32, family = "Roboto Light"),
                              legend.position = c(.1,.9)
                              #legend.text = element_text(size = 32)
                              )
         )+
                              
  labs(title = "Zambia - Soil Type and Location of Rural Health Facilities",
       caption = "Oxford Policy Management, 2021") +
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
    legend.position = c(.2,.8),
    legend.key =  element_rect(fill = NA, color = color_panel, size = 10),
    legend.text = element_text(size = 32, margin = margin(b=20), family = "Roboto Light", face = "bold"),
    legend.key.width = unit(3, 'cm'),
    legend.background = element_rect(fill = color_panel),
    #title
    plot.title = element_text(size = 46, hjust = .5, family = "Calibri", color = color_OPM, margin = margin(t = 20)),
    plot.caption = element_text(size = 24, hjust = 1, family = "Roboto", color = color_OPM),
       
        #space between text
        
        
  
        
        )


map_sanitation5_hf
 
xmin
ymax
ymin
xmax
exfile = file.path("maps_final", "sanitation5_HF.png")

ggsave(exfile, map_sanitation5_hf, 
       width = 50,
       height = 50,
       units = 'cm',
       dev = 'png') 



