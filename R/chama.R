#Maps of mongu district
if(!exists("ward_shape", envir = globalenv())){
  source("set_up.R")
  source("read_data.R")
  
}


#define colors of stable soil
alpha_stable = .1 #for stable land
color_stable = alpha('#C0504D', alpha_stable)


#read soil types of cchoma ---------------------------------------------------- 
soils_choma_raw = import(file.path(andres_files, "Copy of ward map zambia choma soils alfred.xlsx"))

#create layers for water logged and rocky soils 
soils_choma = soils_choma_raw %>%
  filter(NAME2_ == "Choma",
         !is.na(...14)) %>%
  rename(soil = ...14) %>%
  select(NAME1_, soil) %>%
  mutate(water_logged = str_detect(soil, "waterlogged"),
         rocky = str_detect(soil, "rocky"),
         soil = str_remove(soil,"Mixed rocky, |Mixed waterlogged and | and rocky and waterlogged| and waterlogged" ),
         soil = case_when(soil == "Mixed  stable"  ~ "Mainly stable soil",
                          soil %in% c("unstable and stable", "Mixed stable and unstable") ~ "Mixed stable and unstable soil",
                          soil == "unstable" ~ "Largely unstable soil",
                          T ~ soil
         ),
         soil = factor(soil, 
                       levels = c("Mainly stable soil",
                                  "Mixed stable and unstable soil",
                                  "Largely unstable soil" 
                       )
         )
         
         
  )




#join soil types with polygons -----------------------------------------------------------
my_wards = ward_shape %>%
  filter(NAME == "Choma") %>%
  left_join(soils_choma)


limits_ward = st_bbox(my_wards) #limits of polygon


##rocky layer -----------------------------------------------------------------
rocky_layer = st_make_grid(filter(my_wards, rocky), n = c(20, 20)) 


#identify that is within geom
rocky_within = st_covers(my_wards, test) %>%
  do.call(rbind,.) %>%as.numeric()

rocky_layer_within = rocky_layer[rocky_within, ]



##water logged layer -----------------------------------------------------------

water_logged_layer = my_wards %>%
  filter(water_logged)


##river and dam layer ----------------------------------------------------------

river_layer = shape_rivers %>%
  st_crop(., my_wards)

dam_layer = shape_dams %>%
  st_crop(., my_wards)

unique(shape_sanitation5$code)


##health facilities layer  ---------------------------------------------------

hf_layers = shape_hfs %>%
  st_crop(., my_wards)

hf_within = st_covers(my_wards, hf_layers) %>%
  do.call(rbind,.) %>%as.numeric()

hf_layer_within = hf_layers[hf_within, ]

##key rocky -------------------------------------------------

library(png)
library(grid)

img <- readPNG("maps_final/rocky_key.png")
g <- rasterGrob(img, interpolate=TRUE)


#names ward -------------------------------------------------------
names_wards <- cbind(my_wards, st_coordinates(st_centroid(my_wards))) #names for map




#map of zambia ------------------------------------------------------------------
#create plot of zambia to be used as a grob in the district map
zambia_map = ggplot() +
  geom_sf(data = district_shape,
          size = .1) +
  geom_rect(aes(xmin = limits_ward['xmin']-.1,
                xmax = limits_ward['xmax']+.08,
                ymin = limits_ward['ymin']-.09,
                ymax = limits_ward['ymax'] +.08
  ),
  color = 'black',
  fill = NA,
  size = 1
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = color_panel),
    panel.background = element_rect(fill = color_panel),
    panel.grid = element_blank()
    
  )


zambia_map
grob_zambia = ggplotGrob(zambia_map)



#map of choma -------------------------------------------------------------------
key_start_x = 26.15
key_end_x = 26.253
annot_x = 26.335
y_rocky = -17.36


ggplot(data = my_wards) +
  #limits ---------------------------------------------------------------------
  xlim(limits_ward['xmin']-.56, xmax = limits_ward['xmax']+.5) +
  ylim(limits_ward['ymin'], limits_ward['ymax']+.2
       ) +
  #map of zambia as a reference ---------------------------------------------
  annotation_custom(grob = grob_zambia, 
                    xmin = 27.4,
                    xmax = Inf,
                    ymin = limits_ward['ymin'],
                    ymax = -17.1
  ) +
  #Layer: soil types ----------------------------------------------------------
  geom_sf(aes(fill = soil)) +
  scale_fill_manual(values = c(color_stable, color_mixed, color_unstable)
                    ) +
  
  #layer: water logged ---------------------------------------------------------
  geom_sf(data = water_logged_layer,
          fill = NA,
          color = color_water,
          size = 1
  ) +
  #layer:rivers----------------------------------------------------------------- 
  geom_sf(data = river_layer,
          color = "blue",
          size = 1
  ) +
  #layer health facilities -----------------------------------------------------
  geom_sf(data = hf_layer_within,
          color = color_hf
  ) +
  
  #layer: rocky soil -----------------------------------------------------------
  geom_sf(data = rocky_layer_within,
          fill = NA) +
  
  #names of wards --------------------------------------------------------------
  geom_label_repel(data = names_wards,
                   aes(x = X,
                       y = Y,
                       label = NAME1_),
                   box.padding = 1,
                   min.segment.length = 0,
                   direction = "both",
                   fill = alpha('white', .5),
                   segment.colour="gray",
                   segment.size = .5,
                   family = "Open Sans Light",
                   size = 3
  ) +
  coord_sf()+
  #legend rocky ----------------------------------------------------------
annotation_custom(g, 
                  xmin = key_start_x -.02,
                  xmax = key_end_x +.02 , 
                  ymin=y_rocky-.03, 
                  ymax=y_rocky+.02
) +
  annotate("text",
           x = annot_x,
           y = y_rocky,
           label = "Rocky soil",
           family = "Open Sans Light",
           size = 4,
           hjust = 0) +
  #legend water_logged ---------------------------------------------------

annotate("segment",
         x = key_start_x, xend = key_end_x,
         y =y_rocky +.05, yend =y_rocky +.05,
         color = color_water,
         size = 3) +
  
  
  annotate("text",
           x = annot_x,
           y = y_rocky +.05,
           label = "Watterlogged wards",
           family = "Open Sans Light",
           size = 4,
           hjust = 0) +
  
  #legend rivers ------------------------------------------------------------

annotate("segment",
         x = key_start_x, xend = key_end_x,
         y =y_rocky +.1, yend =y_rocky +.1,
         color = "blue",
         size = 3) +
  
  
  annotate("text",
           x = annot_x,
           y = y_rocky +.1,
           label = "Rivers",
           family = "Open Sans Light",
           size = 4,
           hjust = 0) +
  
  #legend health facilities ----------------------------------------------------
annotate("point",
         x = key_start_x+.06,
         y =y_rocky +.15,
         color = color_hf,
         size = 5) +
  
  
  annotate("text",
           x = annot_x,
           y = y_rocky +.15,
           label = "Rural health facilities",
           family = "Open Sans Light",
           size = 4,
           hjust = 0) +
  
  #title and caption -----------------------------------------------------------
  labs(title = "Wards of Choma District - Soil Map For Sanitation Technologies",
       caption = "Oxford Policy Management, 2021") +
  
  #theme ----------------------------------------------------------------------
  
  theme(
    
    legend.position = c(.25, .1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(family= "Open Sans Light", size = 11),
    legend.key.width = unit(2, 'cm'),
    plot.background = element_rect(fill = color_panel),
    panel.background = element_rect(fill = color_panel),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #title
    plot.title = element_text(size = 17, hjust = .5, family = "Calibri", color = color_OPM, margin = margin(t = 20)),
    plot.caption = element_text(size = 12, hjust = 1, family = "Roboto", color = color_OPM),
  )







ggsave(file.path(andres_files, "Choma_soils.png"),
       last_plot())

