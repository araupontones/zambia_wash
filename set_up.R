library(pacman)

#load packages 
p_load(tidyverse, sf, cartography, ggrepel, rio)



library(extrafont)
loadfonts(dev = 'win')


#define directories ---------------------------------------------------------------

dropbox = "C:/Users/andre/Dropbox/A4413 Zambia UNICEF sanitation assessment (shared)"
shapefiles = file.path(dropbox, "Shape files")

sf_lavvun = file.path(shapefiles,"Lavuun shape files")
andres_files = file.path(shapefiles, "Andres Maps")
health_facilities_points = file.path(shapefiles,"MoH RHC coords", "Health Facility Listing with GeoCodes.xlsx")
health_facilities_shape = file.path(shapefiles,"MoH shape files")


#define styles -------------------------------------------------------------------
font_legend = "Calibri Light"


alpha_1 = .8 #for soil layers
alpha_stable = .1 #for stable land

color_stable = alpha('#C0504D', alpha_stable)
#"#C0504D" Johns'
# '#F1F3F4' ANdres 
color_mixed = alpha("#E26B0A", alpha_1)
color_unstable = alpha("#FFC000", alpha_1)
color_sandy = alpha("#FFFF00", alpha_1)
color_risk_water = "#B7DEE8"
color_water = "#0070C0"
color_borders = '#8DA2B4'
color_hf = "#D00024"
#"#A50014" pink
color_panel = "#D7E7EF"
color_OPM = "#0B1F51"


color_soil = c(color_stable, color_mixed, color_unstable,color_sandy )



#load basemap ------------------------------------------------------------------
unzip(file.path(health_facilities_shape, "2020 Shapefiles_Zambia.zip"),
      exdir = tempdir())

shapes_zambia = file.path(tempdir(),"2020 Shapefiles_Zambia" )



#unzip districts
unzip(file.path(health_facilities_shape, "admbound_zambia_districtsupdated.zip"),
      exdir = tempdir())



#unzip wards 
unzip(file.path(health_facilities_shape, "zmb03_ward.zip"),
      exdir = tempdir())



#read shapes
zambia_shape = read_sf(file.path(shapes_zambia, "zmb_admbnda_adm0_2020.shp"))

province_shape = read_sf(file.path(shapes_zambia, "zmb_admbnda_adm1_2020.shp")) %>%
  st_transform(4326)

district_shape = read_sf(file.path(tempdir(), "admbound_featuretopolygon_Pr.shp"))


#only keeping relevant districts for study
ward_shape = read_sf(file.path(tempdir(), "zmb03_ward.shp")) %>%
#mutate(NAME2_ = case_when(str_detect(NAME2_, "Chama") ~"Chama", T ~ NAME2_)) %>%
  filter(NAME2_ %in% c("Mongu","Monze","Choma")) %>%
  rename(NAME =  NAME2_) %>%
  st_set_crs(4326)






#export data for John
# province_shape %>%
#   tibble() %>%
#   select(-geometry) %>%
#   rio::export(., "data_John/provinces.xlsx")
# 
# district_shape %>%
#   tibble() %>%
#   select(-geometry) %>%
#   rio::export(., "data_John/districts.xlsx")
# 
# 
# ward_shape %>%
#   tibble() %>%
#   select(-geometry) %>%
#   rio::export(., "data_John/wards.xlsx")




# 
# 
# 
# list.files(shapes_zambia)

basemap = ggplot()+
  geom_sf(data = district_shape,
          fill = NA) +
  geom_sf(data = province_shape,
          fill = NA,
          size = 1) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

