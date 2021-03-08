if(!exists("basemap", envir = globalenv())){
  source("set_up.R")
  
}


#read health facility points -----------------------------------------------
hf_data = readxl::read_xlsx(health_facilities_points)
names(hf_data) = str_remove_all(names(hf_data), " \\(D.+") #clean names



#convert to sf
shape_hfs = hf_data %>%
  filter(!is.na(Longitude),
         Longitude >16,
         Latitude > -20) %>%
  filter(str_detect(`Type of Facility`, "Rural Health Centre")) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs=4326)


#dams and lakes --------------------------------------------------------------
shape_dams = read_sf(file.path(shapefiles, "Zam_dams_lakes_epsg3857.shp" )) %>%
  st_transform(4326)


#rivers -----------------------------------------------------------------------

shape_rivers = read_sf(file.path(shapefiles, "gis.osm_waterways_free_1.shp" )) %>%
  st_transform(4326)

#soil and sanitation5
shape_soil_type = read_sf(file.path(shapefiles,"20141210_SoilSuitabilitySanitation_5_epsg3857.shp" )) %>%
  mutate(code = case_when(code == "W" ~ "Prone to high water",
                          code == "WB" ~ "Water body",
                          code == "F" ~ "Firm soil",
                          code == "S" ~ "Sandy soil",
                          T ~ code)
         )

#sanitation 3857
#sanitation3857 = read_sf(file.path(shapefiles,"20141210_SoilSuitabilitySanitation_epsg3857.shp" ))

shape_sanitation5 = read_sf(file.path(shapefiles, "20141210_SoilSuitabilitySanitation_5_epsg3857.shp")) %>%
  mutate(code = case_when(code == "F" ~ "Firm ground",
                          code == "S" ~ "Sandy soil",
                          code == "W" ~ "Porne to high water",
                          code == "WB" ~ "Water body"
                          )
         ) %>%
  st_transform( 4326 )


water_logged_shape = shape_sanitation5 %>%
  filter(code == "Porne to high water")

#District soil types (given by Jim) ------------------------------------------------

#soils
district_soils = xlsx::read.xlsx2(file.path(andres_files, 
                                            "district shape file soil types - soil codes jim 25.02.2021.xlsx"), 
                                  sheetName = "Sheet 1")


#unicef soils
unicef_districts = xlsx::read.xlsx2(file.path(andres_files, 
                                              "Copy of Copy of district shape file soil types - soil codes jim 25.02.2021 jp.xlsx"), 
                                    sheetName = "UNICEF Priority Districts") %>%
  select(NAME, Priority, Soil.types)
  
  
#join unicef with soils
districts_soils_unicef = district_soils %>%
  left_join(unicef_districts) %>%
  mutate(unicef = !is.na(Priority),
         Soil_types = case_when(unicef == T ~ Soil.types,
                                T ~ Soil_types))



#keys
soil_keys = xlsx::read.xlsx2(file.path(andres_files, 
                                       "district shape file soil types - soil codes jim 25.02.2021.xlsx"), 
                             sheetName = "key")


district_soils_key = districts_soils_unicef %>%
  left_join(soil_keys) %>%
  rename(labels_soil = Labels)

district_shape_soil = district_shape %>%
    left_join(district_soils_key , by= c("NAME")) %>%
  mutate(labels_soil = factor(labels_soil,
                              levels = c("Mainly stable soil",
                                         "Mixed stable and unstable soils",
                                         "Largely unstable soils",
                                         "Largely sandy soils")
                              )
         ) %>%
    st_transform( 4326 )



#Priority disctricts of UNICEF
shape_unicef = district_shape_soil %>%
  filter(unicef)



