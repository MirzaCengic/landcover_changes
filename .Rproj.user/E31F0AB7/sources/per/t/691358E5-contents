# Get data from ESA landcover data for B&H (1992 - 2015 period)


# Script setup ------------------------------------------------------------

pacman::p_load(Rahat, raster, sf, sp, tidyverse, gdalR, mapview, tictoc)




# Crop ESA data to BiH outline --------------------------------------------

esa_rasters <- "ESA_landcover/TIFF" %>% 
  milkunize2("data") %>% 
  list.files(pattern = ".tif$", full.names = TRUE)


for (layer in esa_rasters)
{
  output_name <- paste0(milkunize2("Projects/Other/BiH/BiH_landcover/Data/Raster/BiH_landcover_"), str_sub(layer, nchar(layer) - 14 , nchar(layer) - 11), ".tif")
  
  if (!file.exists(output_name))
  {
    gdalR::GDAL_crop(layer, filename = output_name, 
                     shapefile_path = milkunize2("Projects/Other/BiH/BiH_landcover/Data/Vector/BiH_boundary.gpkg"))
  }
}


####

bih_layers <- "Projects/Other/BiH/BiH_landcover/Data/Raster" %>% 
  milkunize2() %>% 
  list.files(full.names = TRUE) %>% 
  stack()

# bih_layers %>% 
#   plot
map()

aa <- bih_layers[[1]]

bih_layers[[1:3]] %>% 
  map(~layerize(.x))

clump()

qq <- layerize(bih_layers[[24]])

stack_92 <- layerize(bih_layers[[1]])
stack_15 <- layerize(bih_layers[[24]])

crop_diff <- stack_15[[7]] - stack_92[[7]]

names(stack_15)


crop_diff[crop_diff == 0] <- NA

changes_forest_esa60 <- rasterToPoints(crop_diff, spatial = TRUE)

plot(changes_forest_esa60)

tic("Extract")
changes_points <- raster::extract(bih_layers[[c(1, 24)]],changes_forest_esa60, sp = TRUE)
toc()


changes_sf <- changes_points %>% 
  st_as_sf()



changes_df <- changes_sf %>% 
  st_set_geometry(NULL) %>% 
  transmute(
    lc_1992 = BiH_landcover_1992,
    lc_2015 = BiH_landcover_2015,
    Change = layer,
    Conversion = str_c(lc_1992, "_to_", lc_2015))


# Plot which category of land cover changed into forests, and which ones changed from forests (-1 is loss between 92-15, 1 is gain of forest)
changes_df %>% 
  ggplot(aes(x = Conversion)) +
  geom_bar() + 
  facet_wrap(~ Change) + 
  coord_flip() + 
  theme_minimal()


####
bih_adm3 <- "Data_RAW/BiH/GADM/GADM_2.8_BIH_adm3.rds" %>% 
  milkunize2("archive") %>% 
  read_rds() %>% 
  st_as_sf()

plot()


bih_adm3$NAME_2 %>% unique


changes_bih_sf <- bih_adm3  %>% 
  st_intersection(changes_sf)

changes_sf  %>% 
  st_intersection(bih_adm3)


changes_bih_sf %>% head


changes_municipality <- changes_bih_sf %>% 
  group_by(NAME_3) %>% 
  summarize(
    num = n()
  ) %>% 
  st_set_geometry(NULL)


aa <- bih_adm3 %>% 
  inner_join(changes_municipality, by = "NAME_3") 

aa %>% 
  ggplot() + 
  geom_sf(aes(fill = num))


mapview::mapview(aa, zcol = "num") + mapview(changes_sf, zcol = "layer")

changes_bih_sf %>% 
  filter(NAME_2) %>% 
  select()

##############

tic("Crosstab")
changes_raster <- crosstab(bih_layers[[c(1, 24)]], crop_diff)
toc()


####
changes_values <- changes_raster %>% 
  filter(Freq > 0) %>% 
  transmute(
    ESA_1992 = Var1,
    ESA_2015 = Var2,
    Change = Var3,
    Freq
  )

changes_values

bih_landcover_gg <- Rahat::raster_to_gg(bih_layers)

head(bih_landcover_gg)
####
plot(crop_diff, col=c('red','chartreuse4'))

save.image("/home/mirza/Desktop/bih_lc.RData")

mapview::mapview(crop_diff, maxpixels = ncell(crop_diff))
plot(stack_initial[[1]])



