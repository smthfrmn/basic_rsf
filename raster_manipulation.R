library(terra)
library(here)

# Global Human Modification
raster_ghm <- terra::rast(here("data/local_app_files/provided-app-files/raster_hm_file/raster_hm.tif"))
plot(raster_ghm)

raster_ghm

names(raster_ghm) <- c("global_human_modification")

raster_ghm


terra::writeRaster(raster_ghm, file = here("data/local_app_files/provided-app-files/global_human_modification/raster.tif"))


# Tree cover

raster_lc_tcc <- terra::rast(here("data/local_app_files/provided-app-files/raster_lc_tcc_file/raster.tif"))
plot(raster_lc_tcc)


tree_cover <- raster_lc_tcc$tree_canopy_cover

names(tree_cover) <- c("percent_tree_cover")

terra::writeRaster(tree_cover, file = here("data/local_app_files/provided-app-files/percent_tree_cover/raster.tif"))


# Land cover Type

lc_type <- raster_lc_tcc$LC

names(lc_type) <- c("land_cover_type")
plot(lc_type)

lc_typ_fact <- as.factor(lc_type)

plot(lc_typ_fact)

terra::writeRaster(lc_typ_fact, file = here("data/local_app_files/provided-app-files/land_cover_type/raster.tif"))

lc <- terra::rast(here("data/local_app_files/provided-app-files/land_cover_type/raster.tif"))
plot(lc)

lc_fact <- as.factor(lc)

plot(lc_fact)


### testing user uploaded

rast <- terra::rast(here("data/local_app_files/uploaded-app-files/raster_cat_file_hide/raster_cat.tif"))



#### bioclimatic vars

bio_rast <- terra::rast(here("data/local_app_files/uploaded-app-files/raster_cat_file_hide/wc2.1_10m_bio_3.tif"))

plot(bio_rast)


biomes <- terra::vect("~/Downloads/official/wwf_terr_ecos.shp")
plot(biomes)



lc <- terra::rast("data/local_app_files/provided-app-files/land_cover_type/raster.tif")

base_rast <- rast(xmin = xmin(lc), ncols = ncol(lc), nrows = nrow(lc))

biome_raster <- rasterize(biomes, base_rast, field = "BIOME")
plot(as.factor(biome_raster))


terra::writeRaster(biome_raster, filename = here("data/local_app_files/uploaded-app-files/raster_cat_file_hide/biome.tif"))

### continue

precip <- terra::rast(here("data/local_app_files/uploaded-app-files/user_raster_file_2/wc2.1_10m_bio_12.tif"))
plot(precip)


current <- terra::rast(here("data/local_app_files/uploaded-app-files/user_raster_file_2/precip_and_temp.tif"))

current$annual_precipitation <- precip
writeRaster(current_loaded, here("data/local_app_files/uploaded-app-files/user_raster_file_2/precip_and_temp.tif"), overwrite = TRUE)

current_loaded <- terra::rast(here("data/local_app_files/uploaded-app-files/user_raster_file_2/precip_and_temp.tif"))
plot(current_loaded)

current_loaded$scaled_annual_precipitation <- scale(current_loaded$annual_precipitation, center = TRUE)
current_loaded$scaled_annual_temperature <- scale(current_loaded$annual_temperature, center = TRUE)


current_loaded_sel <- current_loaded |>
  select("scaled_annual_precipitation", "scaled_annual_temperature")
  
plot(current_loaded_sel)

writeRaster(current_loaded_sel, here("data/local_app_files/uploaded-app-files/user_raster_file_2/precip_and_temp.tif"), overwrite = TRUE)


current_proj <- terra::project(current_loaded_sel, y = "epsg:4087")
plot(current_proj)

writeRaster(current_proj, here("data/local_app_files/uploaded-app-files-projected/user_raster_file_2/precip_and_temp.tif"), overwrite = TRUE)

temp <- terra::rast(here("data/local_app_files/uploaded-app-files/raster_file_hide/annual_temperature.tif"))
plot(temp)

precip$annual_temperature <- values(temp$annual_temperature)

precip

plot(precip)

terra::writeRaster(precip, here("data/local_app_files/uploaded-app-files/raster_file_hide/precip_and_temp.tif"))

precip_proj <- terra::project(current_loaded, y = "epsg:4087")
plot(precip_proj)

terra::writeRaster(precip_proj, here("data/local_app_files/uploaded-app-files-projected/user_raster_file_2/precip_and_temp.tif"))


biome_raster_proj <- terra::project(biome_raster, y = "epsg:4087")

plot(biome_raster_proj)

terra::writeRaster(biome_raster_proj, here("data/local_app_files/uploaded-app-files-projected/user_raster_file_1/biome.tif"))



## cropping

precip_temp <- terra::rast(here("data/local_app_files/uploaded-app-files/user_raster_file_2/precip_and_temp.tif"))


rast_ext <- ext(as.vector(ext(sample_data)) + c(-1, 1, -1, 1))
cropped_precip_temp <- terra::crop(precip_temp, rast_ext)

terra::writeRaster()
