library(rgpdd)
library(dplyr)
library(raster)
library(rgdal)

#Read in average monthly temperatures from WorldClim (Hijmans, R. J. et al. 2005. Very high resoluction interpolated climate surfaces for flobal land areas.)
#Data is in degrees celcius * 10.
temp_1_data <- raster("./tmean_10m_bil/tmean1.bil")
temp_2_data <- raster("./tmean_10m_bil/tmean2.bil")
temp_3_data <- raster("./tmean_10m_bil/tmean3.bil")
temp_4_data <- raster("./tmean_10m_bil/tmean4.bil")
temp_5_data <- raster("./tmean_10m_bil/tmean5.bil")
temp_6_data <- raster("./tmean_10m_bil/tmean6.bil")
temp_7_data <- raster("./tmean_10m_bil/tmean7.bil")
temp_8_data <- raster("./tmean_10m_bil/tmean8.bil")
temp_9_data <- raster("./tmean_10m_bil/tmean9.bil")
temp_10_data <- raster("./tmean_10m_bil/tmean10.bil")
temp_11_data <- raster("./tmean_10m_bil/tmean11.bil")
temp_12_data <- raster("./tmean_10m_bil/tmean12.bil")

crs(temp_1_data)
x <- extract(temp_1_data)
lat_long_1_df <- rasterToPoints(temp_1_data, spatial = T)
cord_ref <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
lat_long_1 <- spTransform(lat_long_1_df, CRS(cord_ref))
head(lat_long_1)
#I'm interested in 2 values for terrestrial organisms: average annual temperature, and average temperatures during the "growing season," April - October in Northern latitudes.

avg_annual_temp <- mean(temp_1_data, temp_2_data, temp_3_data, temp_4_data, temp_5_data, temp_6_data, temp_7_data, temp_8_data, temp_9_data, temp_10_data, temp_11_data, temp_12_data)
plot(avg_annual_temp / 10)


#To use extract, I think that I need the Lat-long values as a raster, in the same projection as the WorldClim data
head(gpdd_location)
location_ID <- inner_join(gpdd_location, gpdd_main, by = "LocationID")
locations_gpdd <- location_ID %>% dplyr::select(LongDD, LatDD, MainID) %>% matrix()
head(locations_gpdd)
e <- extent(locations_gpdd[,1:2])
x <- rasterize(locations_gpdd[,1:2], location_ID, locations_gpdd[,3], fun=mean)
raster_gpdd_locations <- rasterFromXYZ(locations_gpdd)
