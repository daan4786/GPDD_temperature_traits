#########################################################
#
# The purpose of this code is to integrate average monthly temperatures from WorldClim (Hijmans, R. J. et al. 2005. Very high resoluction interpolated climate surfaces for flobal land areas.)
# into the global population dynamics database, which is available through the package 'rgpdd'
#
#########################################################


library(rgpdd)
library(dplyr)
library(raster)
library(rgdal)
library(ggplot2)

#########################################################
#
# Read in temperature data from WorldClim 
# The number corresponds to the month: 1 is January, 2 is February, etc. 
# Temperature data is in degrees celcius * 10.
#
#########################################################

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

#I want to work with the climate data in a data frame because population data from the GPDD is in a data frame.

temp_1_data_df <- rasterToPoints(temp_1_data) %>% data.frame()
temp_2_data_df <- rasterToPoints(temp_2_data) %>% data.frame()
temp_3_data_df <- rasterToPoints(temp_3_data) %>% data.frame()
temp_4_data_df <- rasterToPoints(temp_4_data) %>% data.frame()
temp_5_data_df <- rasterToPoints(temp_5_data) %>% data.frame()
temp_6_data_df <- rasterToPoints(temp_6_data) %>% data.frame()
temp_7_data_df <- rasterToPoints(temp_7_data) %>% data.frame()
temp_8_data_df <- rasterToPoints(temp_8_data) %>% data.frame()
temp_9_data_df <- rasterToPoints(temp_9_data) %>% data.frame()
temp_10_data_df <- rasterToPoints(temp_10_data) %>% data.frame()
temp_11_data_df <- rasterToPoints(temp_11_data) %>% data.frame()
temp_12_data_df <- rasterToPoints(temp_12_data) %>% data.frame()

colnames(temp_1_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_2_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_3_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_4_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_5_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_6_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_7_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_8_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_9_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_10_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_11_data_df)[1:2] <- c("Long", "Lat") 
colnames(temp_12_data_df)[1:2] <- c("Long", "Lat") 
nrow(temp_12_data_df)

#amalgamate data frames. Because I don't know of a better way, I iteratively add df's to the main df "monthly_temp_df"
#I join by longitude, then filter out the duplicate values
monthly_temp_df <- left_join(temp_1_data_df, temp_2_data_df, by = "Long") %>% filter(Lat.x == Lat.y)
monthly_temp_df <- monthly_temp_df %>% dplyr::select(Long, Lat.x, tmean1, tmean2)
monthly_temp_df <- left_join(monthly_temp_df, temp_3_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3)
monthly_temp_df <- left_join(monthly_temp_df, temp_4_data_df, by = "Long")  
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4)
monthly_temp_df <- left_join(monthly_temp_df, temp_5_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5)
monthly_temp_df <- left_join(monthly_temp_df, temp_6_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6)
monthly_temp_df <- left_join(monthly_temp_df, temp_7_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7)
monthly_temp_df <- left_join(monthly_temp_df, temp_8_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8)
monthly_temp_df <- left_join(monthly_temp_df, temp_9_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9)
monthly_temp_df <- left_join(monthly_temp_df, temp_10_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.y, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9, tmean10)
monthly_temp_df <- left_join(monthly_temp_df, temp_11_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.y == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9, tmean10, tmean11)
monthly_temp_df <- left_join(monthly_temp_df, temp_12_data_df, by = "Long")
monthly_temp_df <- monthly_temp_df %>% filter(Lat.y == Lat.x) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9, tmean10, tmean11, tmean12)

write.csv(file = "WorldClim_avg_monthly_temp.csv",monthly_temp_df)
head(monthly_temp_df)

#Check - should see temperature gradient along lat
monthly_temp_df %>% ggplot() + geom_point(aes(x = Lat.x, y = tmean1 / 10), alpha = 0.01)

#########################################################
#
# Next, I need to create the data frame of population locations from the Global Population Dynamics Database
# The object 'gpdd_location' contains location metadata for each population time series in the GPDD 
#
#########################################################

#Create a dataframe with latitude, longitude, and location ID from the GPDD. This will be linked with the worldclim data.
locations_gpdd <- gpdd_location %>% dplyr::select(LongDD, LatDD, LocationID)
head(locations_gpdd)  


#This empty data frame will be filled with temperature data for each location in the GPDD, with are indexed with the Location ID number.
gpdd_location_temp <- data.frame(LocationID = numeric(max(locations_gpdd$LocationID)), Long = numeric(max(locations_gpdd$LocationID)), Lat = numeric(max(locations_gpdd$LocationID)),
                                 Jan_temp = numeric(max(locations_gpdd$LocationID)), Feb_temp = numeric(max(locations_gpdd$LocationID)), March_temp = numeric(max(locations_gpdd$LocationID)), 
                                 April_temp = numeric(max(locations_gpdd$LocationID)), May_temp = numeric(max(locations_gpdd$LocationID)), June_temp = numeric(max(locations_gpdd$LocationID)),
                                 July_temp = numeric(max(locations_gpdd$LocationID)), Aug_temp = numeric(max(locations_gpdd$LocationID)), Sept_temp = numeric(max(locations_gpdd$LocationID)),
                                 Oct_temp = numeric(max(locations_gpdd$LocationID)), Nov_temp = numeric(max(locations_gpdd$LocationID)), Dec_temp = numeric(max(locations_gpdd$LocationID)))


for(i in 1:max(locations_gpdd$LocationID)){

  location <- subset(locations_gpdd, LocationID == i)
  
  if(nrow(location) != 0){
  
    world_clim_location <- subset(monthly_temp_df, Long > location$LongDD - 0.1 & Long < location$LongDD + 0.1 & Lat.x > location$LatDD - 0.1 & Lat.x < location$LatDD + 0.1)
    
    gpdd_location_temp$LocationID[i] <- location$LocationID
    gpdd_location_temp$Long[i] <- location$LongDD
    gpdd_location_temp$Lat[i] <- location$LatDD
    
    if(nrow(world_clim_location) > 0){
     
      gpdd_location_temp$Jan_temp[i] <- world_clim_location %>% summarise(mean(tmean1) / 10)
      gpdd_location_temp$Feb_temp[i] <- world_clim_location %>% summarise(mean(tmean2) / 10)
      gpdd_location_temp$March_temp[i] <- world_clim_location %>% summarise(mean(tmean3) / 10) 
      gpdd_location_temp$April_temp[i] <- world_clim_location %>% summarise(mean(tmean4) / 10) 
      gpdd_location_temp$May_temp[i] <- world_clim_location %>% summarise(mean(tmean5) / 10) 
      gpdd_location_temp$June_temp[i] <- world_clim_location %>% summarise(mean(tmean6) / 10) 
      gpdd_location_temp$July_temp[i] <- world_clim_location %>% summarise(mean(tmean7) / 10) 
      gpdd_location_temp$Aug_temp[i] <- world_clim_location %>% summarise(mean(tmean8) / 10)
      gpdd_location_temp$Sept_temp[i] <- world_clim_location %>% summarise(mean(tmean9) / 10) 
      gpdd_location_temp$Oct_temp[i] <- world_clim_location %>% summarise(mean(tmean10) / 10) 
      gpdd_location_temp$Nov_temp[i] <- world_clim_location %>% summarise(mean(tmean11) / 10) 
      gpdd_location_temp$Dec_temp[i] <- world_clim_location %>% summarise(mean(tmean12) / 10)
      
    } else{
      gpdd_location_temp[i,4:length(gpdd_location_temp)] <- NA
    }
    
  } else{
    gpdd_location_temp[i,1:length(gpdd_location_temp)] <- NA
  }
    
}

