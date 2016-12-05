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

#The code below simply documents my process of extracting temperature data from WorldClim, putting it in a data frame, and amalgamating it.
#This data is stored in a csv table, read in below.

# temp_1_data <- raster("./tmean_10m_bil/tmean1.bil")
# temp_2_data <- raster("./tmean_10m_bil/tmean2.bil")
# temp_3_data <- raster("./tmean_10m_bil/tmean3.bil")
# temp_4_data <- raster("./tmean_10m_bil/tmean4.bil")
# temp_5_data <- raster("./tmean_10m_bil/tmean5.bil")
# temp_6_data <- raster("./tmean_10m_bil/tmean6.bil")
# temp_7_data <- raster("./tmean_10m_bil/tmean7.bil")
# temp_8_data <- raster("./tmean_10m_bil/tmean8.bil")
# temp_9_data <- raster("./tmean_10m_bil/tmean9.bil")
# temp_10_data <- raster("./tmean_10m_bil/tmean10.bil")
# temp_11_data <- raster("./tmean_10m_bil/tmean11.bil")
# temp_12_data <- raster("./tmean_10m_bil/tmean12.bil")
# 
# #I want to work with the climate data in a data frame because population data from the GPDD is in a data frame.
# 
# temp_1_data_df <- rasterToPoints(temp_1_data) %>% data.frame()
# temp_2_data_df <- rasterToPoints(temp_2_data) %>% data.frame()
# temp_3_data_df <- rasterToPoints(temp_3_data) %>% data.frame()
# temp_4_data_df <- rasterToPoints(temp_4_data) %>% data.frame()
# temp_5_data_df <- rasterToPoints(temp_5_data) %>% data.frame()
# temp_6_data_df <- rasterToPoints(temp_6_data) %>% data.frame()
# temp_7_data_df <- rasterToPoints(temp_7_data) %>% data.frame()
# temp_8_data_df <- rasterToPoints(temp_8_data) %>% data.frame()
# temp_9_data_df <- rasterToPoints(temp_9_data) %>% data.frame()
# temp_10_data_df <- rasterToPoints(temp_10_data) %>% data.frame()
# temp_11_data_df <- rasterToPoints(temp_11_data) %>% data.frame()
# temp_12_data_df <- rasterToPoints(temp_12_data) %>% data.frame()
# 
# colnames(temp_1_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_2_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_3_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_4_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_5_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_6_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_7_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_8_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_9_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_10_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_11_data_df)[1:2] <- c("Long", "Lat") 
# colnames(temp_12_data_df)[1:2] <- c("Long", "Lat") 
# nrow(temp_12_data_df)
# 
# #amalgamate data frames. Because I don't know of a better way, I iteratively add df's to the main df "monthly_temp_df"
# #I join by longitude, then filter out the duplicate values
# monthly_temp_df <- left_join(temp_1_data_df, temp_2_data_df, by = "Long") %>% filter(Lat.x == Lat.y)
# monthly_temp_df <- monthly_temp_df %>% dplyr::select(Long, Lat.x, tmean1, tmean2)
# monthly_temp_df <- left_join(monthly_temp_df, temp_3_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3)
# monthly_temp_df <- left_join(monthly_temp_df, temp_4_data_df, by = "Long")  
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4)
# monthly_temp_df <- left_join(monthly_temp_df, temp_5_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5)
# monthly_temp_df <- left_join(monthly_temp_df, temp_6_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6)
# monthly_temp_df <- left_join(monthly_temp_df, temp_7_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7)
# monthly_temp_df <- left_join(monthly_temp_df, temp_8_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8)
# monthly_temp_df <- left_join(monthly_temp_df, temp_9_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9)
# monthly_temp_df <- left_join(monthly_temp_df, temp_10_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.x == Lat.y) %>% dplyr::select(Long, Lat.y, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9, tmean10)
# monthly_temp_df <- left_join(monthly_temp_df, temp_11_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.y == Lat) %>% dplyr::select(Long, Lat, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9, tmean10, tmean11)
# monthly_temp_df <- left_join(monthly_temp_df, temp_12_data_df, by = "Long")
# monthly_temp_df <- monthly_temp_df %>% filter(Lat.y == Lat.x) %>% dplyr::select(Long, Lat.x, tmean1, tmean2, tmean3, tmean4, tmean5, tmean6, tmean7, tmean8, tmean9, tmean10, tmean11, tmean12)
# 
# write.csv(file = "WorldClim_avg_monthly_temp.csv",monthly_temp_df)

monthly_temp_df <- read.csv("./WorldClim_avg_monthly_temp.csv", stringsAsFactors = F)

head(monthly_temp_df)

avgs_ordered_wc <- monthly_temp_df %>% mutate(avg_april_oct = mean(c(tmean4,tmean5,tmean6,tmean7,tmean8,tmean9,tmean10)), 
                           avg_annual = mean(c(tmean1,tmean2,tmean3,tmean4,tmean5,tmean6,tmean7,tmean8,tmean9,tmean10,tmean11,tmean12))) %>%
                            arrange(desc(Lat.x), Long) %>% dplyr::select(Long, Lat.x, avg_april_oct, avg_annual)
write.csv(file = "World_clim_data_org_for_cycles_proj.csv", avgs_ordered_wc)
head(avgs_ordered_wc)
#Check - should see temperature gradient along lat
#monthly_temp_df %>% ggplot() + geom_point(aes(x = Lat.x, y = tmean1 / 10), alpha = 0.01)

#########################################################
#
# Next, I need to create the data frame of population locations from the Global Population Dynamics Database
# The object 'gpdd_location' contains location metadata for each population time series in the GPDD 
#
#########################################################

# #Create a dataframe with latitude, longitude, and location ID from the GPDD. This will be linked with the worldclim data.
# locations_gpdd <- gpdd_location %>% dplyr::select(LongDD, LatDD, LocationID)
# head(locations_gpdd)  
# 
# 
# #This empty data frame will be filled with temperature data for each location in the GPDD, with are indexed with the Location ID number.
# gpdd_location_temp <- data.frame(LocationID = numeric(max(locations_gpdd$LocationID)), Long = numeric(max(locations_gpdd$LocationID)), Lat = numeric(max(locations_gpdd$LocationID)),
#                                  close_class = numeric(max(locations_gpdd$LocationID)), Jan_temp = numeric(max(locations_gpdd$LocationID)), Feb_temp = numeric(max(locations_gpdd$LocationID)), 
#                                  March_temp = numeric(max(locations_gpdd$LocationID)), April_temp = numeric(max(locations_gpdd$LocationID)), May_temp = numeric(max(locations_gpdd$LocationID)), 
#                                  June_temp = numeric(max(locations_gpdd$LocationID)), July_temp = numeric(max(locations_gpdd$LocationID)), Aug_temp = numeric(max(locations_gpdd$LocationID)), 
#                                  Sept_temp = numeric(max(locations_gpdd$LocationID)), Oct_temp = numeric(max(locations_gpdd$LocationID)), Nov_temp = numeric(max(locations_gpdd$LocationID)), 
#                                  Dec_temp = numeric(max(locations_gpdd$LocationID)))
# 
# 
# for(i in 1:max(locations_gpdd$LocationID)){
# 
#   location <- subset(locations_gpdd, LocationID == i)
#   
#   if(nrow(location) != 0){
#   
#     gpdd_location_temp$LocationID[i] <- location$LocationID
#     gpdd_location_temp$Long[i] <- location$LongDD
#     gpdd_location_temp$Lat[i] <- location$LatDD
#     
#     #Only take temperature data from within 0.1 Lat and Long
#     world_clim_location <- subset(monthly_temp_df, Long > location$LongDD - 0.1 & Long < location$LongDD + 0.1 & Lat.x > location$LatDD - 0.1 & Lat.x < location$LatDD + 0.1)
#     
#     if(nrow(world_clim_location) > 0){
#       gpdd_location_temp$close_class[i] <- "0.1"
#       gpdd_location_temp$Jan_temp[i] <- mean(world_clim_location$tmean1) / 10
#       gpdd_location_temp$Feb_temp[i] <- mean(world_clim_location$tmean2) / 10
#       gpdd_location_temp$March_temp[i] <- mean(world_clim_location$tmean3) / 10
#       gpdd_location_temp$April_temp[i] <- mean(world_clim_location$tmean4) / 10 
#       gpdd_location_temp$May_temp[i] <- mean(world_clim_location$tmean5) / 10 
#       gpdd_location_temp$June_temp[i] <- mean(world_clim_location$tmean6) / 10 
#       gpdd_location_temp$July_temp[i] <- mean(world_clim_location$tmean7) / 10 
#       gpdd_location_temp$Aug_temp[i] <- mean(world_clim_location$tmean8) / 10
#       gpdd_location_temp$Sept_temp[i] <- mean(world_clim_location$tmean9) / 10 
#       gpdd_location_temp$Oct_temp[i] <- mean(world_clim_location$tmean10) / 10 
#       gpdd_location_temp$Nov_temp[i] <- mean(world_clim_location$tmean11) / 10 
#       gpdd_location_temp$Dec_temp[i] <- mean(world_clim_location$tmean12) / 10
#       
#     } else{
#       
#       world_clim_location_0.5 <- subset(monthly_temp_df, Long > location$LongDD - 0.5 & Long < location$LongDD + 0.5 & Lat.x > location$LatDD - 0.5 & Lat.x < location$LatDD + 0.5)
#       
#       if(nrow(world_clim_location_0.5) > 0){
#         gpdd_location_temp$close_class[i] <- "0.5"
#         gpdd_location_temp$Jan_temp[i] <- mean(world_clim_location_0.5$tmean1) / 10
#         gpdd_location_temp$Feb_temp[i] <- mean(world_clim_location_0.5$tmean2) / 10
#         gpdd_location_temp$March_temp[i] <- mean(world_clim_location_0.5$tmean3) / 10
#         gpdd_location_temp$April_temp[i] <- mean(world_clim_location_0.5$tmean4) / 10 
#         gpdd_location_temp$May_temp[i] <- mean(world_clim_location_0.5$tmean5) / 10 
#         gpdd_location_temp$June_temp[i] <- mean(world_clim_location_0.5$tmean6) / 10 
#         gpdd_location_temp$July_temp[i] <- mean(world_clim_location_0.5$tmean7) / 10 
#         gpdd_location_temp$Aug_temp[i] <- mean(world_clim_location_0.5$tmean8) / 10
#         gpdd_location_temp$Sept_temp[i] <- mean(world_clim_location_0.5$tmean9) / 10 
#         gpdd_location_temp$Oct_temp[i] <- mean(world_clim_location_0.5$tmean10) / 10 
#         gpdd_location_temp$Nov_temp[i] <- mean(world_clim_location_0.5$tmean11) / 10 
#         gpdd_location_temp$Dec_temp[i] <- mean(world_clim_location_0.5$tmean12) / 10
#       } else{
#       
#       gpdd_location_temp[i,4:length(gpdd_location_temp)] <- NA
#       }
#       
#     }
#     
#   } else{
#     gpdd_location_temp[i,1:length(gpdd_location_temp)] <- NA
#   }
#     
# }
# 
# #Check
# 
# 
# #Add in a column with average annual temperature.
# gpdd_location_temp_an <- gpdd_location_temp %>% filter(!is.na(LocationID)) %>% rowwise() %>%
#   mutate(Average_annual_temp = mean(c(Jan_temp, Feb_temp, March_temp, April_temp, May_temp, June_temp, July_temp, Aug_temp, Sept_temp, Oct_temp, Nov_temp, Dec_temp))) %>% data.frame()
# 
# gpdd_location_temp_an %>% ggplot() + geom_point(aes(x = Lat, y = Average_annual_temp))           
# 
# str(gpdd_location_temp_an)
# 
# write.csv(file = "Avg_wc_temp_for_GPDD_pops.csv", gpdd_location_temp_an)
# 

gpdd_location_temp_an <- read.csv("Avg_wc_temp_for_GPDD_pops.csv", stringsAsFactors = F)


#########################################################
#
# Next, I will work with my taxon specific information on life history (body size, maturation time, etc.)
# This information will fit in the 'gpdd_taxon' data table from the 'rgpdd' 
#
#########################################################


life_history_data <- read.csv("./GPDD_Taxon_specific_info.csv", stringsAsFactors = F)

head(life)
str(more_lf_dat_insects)


#1. I want temperature data and life history data available in their respective data tables.
#   These data tables will open upon running the library.










#misc
life_history_data %>% filter(!is.na(Mass_kg))
nrow(gpdd_taxon)
str(life_history_data)

check_sibly <- inner_join(gpdd_taxon, life_history_data, by = "TaxonID") %>%
  inner_join(., gpdd_main, by = "TaxonID")

check_sibly %>% filter(SiblyReturnRate > 0) %>% 
  ggplot() + geom_point(aes(x = log(age_first_reproduction_years), y = log(SiblyReturnRate), color = Class)) + 
  geom_smooth(aes(x = log(age_first_reproduction_years), y = log(SiblyReturnRate)), method = "lm", se = F) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"), text = element_text(size = 15), legend.position = "bottom")
check_sibly %>% filter(SiblyReturnRate > 0, Class == "Mammalia") %>% do(tidy(lm(log(SiblyReturnRate) ~ log(Mass_kg), data = .)))
life_history_and_location <- inner_join(check_sibly, gpdd_location_temp_an, by = "LocationID")
head(life_history_and_location)

ect_body_temp <- life_history_and_location %>% filter(Thermo == "Ectotherm") %>% mutate(temp = Average_annual_temp) %>% dplyr::select(MainID.x, temp)
end_body_temp <- life_history_and_location %>% filter(Thermo == "Endotherm") %>% mutate(temp = Temp) %>% dplyr::select(MainID.x, temp)

all_temp <- rbind(ect_body_temp, end_body_temp)

all_temp <- inner_join(all_temp, life_history_and_location, by = "MainID.x")
library(broom)
exp <- all_temp %>% filter(SiblyReturnRate > 0, Reliability > 1, (Class == "Mammalia" | Class == "Insecta" | Class == "Aves")) %>% 
  do(tidy(lm(log(SiblyReturnRate) ~ log(Mass_kg) + temp, data = .)))


all_temp %>% filter(SiblyReturnRate > 0, Reliability > 1, (Class == "Mammalia" | Class == "Insecta" | Class == "Aves")) %>%
  mutate(tempcorr = exp(-exp[3,2] * temp) * SiblyReturnRate) %>% ggplot() + geom_point(aes(x = log(Mass_kg), y = log(tempcorr), color = Class)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"), text = element_text(size = 15), legend.position = "bottom")
all_temp$SamplingFrequency <- as.numeric(as.character(all_temp$SamplingFrequency))

all_temp %>% ggplot() + geom_point(aes(x = age_first_reproduction_years, y = 1 / SamplingFrequency, color = Class), size = 3, alpha = 0.2) +
  geom_smooth(aes(x = age_first_reproduction_years, y = 1 / SamplingFrequency)) +
  scale_x_log10() + scale_y_log10() + xlab("Maturation time (yr)") + ylab("Time step (yr)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"), text = element_text(size = 15), legend.position = "bottom")


all_temp %>% ggplot() + geom_point(aes(x = age_first_reproduction_years, y = DatasetLength * (1 / SamplingFrequency), color = Class), size = 2, alpha = 0.2) +
  geom_smooth(aes(x = age_first_reproduction_years, y = DatasetLength * (1 / SamplingFrequency)), span = 0.1) +
  scale_x_log10() + scale_y_log10() + xlab("Maturation time (yr)") + ylab("Length of time series (yr)") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"), text = element_text(size = 15), legend.position = "bottom")

head(all_temp)

all_temp %>% geom_histogram(aes(SamplingFrequency))
  
unique(all_temp$Class)
