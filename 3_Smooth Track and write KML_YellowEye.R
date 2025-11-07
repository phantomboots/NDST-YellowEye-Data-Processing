#=====================================================================================================
# Script Name: Smooth track position, write KML files and Shape files.
# Script Function: This script reads in the .CSV files created from "1_Hypack Data Parser_****.R', "2_ASDL Data Processing.R' and
#                  "2b_Manual Beacon Position Calculator.R". It extracts Hypack planned lines' start and end waypoints, and
#                 fills in the best position and depth data source for each dive, and interpolates Lat/Longs for Ship GPS position and 
#                 vehicle beacon. Next, distance to GPS track is calculated for each beacon point, and points outside of median+1.5*IQR
#                 are identified as outliers; beacon fixes at these time points are removed. Beacon positions are interpolated a 
#                 second time (with outliers removed). Beacon positions are then smoothed using a rolling median (overlapping medians),
#                 using stats::runmed(). Bandwidth is set in the 'EDIT THESE DATA'portion of these scripts. GPS tracks, 
#                 unsmoothed/smoother beacon fixes and planned lines are written to .KML and .SHPfiles. Final data for each dive are 
#                 written to .CSV at the end of this script.
#
# Script Author: Ben Snow
# Script Date: Sep 9, 2019
# R Version: 3.5.1
#=====================================================================================================

#Required packages#

require(lubridate)
require(readxl)
require(readr)
require(dplyr)
require(stringr)
require(smoothr)
require(rgdal) #This loads package sp 
require(zoo)
require(ggplot2)
require(geosphere)
require(sf)


#########################################EDIT THESE DATA################################################################

#Cruise ID. Use an underscore to seperate portions of the cruise ID - underscores work better when generating filenames for .SHP 
#and .KML files.

cruise_ID = "PAC2025_046"

#Name of Ship used in the survey

ship_name <- "CCGS Tully"

#Specify offsets for Ship GPS source. If more than one GPS is used, specify both sources independently. Offset to the port side are 
#positive values for 'GPS_abeam' and offset towards the bow are positive for 'GPS_along'.

GPS_abeam <- 6.6
GPS_along <- -13.2

#Set working directory for location of Hypack .RAW files

Hypack_input <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/Hypack_Raw"

#Directory for processed dives

processed_dir <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/Processed"

#Path for ASDL Master Log files

Master_ASDL <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/ASDL/Full_Cruise"

#Path for .KML files that are created in this script.

KML_path <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/KML_Files"

#Path for .SHP files that are created in this script.

SHP_path <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/Shape_Files"

#Path for final exports of .CSV that are created in this script.

final_dir <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/Final_Exports"

#UTM Zone used in the survey 

UTM_Zone = 10

#Set the value to use for the 'window' size of the running median smoothing of the beacon position, at the end of this script.
#value is in seconds.

smooth_window <- 31

#############################READ IN THE PLANNED LINE COORDINATES AND CONVERT TO LAT/LONG###########################

#Planned line file coordinates are logged in each Hypack .RAW file, and have the line designations 'PTS' and 
#'LNN'. The exact line number will vary depending on how many devices are present in the Hypack Hardware file, but 
#its a safe assumption that they will be in the first 100 lines, so only need to read in this much data.

planned_lines <- list() #Empty list to fill with line points data.frames
line_name <- list()
line_points <- list()

setwd(Hypack_input)
input_files <- list.files(pattern = ".RAW")

for(i in 1:length(input_files))
{
  planned_lines[[i]] <- data.table::fread(input_files[i], fill=12, blank.lines.skip = TRUE, quote="", data.table = FALSE, showProgress = TRUE, nrows = 100)
  line_name[[i]] <- filter(planned_lines[[i]], V1 =="LNN") # "LNN" is the line name
  line_points[[i]] <- filter(planned_lines[[i]], V1 == "PTS")
}

#Remove any duplicates, remove LNN idenfiers. Convert UTM coords to numeric values

# planned_lines <- planned_lines[!duplicated(planned_lines$X2),]
# planned_lines <- planned_lines[!duplicated(planned_lines$X4),] #Also check for duplicate file name entries, remove them if there are any
# planned_lines <- filter(planned_lines, X1 != "LNN")
# planned_lines$X2 <- as.numeric(planned_lines$X2)
# planned_lines$X3 <- as.numeric(planned_lines$X3)
# 
# #Build Spatial DF to convert from UTM to lat/long. Then convert back to regular DF. Round decimal degree values to 5 decimal places.
#   
# coordinates <- planned_lines[,c(2,3)]
# data <- as.data.frame(planned_lines[,c(4)]) #Need to coerce to a DF explicitly, since there is only one column.
# crs <- CRS(paste0("+proj=utm +zone=", UTM_Zone," +datum=WGS84"))
# 
# planned_lines_DF <- SpatialPointsDataFrame(coords = coordinates, data = data, proj4string = crs)
# planned_lines_DF <- spTransform(planned_lines_DF, CRS("+proj=longlat +datum=WGS84"))
# planned_lines_DF <- as.data.frame(planned_lines_DF)
# names(planned_lines_DF) <- c("Line_Name","Long","Lat")
# planned_lines_DF$Lat <- round(planned_lines_DF$Lat, digits = 5)
# planned_lines_DF$Long <- round(planned_lines_DF$Long, digits =5)
# 
# #Remove the "_PlannedEnd" and "_PlannedStart" portions of the planned line names, to faciliate plotting as line features.
# #Remove any NA values as well.
# 
# planned_lines_DF$Line_Name <- gsub("_PlannedStart","", planned_lines_DF$Line_Name)
# planned_lines_DF$Line_Name <- gsub("_PlannedEnd","", planned_lines_DF$Line_Name)
# 
# #Remove any lines that may, for whatever reason, are unnammed (i.e NA values)
# 
# planned_lines_DF <- planned_lines_DF[which(!is.na(planned_lines_DF$Line_Name)),]
# 
# #Set Longitude values to negative, so that they match the sign of other Long/Lat values use later in this script
# 
# planned_lines_DF$Long <-  -1*abs(planned_lines_DF$Long)
# 
# #Create an empty list to fill with as a 'Lines' class object, in the short loop below.
# 
# Planned_Lines <- list()
# 
# #Filter the data frame containing the start/end point for each plan line, and extract the coordinates and name of each planned line
# #to its own, DF. Add each of these individual DFs as elements into a Lines class object, for inclusion into .KML and .SHP files later
# #in this script.
# 
# for(i in unique(planned_lines_DF$Line_Name))
# {
#   name <- filter(planned_lines_DF, Line_Name == i)
#   coords <- cbind(name$Long, name$Lat)
#   colnames(coords) <- c("Long", "Lat")
#   Planned_Lines <- append(Planned_Lines, Lines(list(Line(coords)), ID = i)) #Append each unique planned line into a seperate slot in the Lines object
# }


############################READ IN THE DIVE LOG AND BOOTS STRING #################################


# #Read all Master Log files.
# 
# setwd(Master_ASDL)
# Slant_Range_Master <- read_csv("Tritech_SlantRange_MasterLog.csv")
# MiniZeus_ZFA_Master <- read_csv("MiniZeus_ZFA_MasterLog.csv")
# RBR_Master <- read_csv("RBR_CTD_MasterLog.csv")
# Manual_Tracking_Master <- read_csv("Manual_Beacon_Tracking_MasterLog.csv")
# Hemisphere_Master <- read_csv("Hemisphere_GPS_Heading_MasterLog.csv")
# Ship_Heading_Master <- read_csv("Hemisphere_Heading_MasterLog.csv")
# Cyclops_Master <- read_csv("Cyclops_YPR_MasterLog.csv")
# 
# #Append a designation column to BOOTS_Master, RBR_Master and Manual Tracking Master
# 
# Hemisphere_Master$ID <- "Ship GPS backup"
# RBR_Master$ID <- "RBR CTD backup"
# Manual_Tracking_Master$ID <- "Manual tracking backup"

###########################################FILL IN MISSING POSITION AN DEPTH FROM SECONDARY DEVICES ###########################################

#Read in each processed transect file as its own data frame. Fill in missing position and depth data if it was missing from a file
#due to a Hypack crash (or failure to start logging!)

setwd(processed_dir)
Dives <- list.files(processed_dir)

for(i in unique(Dives))
{
  name <- i
  assign(name, read_csv(i)) #This will assign "Transect_Name' as character, 'Gaps' as a logical and all other columns and doubles (numeric) data types.
}

# for(i in unique(Dives))
# {
#   
#First, check and fill in any Hypack crash periods with the Ship_GPS and onboard Depth sensor.
  # 
  # name <- i
  # assign(name, read_csv(i))
  # fill <- get(name)
  # temp1 <- which(is.na(fill$Main_Beacon_Long) & fill$Gaps == T) #The indices where GAPS = T and Longitude is NA
  # temp2 <- fill$date_time[temp1] #The timestamps values at indices where GAPS = T and Longitude is NA
  # GPS_to_fill <- get("Hemisphere_Master") #Use quote since the object is outside of the environment in the for() loop.
  # index <- match(temp2, GPS_to_fill$date_time)
  # fill$Main_Beacon_Long[temp1] <- GPS_to_fill$Long[index] #The Long from the Ship GPS
  # fill$Main_Beacon_Lat[temp1] <- GPS_to_fill$Lat[index]  #The Lat from the Ship GPS
  # fill$Heading[temp1] <- GPS_to_fill$Heading[index] #Heading from Ship GPS
  # fill$Depth_m[temp1] <- GPS_to_fill$Depth_m[index] #Depth from Ship GPS
  # fill$Position_Source[temp1] <- GPS_to_fill$ID[index]
  # 
#Next, check and see if better position data can be retrieved from the manual tracking of the main
#beacon, fill it in if possible.
  
  # Tracking_to_fill <- get("Manual_Tracking_Master")
  # index <- match(temp2, Tracking_to_fill$date_time)
  # swap <- which(!is.na(index))
  # if(length(swap) != 0)
  # {fill$Main_Beacon_Long[temp1] <- Tracking_to_fill$Beacon_Long[swap] 
  # fill$Main_Beacon_Lat[temp1] <- Tracking_to_fill$Beacon_Lat[swap]  
  # fill$Position_Source[temp1] <- Tracking_to_fill$ID[swap]
  # }

# #Now check to see if better depth data is available from the RBR CTD depth data source, fill it in if possible.
#   
#   RBR_to_fill <- get("RBR_Master")
#   index <- match(temp2, RBR_to_fill$date_time)
#   swap <- which(!is.na(index))
#   if(length(swap) != 0)
#   {fill$Depth_m[temp1] <- RBR_to_fill$Depth_m[index] 
#   fill$Depth_Source[temp1] <- RBR_to_fill$ID[swap]
#   }
#   
# #Make sure the Long and Lat values area numeric.
#   
#   fill$Main_Beacon_Long <- as.numeric(fill$Main_Beacon_Long)
#   fill$Main_Beacon_Lat <- as.numeric(fill$Main_Beacon_Lat)
#   fill$Secondary_Beacon_Lat <- as.numeric(fill$Secondary_Beacon_Lat)
#   fill$Secondary_Beacon_Long <- as.numeric(fill$Secondary_Beacon_Long)
#   fill$Depth_m <- as.numeric(fill$Depth_m)
#   
# #Re-assign the filled in data to to the dive file.
#   
#   assign(i, fill) #Re-assign the filled in data to the dive file
#   rm(list = c("Tracking_to_fill","RBR_to_fill","GPS_to_fill","fill"))
# }


############COERCE THE LAT AND LONGS TO A ZOO OBJECT AND INTERPOLATE MISSING POSITIONS##########

#Zoo objects are totally ordered observations, and each observation must be unique. Coerce the Lat/Long for the 
#beacon data to a Zoo object, and interpolate missing records.

for(i in unique(Dives))
{
  name <- get(i)
  Long <- zoo(name$Main_Beacon_Long, order.by = name$date_time)
  Lat <- zoo(name$Main_Beacon_Lat, order.by = name$date_time)
  Long_intepolated <- na.approx(Long, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Long_intepolated <- as.matrix(Long_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Main_Beacon_Long_interp <- Long_intepolated[1:length(Long_intepolated)]
  Lat_intepolated <- na.approx(Lat, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Lat_intepolated <- as.matrix(Lat_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Main_Beacon_Lat_interp <- Lat_intepolated[1:length(Lat_intepolated)]
  assign(i, cbind(name, Main_Beacon_Long_interp, Main_Beacon_Lat_interp))
}

#Coerce the Lat/Longs for the Ship's GPS position to a Zoo object and interpolate, in the same manner as above.

for(i in unique(Dives))
{
  name <- get(i)
  Long <- zoo(name$Ship_Long, order.by = name$date_time)
  Lat <- zoo(name$Ship_Lat, order.by = name$date_time)
  Long_intepolated <- na.approx(Long, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Long_intepolated <- as.matrix(Long_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Ship_Long_interp <- Long_intepolated[1:length(Long_intepolated)]
  Lat_intepolated <- na.approx(Lat, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Lat_intepolated <- as.matrix(Lat_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Ship_Lat_interp <- Lat_intepolated[1:length(Lat_intepolated)]
  assign(i, cbind(name, Ship_Long_interp, Ship_Lat_interp))
}

#############################ADD IN ALL OTHER DATA SOURCES FROM ASDL PROCESSED LOG FILES#####################################################

# for(i in unique(Dives))
# {
#   name <- get(i)
#   name <- left_join(name, Hemisphere_Master, by = "date_time")
#   name <- left_join(name, Ship_Heading_Master, by = "date_time")
#   name <- left_join(name, Cyclops_Master, by = "date_time")
#   name <- left_join(name, MiniZeus_ZFA_Master, by = "date_time")
#   name <- name[,c(1:8,17:20,9:10,25,11:16,26:31)]
#   names(name) <- c("date_time","Transect_Name","Main_Beacon_Long_raw","Main_Beacon_Lat_raw","Secondary_Beacon_Long_Raw","Secondary_Beacon_Lat_Raw",
#                     "Ship_Long_raw","Ship_Lat_raw","Main_Beacon_Long_interp","Main_Beacon_Lat_interp","Ship_Long_interp","Ship_Lat_interp","Depth_m",
#                "Phantom_heading","Ship_Heading","Speed_kts","Altitude_m","Slant_Range_m","Best_Depth_Source","Best_Position_Source","Gaps","Cyclops_yaw",
#                "Cyclops_pitch","Cyclops_roll","MiniZeus_Zoom_percent","MiniZeus_Focus_percent","MiniZeus_aperture")
#   name$Best_Position_Source[is.na(name$Best_Position_Source)] <- "Linear Interpolation"
#   assign(i, name)
# }

############COERCE THE SHIPS HEADING TO A ZOO OBJECT AND INTERPOLATE MISSING POSITIONS##########

# Coerce the Ship's Heading data to a zoo object, and interpolate missing records.

for(i in unique(Dives))
{
  name <- get(i)
  Heading <- zoo(name$Ship_Heading, order.by = name$date_time)
  Heading_intepolated <- na.approx(Heading, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Heading_intepolated <- as.matrix(Heading_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Heading_intepolated <- Heading_intepolated[1:length(Heading_intepolated)]
  assign(i, mutate(name, Ship_Heading = Heading_intepolated)) #Replace the column with missing values with the one that were just interpolated
}

############COERCE THE ROV DEPTH TO A ZOO OBJECT AND INTERPOLATE MISSING DATA ###################


for(i in unique(Dives))
{
  name <- get(i)
  Depth <- zoo(name$Depth_m, order.by = name$date_time)
  Depth_intepolated <- na.approx(Depth, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Depth_intepolated <- as.matrix(Depth) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  Depth_intepolated <- Depth_intepolated[1:length(Depth_intepolated)]
  assign(i, mutate(name, Depth_m = Depth_intepolated)) #Replace the column with missing values with the one that were just interpolated
}



###################################APPLY OFFSETS TO POSITIONS DATA SOURCES############################

# #Calculate angle created by the abeam and along ship centerline offset values, to be used in calculation of offset from center of ship
# #to the GPS antenna. For trigonometry purposes, abeam = opposite and along = adjacent.
# 
# #Compute length of hypotenuse to determine offset distance, in meters.
# offset_dist = sqrt((GPS_abeam^2) + (GPS_along^2))
# 
# #For cases where the GPS antenna is offset in both abeam and along ship axes, calculate the angle from the center of the ship to the 
# #antenna location.
# 
# offset_angle <- atan(GPS_abeam/GPS_along)
# offset_angle <- offset_angle * (180/pi) #Convert to degrees.
# 
# for(i in unique(Dives))
# {
#   name <- get(i)
#   
#   if(GPS_abeam == 0 & GPS_along == 0) {   #Case 1: the GPS antenna is dead center on the ship 
#     offset_dist = 0 
#   } else if (GPS_abeam == 0) {  #Case 2: GPS antenna along keel line, but fore/aft of center of ship. 
#     
#     name$bearing <- name$Ship_Heading - 180 #Subtract 180 if it is astern of the center of the ship
#     name$bearing <- name$Ship_Heading  #No change to the heading if it is ahead of the center of the ship
# 
#   } else if (GPS_along == 0) {  #Case 3: GPS antenna centered fore/aft, but not along keel 
#     
#     name$bearing <- heading_data$Heading - 90 #Subtract 90 if is to the port of center.
#     name$bearing <- heading_data$Heading + 90 #Add 90 if it is to the stbd of center.
# 
#   } else if (GPS_along != 0 & GPS_abeam != 0) {   #Case 4: Standard case, GPS offset in both abeam and alongship axes.
#     
#     name$bearing <- name$Ship_Heading + offset_angle
#   }
#   
#   name$bearing[name$bearing > 360] <- name$bearing[name$bearing > 360] - 360 #Reduce integer >360 back to values less than of equal to 360
#   name$bearing[name$bearing < 0] <- name$bearing[name$bearing < 0] + 360 #Increase negative values by 360, to get correct bearing between 0 and 360
# 
#   #Calculate the offset positions long/lat.
#     
#     for(k in 1:length(name$date_time))
#     {
#       start <- cbind(name$Main_Beacon_Long_interp[k], name$Main_Beacon_Lat_interp[k])
#       offset <- destPoint(start, name$bearing[k], offset_dist)
#       name$offset_long[k] <-  offset[1]
#       name$offset_lat[k] <- offset[2]
#       
#     }
#   mutate(name, Main_Beacon_Long_interp = offset_long, Main_Beacon_Lat_interp = offset_lat)
#   name <- name[,1:28] #Drop the offset_lat and offset_long columns
#   
#   assign(i, name)
# }

################################################REMOVE BEACON DATA OUTLIERS#################################

#Calculate cross track distance between GPS track of the Ship and beacon position. Do this by creating a point distance matrix between
#each interpolated beacon position, and the Ship_GPS point for that same second.
#The distm() function in the R geosphere package computes a distance matrix between geographic points, or between two sets of geographic points, this 
#is used to generate a cross-track distance 'XTE' for each point in the time series - essentially how far from the ship the and the ROV were away from each other
#due to the clump weight, this value of XTE should be relatively stable within a single dive.

for(k in unique(Dives))
{
  name <- get(k)
  for(i in 1:length(name$date_time))
  {
    ship <- cbind(name$Ship_Long_interp[i], name$Ship_Lat_interp[i])
    beacon <- cbind(name$Main_Beacon_Long_interp[i], name$Main_Beacon_Lat_interp[i])
    XTE <- geosphere::distm(ship, beacon)
    name$XTE[i] <- as.numeric(XTE[,1])
  }
  assign(k, name)
}

#Calculate the median XTE value for each dive, and detect outliers as median +/- 1.5*Interquartile range. Set the lat/long values 
#for any indices outside of the values to NA. These are classified as outliers, and rejected.

for(k in unique(Dives))
{
  name <- get(k)
  upper <- median(name$XTE) + (1.5*IQR(name$XTE))
  lower <- median(name$XTE) - (1.5*IQR(name$XTE))
  name$Main_Beacon_Lat_interp[name$XTE > upper | name$XTE < lower] <- NA
  name$Main_Beacon_Long_interp[name$XTE > upper | name$XTE < lower] <- NA
  assign(k, name)
}

#Coerce to a zoo object (again) and run linear interpolation on the Beacon Lat/Long that have had the outlier values set to NA, in order to replace them with
#linearly interpolated data.

for(i in unique(Dives))
{
  name <- get(i)
  Long <- zoo(name$Main_Beacon_Long_interp, order.by = name$date_time)
  Lat <- zoo(name$Main_Beacon_Lat_interp, order.by = name$date_time)
  Long_intepolated <- na.approx(Long, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Long_intepolated <- as.matrix(Long_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  name$Main_Beacon_Long_interp <- Long_intepolated[1:length(Long_intepolated)]
  Lat_intepolated <- na.approx(Lat, rule = 2) #Rule 2 means interpolate both forwards and backwards in the time series.
  Lat_intepolated <- as.matrix(Lat_intepolated) #Convert back to a base object class (a matrix) to extract the components of the zoo object.
  name$Main_Beacon_Lat_interp <- Lat_intepolated[1:length(Lat_intepolated)]
  assign(i, name)
}

#Smooth the interpolated beacon position with the Loess smoother, with an alpha value of 0.05

for(i in unique(Dives))
{
  name <- get(i)
  loess_lat <- stats::loess(name$Main_Beacon_Lat_interp ~ as.numeric(name$date_time), span = 0.05)
  name$loess_lat <- loess_lat$fitted
  loess_long <- stats::loess(name$Main_Beacon_Long_interp ~ as.numeric(name$date_time), span = 0.05)
  name$loess_long <- loess_long$fitted
  assign(i, name)
}


#Smooth the interpolated beacon position with a running median - set the bandwitdth to ~ 1 min (59 seconds). Must be an odd value.
#Round the values of the smoothed data to 5 decimal places; keep trailing zeros!

for(i in unique(Dives))
{
  name <- get(i)
  name$Main_Beacon_Lat_smooth <- runmed(name$Main_Beacon_Lat_interp, smooth_window)
  name$Main_Beacon_Long_smooth <- runmed(name$Main_Beacon_Long_interp, smooth_window)
  name$Main_Beacon_Lat_smooth <- round(name$Main_Beacon_Lat_smooth, digits = 5)
  name$Main_Beacon_Long_smooth <- round(name$Main_Beacon_Long_smooth, digits = 5)
  assign(i, name)
}

###################################CREATE SPATIAL LINES OBJECTS#######################################################

#First, make sure that all the Lat/Long values are all of the same sign (negative or positive). By convention, all
#Long in BC are negative, and lats should be positive

for(i in unique(Dives))
{
  name <- get(i)
  name$Main_Beacon_Lat_interp <- abs(name$Main_Beacon_Lat_interp)
  name$Main_Beacon_Long_interp <- -1*abs(name$Main_Beacon_Long_interp) #Set longitudes to all negatives.
  name$Ship_Lat_interp <- abs(name$Ship_Lat_interp)
  name$Ship_Long_interp <- -1*abs(name$Ship_Long_interp) #As above
  assign(i, name)
}


#For each files, create a Line class object for the interpolated vehicle position, the ship's position and the planned
#line files for each dive.

Dives2 <- gsub(".csv","",Dives) #Make a new vector of to use as names for the Dive Lines that will be produced.

#Empty data.frame and list object to fill in for loop below.
Dives_merge <- list()
full_set <- data.frame()

#Generate coordinates and Lines() class objects for the various interpolated beacon positions, as well as the ship's positions.

#Merge all data frames to one large one to build SpatialLayers. This allow for one Shape file, containing the coordinate data for all dives 
#from a given survey. The Shapefile attributes will contain the Transect Name.

for(k in 1:length(Dives))
{
  dive <- get(Dives[k])
  Dives_merge[[k]] <- dive
  
  full_set <- do.call(rbind, Dives_merge)
}

#Build sf() data frames, from merged data set. These SpatialDataFrames will have point type geometry, and will load into QGIS/ARCGis as 
#a series of points, strictly speaking, not a line type feature. This is the desired output.

smooth_only <- full_set[,c("Transect_Name","Main_Beacon_Long_smooth","Main_Beacon_Lat_smooth")]
interp_only <- full_set[,c("Transect_Name","Main_Beacon_Long_interp","Main_Beacon_Lat_interp")]
ship_only <- full_set[,c("Transect_Name","Ship_Long_interp","Ship_Lat_interp")]
loess_only <- full_set[,c("Transect_Name","loess_long", "loess_lat")]
Smooth_Lines <- st_as_sf(smooth_only, coords = c("Main_Beacon_Long_smooth","Main_Beacon_Lat_smooth"), crs = "+proj=longlat +datum=WGS84")
Interp_Lines <- st_as_sf(interp_only, coords = c("Main_Beacon_Long_interp","Main_Beacon_Lat_interp"), crs = "+proj=longlat +datum=WGS84")
Ship_Lines <- st_as_sf(ship_only, coords = c("Ship_Long_interp","Ship_Lat_interp"), crs = "+proj=longlat +datum=WGS84")
Loess_Lines <- st_as_sf(loess_only, coords = c("loess_long", "loess_lat"), crs = "+proj=longlat +datum=WGS84")


################################WRITE SPATIAL FILES#######################################################

#Write a .KML file for the lines of interpolated beacon data, the smoothed beacon data, and the ship GPS track.

st_write(obj = Smooth_Lines, dsn = paste0(KML_path,"/",cruise_ID,"_runmed.kml"), driver = "KML", append=FALSE)
st_write(obj = Loess_Lines, dsn = paste0(KML_path,"/",cruise_ID,"_loess.kml"), driver = "KML",append=FALSE)  
st_write(obj = Ship_Lines, dsn = paste0(KML_path,"/",cruise_ID,"_ship_track.kml"),driver = "KML", append=FALSE)  
st_write(obj = Interp_Lines, dsn = paste0(KML_path,"/",cruise_ID,"_unsmoothed.kml"),driver = "KML", append=FALSE) 

#Write a .SHP file for the lines of interpolated beacon data, the smoothed beacon data, and the ship GPS track, and 
#the planned survey lines. Append=FALSE will overwrite existing .SHP files in the output directory, so you can re-run these lines if you need
#to tweak something and new Shapefile will be produced by overwriting those that are already in the directory.

st_write(obj = Smooth_Lines, dsn = paste0(SHP_path,"/",cruise_ID,"_runmed.shp"), append=FALSE)
st_write(obj = Loess_Lines, dsn = paste0(SHP_path,"/",cruise_ID,"_loess.shp"), append=FALSE)  
st_write(obj = Ship_Lines, dsn = paste0(SHP_path,"/",cruise_ID,"_ship_track.shp"), append=FALSE)  
st_write(obj = Interp_Lines, dsn = paste0(SHP_path,"/",cruise_ID,"_unsmoothed.shp"), append=FALSE)  


###################################WRITE FINAL PROCESSED DATA TO FILE####################################################

#Create final processed files, and write to .CSV.

for(i in unique(Dives))
{
  name <- get(i)
  name <- name[,c(1:2,22:23,17,16,24,25,19,18,7:14)]
  names(name) <- c("date_time","Transect_Name","Beacon_Lat_loess","Beacon_Long_loess","Beacon_Lat_interp","Beacon_Long_interp","Beacon_Lat_smoothed",
                   "Beacon_Long_smoothed","Ship_Lat","Ship_Long","Depth_m","ROV_heading","ROV_Pitch","ROV_Roll","Ship_Heading","Speed_kts",
                   "Altitude_m","Slant_Range_m")
  write.csv(name, file = paste0(final_dir,"/",i), quote = F, row.names = F)
  assign(i, name)
}


