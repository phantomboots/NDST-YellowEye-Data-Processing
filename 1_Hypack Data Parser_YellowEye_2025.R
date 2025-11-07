#=====================================================================================================
# Script Name: Parse Hypack .RAW files from YellowEye Dives
# Script Function: 
# Script Author: Ben Snow
# Script Date: Oct 28, 2025
# R Version: 4.4.3
#=====================================================================================================

#Required packages#

require(lubridate)
require(readxl)
require(readr)
require(dplyr)
require(stringr)
require(data.table)
require(rgdal) #This loads package sp 
require(zoo)
require(sf)


###############################EDIT THESE VALUES######################################################

#Specify what UTM Zone you are working in (just the number).

UTM_Zone <- 10

#Set working directory for location of Hypack .RAW files

Hypack_input <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/Hypack_Raw"

#Set a directory for the location of the Dive Log

Log_path <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/Dive_Log"

#Names for prefferred devices for position, depth, heading, draft and heave data sources. Must match the names 
#as listed in hardware devices. If a device is not present, write NULL. MAKE SURE DEVICE NAMES MATCH .RAW FILES 
#EXACTLY!

pos_pref <- "ROV_Pos_in"
depth_pref <- "RBR_CTD_Depth"
speed_pref <- "A125_DVL_Alt_Speed"
rov_heading_pref <- "ROV_Heading_Depth"
ship_heading_pref <- "Pyxis_GPS"
altitude_pref <- "A125_DVL_Alt_Speed"
slant_pref <- "Tritech_Slant_Altimeter"
GPS_pref <- "Pyxis_GPS"
pitch_roll_pref <- "ROV_Heading_Depth" #This device also reports the YellowEye pitch and roll values.

#Names for secondary or alternate hardware devices, for cases were primary device may have been malfunctioning or where there is more than
#one choice for a given datum (such as A125 DVL altitude).

pos_secondary <- "USBL_Model 4370_Wide angle"
depth_secondary <- "RBR_Heading_Depth"
altitude_alt <- "ROV_Heading_Depth"

#Directory for saving .CSV files

save_dir <- "~/Projects/2025_Tully_ZimKingston_ColdSeeps/Data/Processed"

################################CHECK DEVICE CODES AND FUNCTIONS ####################################
#This code block reads through all the input files, and extracts the three letter device codes, the device number, and the device name
#this data is extracted for every input file, and then joined into a single data frame at the end.
#The results are summarise in the for() loop for each individual file, and then again after the loop for all files together. If device name 
#and function have at least one record, they are present in the data set for that cruise, and should be handled.
#the notable exeption to this is position and depth records for the clump - these are logged by Hypack, but are not of interest to the end user.

setwd(Hypack_input)
input_files <- list.files(pattern = ".RAW")

device_summary_list = list()
device_functions = data.frame()

for(i in 1:length(input_files))
{
  functions_check <- fread(input_files[i], select = c(1,2,3,4), blank.lines.skip = TRUE, quote="", data.table = FALSE, fill = 12)
  devices = filter(functions_check, V1 == "DEV")
  names(devices) = c("dev_code","dev_num","V3","dev_name")
  only_present <- filter(functions_check, V1 =="POS"| V1 == "EC1"| V1 == "HCP" | V1 == "GYR" | V1 == "DFT" | V1 == "DEV") 
  names(only_present) = c("dev_code","dev_num","V3","v4")
  check <- summarise(group_by(only_present, dev_code, dev_num), n())
  check <- filter(check, dev_code != "DEV" & dev_num != "99") #Dev num 99 is an internal Hypack device code, and can be ignored.
  full_check <- left_join(check, devices, by = "dev_num") #Join the device text string names to the device codes
  full_check <- arrange(full_check, dev_name)
  full_check <- full_check[c(6, 1:3)] #drop unused columns, keep data source name, dev_code, dev_number and number of records 
  device_summary_list[[i]] <- full_check
  
  device_functions <- do.call(rbind, device_summary_list)

} 

names(device_functions) <- c("data_source_name","dev_code","dev_num","obs")
data_sources_summary <- summarize(group_by(device_functions, data_source_name, dev_code), totals_records = sum(obs))
  
################################READ IN DATA##########################################################

#Files use a single space as a delimeter. Metadata in first 9 lines does not contain sufficient columns, so for now,
#it is skipped. No header row. Read in RAW file exports; loop through all files in the Hypack .RAW directory, 
#and merge them into one file with all the data for a cruise.

#This loop reads in the data into a data frame called 'name', the date for each .RAW file into a DF named 'day and
#the device name into a DF named 'dev'. All three DFs are bound together, and then each file is add to a master 
#'all_input' file. Column types for each read is component are defined explicitly, to expedite the read in process.

#######################################GO GET A COFFEE, THIS TAKES AWHILE!!!#########################

all_input <- data.frame() #Empty DF to hold the final amalgamated output from the for loop below.

raw_data = list()
devices = list()
raw_day = list()

start_time <- Sys.time()
for(i in 1:length(input_files))
{
  raw_data[[i]] <- data.table::fread(input_files[i], fill=12, blank.lines.skip = TRUE, quote="", data.table = FALSE, showProgress = TRUE)
  #
  raw_day[[i]] <- filter(raw_data[[i]], V1 == "TND") #Filter out the line that holds the date value
  devices[[i]] <- filter(raw_data[[i]], V1 == "DEV") #Filter out the devices
  devices[[i]] <- devices[[i]][,c(1,2,4)] #Keep only the columns with 'DEV' identifier, device number, and device title.
  raw_day[[i]] <- filter(raw_data[[i]], V1 == "TND") #Filter out the line that holds the date value
  raw_data[[i]] <- filter(raw_data[[i]], V1 =="POS"| V1 == "EC1"| V1 == "HCP" | V1 == "GYR" | V1 == "DFT" ) #Filter to data records only
  
  #Handle dates that roll over midnight in this next block.
  
  secs_of_day <- raw_data[[i]][3] #Assign the seconds of day column to a temporary variable 'secs_of_day'
  secs_of_day$V3 <- as.integer(secs_of_day$V3) #Set all seconds values to integers
  first_record <- secs_of_day[1,] #Get the first timestamp value from the 'secs_of_day' vector.
  first_record <- as.integer(first_record) 
  start_day <- lubridate::mdy(raw_day[[i]][3]) #Get the starting day value
  next_day <- start_day + 1 #Add one to get the next day value.
  raw_data[[i]]$day_num[secs_of_day$V3 >= first_record] <- as.character(start_day) #If the seconds are less than or equal to the seconds value of the first record in the .RAW file, use the starting day.
  raw_data[[i]]$day_num[secs_of_day$V3 < first_record] <- as.character(next_day) #If the current record has a higher seconds count than first record, increment the day value by one.
  
  #Merge device columns and data columns
  
  raw_data[[i]] <- merge(raw_data[[i]], devices[[i]], by.x = "V2", by.y = "V2") #Merge the device names with the data.
  raw_data[[i]] <- raw_data[[i]][c(1,2,3,4,5,6,13,15)] #Drop columns X7 to X9, which are all NAs. Keep day column and device name column.
  
  #Generate a unique column to sort by and to use to remove duplicate timestamp values.
  
  raw_data[[i]]$sort <- paste0(raw_data[[i]]$V3, raw_data[[i]]$day_num, raw_data[[i]]$V4.y)
  raw_data[[i]] <- raw_data[[i]][!duplicated(raw_data[[i]]$sort),] #Remove values that occur more than once per second, based on sort column values.
  raw_data[[i]] <- raw_data[[i]][,c(1:8)] #Drop the sort column.
  
  #Bind data frames together, row wise.
  
  all_input <- do.call(rbind, raw_data)
  
}

end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)

#Format the date, merge with the seconds of the day value in column X3, then arrange all values by date.

all_input$date <- ymd(all_input$day_num)
all_input$date_time <- ymd_hms(all_input$date + seconds(all_input$V3))
all_input$date_time <- floor_date(all_input$date_time, unit = 'seconds') #This line strips out milliseconds. These must be removed to match with full_seq(), below.
all_input <- dplyr::arrange(all_input, date_time)



################################TRIM THE FILE TO TRANSECT START AND END TIMES#########################

#Read in the start and end time from the Dive Log. Recommend saving dive log as an .XLSX file, since .CSV sometimes has trouble keeping the
#yyyy-mm-dd hh:mm:ss date time format required in the for() loop below.

excel_dive_log = list.files(Log_path, pattern = "^[^~$].*\\.xlsx$")  

log <- read_excel(paste0(Log_path, "/", excel_dive_log))

#Generate a second by second sequence of timestamps from the start to finish of each dive, coerce to DF
#which include the dive number. Bind sequenced for all dives together.
for(h in 1:length(log$Transect_Name))
{
  name <- log$Transect_Name[h]
  temp <- seq(log$Start_time_UTC[h],log$End_time_UTC[h], 1)  #Start_time_UTC and End_time_UTC MUST be formatted as yyyy-mm-dd hh:mm:ss.
  sec_seq <- data.frame(Transect_Name = name, date_time = temp)
  if(h == 1)
  {full_seq <- sec_seq
  } else full_seq <- rbind(full_seq, sec_seq)
}
rm(sec_seq)

#Write the second by second sequence to a .CSV, for use in later processing stages

write.csv(full_seq, paste(Log_path,"Dive_Times_1Hz.csv", sep = "/"), quote = F, row.names = F)

#Merge the full_seq and all_input data frame, using common date time sequence.

dives_full <- left_join(full_seq, all_input, by = "date_time")
dives_full <- dives_full[,c(1,2,4,6:8,10)] #Drop extra columns, drop device ID codes, fractional seconds, and extra date columns.
names(dives_full) <- c("Transect_Name","date_time","dev_code","val1","val2","val3","device") #Keep only the columns named here.
dives_full$device <- gsub("\"","", dives_full$device) #Remove any double quotes around the 'device' character string that may have been added by fread()

################################EXTRACT DEPTH DATA####################################################

#Select rows that have a 'EC1' identifier device code. These are depth devices. Keep only the data from the primary and secondary
#depth data sources.

depth_data <- filter(dives_full, dev_code == "EC1" & device == depth_pref)
depth_data2 <- filter(dives_full, dev_code == "EC1" & device == depth_secondary)

#Drop unused columns.

depth_data <- depth_data[, c("Transect_Name","date_time","device","val1")]
depth_data2 <- depth_data2[,  c("Transect_Name","date_time","device","val1")]

#Where its available, insert the depth from the primary depth data source (the CTD). If not, 
#label these rows as NA. Remove duplicates, just to be safe.

depth_all <- left_join(full_seq, depth_data, by = "date_time") #This left join will ensure that any missing depth data timestamps are included as NA values.
depth_all <- depth_all[!duplicated(depth_all$date_time),]
depth_all <- depth_all[, c(1:2,4:5)] #Keep only the relevant columns


# #Insert the secondary depth data source in a seperate column, adjacent to the primary depth source.
# 
# depth_all <- left_join(depth_all, depth_data2, by = "date_time")
# depth_all <- depth_all[!duplicated(depth_all$date_time),]
# depth_all <- depth_all[, c(1:4,6:7)] #Keep only the relevant columns
# 
# #Loop through the elements of depth_all, where the there are no depth records from the CTD, fill in with depth records from the StellarTech depth sensor.
# #if there are no records from either devices, the depth_all value will remains as NA.
# 
# for(k in 1:length(depth_all$date_time))
# {
#   if(is.na(depth_all$Depth_m.x[k]))
#   {depth_all$Depth_m.x[k] <- depth_all$Depth_m.y[k]
#   depth_all$device.x[k] <- depth_all$device.y[k]
#   }
# }
# 
# #Keep only required columns, and rename them.


names(depth_all) <- c("Transect_Name","date_time","device","Depth_m")

################################EXTRACT ROV AND SHIP HEADING DATA###################################################

#Select rows in the dev_code column that have a 'GYR' identifier and 'heading_pref' as the device code. These should be heading values from 
#the ship's GPS source, as well as the onboard compass. Keep the data from the YellowEye's onboard heading sensor,
#the Impact SubSea AHRS unit.

rov_heading_data <- filter(dives_full, dev_code == "GYR" & device == rov_heading_pref) 
rov_heading_data <- left_join(full_seq, rov_heading_data, by = "date_time")
rov_heading_data <- rov_heading_data[!duplicated(rov_heading_data$date_time),]
rov_heading_data <- rov_heading_data[, c(1,2,8,5)]
names(rov_heading_data) <- c("Transect_Name","date_time","device","ROV_Heading")

#Select rows in the dev_code column that have a 'GYR' identifier and 'ship_heading_pref' as the device code. These should be heading values from 
#the ship's GPS source, as well as the onboard compass. Keep only the data from ship's heading, provide by Pyxis

ship_heading_data <- filter(dives_full, dev_code == "GYR" & device == ship_heading_pref) 
ship_heading_data <- left_join(full_seq, ship_heading_data, by = "date_time")
ship_heading_data <- ship_heading_data[!duplicated(ship_heading_data$date_time),]
ship_heading_data <- ship_heading_data[, c(1,2,8,5)]
names(ship_heading_data) <- c("Transect_Name","date_time","device","Ship_Heading")

################################EXTRACT ROV PITCH AND ROLL ########################################################

#Select rows in the dev_code column that have a "HCP" identifier and 'pitch_roll_pref' as the device code. These should be pitch and roll values from 
#the onboard Impact SubSea AHRS unit. val2 and val3 contain the pitch and roll data, respectively. val1 will be zeros.

pitch_roll_data <- filter(dives_full, dev_code == "HCP" & device == pitch_roll_pref) 
pitch_roll_data <- left_join(full_seq, pitch_roll_data, by = "date_time")
pitch_roll_data <- pitch_roll_data[!duplicated(pitch_roll_data$date_time),]
pitch_roll_data <- pitch_roll_data[, c(1,2,8,6,7)]
names(pitch_roll_data) <- c("Transect_Name","date_time","device","ROV_pitch","ROV_roll")

#############################SEARCH FOR ALTITUDE, EXTRACT IT IF ITS PRESENT############################

#YellowEye altitude is read in as a depth data source, through the A125 DVL. Select depth device ID (EC1), then
#select the prefered altitude device. No substitute device, so those rows that are missing data will retain NA values.
#it is also possible to read in Altitude from the output string generated from the subCAN PC. This string is the 'ROV_Heading_Depth' 
#device, and will be a 'DFT' device code if present. Check if present, and fill in missing values where present.

#Primary altitude data source, the A125 DVL, and EC1 type device code

altitude_data <- filter(dives_full, dev_code == "EC1" & device == altitude_pref)
altitude_data <- left_join(full_seq, altitude_data, by = "date_time")
altitude_data <- altitude_data[!duplicated(altitude_data$date_time),]
altitude_data <- altitude_data[, c(1,2,8,5)]
names(altitude_data) <- c("Transect_Name","date_time","device","altitude_m")

#Alternate source for the altitude data to be read in, from the subCAN PC output string.

altitude_alternate_source <- filter(dives_full, dev_code == "DFT" & device == altitude_alt)
altitude_alternate_source <- left_join(full_seq, altitude_alternate_source, by = "date_time")
altitude_alternate_source <- altitude_alternate_source[!duplicated(altitude_alternate_source$date_time),]
altitude_alternate_source <- altitude_alternate_source[, c(1,2,8,5)]
names(altitude_alternate_source) <- c("Transect_Name","date_time","device","altitude_m")

#Fill in altitude data from the alternate source, where available. Remember, data is from the same hardware device (the A125 DVL), just
#potentially a different data source in Hypack

for(k in 1:length(altitude_data$date_time))
{
  if(is.na(altitude_data$altitude_m[k])) #Only fill is gaps are in the primary altitude data set.
  {altitude_data$altitude_m[k] <- altitude_alternate_source$altitude_m[k] #Fill in from the alternate data source
  }
}

#Generate a summary data table, to check on quantity of records
altitude_summary <- dplyr::summarise(group_by(altitude_data,Transect_Name), n())
names(altitude_summary) <- c("Transect_Name","seconds_of_data")
altitude_summary$hours_of_data <- altitude_summary$seconds_of_data /3600

#####################################EXTRACT MINIZEUS SLANT RANGE######################################

#YellowEye slant range altitude is read in as a depth data source, through the Tritech Altimeter. Select depth device ID (EC1), then
#select the preferred slant_range device. No substitute device, so those rows that are missing data will retain NA values.


slant_data <- filter(dives_full, dev_code == "EC1" & device == slant_pref)
slant_data <- left_join(full_seq, slant_data, by = "date_time")
slant_data <- slant_data[!duplicated(slant_data$date_time),]
slant_data <- slant_data[, c(1,2,8,5)]
names(slant_data) <- c("Transect_Name","date_time","device","slant_range_m")

#Generate a summary data table, to check on quantity of records
slant_summary <- dplyr::summarise(group_by(slant_data,Transect_Name), n())
names(slant_summary) <- c("Transect_Name","seconds_of_data")
slant_summary$hours_of_data <- slant_summary$seconds_of_data /3600

###################SEARCH FOR DVL SPEED DATA; EXTRACT IT IF IT'S PRESENT################################

#YellowEye DVL speed data from the A125DVL is read in as a draft data source - draft is used as placeholder for speed. Select draft device ID (DFT), then
#select the preferred altitude device. No substitute device, so those rows that are missing data will retain NA values.
#Values are in nautical miles per hour - values can be negative, indicating the ROV was moving astern.

#if(which(dives_full$dev_code == "DFT") != 0)
#{
speed_data <- filter(dives_full, dev_code == "DFT" & device == speed_pref)
speed_data <- left_join(full_seq, speed_data, by = "date_time")
speed_data <- speed_data[!duplicated(speed_data$date_time),]
speed_data <- speed_data[, c(1,2,8,5)]
names(speed_data) <- c("Transect_Name","date_time","device","speed_kts")
#}

#Generate a summary data table, to check on quantity of records
speed_summary <- dplyr::summarise(group_by(speed_data,Transect_Name), n())
names(speed_summary) <- c("Transect_Name","seconds_of_data")
speed_summary$hours_of_data <- speed_summary$seconds_of_data /3600

################################EXTRACT POSITION DATA FOR BEACONS#######################################

#Select rows in the first column (X1) that have a 'POS' identifier. Keep only the preffered and secondary POS devices
#If the preferred source is unavailable, use the secondary source to fill larger gaps. If there is a gap of only 1 or 2 seconds
#ignore it. Focus on gaps larger then 60 secs; fill these in with the secondary position source (secondary device beacon, if present) 
#where possible. If  there is no secondary position source, leave as NA and deal with this in further post-processing steps.

position_data <- filter(dives_full, dev_code == "POS" & device == pos_pref)


position_data <- position_data[, c(1,2,7,4,5)]
names(position_data) <- c("Transect_Name","date_time","device","Beacon_Easting","Beacon_Northing")

#Where its available, insert the position from the primary position data source (the AAE Responder). If not, 
#label these rows as NA. Remove duplicated just to be safe.

position_all <- left_join(full_seq, position_data, by = "date_time")
position_all <- position_all[!duplicated(position_all$date_time),]

#Find sequences where the AAE Responder position is missing for more than 60 seconds. If such sequences are found, append 'TRUE' to a 
#categorical variable called $gaps. If not, append 'FALSE' to the $gaps variable.

gaps <- rle(is.na(position_all[,5])) #Search for NA values in column 5
gaps$values <- gaps$values & gaps$lengths >= 60 #This line searches for more than 60 NA values in a row.
position_all$gaps <- inverse.rle(gaps) #Put the TRUE/FALSE indices back into the data frame.

#Loop through the elements of position_all, where the there are no position records from the AAE Responder, 
#fill in with position records from the secondary position data source if it is available.

for(k in 1:length(position_all$date_time))
{
  if(position_all$gaps[k] == T & 'Easting.y' %in% names(position_all)) #Only fill is gaps are 'TRUE'. If there is no Lat, there will be no Long either, so just have to search for one of the two
  {position_all$Easting.x[k] <- position_all$Easting.y[k] 
  position_all$Northing.x[k] <- position_all$Northing.y[k]
  position_all$device.x[k] <- position_all$device.y[k]
  }
}

position_all <- position_all[,c(1,2,4:7)]
names(position_all) <- c("Transect_Name","date_time","device","Main_Beacon_Easting","Main_Beacon_Northing","Gaps")

#Generate a summary data table, to check on quantity of records
beacon_summary <- dplyr::summarise(group_by(position_all,Transect_Name), n())
names(beacon_summary) <- c("Transect_Name","seconds_of_data")
beacon_summary$hours_of_data <- beacon_summary$seconds_of_data /3600


####################################EXTRACT SHIP POSITION#############################################

ship_GPS_data <- filter(dives_full, dev_code == "POS" & device == GPS_pref)
ship_GPS_data <- left_join(full_seq, ship_GPS_data, by = "date_time")
ship_GPS_data <- ship_GPS_data[!duplicated(ship_GPS_data$date_time),]
ship_GPS_data <- ship_GPS_data[, c(1,2,8,5,6)]
names(ship_GPS_data) <- c("Transect_Name","date_time","device","Ship_Easting","Ship_Northing")



######################CONVERT THE POSITION DATA TO DECIMAL DEGREES####################################

#A SpatialPointsDataFrame needs to be created to convert from UTMs to Lat/Long. This is an object class
#from library(sf). Need coordinates, data and projection (CRS). NA values are allowed, as long at na.fail=FALSE during the initial step where
#the SpatialPointsDataFrame object is created.

#Generate SpatialPointsDataFrame for Beacon UTM coordinates.
crs <- paste0("+proj=utm +zone=", UTM_Zone," +datum=WGS84"," +units=m") #proj4string of coordinates.
position_all$Main_Beacon_Easting <- as.numeric((position_all$Main_Beacon_Easting)) #Easting must be numeric to generate a SpatialPointsDF, not character
position_all$Main_Beacon_Northing <- as.numeric(position_all$Main_Beacon_Northing) #Northing must be numeric to generate a SpatialPointsDF, not character
main_beacon_coordinates <- sf::st_as_sf(position_all, coords = c("Main_Beacon_Easting", "Main_Beacon_Northing"), crs = crs, na.fail = FALSE) #na.fail=FALSE allows for NA in the input coordinates.

#Generate SpatialPointsDataFrame for Ship UTM coordinates.
crs <- paste0("+proj=utm +zone=", UTM_Zone," +datum=WGS84"," +units=m") #proj4string of coordinates.
ship_GPS_data$Ship_Easting <- as.numeric((ship_GPS_data$Ship_Easting)) #Easting must be numeric to generate a SpatialPointsDF, not character
ship_GPS_data$Ship_Northing <- as.numeric(ship_GPS_data$Ship_Northing) #Northing must be numeric to generate a SpatialPointsDF, not character
ship_coordinates <- sf::st_as_sf(ship_GPS_data, coords = c("Ship_Easting", "Ship_Northing"), crs = crs, na.fail = FALSE) #na.fail=FALSE allows for NA in the input coordinates.

#Transform the UTMs coordinates in lat/longs into decimal degrees. 
main_beacon_geographic <- st_transform(main_beacon_coordinates, crs = "+proj=longlat +datum=WGS84")
ship_geographic <- st_transform(ship_coordinates, crs = "+proj=longlat +datum=WGS84")

#Extract coordinates from the SpatialPointsDataFrame object

main_beacon_latlons <- st_coordinates(main_beacon_geographic)
ship_latlons <- st_coordinates(ship_geographic)

#Slot the Long/Lat positions back in the position_all DF, and remove the northing and eastings

position_all$Main_Beacon_Long <- main_beacon_latlons[, "X"]
position_all$Main_Beacon_Lat <- main_beacon_latlons[,"Y"]
position_all$Ship_Long <- ship_latlons[,"X"]
position_all$Ship_Lat <- ship_latlons[,"Y"]

#Drop UTMs, rename columns.

position_all <- position_all[,c(1:3,7:10,6)]
names(position_all) <- c("Transect_Name","date_time","device","Main_Beacon_Long","Main_Beacon_Lat","Ship_Long","Ship_Lat","Gaps")



#####################################ASSEMBLE ALL DATA TO SINGLE DATA FRAME###########################

#Join position data with depth, heading, altitude and slant range data records

all_data <- position_all
all_data$Depth_m <- depth_all$Depth_m
all_data$ROV_heading <- rov_heading_data$ROV_Heading
all_data$ROV_Pitch <- pitch_roll_data$ROV_pitch
all_data$ROV_Roll <- pitch_roll_data$ROV_roll
all_data$Ship_heading <- ship_heading_data$Ship_Heading
all_data$Speed_kts <- speed_data$speed_kts
all_data$Altitude_m <- altitude_data$altitude_m
all_data$Slant_Range_m <- slant_data$slant_range_m

#Add the data source for each depth records, and the column listing gaps in position

all_data$Depth_Source <- depth_all$device
all_data$Position_Source <- position_all$device

#Re-order columns to the following order: date_time, Transect_Name, Main Beacon Long/Lat, Ship Lat/Long,
#Depth_m, ROV_Heading, ROV_Pitch, ROV_Roll, Ship_Heading, Speed, Altitude, Slant Range, Depth Source, Position Source, Gaps. Device column is dropped.

all_data <- all_data[,c(2,1,4:7,9:16,8)]


#######################################WRITE .CSV FILES FOR EACH DIVE##################################

#Loop through the Dive Names in the all_data frames, write one .CSV for each dive.

for(j in unique(all_data$Transect_Name))
{
  to_write <- filter(all_data, Transect_Name == j)
  write.csv(to_write, file = paste0(save_dir,"/",j,".csv"), quote = F, row.names = F)
}



