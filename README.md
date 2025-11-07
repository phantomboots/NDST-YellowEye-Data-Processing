This script is designed to read in .RAW formatted data file, collected by the Navigation software Hypack. These .RAW files are tab delimeted, with a number of file header rows and one row for each device sensor reading. 

The script extracts all collected data readings from Hypack .RAW files and filters them by device code. The Hypack device codes of interest are three letter identifiers, and include the following:

'POS' - Device code for position data type, sourced from a ship mounted GPS or an ROV beacon
'GYR' - Device code for 'gyro', meaning a heading data source in the case. Can be either ship heading or heading from an ROV mounted sensor.
'EC1' - Device code for 'echosounder', meaning a depth data source. Used for both ROV depth, as well as a placeholder used for winch payout, altitude and slant range, depending on the sensor type.
'DFT' - Device code for 'Draft'. This is used as a place holder for altitude and (potentially) speed from an ROV mounted DVL. Draft is used in instances where there is more than one depth data source already present on a given Hypack 'mobile'; two depth sources must not be used on the same mobile device, and the use of the draft code avoids this. 
'HCP' - Device code for a heave, pitch and roll sensor. Use for ROV pitch and roll from the onboard AHRS.

The scripts filter .RAW files to the device codes listed above, and it builds a 1 Hz times series of the sensor data logged by Hypack during the survey, and converts position data from a projected coordinate reference system (UTM North Zones, 8 9 or 10) to an unprojected data set of latitude and longitude as decimal degrees using package sf(). No interpolation of missing values is done at this stage. The script will preferrentially use the depth and position data records from the user defined 'preferred' depth and position sources, but a secondary position, depth and altitude data source can be specified.  

A dive log (in .xlsx format) is required to generate a 1Hz time series for each dive from the start and end time in UTC. Datetime fields in the dive log must be formatted yyyy-mm-dd hh:mm:ss. This time series can be generated for the entire dive or just the ontransect portions.
