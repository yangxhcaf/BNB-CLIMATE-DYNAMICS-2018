library(ncdf4)
library(reshape2)
library(dplyr)
# Clear workspace
rm(list=ls())

setwd("C:/Users/yassmin/Dropbox/PhD Thesis & Analysis/My PhD R work/TRMM3B43 nc4/")
# retrieve a list of nc files in my data folder:
flist <- list.files(path = "C:/Users/yassmin/Desktop/Combine/", pattern = "^.*\\.(nc4|nc|NC|Nc|Nc)$")

# Open a connection to the first file in our list
nc <- nc_open(paste0("C:/Users/yassmin/Desktop/Combine/", flist[1]))
print(nc)

# Save the print(nc) dump to a text file (same name as the nc file with a txt extension)
{
  sink(paste0("C:/Users/yassmin/Desktop/Combine/", flist[1], ".txt"))
  print(nc)
  sink()
}

# Get a list of the NetCDF's R attributes:
attributes(nc)$names

print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"NetCDF attributes"))

# Get a list of the nc variable names.
attributes(nc$var)$names

# Take a look at the chlorophyll variable's nc attributes (units etc).
ncatt_get(nc, attributes(nc$var)$names[2])

#Retrieve a matrix of the chlorophyll data using the ncvar_get function:
sm <- ncvar_get(nc, attributes(nc$var)$names[2])

# Print the data's dimensions
dim(sm)

# Retrieve the latitude and longitude values.
attributes(nc$dim)$names

nc_lat <- ncvar_get( nc, attributes(nc$dim)$names[2])
nc_lon <- ncvar_get( nc, attributes(nc$dim)$names[3])

print(paste(dim(nc_lat), "latitudes and", dim(nc_lon), "longitudes"))

# a quick look at a random section of this data
sm[135:137, 240:247]

# Change the dimension names of our matrix to "lon" and "lat", 
# and the row and column names to the latitude and longitude values.
dimnames(sm) <- list(lon=nc_lon, lat=nc_lat)
sm[35:37, 40:47]


# Retrieve the start date-times
time=as.Date(sub( ".*?.(\\d{8}).*", "\\1", paste0("C:/Users/yassmin/Desktop/Combine/", flist[1])), "%Y%m%d" )
  

