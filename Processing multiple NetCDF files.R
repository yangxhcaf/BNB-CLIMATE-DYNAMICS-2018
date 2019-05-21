######
######Processing multiple NetCDF files
######
library(ncdf4)
library(reshape2)
library(dplyr)
# Clear workspace
rm(list=ls())
# retrieve a list of nc files in my data folder:
flist <- list.files(path = "C:/Users/yassmin/Desktop/Combine/", pattern = "^.*\\.(nc4|nc|NC|Nc|Nc)$")

# Define our function
process_nc <- function(files){
  # iterate through the nc
  for (i in 1:length(files)){
    # open a conneciton to the ith nc file
    nc_tmp <- nc_open(paste0("C:/Users/yassmin/Desktop/Combine/", files[i]))
    # store values from variables and atributes
    nc_sm <- ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[2])
    nc_lat <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[2])
    nc_lon <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[3])
    nc_start_date=as.Date(sub( ".*?.(\\d{8}).*", "\\1", paste0("C:/Users/yassmin/Desktop/Combine/", files[i])), "%Y%m%d" )
  
    # close the connection sice were finished
    nc_close(nc_tmp)
    # set the dimension names and values of your matrix to the appropriate latitude and longitude values
    dimnames(nc_sm) <- list(lon=nc_lon, lat=nc_lat)
    
    # I'm choosing to store all the data in long format.
    # depending on your workflow you can make different choices here...
    # Your variable may get unmanageably large here
    # if you have high spatial and temporal resolution nc data.
    tmp_sm_df <- melt(nc_sm, value.name = "SM")
    tmp_sm_df$date_start <- nc_start_date
    
    # set the name of my new variable and bind the new data to it
    if (exists("sm_data_daily")){
      sm_data_daily<- bind_rows(sm_data_daily, tmp_sm_df)
    }else{
      sm_data_daily <- tmp_sm_df
    }
    # tidy up, not sure if necesarry really, but neater
    rm(nc_sm, nc_lat, nc_lon, nc_tmp, nc_start_date, tmp_sm_df)
  }
  
  return(sm_data_daily)
}


sm19201031122012<- process_nc(flist)
save(sm19201031122012, file="sm19201031122012.RData")
load(sm19201031122012.RData)
write.csv(sm19201031122012, "sm19201031122012.csv")
