# paper 2 PhD code ------------------------------------------------------
##Spatiotemporal Precipitation Variability Modeling in the Blue Nile Basin: 1998-2016 R Code
## Jan 2018

#Install packages------------------------------------------------------
install.packages("plm", "Formula")
install.packages("zoo")
install.packages(c("gstat", "spacetime", "geoR"))
install.packages(c("sp", "xts", "raster", "rgdal", "rgeos",  "ggplot2", "plyr"))
install.packages(c("ggmap", "maps", "mapproj", "rasterVis", "maptools",  "dismo"))
library(plm)
library(gstat)
library(spacetime)
library(xts)
library(rgdal)
library(rgeos)
library(zoo)
library(plyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(dismo)


#create time object ------------------------------------------------------
time_index <- seq(from = as.POSIXct("1998-01-01", tz = 'UTC'), to = as.POSIXct("2016-12-01", tz = 'UTC'), by = "month") 
time_index <- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month") 

rainSP=subset(TRMMENSO, TRMMENSO$date =="1998-01-01")


# create a grid dataframe------------------------------------------------------

rainSP <- subset(TRMMENSO, TRMMENSO$Year =="1998" & TRMMENSO$Month=="1" ) 
rainSP <-data.frame(cbind(rainSP$lon, rainSP$lat))
rainSP=rename (rainSP, c("X1"="lon","X2"="lat"))

# Define grid dataframe as spatial------------------------------------------------------

coordinates(rainSP)=~lon+lat
proj4string(rainSP)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
str(rainSP)


# make anomaly dataset------------------------------------------------------
TRMMENSO$month <- format(TRMMENSO$date, "%m")
TRMMENSO$rainanom <- TRMMENSO$monthlyrain - ave(TRMMENSO$monthlyrain, TRMMENSO$month, FUN = function(x) mean(x, na.rm = TRUE))
library(reshape2)
TRMMwide=acast(TRMMENSO, date~lon+lat, value.var="anom")
save(TRMMwide, file = "TRMMwide.RData")
save(TRMMENSO, file = "TRMMENSO.RData")


### Defining Spatiotemporal------------------------------------------------------

library(spacetime)
m = stConstruct(TRMMENSO, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"monthlyrain"])
STFDF.years = 1998:2016

#STplot yearly------------------------------------------------------
sel = (1:19) * 1
stplot(STFDF[,sel, "monthlyrain"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, 
        alternating=c(1,2), cex=.8, rot=45), cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))

# Aggregate by month------------------------------------------------------

library(lubridate)
bymonth <- aggregate(monthlyrain ~Month+lon+lat, data=STFDF,FUN=mean)
bymonth$date=seq(as.Date("2000/1/1"), by = "month", length.out = 12)
summary(bymonth)
# summary statistics by month and year raindall----------------------------------
library(purrr)
bymonth %>% split(.$Month) %>% map(summary)
rain<- subset(TRMMENSO, select = c(Year, monthlyrain, onefourth))
rain %>% split(.$Year) %>% map(summary)


#STplot monthly------------------------------------------------------

m = stConstruct(bymonth, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
STFDF.months = 1:12
#STplot yearly------------------------------------------------------
sel = (1:12) * 1
stplot(STFDF[,sel, "monthlyrain"], 
       STFDF.months[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, 
alternating=c(1, 2), cex=.8, rot=45), 
cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))

# Aggregate by kirmet------------------------------------------------------

kirmet <- subset(TRMMENSO, Month > 5 & Month < 10)
library(spacetime)
m = stConstruct(kirmet, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"monthlyrain"])
STFDF.years = 1998:2016
#STplot kirmet yearly------------------------------------------------------
sel = (1:19) * 1
stplot(STFDF[,sel, "monthlyrain"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, 
        alternating=c(1, 2), cex=.8, rot=45), cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))

dev.off()

# Aggregate by Belg------------------------------------------------------


belg=subset(TRMMENSO, Month > 2 & Month < 6)

library(spacetime)
m = stConstruct(belg, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"monthlyrain"])
STFDF.years = 1998:2016

#STplot Belg yearly------------------------------------------------------

sel = (1:19) * 1
stplot(STFDF[,sel, "monthlyrain"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, 
         alternating=c(1, 2), cex=.9, rot=45), cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))



# Aggregate by Bega------------------------------------------------------

bega=subset(TRMMENSO, Month =c(1, 2, 10, 11, 12))

summary(bega)

library(spacetime)
m = stConstruct(bega, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"monthlyrain"])
STFDF.years = 1998:2016

#STplot bega yearly------------------------------------------------------

sel = (1:19) * 1
stplot(STFDF[,sel, "monthlyrain"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, alternating=c(1, 2), cex=.8, rot=45), cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))



# Aggregate by spatial  ------------------------------------------------------
library(lubridate)
bysptial <- aggregate(monthlyrain ~Month+Year, data=STFDF,FUN=mean)
bysptial$date<- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month") 

# Merge TS datasets -----------------------------------------------------------
rainglobal=merge(bysptial, global_sst_2016, by = c("Month", "Year"))
rainglobal$date<- seq(from = as.POSIXct("1998-01-01", tz = 'UTC'), to = as.POSIXct("2016-12-01", tz = 'UTC'), by = "month") 
global.temp=merge(global_sst_2016, temp, by = c("Month", "Year"))
save(temp,file = "temp.RData")
save(global.temp,file = "globaltemp.RData")

# TS graph of Rainfall ------------------------------------------------------
library(ggplot2)
library(scales)

rain=ggplot(data = bysptial, 
            aes(x = date,y = monthlyrain, ymin = 0, ymax = monthlyrain)) + geom_linerange(color="blue", size=1)
rain=rain + labs(y = "Average monthly precip. ")+ labs(x = "Date")+ theme(legend.position='none')
rain+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

## define date in global_sst_2016$date as as.POSIXct------------------------------------------------------

global_sst_2016$date=as.POSIXct(global_sst_2016$date, tz="UTC")
class(global_sst_2016$date)
global_sst_2016$date<- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month")
save(global_sst_2016, file = "globalsst.RData")


# TS graph of ENSO ------------------------------------------------------
library(ggplot2)

NINO12graph=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = nino1.2anom, ymin = 0, ymax = nino1.2anom, colour = ifelse(nino1.2anom <0, "blue", "red")))
NINO12graph=NINO12graph + labs(y = "Niño 1+2")+ labs(x = "Date")+ theme(legend.position='none')
NINO12graph+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 


NINO3graph=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = nino3anom, ymin = 0, ymax = nino3anom, colour = ifelse(nino3anom <0, "blue", "red")))
NINO3graph=NINO3graph + labs(y = "Niño 3")+ labs(x = "Date")+ theme(legend.position='none')
NINO3graph+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 


NINO4graph=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = nino4anom, ymin = 0, ymax = nino4anom, colour = ifelse(nino4anom <0, "blue", "red")))
NINO4graph=NINO4graph + labs(y = "Niño 4")+ labs(x = "Date")+ theme(legend.position='none')
NINO4graph+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

NINO34graph=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = nino3.4anom, ymin = 0, ymax = nino3.4anom, colour = ifelse(nino3.4anom <0, "blue", "red")))
NINO34graph=NINO34graph + labs(y = "Niño 3.4")+ labs(x = "Date")+ theme(legend.position='none')
NINO34graph+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

ammgraph=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = amm, ymin = 0, ymax = amm, colour = ifelse(amm <0, "blue", "red")))
ammgraph=ammgraph + labs(y = "AMM")+ labs(x = "Date")+ theme(legend.position='none')
ammgraph+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

pdograph=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = pdo, ymin = 0, ymax = pdo, colour = ifelse(pdo <0, "blue", "red")))
pdograph=pdograph + labs(y = "PDO")+ labs(x = "Date")+ theme(legend.position='none')
pdograph+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

meigraph=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = mei, ymin = 0, ymax = mei, colour = ifelse(mei <0, "blue", "red")))
meigraph=meigraph + labs(y = "MEI")+ labs(x = "Date")+ theme(legend.position='none')
meigraph+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

tsa=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = tsa, ymin = 0, ymax = tsa, colour = ifelse(tsa <0, "blue", "red")))
tsa=tsa + labs(y = "TSA")+ labs(x = "Date")+ theme(legend.position='none')
tsa+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

soi=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = soi, ymin = 0, ymax = soi, colour = ifelse(soi <0, "blue", "red")))
soi=soi + labs(y = "SOI")+ labs(x = "Date")+ theme(legend.position='none')
soi+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

oni=ggplot() + geom_linerange(data = global_sst_2016, 
aes(x = date,y = soi, ymin = 0, ymax = oni, colour = ifelse(oni <0, "blue", "red")))
oni=oni + labs(y = "ONI")+ labs(x = "Date")+ theme(legend.position='none')
oni+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

# Graph Global Land Temperature -------
library(ggplot2)

landtemp=ggplot() + geom_linerange(data = global.temp, 
                                      aes(x = date,y = temp, ymin = 0, ymax = temp, colour = ifelse(temp <0, "blue", "red")))
landtemp=landtemp + labs(y = "Global land temp.")+ labs(x = "Date")+ theme(legend.position='none')
landtemp+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

# Put ggplot in one page -------
library(gridExtra)
grid.arrange(NINO12graph, NINO34graph, meigraph, ncol=1)
grid.arrange(ammgraph, pdograph, tsa, soi, oni, ncol=1)


# make anomaly dataset -------
TRMMENSO$month <- format(TRMMENSO$date, "%m")
TRMMENSO$anom <- TRMMENSO$monthlyrain - ave(TRMMENSO$monthlyrain, month, FUN = function(x) mean(x, na.rm = TRUE))
library(reshape2)
TRMMwide=acast(TRMMENSO, date~lon+lat, value.var="anom")

## Monthly mean
TRMMENSO$monthlymean= tapply(TRMMENSO$monthlyrain, list(TRMMENSO$Month), mean, na.rm = TRUE)

TRMMENSO$error=TRMMENSO$monthlyrain-TRMMENSO$monthlymean



# EOF/PCA of Precipitation anom -------
library(spacetime)
library(gstat)
m = stConstruct(TRMMENSO, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
raineof= eof(STFDF[,, "anom"], "spatial", scale.=TRUE, center = TRUE)
raineof.PCs = eof(STFDF[,, "anom"], "spatial", scale.=TRUE, center = TRUE, returnEOFs = FALSE)
summary(raineof.PCs)
plot(raineof.PCs)

raineof.PCs = eof(STFDF[,, "anom"], "spatial", scale.=TRUE, center = TRUE, returnEOFs = FALSE)
varimaxpc2=varimax(raineof.PCs$rotation[, 1:2])
newData= scale(TRMMwide)%*%varimaxpc2$loadings

## Map the leading EOF mode
# Figure 5.17: EOF's
spplot(raineof[1], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")
spplot(raineof[2], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")
spplot(raineof[3], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")


library(xts)
eof.t = xts(predict(raineof.PCs), time_index)
plot(eof.t[,1], main = " ")
plot(eof.t[,2], main = "")


# ... and so on.
names(raineof.PCs)
v = raineof.PCs$sdev^2
w=100*cumsum(v[1:228])/sum(v)

plot(100*cumsum(v[1:100])/sum(v),
ylim=c(30, 100), xlim=c(0, 100),ylab = "Percent", 
xlab = "EOF", main = "")
library(Hmisc)
minor.tick(nx=10, ny=10, tick.ratio = 10)

plot(raineof.PCs$sdev, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', type = "lines", main = " ")
lines(raineof.PCs$sdev)
screeplot(raineof.PCs, npcs = min(10, length(raineof.PCs$sdev)), type = "lines")


# REOF/RPCA of Precipitation anom -------
library(gstat)
library(sp)
library(spacetime)
m = stConstruct(TRMMENSO, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
raineof= eof(STFDF[,, "anom"], "spatial", scale.=TRUE, center = TRUE, retx = TRUE, rank. = 3)
raineof.PCs = eof(STFDF[,, "anom"], "spatial", scale.=TRUE, center = TRUE, retx = TRUE, rank. = 3, returnEOFs = FALSE)
summary(raineof.PCs)
plot(raineof.PCs)




## Map the leading EOF mode
# Figure 5.17: EOF's
spplot(raineof[1], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")
spplot(raineof[2], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")


#Mapping EOF using ggplot2 ----------------------------
raineofdf=as.data.frame(raineof)
fix(raineofdf)
library(ggplot2)
EOF1<- ggplot(raineofdf, aes(lon, lat, z = REOF1), colour = ..level..)
EOF1=EOF1 + geom_raster(aes(fill = REOF1)) +
  scale_fill_gradient(high = "midnightblue", low= "dodgerblue", breaks=c(-0.06, -0.04, -0.02, 0, 0.02, 0.04)) +
  geom_contour(colour = "white")+ggtitle("") +
  labs(x = "Longitude",y = "Latitude")
EOF1+guides(fill = guide_legend(" "))
EOF1

EOF2<- ggplot(raineofdf, aes(lon, lat, z = REOF2), colour = ..level..)
EOF2=EOF2 + geom_raster(aes(fill = REOF2)) +
  scale_fill_gradient(high = "dodgerblue3", low= "yellow", breaks=c(-0.06, -0.04, -0.02, 0, 0.02, 0.04)) +
  geom_contour(colour = "white")+ggtitle("") +
  labs(x = "Longitude",y = "Latitude")
EOF2+guides(fill = guide_legend(" "))
EOF2

EOF3<- ggplot(raineofdf, aes(lon, lat, z = REOF3), colour = ..level..)
EOF3=EOF3 + geom_raster(aes(fill = REOF3)) +
  scale_fill_gradient(high = "dodgerblue", low= "yellow", breaks=c(-0.06, -0.04, -0.02, 0, 0.02, 0.04)) +
  geom_contour(colour = "white")+ggtitle("") +
  labs(x = "Longitude",y = "Latitude")
EOF3+guides(fill = guide_legend(" "))
EOF3


library(xts)
eof.t = xts(predict(raineof.PCs), time_index)
RPC1=plot(eof.t[,1], main = "RPC 1")
RPC1=plot(eof.t[,2], main = "RPC 2")

library(ggplot2)
rainpcdf=as.data.frame(eof.t)

rainpcdf$date <- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month") 
save(rainpcdf, file = "rainpcdf.csv")
save(rainpcdf, file = "rainpcdf.RData")

RPC1=ggplot() + geom_linerange(data = rainpcdf, 
aes(x = date,y = PC1, ymin = 0, ymax = PC1, colour = ifelse(PC1 <0, "blue", "red")))
RPC1=RPC1 + labs(y = "mm")+ labs(x = "Date")+ theme(legend.position='none')+ggtitle("(a)")
RPC1+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

RPC2=ggplot() + geom_linerange(data = rainpcdf, 
                               aes(x = date,y = PC2, ymin = 0, ymax = PC2, colour = ifelse(PC2 <0, "blue", "red")))
RPC2=RPC2 + labs(y = "mm")+ labs(x = "Date")+ theme(legend.position='none')+ggtitle("(b)")
RPC2+ scale_x_date(date_breaks = "2 year", date_labels =  "%Y") 

RPC3=ggplot() + geom_linerange(data = rainpcdf, 
                               aes(x = date,y = PC3, ymin = 0, ymax = PC3, colour = ifelse(PC3 <0, "blue", "red")))
RPC3=RPC3 + labs(y = "mm")+ labs(x = "Date")+ theme(legend.position='none')+ggtitle("(c)")
RPC3+ scale_x_date(date_breaks = "3 year", date_labels =  "%Y") 

# ... and so on.
names(raineof.PCs)
v = raineof.PCs$sdev^2
plot(100*cumsum(v[1:228])/sum(v),
     ylim=c(30, 100), ylab = "Percent", xlab = "EOF", main = "")


plot(raineof.PCs$sdev, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', type = "lines", main = " ")
lines(raineof.PCs$sdev)
screeplot(raineof.PCs, npcs = min(10, length(raineof.PCs$sdev)), type = "lines")


# https://www.rdocumentation.org/packages/spacetime/versions/1.1-5/topics/EOF
## https://stats.stackexchange.com/questions/72421/showing-spatial-and-temporal-correlation-on-maps
# https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/prcomp

# Put REOF/RPC ggplot in one page -------
library(gridExtra)
grid.arrange(EOF1, EOF2, EOF3, RPC1, RPC2,RPC3, ncol=3)

# Merge 2 TS datasets (there is something wrong in this merge)-----------------------------------------------------------
rainglobal$date<- seq(from = as.POSIXct("1998-01-01", tz = 'UTC'), to = as.POSIXct("2016-12-01", tz = 'UTC'), by = "month") 
global.temp=merge(global_sst_2016, temp, by = c("Month", "Year"))
global.temp$date <- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month") 

pc.global.temp=merge(global.temp, rainpcdf, by = "date")
save(global_temp,file = "global_temp.RData")


# Select variables from PC + Global SST Rainfall-------
pc.global.temp.subset <- subset(pc.global.temp, select = c(nino1.2anom, nino3.4anom, amm, pdo, mei, tsa, soi, temp, PC1, PC2, PC3))

#Correlation matrix with significance levels (p-value) -----------
res=cor(pc.global.temp.subset,method="pearson")
round(res, 2)

library("Hmisc")
res2 <- rcorr(as.matrix(pc.global.temp.subset))
res2

#Visualize correlation matrix-----------------
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "pch")
# Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients. In the right side of the correlogram, the legend color shows the correlation coefficients and the corresponding colors.

#Use chart.Correlation()--------------
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(pc.global.temp.subset, histogram=TRUE, pch=19)
#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols("***", "**", "*", ".", " ")

#beautiful LATEX, HTML and ASCII tables from R statistical output -------------
install.packages("stargazer")
library(stargazer)

# Select variables from Global SST Rainfall-------
global.temp.subset <- subset(global.temp, select = 
                               -c(date, Year, Month, NINO1.2, NINO3, nino3anom, NINO4, nino4anom, NINO3.4))


