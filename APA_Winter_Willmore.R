library(unmarked)
library(tidyverse)
library(rgdal)
library(ggplot2)
library(janitor)
library(plyr)
library(stringr)
library(MASS)
library(Matrix)
library(ggplot2)
library(overlap)
library(tidyverse)
library(plyr)
library(rgdal)
library(tidyverse)
library(survival)
library(survminer)
library(overlap)
library(sp)
library(maptools)
# Winter Willmore

# load in data Willmore
load(file = 'file location here')
camera.trap.data <- bfmin
camera.trap.data <- rename(camera.trap.data, c("Site" = "Camera_ID"))
camera.trap.data <- camera.trap.data[!(is.na(camera.trap.data$Camera_ID) | camera.trap.data$Camera_ID=="" | camera.trap.data$Camera_ID=="55?"), ]

# load in camera trap locations
camera.trap.locations <- data.frame(readOGR('file location here'))

#remove unwanted rows and rename camera ID column
camera.trap.locations <- camera.trap.locations[c("ORIG_OID", "ELEVATION", "LANDCOVER1", "UTMX", "UTMY")]
camera.trap.locations <- rename(camera.trap.locations, c("ORIG_OID" = "Camera_ID"))

# merge datasets
camera.trap.data <- merge(camera.trap.data, camera.trap.locations, by="Camera_ID")

# remove data which falls outside the Date Range
# 2006-12-18 to 2007-03-20
# 2007-12-27 to 2008-03-08
camera.trap.data$Date <- as.Date(camera.trap.data$Date, format = "%d-%B-%y")
camera.trap.data <- camera.trap.data[(camera.trap.data$Date >= "2006-12-18" & camera.trap.data$Date <= "2007-03-20")| 
                                       (camera.trap.data$Date >= "2007-12-27" & camera.trap.data$Date <= "2008-03-08"),]

# remove sites: 7,21,22,31,49,53,58 (outlier dates)
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '7') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '21') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '22') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '31') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '49') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '53') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '58') ,]

#add status column '1' for now
camera.trap.data$Status <- '1'

# convert species column to character
camera.trap.data$Species <- as.character(camera.trap.data$Species)

# Start wolf pairing process
tte.species.df <- camera.trap.data

# Sort by Camera_ID, then date time
tte.species.df <- tte.species.df[order(as.numeric(tte.species.df$Camera_ID), tte.species.df$date_time),]

#####################
# Solar Time Column #
#####################
# willmore timezone: GMT-7 (Mountain Standard Time)

# lat/long coordinates
utmcoor<-SpatialPoints(cbind(camera.trap.data$UTMX,camera.trap.data$UTMY), proj4string=CRS("+proj=utm +zone=11"))
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
camera.trap.data$long <- coordinates(longlatcoor)[,1]
camera.trap.data$lat <- coordinates(longlatcoor)[,2]

# coordinates
coord.df <- data.frame(camera.trap.data$long, camera.trap.data$lat)
Coords <- sp::SpatialPoints(coord.df, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# convert to decimal hours (3:30 to 15.5)
#x.lt <- as.POSIXlt(camera.trap.data$date_time)
#x.hhdd <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
#camera.trap.data$hhdd <- x.hhdd

# Convert dates to POSIXct object with the right timezone
Dates <- as.POSIXct(camera.trap.data$Date, tz="GMT-7")

# time to TimeRad 
dat<-c(camera.trap.data$Time)
dec_time <- sapply(strsplit(dat,":"),
                   function(x) {
                     x <- as.numeric(x)
                     (x[1]*60+x[2]+x[3])/1440
                   }
)
range(dec_time)
timeRad <- dec_time * 2 * pi
camera.trap.data$TimeRad <- timeRad

st <- sunTime(camera.trap.data$TimeRad, Dates, Coords)


par(mfrow=2:1)
densityPlot(st, col='red', lwd=2, xaxt='n', main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(camera.trap.data$TimeRad, lwd=2, main="Clock time")
par(mfrow=c(1,1))

camera.trap.data$suntime <- st


# Start wolf pairing process
tte.species.df <- camera.trap.data

# Sort by Camera_ID, then date time
tte.species.df <- tte.species.df[order(as.numeric(tte.species.df$Camera_ID), tte.species.df$date_time),]

save(tte.species.df, file = 'file location here')



###############################################################################
#Begin Temporal Analysis 

# Time is in days, ie. 0 to 1
date.range.species$decimal_Time <- sapply(strsplit(date.range.species$Time,":"),
                                          function(x) {
                                            x <- as.numeric(x)
                                            (x[1]*60+x[2]+x[3])/1440
                                          }
)

# Check time is from 0 to 1
# range(date.range.species$decimal_Time)

# Convert time to radians 
date.range.species$radians_Time <- date.range.species$decimal_Time * 2*pi

clean.date.range <- date.range.species


###############################################################################
### Example from the Documentation for 'Overlap' ###

# Extract data for wolf & wolverine
speciesA <- clean.date.range[which(clean.date.range$Species=='Wolverine'), ]
speciesB <- clean.date.range[which(clean.date.range$Species=='Marten'), ]

# Check sample sizes:
length(speciesA$radians_Time)
length(speciesB$radians_Time)

# Plot the data:
overlapPlot(speciesA$radians_Time, speciesB$radians_Time)
legend('topleft', c("Wolverine", "Marten"), lty=c(1, 2), col=c("black", "blue"), bty='n')


#########################################################################################
# Not sure what this code does, but it works? more to dig into...

# Check sample sizes:
length(speciesA$radians_Time)
length(speciesB$radians_Time) 
# If the smaller sample is less than 50, Dhat1 gives the best estimates, together with
# confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.

# Calculate estimates of overlap:
( Dhat1 <- overlapEst(speciesA$radians_Time, speciesB$radians_Time, type="Dhat1") )

# Do 999 smoothed bootstrap values:
bs <- bootstrap(speciesA$radians_Time, speciesB$radians_Time, 999, type="Dhat1")
mean(bs)
hist(bs)
abline(v=Dhat1, col='red', lwd=2)
abline(v=mean(bs), col='blue', lwd=2, lty=3)

# Get confidence intervals:
bootCI(Dhat1, bs)['norm0', ]
bootCI(Dhat1, bs)['basic0', ]


