library(unmarked)
library(tidyverse)
library(rgdal)
library(ggplot2)
library(janitor)
library(plyr)
library(stringr)
library(MASS)

### ADD THE DATE RANGE LIMIT BEFORE RUNNING CODE. ERROR!! ###

###############################################################################
## Load and clean the data ##
###############################################################################

# load in data Willmore
load(file = 'file location here')

# Winter Willmore
camera.trap.data <- bfmin
camera.trap.data <- rename(camera.trap.data, c("Site" = "Parks_ID"))
camera.trap.data <- camera.trap.data[!(is.na(camera.trap.data$Parks_ID) | camera.trap.data$Parks_ID=="" | camera.trap.data$Parks_ID=="55?"), ]

# remove sites: 7,21,22,31,49,53,58 (outlier dates)
camera.trap.data <- camera.trap.data[(camera.trap.data$Parks_ID != '7') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Parks_ID != '21') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Parks_ID != '22') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Parks_ID != '31') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Parks_ID != '49') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Parks_ID != '53') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Parks_ID != '58') ,]

# only species we want
camera.trap.data <- camera.trap.data[(camera.trap.data$Species == 'Wolverine' | 
                                        camera.trap.data$Species == 'Fisher' |
                                        camera.trap.data$Species == 'Marten' |
                                        camera.trap.data$Species == 'Red Fox'), ]

# remove data which falls outside the Date Range
# 2006-12-18 to 2007-03-20
# 2007-12-27 to 2008-03-08
camera.trap.data$Date <- as.Date(camera.trap.data$Date, format = "%d-%B-%y")
camera.trap.data <- camera.trap.data[(camera.trap.data$Date >= "2006-12-29" & camera.trap.data$Date <= "2007-03-08")| 
                                       (camera.trap.data$Date >= "2007-12-29" & camera.trap.data$Date <= "2008-03-08"),]

camera.trap.data <- camera.trap.data[(camera.trap.data$Date != '2008-02-29') ,]

species.site.total <- camera.trap.data

# factor to character & factor to Date
species.site.total$Species <- as.character(species.site.total$Species)
species.site.total$Date <- as.Date(species.site.total$Date, format = "%d-%B-%y")

# sort by date
species.site.total <- species.site.total[order(species.site.total$Date),]

# 2006-12-18 to 2007-03-20
# 2007-12-27 to 2008-03-08
# number of days (Sampling period)
winter.period.1.first.day <- as.Date("2006-12-29")
winter.period.1.last.day <- as.Date("2007-03-09")

winter.period.2.first.day <- as.Date("2007-12-29")
winter.period.2.last.day <- as.Date("2008-03-09")

sampling.period.days = as.numeric(difftime(winter.period.1.last.day, winter.period.1.first.day, units = "days")) + 
  as.numeric(difftime(winter.period.2.last.day, winter.period.2.first.day, units = "days"))

###############################################################################
## Creating dataframes templates for the Matrix code below.
###############################################################################

# placeholder for the days list
days.matrix <-matrix(nrow = sampling.period.days, ncol = 5)
colnames(days.matrix) <- c("Date", "Day_Number","Parks_ID","Species", "Detection")
days.df <- data.frame(days.matrix)

first.year <- seq(winter.period.1.first.day, by = "day", 
                  length.out = as.numeric(difftime(winter.period.1.last.day, winter.period.1.first.day, units = "days")))
second.year <- seq(winter.period.2.first.day, by = "day", 
                   length.out = as.numeric(difftime(winter.period.2.last.day, winter.period.2.first.day, units = "days")))

days.df$Date <- c(first.year, second.year)

# remove feb 29 in winter season 2
days.df <- days.df[-c(133), ]

days.df$Day_Number <- c(1:140)

# add week (10days) # column
days.df$Week <- ''
counter <- 1
week.counter <- 1

for (i in 1:nrow(days.df)){
  days.df$Week[i] <- week.counter
  if (counter%%10==0){
    week.counter <- week.counter + 1
    if (week.counter == 8){
      week.counter <- 1
    }
  }
  counter <- counter + 1
}

# add week # column
days.df$Week_Detection <- '0'


##############################################################################
## WOLVERINE MATRIX CREATION ##
##############################################################################

## The camera traps were active during two Winter seasons, for a total of 24 time periods (240 days/10 day time periods).

## Each column on the matrix represents a 10 day time interval, for a total of 24 columns in calender order. 
## Each row of the matrix represents the camera site it belongs to (66 camera sites = 66 rows).
## e.g) 66 sites and 24 time periods = [66 x 24] Matrix.

## Each Site/time period that has a detection is represented by '1' on the matrix.
## e.g) A wolverine is dectected within time period 1 (December 1st to 10th) at site 16, this would make the matrix entry: [16, 1] = '1'. 

## Each Site/time period that does not have a detection is represented as '0' on the matrix. 
## e.g) No detection at site 25 during time period 2 (December 10th to 20th) would make the matrix entry: [25, 2] = '0'.

## Eventually, the matrix is populated by 1's (detected) and 0's (not-detected) for each time period for every site.


## cougar matrix ##
# Number of Sites
M <- 66

# Number of Weeks
J <- 7

Wolverine.matrix <- matrix(nrow = M, ncol = J)
colnames(Wolverine.matrix) <- c(1:7)

Wolverine.days.total <- days.df

# subset cougar
Wolverine <- species.site.total[which(species.site.total$Species=='Wolverine'), ]

# get number of occurences per day (i.e. these are dates that get a value of 1 on the matrix)
# if there are 50 wolf sightings (images taken) at site 30 on one day, we are only going to count that at one sighting.
Wolverine.dates <- count(Wolverine, c("Date", "Parks_ID", "Species"))
Wolverine.dates$Detection <- '1'

# all sites for cougar
list.of.sites <- data.frame(as.numeric(unique(Wolverine.dates$Parks_ID)))
colnames(list.of.sites) <- "Parks_ID"

for (i in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Parks_ID[i]
  Wolverine.dates.1 <- Wolverine.dates[which(Wolverine.dates$Parks_ID==current.site), ]
  
  # create total days for that site
  Wolverine.days.total.1 <- days.df
  
  counter <- 1
  for (j in 1:nrow(Wolverine.days.total.1)){
    if (Wolverine.days.total.1$Date[j] == Wolverine.dates.1$Date[counter]){
      Wolverine.days.total.1$Parks_ID[j] <- Wolverine.dates.1$Parks_ID[counter]
      Wolverine.days.total.1$Species[j] <- Wolverine.dates.1$Species[counter]
      Wolverine.days.total.1$Detection[j] <- '1'
      if (counter != nrow(Wolverine.dates.1)){
        counter <- counter + 1
      }
    }
    else {
      Wolverine.days.total.1$Parks_ID[j] <- 'No Detection'
      Wolverine.days.total.1$Species[j] <- Wolverine.dates.1$Species[1]
      Wolverine.days.total.1$Detection[j] <- '0'
    }
  }
  
  for (k in 1:nrow(Wolverine.days.total.1)){
    if (Wolverine.days.total.1$Detection[k]=='1'){
      week.number <- Wolverine.days.total.1$Week[k]
      for (m in 1:nrow(Wolverine.days.total.1)){
        if (Wolverine.days.total.1$Week[m]==week.number){
          Wolverine.days.total.1$Week_Detection[m] <- 1
        }
      }
    }
  }
  # condense matrix by $Week_Detection, so it only has 24 rows.
  Wolverine.weeks <- unique(Wolverine.days.total.1[c("Week", "Week_Detection")])
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  Wolverine.matrix[add.site, ] <- Wolverine.weeks$Week_Detection
}

Wolverine.matrix[5, ] <- 0
Wolverine.matrix[7, ] <- 0
Wolverine.matrix[21:22, ] <- 0
Wolverine.matrix[24, ] <- 0
Wolverine.matrix[30:33, ] <- 0
Wolverine.matrix[47:49, ] <- 0
Wolverine.matrix[53, ] <- 0
Wolverine.matrix[58, ] <- 0
Wolverine.matrix[64, ] <- 0
Wolverine.matrix[66, ] <- 0

# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
Wolverine.matrix <- Wolverine.matrix[-7,]

# remove site 21
Wolverine.matrix <- Wolverine.matrix[-20,]

# remove site 22
Wolverine.matrix <- Wolverine.matrix[-20,]

# remove site 31
Wolverine.matrix <- Wolverine.matrix[-28,]

# remove site 49
Wolverine.matrix <- Wolverine.matrix[-45,]

# remove site 53
Wolverine.matrix <- Wolverine.matrix[-48,]

# remove site 58
Wolverine.matrix <- Wolverine.matrix[-52,]

# Convert to numeric Matrix to work with MSOM
Wolverine.matrix.num <- data.frame(Wolverine.matrix)
Wolverine.matrix.numeric <- apply(Wolverine.matrix.num, 2, function(x) as.numeric(as.character(x)))
colnames(Wolverine.matrix.numeric) <- c(1:7)

# Save Matrix
save(Wolverine.matrix.numeric, file = 'file location here')

##############################################################################

## Wolverine MATRIX CREATION ##
## For each site (/66), find out if this animal was detected there. If detected at that site, find out how many different days
## that it was detected there. If there was 1 detection at site 37, add the '1' to the respective day that it was found.
## then, add this 1x1 column to the final matrix as a row. 0000000100000000. For all other sites, populate them with 0's.


## cougar matrix ##
# Number of Sites
M <- 66

# Number of Weeks
J <- 7

Martin.matrix <- matrix(nrow = M, ncol = J)
colnames(Martin.matrix) <- c(1:7)

Martin.days.total <- days.df

# subset cougar
Martin <- species.site.total[which(species.site.total$Species=='Marten'), ]

# get number of occurences per day (i.e. these are dates that get a value of 1 on the matrix)
# if there are 50 wolf sightings (images taken) at site 30 on one day, we are only going to count that at one sighting.
Martin.dates <- count(Martin, c("Date", "Parks_ID", "Species"))
Martin.dates$Detection <- '1'

# all sites for cougar
list.of.sites <- data.frame(as.numeric(unique(Martin.dates$Parks_ID)))
colnames(list.of.sites) <- "Parks_ID"

for (i in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Parks_ID[i]
  Martin.dates.1 <- Martin.dates[which(Martin.dates$Parks_ID==current.site), ]
  
  # create total days for that site
  Martin.days.total.1 <- days.df
  
  counter <- 1
  for (j in 1:nrow(Martin.days.total.1)){
    if (Martin.days.total.1$Date[j] == Martin.dates.1$Date[counter]){
      Martin.days.total.1$Parks_ID[j] <- Martin.dates.1$Parks_ID[counter]
      Martin.days.total.1$Species[j] <- Martin.dates.1$Species[counter]
      Martin.days.total.1$Detection[j] <- '1'
      if (counter != nrow(Martin.dates.1)){
        counter <- counter + 1
      }
    }
    else {
      Martin.days.total.1$Parks_ID[j] <- 'No Detection'
      Martin.days.total.1$Species[j] <- Martin.dates.1$Species[1]
      Martin.days.total.1$Detection[j] <- '0'
    }
  }
  
  for (k in 1:nrow(Martin.days.total.1)){
    if (Martin.days.total.1$Detection[k]=='1'){
      week.number <- Martin.days.total.1$Week[k]
      for (m in 1:nrow(Martin.days.total.1)){
        if (Martin.days.total.1$Week[m]==week.number){
          Martin.days.total.1$Week_Detection[m] <- 1
        }
      }
    }
  }
  # condense matrix by $Week_Detection, so it only has 24 rows.
  Martin.weeks <- unique(Martin.days.total.1[c("Week", "Week_Detection")])
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  Martin.matrix[add.site, ] <- Martin.weeks$Week_Detection
}

Martin.matrix[33, ] <- 0
Martin.matrix[40, ] <- 0
Martin.matrix[43, ] <- 0
Martin.matrix[51, ] <- 0
Martin.matrix[54, ] <- 0
Martin.matrix[66, ] <- 0

# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
Martin.matrix <- Martin.matrix[-7,]

# remove site 21
Martin.matrix <- Martin.matrix[-20,]

# remove site 22
Martin.matrix <- Martin.matrix[-20,]

# remove site 31
Martin.matrix <- Martin.matrix[-28,]

# remove site 49
Martin.matrix <- Martin.matrix[-45,]

# remove site 53
Martin.matrix <- Martin.matrix[-48,]

# remove site 58
Martin.matrix <- Martin.matrix[-52,]

# Convert to numeric Matrix to work with MSOM
Martin.matrix.num <- data.frame(Martin.matrix)
Martin.matrix.numeric <- apply(Martin.matrix.num, 2, function(x) as.numeric(as.character(x)))
colnames(Martin.matrix.numeric) <- c(1:7)

# Save Matrix
save(Martin.matrix.numeric, file = 'file location here')

#####################################################################################################################################


##############################################################################

## Wolverine MATRIX CREATION ##
## For each site (/66), find out if this animal was detected there. If detected at that site, find out how many different days
## that it was detected there. If there was 1 detection at site 37, add the '1' to the respective day that it was found.
## then, add this 1x1 column to the final matrix as a row. 0000000100000000. For all other sites, populate them with 0's.


## cougar matrix ##
# Number of Sites
M <- 66

# Number of Weeks
J <- 7

Fisher.matrix <- matrix(nrow = M, ncol = J)
colnames(Fisher.matrix) <- c(1:7)

Fisher.days.total <- days.df

# subset cougar
Fisher <- species.site.total[which(species.site.total$Species=='Fisher'), ]

# get number of occurences per day (i.e. these are dates that get a value of 1 on the matrix)
# if there are 50 wolf sightings (images taken) at site 30 on one day, we are only going to count that at one sighting.
Fisher.dates <- count(Fisher, c("Date", "Parks_ID", "Species"))
Fisher.dates$Detection <- '1'

# all sites for cougar
list.of.sites <- data.frame(as.numeric(unique(Fisher.dates$Parks_ID)))
colnames(list.of.sites) <- "Parks_ID"

for (i in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Parks_ID[i]
  Fisher.dates.1 <- Fisher.dates[which(Fisher.dates$Parks_ID==current.site), ]
  
  # create total days for that site
  Fisher.days.total.1 <- days.df
  
  counter <- 1
  for (j in 1:nrow(Fisher.days.total.1)){
    if (Fisher.days.total.1$Date[j] == Fisher.dates.1$Date[counter]){
      Fisher.days.total.1$Parks_ID[j] <- Fisher.dates.1$Parks_ID[counter]
      Fisher.days.total.1$Species[j] <- Fisher.dates.1$Species[counter]
      Fisher.days.total.1$Detection[j] <- '1'
      if (counter != nrow(Fisher.dates.1)){
        counter <- counter + 1
      }
    }
    else {
      Fisher.days.total.1$Parks_ID[j] <- 'No Detection'
      Fisher.days.total.1$Species[j] <- Fisher.dates.1$Species[1]
      Fisher.days.total.1$Detection[j] <- '0'
    }
  }
  
  for (k in 1:nrow(Fisher.days.total.1)){
    if (Fisher.days.total.1$Detection[k]=='1'){
      week.number <- Fisher.days.total.1$Week[k]
      for (m in 1:nrow(Fisher.days.total.1)){
        if (Fisher.days.total.1$Week[m]==week.number){
          Fisher.days.total.1$Week_Detection[m] <- 1
        }
      }
    }
  }
  # condense matrix by $Week_Detection, so it only has 24 rows.
  Fisher.weeks <- unique(Fisher.days.total.1[c("Week", "Week_Detection")])
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  Fisher.matrix[add.site, ] <- Fisher.weeks$Week_Detection
}

Fisher.matrix[1:34, ] <- 0
Fisher.matrix[37:38, ] <- 0
Fisher.matrix[40, ] <- 0
Fisher.matrix[43:49, ] <- 0
Fisher.matrix[53:54, ] <- 0
Fisher.matrix[57, ] <- 0
Fisher.matrix[58, ] <- 0
Fisher.matrix[59, ] <- 0
Fisher.matrix[62:66, ] <- 0


# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
Fisher.matrix <- Fisher.matrix[-7,]

# remove site 21
Fisher.matrix <- Fisher.matrix[-20,]

# remove site 22
Fisher.matrix <- Fisher.matrix[-20,]

# remove site 31
Fisher.matrix <- Fisher.matrix[-28,]

# remove site 49
Fisher.matrix <- Fisher.matrix[-45,]

# remove site 53
Fisher.matrix <- Fisher.matrix[-48,]

# remove site 58
Fisher.matrix <- Fisher.matrix[-52,]

# Convert to numeric Matrix to work with MSOM
Fisher.matrix.num <- data.frame(Fisher.matrix)
Fisher.matrix.numeric <- apply(Fisher.matrix.num, 2, function(x) as.numeric(as.character(x)))
colnames(Fisher.matrix.numeric) <- c(1:7)

# Save Matrix
save(Fisher.matrix.numeric, file = 'file location here')

#####################################################################################################################################
##############################################################################

## Wolverine MATRIX CREATION ##
## For each site (/66), find out if this animal was detected there. If detected at that site, find out how many different days
## that it was detected there. If there was 1 detection at site 37, add the '1' to the respective day that it was found.
## then, add this 1x1 column to the final matrix as a row. 0000000100000000. For all other sites, populate them with 0's.


## cougar matrix ##
# Number of Sites
M <- 66

# Number of Weeks
J <- 7

redfox.matrix <- matrix(nrow = M, ncol = J)
colnames(redfox.matrix) <- c(1:7)

redfox.days.total <- days.df

# subset cougar
redfox <- species.site.total[which(species.site.total$Species=='Red Fox'), ]

# get number of occurences per day (i.e. these are dates that get a value of 1 on the matrix)
# if there are 50 wolf sightings (images taken) at site 30 on one day, we are only going to count that at one sighting.
redfox.dates <- count(redfox, c("Date", "Parks_ID", "Species"))
redfox.dates$Detection <- '1'

# all sites for cougar
list.of.sites <- data.frame(as.numeric(unique(redfox.dates$Parks_ID)))
colnames(list.of.sites) <- "Parks_ID"

for (i in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Parks_ID[i]
  redfox.dates.1 <- redfox.dates[which(redfox.dates$Parks_ID==current.site), ]
  
  # create total days for that site
  redfox.days.total.1 <- days.df
  
  counter <- 1
  for (j in 1:nrow(redfox.days.total.1)){
    if (redfox.days.total.1$Date[j] == redfox.dates.1$Date[counter]){
      redfox.days.total.1$Parks_ID[j] <- redfox.dates.1$Parks_ID[counter]
      redfox.days.total.1$Species[j] <- redfox.dates.1$Species[counter]
      redfox.days.total.1$Detection[j] <- '1'
      if (counter != nrow(redfox.dates.1)){
        counter <- counter + 1
      }
    }
    else {
      redfox.days.total.1$Parks_ID[j] <- 'No Detection'
      redfox.days.total.1$Species[j] <- redfox.dates.1$Species[1]
      redfox.days.total.1$Detection[j] <- '0'
    }
  }
  
  for (k in 1:nrow(redfox.days.total.1)){
    if (redfox.days.total.1$Detection[k]=='1'){
      week.number <- redfox.days.total.1$Week[k]
      for (m in 1:nrow(redfox.days.total.1)){
        if (redfox.days.total.1$Week[m]==week.number){
          redfox.days.total.1$Week_Detection[m] <- 1
        }
      }
    }
  }
  # condense matrix by $Week_Detection, so it only has 24 rows.
  redfox.weeks <- unique(redfox.days.total.1[c("Week", "Week_Detection")])
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  redfox.matrix[add.site, ] <- redfox.weeks$Week_Detection
}

redfox.matrix[1, ] <- 0
redfox.matrix[3, ] <- 0
redfox.matrix[5, ] <- 0
redfox.matrix[7, ] <- 0
redfox.matrix[9, ] <- 0
redfox.matrix[11:31, ] <- 0
redfox.matrix[33:34, ] <- 0
redfox.matrix[37:38, ] <- 0
redfox.matrix[41:42, ] <- 0
redfox.matrix[45:47, ] <- 0
redfox.matrix[49:50, ] <- 0
redfox.matrix[53:56, ] <- 0
redfox.matrix[58:60, ] <- 0
redfox.matrix[62, ] <- 0
redfox.matrix[65:66, ] <- 0


# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
redfox.matrix <- redfox.matrix[-7,]

# remove site 21
redfox.matrix <- redfox.matrix[-20,]

# remove site 22
redfox.matrix <- redfox.matrix[-20,]

# remove site 31
redfox.matrix <- redfox.matrix[-28,]

# remove site 49
redfox.matrix <- redfox.matrix[-45,]

# remove site 53
redfox.matrix <- redfox.matrix[-48,]

# remove site 58
redfox.matrix <- redfox.matrix[-52,]

# Convert to numeric Matrix to work with MSOM
redfox.matrix.num <- data.frame(redfox.matrix)
redfox.matrix.numeric <- apply(redfox.matrix.num, 2, function(x) as.numeric(as.character(x)))
colnames(redfox.matrix.numeric) <- c(1:7)

# Save Matrix
save(redfox.matrix.numeric, file = 'file location here')


####################################################################################################################################
## Landcover Covariate

# load in camera trap locations
camera.trap.locations <- data.frame(readOGR('file location here'))

# Remove outlier sites (7,21,22,31,49,53,58)
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$ORIG_OID != '7'),]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$ORIG_OID != '21'),]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$ORIG_OID != '22'),]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$ORIG_OID != '31'),]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$ORIG_OID != '49'),]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$ORIG_OID != '53'),]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$ORIG_OID != '58'),]

#remove unwanted rows and rename camera ID column
camera.trap.locations <- camera.trap.locations[c("ELEVATION", "LANDCOVER1")]
camera.trap.locations <- rename(camera.trap.locations, c("LANDCOVER1" = "LANDCOVER"))


# (COVS)
occ_covs <- as.data.frame(camera.trap.locations)


save(occ_covs, file = 'file location here')

##############################################################################

##MSOM

y <- list(
  coyote.matrix.numeric,    # Coyote
  red.fox.matrix.numeric,   # Red Fox
  wolf.matrix.numeric,      # Wolf
  wolverine.matrix.numeric) # Wolverine

names(y) <- c('Coyote','Red Fox', 'Wolf', 'Wolverine')

# landcover covariates
site.covs <- occ_covs
data <- unmarkedFrameOccuMulti(y=y, siteCovs=site.covs, obsCovs=NULL)

#no covariates
data <- unmarkedFrameOccuMulti(y=y, siteCovs=NULL, obsCovs=NULL)

summary(data)
plot(data)



# Formulas for state and detection processes, Length should match number/order of columns in fDesign
View(data@fDesign)

# M1 = species occur independently and the occupancy probabilities of each species are a function
#      of only landscape character (landcover).
occFormulas <- c('~land_cover','~land_cover','~land_cover','~land_cover','~0','~0','~0','~0','~0','~0', '~0','~0','~0', '~0', '~0')

# M2 = Still assumes that occupancies are a function of landscape character 
#       but additionally that  they are conditional on the presence absence of each other species.
# Coyote-Redfox
occFormulas <- c('~1','~1','~1','~1','~land_cover','~0','~0','~0','~0','~0','~0','~0','~0', '~0', '~0')
# Coyote-Wolf 
occFormulas <- c('~1','~1','~1','~1','~0','~land_cover','~0','~0','~0','~0','~0','~0','~0', '~0', '~0')
# Coyote-Wolverine
occFormulas <- c('~1','~1','~1','~1','~0','~0','~land_cover','~0','~0','~0','~0','~0','~0', '~0', '~0')
# Redfox-Wolverine
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~land_cover','~0','~0','~0','~0', '~0', '~0')
# Wolf-Wolverine
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~0','~land_cover','~0','~0','~0', '~0', '~0')
# Redfox-Wolf
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~0','~0','~land_cover','~0','~0', '~0', '~0')

#M3


# no covariates
occFormulas <- c('~1','~1','~1','~1','~0','~0','~0','~0','~0','~0', '~0','~0','~0', '~0', '~0')

#Length should match number/order of species in data@ylist
View(data@ylist)
detFormulas <- c('~1','~1','~1','~1')

# fit model
fit <- occuMulti(detFormulas,occFormulas,data)

#Look at output
fit

plot(fit)

summary(fit)
SE(fit)

names(fit)

#conditional occupancy
head(predict(fit,'state',species='Wolf',cond='Coyote')) #wolf | coyote present
head(predict(fit,'state',species='Wolf',cond='-Coyote')) #coyote absent


#############
# If there ARE covariates, then you need to specify the covariate values at which to make the 
# back transformation. You can do that using the linearComb function (see the unmarked manual here).
#Get the estimates for detection
backTransform(fit['det'])
#Get the estimates for occupancy
backTransform(fit['state'])