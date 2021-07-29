library(tidyverse)
library(plyr)
library(rgdal)
library(tidyverse)
library(survival)
library(survminer)
library(overlap)
library(sp)
library(maptools)
library(data.table)

# Winter Willmore

# load in data Willmore
load(file = 'file location here')
camera.trap.data <- bfmin
camera.trap.data <- rename(camera.trap.data, c("Site" = "Camera_ID"))
camera.trap.data <- camera.trap.data[!(is.na(camera.trap.data$Camera_ID) | camera.trap.data$Camera_ID=="" | camera.trap.data$Camera_ID=="55?"), ]

# load in camera trap locations
camera.trap.locations <- data.frame(readOGR('file location here'))

#remove unwanted rows and rename camera ID column
camera.trap.locations <- camera.trap.locations[c("ORIG_OID", "ELEVATION", "LANDCOVER1", "TRI", "UTMX", "UTMY")]
camera.trap.locations <- rename(camera.trap.locations, c("ORIG_OID" = "Camera_ID"))
camera.trap.locations$Camera_ID_2 <- seq(1, 66, by = 1)


# merge datasets
camera.trap.data <- merge(camera.trap.data, camera.trap.locations, by="Camera_ID")

# remove data which falls outside the Date Range
# 2006-12-18 to 2007-03-20
# 2007-12-27 to 2008-03-08
camera.trap.data$Date <- as.Date(camera.trap.data$Date, format = "%d-%B-%y")
camera.trap.data <- camera.trap.data[(camera.trap.data$Date >= "2006-12-29" & camera.trap.data$Date <= "2007-03-08")| 
                                       (camera.trap.data$Date >= "2007-12-29" & camera.trap.data$Date <= "2008-03-08"),]

camera.trap.data <- camera.trap.data[(camera.trap.data$Date != '2008-02-29') ,]

# remove sites: 7,21,22,31,49,53,58 (outlier dates)
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '7') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '21') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '22') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '31') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '49') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '53') ,]
camera.trap.data <- camera.trap.data[(camera.trap.data$Camera_ID != '58') ,]

camera.trap.data$Species <- as.character(camera.trap.data$Species)

# remove sites: 7,21,22,31,49,53,58 (outlier dates)
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$Camera_ID != '7') ,]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$Camera_ID != '21') ,]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$Camera_ID != '22') ,]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$Camera_ID != '31') ,]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$Camera_ID != '49') ,]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$Camera_ID != '53') ,]
camera.trap.locations <- camera.trap.locations[(camera.trap.locations$Camera_ID != '58') ,]
#red.fox.data <- camera.trap.data[(camera.trap.data$Species == 'Red Fox'),]
#red.fox.data <- red.fox.data[order(as.numeric(red.fox.data$Camera_ID), red.fox.data$date_time),]
#red.fox.dt <- as.data.table(red.fox.data)

#wolverine.data <- camera.trap.data[(camera.trap.data$Species == 'Wolverine'),]
#wolverine.data <- wolverine.data[order(as.numeric(wolverine.data$Camera_ID), wolverine.data$date_time),]
#wolverine.dt <- as.data.table(wolverine.data)

#red.fox.dt <- red.fox.dt[, .N, by = Date]
#wolverine.dt <- wolverine.dt[, .N, by = Date]

#marten.dt <- marten.dt[, .N, by = Date]
#marten.dt <- marten.dt[order(as.numeric(marten.dt$Date)),]
#marten.dt$Date <- as.Date(marten.dt$Date)


###############################################################################
## Creating dataframes for days
###############################################################################

# number of days (Sampling period)
winter.period.1.first.day <- as.Date("2006-12-29")
winter.period.1.last.day <- as.Date("2007-03-09")

winter.period.2.first.day <- as.Date("2007-12-29")
winter.period.2.last.day <- as.Date("2008-03-09")

sampling.period.days = as.numeric(difftime(winter.period.1.last.day, winter.period.1.first.day, units = "days")) + 
  as.numeric(difftime(winter.period.2.last.day, winter.period.2.first.day, units = "days"))

# placeholder for the days list
days.matrix <-matrix(nrow = sampling.period.days, ncol = 2)
colnames(days.matrix) <- c("Group.date", "Total")
days.df <- data.frame(days.matrix)

first.year <- seq(winter.period.1.first.day, by = "day", 
                  length.out = as.numeric(difftime(winter.period.1.last.day, winter.period.1.first.day, units = "days")))
second.year <- seq(winter.period.2.first.day, by = "day", 
                   length.out = as.numeric(difftime(winter.period.2.last.day, winter.period.2.first.day, units = "days")))

days.df$Group.date <- c(first.year, second.year)

# remove feb 29 in winter season 2
days.df <- days.df[-c(133), ]

days.df$Total <- '0'


# add week (10days) # column
days.df$Week <- ''
counter <- 1
week.counter <- 1

for (i in 1:nrow(days.df)){
  days.df$Week[i] <- week.counter
  if (counter%%5==0){
    week.counter <- week.counter + 1
    if (week.counter == 15){
      week.counter <- 1
    }
  }
  counter <- counter + 1
}

###############################################################################
## Creating dataframes for Marten (Double check random sites to make sure this worked past site #1)
###############################################################################

## marten matrix ##
# Number of Sites ##
M <- 66

# Number of Weeks
J <- 14

Marten.matrix <- matrix(nrow = M, ncol = J)
colnames(Marten.matrix) <- c(1:14)

marten.data <- camera.trap.data[(camera.trap.data$Species == 'Marten'),]

list.of.sites <- data.frame(as.numeric(unique(marten.data$Camera_ID)))
colnames(list.of.sites) <- "Camera_ID"

# sort by ID
list.of.sites <- data.frame(list.of.sites[order(list.of.sites$Camera_ID),])
colnames(list.of.sites) <- "Camera_ID"

for (k in 1:nrow(list.of.sites)){
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  
  marten.test <- marten.data[(marten.data$Camera_ID == current.site),]
  
  marten.test1 <- aggregate(x = marten.test[c("Total")],
                            FUN = sum,
                            by = list(Group.date = marten.test$Date))
  
  marten.test1 <- marten.test1[order(marten.test1$Group.date),]
  marten.days <- days.df[order(days.df$Group.date),]
  
  for (i in 1:nrow(marten.days)) {
    date.looking.for <- marten.days$Group.date[i]
    
    for (j in 1:nrow(marten.test1)) {
      if (date.looking.for == marten.test1$Group.date[j]){
        found.total <- marten.test1$Total[j]
        marten.days$Total[i] <- found.total
        break
      } else {
        marten.days$Total[i] <- 0
      }
    }
  }
  
  period.totals <- data.frame(time.period = c(seq(1:14)), total = '0')
  period.totals$total <- as.numeric(period.totals$total)
  marten.test1$Total <- as.numeric(marten.test1$Total)
  marten.days$Total <- as.numeric(marten.days$Total)
  marten.days$Week <- as.numeric(marten.days$Week)
  
  marten.days <- marten.days[order(marten.days$Week),]
  
  period.total <- 0
  counter2 <- 1
  
  for (i in 1:nrow(marten.days)) {
    
    if (i%%10 == 0){
      period.total <- period.total + marten.days$Total[i]
      #period.totals$total[counter2] <- period.total/10
      period.totals$total[counter2] <- period.total
      counter2 <- counter2 + 1
      period.total <- 0
    } else if (i%%10 != 0){
      period.total <- period.total + marten.days$Total[i]
    }else if (counter2 == 15){
      break
    }
  }
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  Marten.matrix[add.site, ] <- period.totals$total
  
}

# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
Marten.matrix <- Marten.matrix[-7,]

# remove site 21
Marten.matrix <- Marten.matrix[-20,]

# remove site 22
Marten.matrix <- Marten.matrix[-20,]

# remove site 31
Marten.matrix <- Marten.matrix[-28,]

# remove site 49
Marten.matrix <- Marten.matrix[-45,]

# remove site 53
Marten.matrix <- Marten.matrix[-48,]

# remove site 58
Marten.matrix <- Marten.matrix[-52,]

Marten.matrix[29, ] <- 0
Marten.matrix[36, ] <- 0
Marten.matrix[39, ] <- 0
Marten.matrix[46, ] <- 0
Marten.matrix[48, ] <- 0
Marten.matrix[59, ] <- 0

###############################################################################
## Creating dataframes for Wolverine (Double check random sites to make sure this worked past site #1)
###############################################################################

## marten matrix ##
# Number of Sites ##
M <- 66

# Number of Weeks
J <- 14

wolverine.matrix <- matrix(nrow = M, ncol = J)
colnames(wolverine.matrix) <- c(1:14)

wolverine.data <- camera.trap.data[(camera.trap.data$Species == 'Wolverine'),]

list.of.sites <- data.frame(as.numeric(unique(wolverine.data$Camera_ID)))
colnames(list.of.sites) <- "Camera_ID"

# sort by ID
list.of.sites <- data.frame(list.of.sites[order(list.of.sites$Camera_ID),])
colnames(list.of.sites) <- "Camera_ID"

for (k in 1:nrow(list.of.sites)){
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  
  wolverine.test <- wolverine.data[(wolverine.data$Camera_ID == current.site),]
  
  wolverine.test1 <- aggregate(x = wolverine.test[c("Total")],
                               FUN = sum,
                               by = list(Group.date = wolverine.test$Date))
  
  wolverine.test1 <- wolverine.test1[order(wolverine.test1$Group.date),]
  wolverine.days <- days.df[order(days.df$Group.date),]
  
  for (i in 1:nrow(wolverine.days)) {
    date.looking.for <- wolverine.days$Group.date[i]
    
    for (j in 1:nrow(wolverine.test1)) {
      if (date.looking.for == wolverine.test1$Group.date[j]){
        found.total <- wolverine.test1$Total[j]
        wolverine.days$Total[i] <- found.total
        break
      } else {
        wolverine.days$Total[i] <- 0
      }
    }
  }
  
  period.totals <- data.frame(time.period = c(seq(1:14)), total = '0')
  period.totals$total <- as.numeric(period.totals$total)
  wolverine.test1$Total <- as.numeric(wolverine.test1$Total)
  wolverine.days$Total <- as.numeric(wolverine.days$Total)
  wolverine.days$Week <- as.numeric(wolverine.days$Week)
  
  wolverine.days <- wolverine.days[order(wolverine.days$Week),]
  
  period.total <- 0
  counter2 <- 1
  
  for (i in 1:nrow(wolverine.days)) {
    
    if (i%%10 == 0){
      period.total <- period.total + wolverine.days$Total[i]
      #period.totals$total[counter2] <- period.total/10
      period.totals$total[counter2] <- period.total
      counter2 <- counter2 + 1
      period.total <- 0
    } else if (i%%10 != 0){
      period.total <- period.total + wolverine.days$Total[i]
    }else if (counter2 == 15){
      break
    }
  }
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  wolverine.matrix[add.site, ] <- period.totals$total
  
}



# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
wolverine.matrix <- wolverine.matrix[-7,]

# remove site 21
wolverine.matrix <- wolverine.matrix[-20,]

# remove site 22
wolverine.matrix <- wolverine.matrix[-20,]

# remove site 31
wolverine.matrix <- wolverine.matrix[-28,]

# remove site 49
wolverine.matrix <- wolverine.matrix[-45,]

# remove site 53
wolverine.matrix <- wolverine.matrix[-48,]

# remove site 58
wolverine.matrix <- wolverine.matrix[-52,]

wolverine.matrix[5, ] <- 0
wolverine.matrix[21, ] <- 0
wolverine.matrix[27:29, ] <- 0
wolverine.matrix[43:44, ] <- 0
wolverine.matrix[57, ] <- 0
wolverine.matrix[59, ] <- 0

###############################################################################
## Creating dataframes for lynx (Double check random sites to make sure this worked past site #1)
###############################################################################

## marten matrix ##
# Number of Sites ##
M <- 66

# Number of Weeks
J <- 14

lynx.matrix <- matrix(nrow = M, ncol = J)
colnames(lynx.matrix) <- c(1:14)

lynx.data <- camera.trap.data[(camera.trap.data$Species == 'Lynx'),]

list.of.sites <- data.frame(as.numeric(unique(lynx.data$Camera_ID)))
colnames(list.of.sites) <- "Camera_ID"

# sort by ID
list.of.sites <- data.frame(list.of.sites[order(list.of.sites$Camera_ID),])
colnames(list.of.sites) <- "Camera_ID"

for (k in 1:nrow(list.of.sites)){
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  
  lynx.test <- lynx.data[(lynx.data$Camera_ID == current.site),]
  
  lynx.test1 <- aggregate(x = lynx.test[c("Total")],
                          FUN = sum,
                          by = list(Group.date = lynx.test$Date))
  
  lynx.test1 <- lynx.test1[order(lynx.test1$Group.date),]
  lynx.days <- days.df[order(days.df$Group.date),]
  
  for (i in 1:nrow(lynx.days)) {
    date.looking.for <- lynx.days$Group.date[i]
    
    for (j in 1:nrow(lynx.test1)) {
      if (date.looking.for == lynx.test1$Group.date[j]){
        found.total <- lynx.test1$Total[j]
        lynx.days$Total[i] <- found.total
        break
      } else {
        lynx.days$Total[i] <- 0
      }
    }
  }
  
  period.totals <- data.frame(time.period = c(seq(1:14)), total = '0')
  period.totals$total <- as.numeric(period.totals$total)
  lynx.test1$Total <- as.numeric(lynx.test1$Total)
  lynx.days$Total <- as.numeric(lynx.days$Total)
  lynx.days$Week <- as.numeric(lynx.days$Week)
  
  lynx.days <- lynx.days[order(lynx.days$Week),]
  
  period.total <- 0
  counter2 <- 1
  
  for (i in 1:nrow(lynx.days)) {
    
    if (i%%10 == 0){
      period.total <- period.total + lynx.days$Total[i]
      #period.totals$total[counter2] <- period.total/10
      period.totals$total[counter2] <- period.total
      counter2 <- counter2 + 1
      period.total <- 0
    } else if (i%%10 != 0){
      period.total <- period.total + lynx.days$Total[i]
    }else if (counter2 == 15){
      break
    }
  }
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  lynx.matrix[add.site, ] <- period.totals$total
  
}



# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
lynx.matrix <- lynx.matrix[-7,]

# remove site 21
lynx.matrix <- lynx.matrix[-20,]

# remove site 22
lynx.matrix <- lynx.matrix[-20,]

# remove site 31
lynx.matrix <- lynx.matrix[-28,]

# remove site 49
lynx.matrix <- lynx.matrix[-45,]

# remove site 53
lynx.matrix <- lynx.matrix[-48,]

# remove site 58
lynx.matrix <- lynx.matrix[-52,]

lynx.matrix[1:7, ] <- 0
lynx.matrix[9:12, ] <- 0
lynx.matrix[14:24, ] <- 0
lynx.matrix[26:28, ] <- 0
lynx.matrix[30:32, ] <- 0
lynx.matrix[34:35, ] <- 0
lynx.matrix[37:38, ] <- 0
lynx.matrix[40:47, ] <- 0
lynx.matrix[51:59, ] <- 0


###############################################################################
## Creating dataframes for Short-tailed Weasel (Double check random sites to make sure this worked past site #1)
###############################################################################

## marten matrix ##
# Number of Sites ##
M <- 66

# Number of Weeks
J <- 14

weasel.matrix <- matrix(nrow = M, ncol = J)
colnames(weasel.matrix) <- c(1:14)

weasel.data <- camera.trap.data[(camera.trap.data$Species == 'Short-tailed Weasel'),]

list.of.sites <- data.frame(as.numeric(unique(weasel.data$Camera_ID)))
colnames(list.of.sites) <- "Camera_ID"

# sort by ID
list.of.sites <- data.frame(list.of.sites[order(list.of.sites$Camera_ID),])
colnames(list.of.sites) <- "Camera_ID"

for (k in 1:nrow(list.of.sites)){
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  
  weasel.test <- weasel.data[(weasel.data$Camera_ID == current.site),]
  
  weasel.test1 <- aggregate(x = weasel.test[c("Total")],
                            FUN = sum,
                            by = list(Group.date = weasel.test$Date))
  
  weasel.test1 <- weasel.test1[order(weasel.test1$Group.date),]
  weasel.days <- days.df[order(days.df$Group.date),]
  
  for (i in 1:nrow(weasel.days)) {
    date.looking.for <- weasel.days$Group.date[i]
    
    for (j in 1:nrow(weasel.test1)) {
      if (date.looking.for == weasel.test1$Group.date[j]){
        found.total <- weasel.test1$Total[j]
        weasel.days$Total[i] <- found.total
        break
      } else {
        weasel.days$Total[i] <- 0
      }
    }
  }
  
  period.totals <- data.frame(time.period = c(seq(1:14)), total = '0')
  period.totals$total <- as.numeric(period.totals$total)
  weasel.test1$Total <- as.numeric(weasel.test1$Total)
  weasel.days$Total <- as.numeric(weasel.days$Total)
  weasel.days$Week <- as.numeric(weasel.days$Week)
  
  weasel.days <- weasel.days[order(weasel.days$Week),]
  
  period.total <- 0
  counter2 <- 1
  
  for (i in 1:nrow(weasel.days)) {
    
    if (i%%10 == 0){
      period.total <- period.total + weasel.days$Total[i]
      #period.totals$total[counter2] <- period.total/10
      period.totals$total[counter2] <- period.total
      counter2 <- counter2 + 1
      period.total <- 0
    } else if (i%%10 != 0){
      period.total <- period.total + weasel.days$Total[i]
    }else if (counter2 == 15){
      break
    }
  }
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  weasel.matrix[add.site, ] <- period.totals$total
  
}



# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
weasel.matrix <- weasel.matrix[-7,]

# remove site 21
weasel.matrix <- weasel.matrix[-20,]

# remove site 22
weasel.matrix <- weasel.matrix[-20,]

# remove site 31
weasel.matrix <- weasel.matrix[-28,]

# remove site 49
weasel.matrix <- weasel.matrix[-45,]

# remove site 53
weasel.matrix <- weasel.matrix[-48,]

# remove site 58
weasel.matrix <- weasel.matrix[-52,]

weasel.matrix[1:27, ] <- 0
weasel.matrix[30:31, ] <- 0
weasel.matrix[33, ] <- 0
weasel.matrix[35, ] <- 0
weasel.matrix[37:38, ] <- 0
weasel.matrix[41, ] <- 0
weasel.matrix[45, ] <- 0
weasel.matrix[47, ] <- 0
weasel.matrix[52:59, ] <- 0

###############################################################################
## Creating dataframes for Wolf (Double check random sites to make sure this worked past site #1)
###############################################################################

## marten matrix ##
# Number of Sites ##
M <- 66

# Number of Weeks
J <- 14

wolf.matrix <- matrix(nrow = M, ncol = J)
colnames(wolf.matrix) <- c(1:14)

wolf.data <- camera.trap.data[(camera.trap.data$Species == 'Wolf'),]

list.of.sites <- data.frame(as.numeric(unique(wolf.data$Camera_ID)))
colnames(list.of.sites) <- "Camera_ID"

# sort by ID
list.of.sites <- data.frame(list.of.sites[order(list.of.sites$Camera_ID),])
colnames(list.of.sites) <- "Camera_ID"

for (k in 1:nrow(list.of.sites)){
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  
  wolf.test <- wolf.data[(wolf.data$Camera_ID == current.site),]
  
  wolf.test1 <- aggregate(x = wolf.test[c("Total")],
                            FUN = sum,
                            by = list(Group.date = wolf.test$Date))
  
  wolf.test1 <- wolf.test1[order(wolf.test1$Group.date),]
  wolf.days <- days.df[order(days.df$Group.date),]
  
  for (i in 1:nrow(wolf.days)) {
    date.looking.for <- wolf.days$Group.date[i]
    
    for (j in 1:nrow(wolf.test1)) {
      if (date.looking.for == wolf.test1$Group.date[j]){
        found.total <- wolf.test1$Total[j]
        wolf.days$Total[i] <- found.total
        break
      } else {
        wolf.days$Total[i] <- 0
      }
    }
  }
  
  period.totals <- data.frame(time.period = c(seq(1:14)), total = '0')
  period.totals$total <- as.numeric(period.totals$total)
  wolf.test1$Total <- as.numeric(wolf.test1$Total)
  wolf.days$Total <- as.numeric(wolf.days$Total)
  wolf.days$Week <- as.numeric(wolf.days$Week)
  
  wolf.days <- wolf.days[order(wolf.days$Week),]
  
  period.total <- 0
  counter2 <- 1
  
  for (i in 1:nrow(wolf.days)) {
    
    if (i%%10 == 0){
      period.total <- period.total + wolf.days$Total[i]
      #period.totals$total[counter2] <- period.total/10
      period.totals$total[counter2] <- period.total
      counter2 <- counter2 + 1
      period.total <- 0
    } else if (i%%10 != 0){
      period.total <- period.total + wolf.days$Total[i]
    }else if (counter2 == 15){
      break
    }
  }
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  wolf.matrix[add.site, ] <- period.totals$total
  
}

# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
wolf.matrix <- wolf.matrix[-7,]

# remove site 21
wolf.matrix <- wolf.matrix[-20,]

# remove site 22
wolf.matrix <- wolf.matrix[-20,]

# remove site 31
wolf.matrix <- wolf.matrix[-28,]

# remove site 49
wolf.matrix <- wolf.matrix[-45,]

# remove site 53
wolf.matrix <- wolf.matrix[-48,]

# remove site 58
wolf.matrix <- wolf.matrix[-52,]

wolf.matrix[1:19, ] <- 0
wolf.matrix[21:28, ] <- 0
wolf.matrix[30:39, ] <- 0
wolf.matrix[42:44, ] <- 0
wolf.matrix[46, ] <- 0
wolf.matrix[48:50, ] <- 0
wolf.matrix[53, ] <- 0
wolf.matrix[55:57, ] <- 0
wolf.matrix[59, ] <- 0

###############################################################################
## Creating dataframes for cougar (Double check random sites to make sure this worked past site #1)
###############################################################################

## marten matrix ##
# Number of Sites ##
M <- 66

# Number of Weeks
J <- 14

cougar.matrix <- matrix(nrow = M, ncol = J)
colnames(cougar.matrix) <- c(1:14)

cougar.data <- camera.trap.data[(camera.trap.data$Species == 'Cougar'),]

list.of.sites <- data.frame(as.numeric(unique(cougar.data$Camera_ID)))
colnames(list.of.sites) <- "Camera_ID"

# sort by ID
list.of.sites <- data.frame(list.of.sites[order(list.of.sites$Camera_ID),])
colnames(list.of.sites) <- "Camera_ID"

for (k in 1:nrow(list.of.sites)){
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  
  cougar.test <- cougar.data[(cougar.data$Camera_ID == current.site),]
  
  cougar.test1 <- aggregate(x = cougar.test[c("Total")],
                          FUN = sum,
                          by = list(Group.date = cougar.test$Date))
  
  cougar.test1 <- cougar.test1[order(cougar.test1$Group.date),]
  cougar.days <- days.df[order(days.df$Group.date),]
  
  for (i in 1:nrow(cougar.days)) {
    date.looking.for <- cougar.days$Group.date[i]
    
    for (j in 1:nrow(cougar.test1)) {
      if (date.looking.for == cougar.test1$Group.date[j]){
        found.total <- cougar.test1$Total[j]
        cougar.days$Total[i] <- found.total
        break
      } else {
        cougar.days$Total[i] <- 0
      }
    }
  }
  
  period.totals <- data.frame(time.period = c(seq(1:14)), total = '0')
  period.totals$total <- as.numeric(period.totals$total)
  cougar.test1$Total <- as.numeric(cougar.test1$Total)
  cougar.days$Total <- as.numeric(cougar.days$Total)
  cougar.days$Week <- as.numeric(cougar.days$Week)
  
  cougar.days <- cougar.days[order(cougar.days$Week),]
  
  period.total <- 0
  counter2 <- 1
  
  for (i in 1:nrow(cougar.days)) {
    
    if (i%%10 == 0){
      period.total <- period.total + cougar.days$Total[i]
      #period.totals$total[counter2] <- period.total/10
      period.totals$total[counter2] <- period.total
      counter2 <- counter2 + 1
      period.total <- 0
    } else if (i%%10 != 0){
      period.total <- period.total + cougar.days$Total[i]
    }else if (counter2 == 15){
      break
    }
  }
  
  # add Site data to cougar matrix
  add.site <- as.numeric(current.site)
  cougar.matrix[add.site, ] <- period.totals$total
  
}

# Remove outlier sites (7,21,22,31,49,53,58)
# remove site 7
cougar.matrix <- cougar.matrix[-7,]

# remove site 21
cougar.matrix <- cougar.matrix[-20,]

# remove site 22
cougar.matrix <- cougar.matrix[-20,]

# remove site 31
cougar.matrix <- cougar.matrix[-28,]

# remove site 49
cougar.matrix <- cougar.matrix[-45,]

# remove site 53
cougar.matrix <- cougar.matrix[-48,]

# remove site 58
cougar.matrix <- cougar.matrix[-52,]

cougar.matrix[1:27, ] <- 0
cougar.matrix[30:34, ] <- 0
cougar.matrix[36:39, ] <- 0
cougar.matrix[41, ] <- 0
cougar.matrix[44:53, ] <- 0
cougar.matrix[55:59, ] <- 0

### Convert matrix into 1D array ###
marten.1d <- data.frame(c(t(Marten.matrix)))
wolverine.1d <- data.frame(c(t(wolverine.matrix)))
lynx.1d <- data.frame(c(t(lynx.matrix)))
weasel.1d <- data.frame(c(t(weasel.matrix)))
wolf.1d <- data.frame(c(t(wolf.matrix)))
cougar.1d <- data.frame(c(t(cougar.matrix)))

## Add landcover and elev. ##
list1 <- data.frame(rep(camera.trap.locations$LANDCOVER1, each=14))
colnames(list1) <- c("Landcover")

list2 <- data.frame(rep(camera.trap.locations$ELEVATION, each=14))
colnames(list2) <- c("Elevation")

list3 <- data.frame(rep(camera.trap.locations$TRI, each=14))
colnames(list3) <- c("TRI")

list4 <- data.frame(rep(camera.trap.locations$Camera_ID_2, each=14))
colnames(list4) <- c("Camera_ID")

## add winter season
for (j in 1:nrow(camera.trap.locations)){
  if(as.numeric(camera.trap.locations$Camera_ID_2[j]) <= 30){
    camera.trap.locations$Winter[j] <- '1'
  } else {
    camera.trap.locations$Winter[j] <- '2'
  }
}

list5 <- data.frame(rep(camera.trap.locations$Winter, each=14))
colnames(list5) <- c("Winter")

################################################################
# figure out snowdepth dataframe
# load in data Willmore SNOW DPETH
load(file = 'file location here')

# change to numeric
snow.depth[] <- lapply(snow.depth, function(x) as.numeric(as.character(x)))

snow.depth.5day.avg <- data.frame()
snow.depth.5day.avg$Camera_ID <- vector(mode="numeric")
snow.depth.5day.avg$days5 <- vector(mode="numeric")
snow.depth.5day.avg$days10 <- vector(mode="numeric")
snow.depth.5day.avg$days15 <- vector(mode="numeric")
snow.depth.5day.avg$days20 <- vector(mode="numeric")
snow.depth.5day.avg$days25 <- vector(mode="numeric")
snow.depth.5day.avg$days30 <- vector(mode="numeric")
snow.depth.5day.avg$days35 <- vector(mode="numeric")
snow.depth.5day.avg$days40 <- vector(mode="numeric")
snow.depth.5day.avg$days45 <- vector(mode="numeric")
snow.depth.5day.avg$days50 <- vector(mode="numeric")
snow.depth.5day.avg$days55 <- vector(mode="numeric")
snow.depth.5day.avg$days60 <- vector(mode="numeric")
snow.depth.5day.avg$days65 <- vector(mode="numeric")
snow.depth.5day.avg$days70 <- vector(mode="numeric")

counter <- 2
counter2 <- 6
counter3 <- 2
for (j in 1:nrow(snow.depth)){
  snow.depth.5day.avg[j,1] <- snow.depth$Camera_ID[j]
  for (i in 1:ncol(snow.depth)){
    
    if (counter3 == '16'){
      counter <- 2
      counter2 <- 6
      counter3 <- 2
      break
    } else if (counter == '72'){
      counter <- 2
      counter2 <- 6
      counter3 <- 2
      break
    }
    
    snow.depth.5day.avg[j,counter3] <- (rowSums(snow.depth[j, counter:counter2]))/5
    counter <- counter + 5
    counter2 <- counter2 + 5
    counter3 <- counter3 + 1
  }
}


iou.df <- data.frame(c(marten.1d, wolverine.1d, lynx.1d, weasel.1d, wolf.1d, cougar.1d, list4, list1, list2, list3, list5))
colnames(iou.df) <- c("Marten", "Wolverine", "Lynx", "Weasel", "Wolf", "Cougar", "Camera_ID","Landcover", "Elevation", "TRI", "Winter")

# add to IOU
test2 <- iou.df
test2$Snow_Depth <- 0

counter1 <- 1
counter2 <- 14

counter3 <- 2
counter4 <- 15
for (j in 1:nrow(snow.depth.5day.avg)){
  test2[counter1:counter2,12] <- c(t(snow.depth.5day.avg[j,counter3:counter4]))
  counter1 <- counter1 + 14
  counter2 <- counter2 + 14
}

iou.count.v5.5day.df <- test2
save(iou.count.v5.5day.df, file = 'file location here')


################################################################