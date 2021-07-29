# Step 1: load these libraries, and install.packages(survminer), etc. if you don't have them installed.
library(tidyverse)
library(plyr)
library(rgdal)
library(tidyverse)
library(survival)
library(survminer)
library(overlap)
library(sp)
library(maptools)

# Set timezone
Sys.setenv(TZ='GMT')

#####################
# STEP 2: load camera trap data.
load(file = 'file location here')

tte.species.df <- tte.species.TRI.df

# Add day of year column
tte.species.df$Year_Day <- as.numeric(strftime(tte.species.df$Date, format = "%j"))

#Add Snow depth
tte.species.df$Snow_Depth <- as.numeric(0)

# as numeric conversion
tte.species.df$Camera_ID <- as.numeric(tte.species.df$Camera_ID)

load(file = 'file location here')
load(file = 'file location here')

for (i in 1:nrow(tte.species.df)){
  if (tte.species.df$Camera_ID[i] <= 30){
    camera.id <- as.numeric(tte.species.df$Camera_ID[i])
    year.day <- tte.species.df$Year_Day[i]
    camera.index <- as.numeric(which(rownames(snow.depth.period.1) == camera.id))
    year.day.index <- as.numeric(grep(year.day, colnames(snow.depth.period.1)))
    snow.depth <- as.numeric(snow.depth.period.1[camera.index, year.day.index])
    tte.species.df$Snow_Depth[i] <- snow.depth
  } else {
    camera.id <- as.numeric(tte.species.df$Camera_ID[i])
    year.day <- tte.species.df$Year_Day[i]
    camera.index <- as.numeric(which(rownames(snow.depth.period.2) == camera.id))
    year.day.index <- as.numeric(grep(year.day, colnames(snow.depth.period.2)))
    snow.depth <- as.numeric(snow.depth.period.2[camera.index, year.day.index])
    tte.species.df$Snow_Depth[i] <- snow.depth
  }
}


# STEP 3: Create dataframe to hold time values
tte.time2.df <- data.frame(Camera_ID = vector(mode = "character"),
                           SpeciesA = vector(mode = "character"),
                           SpeciesA_time = vector(mode="character"),
                           SpeciesB = vector(mode="character"),
                           SpeciesB_time = vector(mode="character"),
                           SpeciesB_censor = vector(mode="character"),
                           SpeciesB_censor_time = vector(mode="character"),
                           time_to_event_hours = vector(mode="character"),
                           Status = vector(mode="character"),
                           elevation = vector(mode="character"),
                           landcover = vector(mode="character"),
                           TRI = vector(mode="character"),
                           Snow_Depth = vector(mode = "character"),
                           Solar_Time = vector(mode = "character"),
                           stringsAsFactors = FALSE)

# STEP 4: This loop produces the dataframe which shows the SpeciesA/SpeciesB pairs, if they are within 10 days of eachother.
## I commented the line where you can change SpeciesA, that is all that needs to be changed in order to change it.

# all sites for Marten
list.of.sites <- data.frame(as.numeric(unique(tte.species.df$Camera_ID)))
colnames(list.of.sites) <- "Camera_ID"

#####################################
# Marten v. Wolverine
tte.time2.df <- tte.time2.df[0,]
for (k in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  marten.data <- tte.species.df[which(tte.species.df$Camera_ID==current.site), ]
  
  flag <- 1
  j <- 1
  counter <- 0
  while (j <= nrow(marten.data)){
    
    # if reached end of list, end loop.
    if (j == nrow(marten.data)){
      
      # STEP 5: Sort Data
      tte.time2.df <- tte.time2.df[order(as.numeric(tte.time2.df$time_to_event_hours)),]
      
      # STEP 5.5: Bucket time into 6 hour buckets
      tte.time3.df <- tte.time2.df 
      tte.time3.df$time_to_event_hours <- as.numeric(tte.time3.df$time_to_event_hours)
      
      counter1 <- 6
      counter2 <- 6
      counter3 <- 12
      
      for (t in 1:nrow(tte.time3.df)){
        
        if(t == nrow(tte.time3.df)){
          break
        } 
        
        if (dim(tte.time3.df)[1] != 0) {
          if(as.numeric(tte.time3.df$time_to_event_hours[t]) < counter1){
            tte.time3.df$bucket_6hr[t] <- counter1
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3){
            tte.time3.df$bucket_6hr[t] <- counter3
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 6 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 6){
            tte.time3.df$bucket_6hr[t] <- counter3 + 6
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 12 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 12){
            tte.time3.df$bucket_6hr[t] <- counter3 + 12
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 18 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 18){
            tte.time3.df$bucket_6hr[t] <- counter3 + 18
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 24 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 24){
            tte.time3.df$bucket_6hr[t] <- counter3 + 24
          }
          
          if(as.numeric(tte.time3.df$time_to_event_hours[t+1]) >= counter3){
            counter2 <- counter2 + 6
            counter3 <- counter3 + 6
          }
        }
      }
      
      tte.time4.df <- tte.time3.df 
      
      
      counter1 <- 1
      counter2 <- 1
      counter3 <- 2
      
      for (r in 1:nrow(tte.time4.df)){
        
        if(r == nrow(tte.time4.df)){
          break
        }
        
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$time_to_event_hours[r]) < counter1){
            tte.time4.df$bucket_1hr[r] <- counter1
          } else if (as.numeric(tte.time4.df$time_to_event_hours[r]) >= counter2 & as.numeric(tte.time4.df$time_to_event_hours[r]) < counter3){
            tte.time4.df$bucket_1hr[r] <- counter3
          } else if ((as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3) > 0){
            value <- ceiling(as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3)
            tte.time4.df$bucket_1hr[r] <- counter3 + value
            if (as.numeric(tte.time4.df$time_to_event_hours[r]) == (counter3 + value)){
              tte.time4.df$bucket_1hr[r] <- counter3 + value + 1
            }
          }
          
          if(as.numeric(tte.time4.df$time_to_event_hours[r+1]) >= counter3){
            counter2 <- counter2 + 1
            counter3 <- counter3 + 1
          }
        }
      }
      
      # STEP 5.53
      # ADD WINTER1 and WINTER2
      for (y in 1:nrow(tte.time4.df)){
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$Camera_ID[y]) <= 30){
            tte.time4.df$winter[y] <- '1'
          } else {
            tte.time4.df$winter[y] <- '2'
          }
        }
      }
      
      tte.time4.df <- tte.time4.df[order(as.numeric(tte.time4.df$Camera_ID), tte.time4.df$SpeciesA_time),]
      
      Marten.Wolverine.TTE <- tte.time4.df
      Marten.Wolverine.TTE$SpeciesA_time <- as.POSIXct(as.numeric(Marten.Wolverine.TTE$SpeciesA_time), origin="1970-01-01")
      Marten.Wolverine.TTE$SpeciesB_time <- as.POSIXct(as.numeric(Marten.Wolverine.TTE$SpeciesB_time), origin="1970-01-01")
      Marten.Wolverine.TTE$SpeciesB_censor_time <- as.POSIXct(as.numeric(Marten.Wolverine.TTE$SpeciesB_censor_time), origin="1970-01-01")
      break
    }  
    
    # CASE 1: Marten/Wolverine
    if (marten.data$Species[j] == 'Marten' & marten.data$Species[j + 1] == 'Wolverine'){
      # no censor 
      flag <- 1
      camera.ID.data <- marten.data$Camera_ID[j]
      elevation.data <- marten.data$ELEVATION[j]
      landcover.data <- as.character(marten.data$LANDCOVER1[j])
      tri.data <- as.character(marten.data$TRI[j])
      snow.depth.data <- as.numeric(marten.data$Snow_Depth[j + 1])
      solar.time.data <- as.numeric(marten.data$suntime[j + 1])
      speciesA.data <- marten.data$Species[j]
      speciesA.time.data <- marten.data$date_time[j]
      speciesB.data <- marten.data$Species[j + 1]
      speciesB.time.data <- marten.data$date_time[j + 1]
      speciesB.censor <- "NA"
      speciesB.censor.time <- "0"
      status.data <- flag
      time.event.data <- as.numeric(difftime(speciesB.time.data, speciesA.time.data, units = "hours"))
      # censor data entry if outside time range
      if (time.event.data > 120){
        status.data <- 0
      }
      tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                 speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time, 
                                                 time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
    }
    
    # CASE 2 and 3: Marten/'Species C'...n/Wolverine
    # if first entry = marten and second entry = Species C, look to see if wolverine shows up
    if (marten.data$Species[j] == 'Marten' & marten.data$Species[j + 1] != 'Marten' & marten.data$Species[j + 1] != 'Wolverine'){
      for (i in j:nrow(marten.data)){
        
        if (i == nrow(marten.data)){
          break
        }
        
        # look for 'Species C' / Wolverine
        if ((marten.data$Species[i] != 'Marten' & marten.data$Species[i] != 'Wolverine') & marten.data$Species[i + 1] == 'Wolverine'){
          # censor 
          flag <- 0
          camera.ID.data <- marten.data$Camera_ID[j]
          elevation.data <- marten.data$ELEVATION[j]
          landcover.data <- as.character(marten.data$LANDCOVER1[j])
          tri.data <- as.character(marten.data$TRI[j])
          snow.depth.data <- as.numeric(marten.data$Snow_Depth[i + 1])
          solar.time.data <- as.numeric(marten.data$suntime[i + 1])
          speciesA.data <- marten.data$Species[j]
          speciesA.time.data <- marten.data$date_time[j]
          speciesB.data <- marten.data$Species[i]
          speciesB.time.data <- marten.data$date_time[i]
          speciesB.censor <- marten.data$Species[i + 1]
          speciesB.censor.time <- marten.data$date_time[i + 1]
          status.data <- flag
          time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "hours")
          tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                     speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time,
                                                     time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
        # CASE 4:
        if ((marten.data$Species[i] != 'Marten' & marten.data$Species[i] != 'Wolverine') & marten.data$Species[i + 1] == 'Marten'){
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
      }
    }
    j <- j + 1
  }
}

# Marten v. Short-tailed Weasel # need to add hourly buckets onto the rest of dataframes
tte.time2.df <- tte.time2.df[0,]
for (k in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  marten.data <- tte.species.df[which(tte.species.df$Camera_ID==current.site), ]
  
  flag <- 1
  j <- 1
  counter <- 0
  while (j <= nrow(marten.data)){
    
    # if reached end of list, end loop.
    if (j == nrow(marten.data)){
      
      # STEP 5: Sort Data
      tte.time2.df <- tte.time2.df[order(as.numeric(tte.time2.df$time_to_event_hours)),]
      
      # STEP 5.5: Bucket time into 6 hour buckets
      tte.time3.df <- tte.time2.df
      tte.time3.df$time_to_event_hours <- as.numeric(tte.time3.df$time_to_event_hours)
      
      counter1 <- 6
      counter2 <- 6
      counter3 <- 12
      
      for (t in 1:nrow(tte.time3.df)){
        
        if(t == nrow(tte.time3.df)){
          break
        } 
        
        if (dim(tte.time3.df)[1] != 0) {
          if(as.numeric(tte.time3.df$time_to_event_hours[t]) < counter1){
            tte.time3.df$bucket_6hr[t] <- counter1
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3){
            tte.time3.df$bucket_6hr[t] <- counter3
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 6 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 6){
            tte.time3.df$bucket_6hr[t] <- counter3 + 6
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 12 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 12){
            tte.time3.df$bucket_6hr[t] <- counter3 + 12
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 18 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 18){
            tte.time3.df$bucket_6hr[t] <- counter3 + 18
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 24 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 24){
            tte.time3.df$bucket_6hr[t] <- counter3 + 24
          }
          
          if(as.numeric(tte.time3.df$time_to_event_hours[t+1]) >= counter3){
            counter2 <- counter2 + 6
            counter3 <- counter3 + 6
          }
        }
      }
      
      tte.time4.df <- tte.time3.df 
      
      
      counter1 <- 1
      counter2 <- 1
      counter3 <- 2
      
      for (r in 1:nrow(tte.time4.df)){
        
        if(r == nrow(tte.time4.df)){
          break
        }
        
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$time_to_event_hours[r]) < counter1){
            tte.time4.df$bucket_1hr[r] <- counter1
          } else if (as.numeric(tte.time4.df$time_to_event_hours[r]) >= counter2 & as.numeric(tte.time4.df$time_to_event_hours[r]) < counter3){
            tte.time4.df$bucket_1hr[r] <- counter3
          } else if ((as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3) > 0){
            value <- ceiling(as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3)
            tte.time4.df$bucket_1hr[r] <- counter3 + value
            if (as.numeric(tte.time4.df$time_to_event_hours[r]) == (counter3 + value)){
              tte.time4.df$bucket_1hr[r] <- counter3 + value + 1
            }
          }
          
          if(as.numeric(tte.time4.df$time_to_event_hours[r+1]) >= counter3){
            counter2 <- counter2 + 1
            counter3 <- counter3 + 1
          }
        }
      }
      
      # STEP 5.53
      # ADD WINTER1 and WINTER2
      for (y in 1:nrow(tte.time4.df)){
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$Camera_ID[y]) <= 30){
            tte.time4.df$winter[y] <- '1'
          } else {
            tte.time4.df$winter[y] <- '2'
          }
        }
      }
      
      tte.time4.df <- tte.time4.df[order(as.numeric(tte.time4.df$Camera_ID), tte.time4.df$SpeciesA_time),]
      
      Marten.Weasel.TTE <- tte.time4.df
      Marten.Weasel.TTE$SpeciesA_time <- as.POSIXct(as.numeric(Marten.Weasel.TTE$SpeciesA_time), origin="1970-01-01")
      Marten.Weasel.TTE$SpeciesB_time <- as.POSIXct(as.numeric(Marten.Weasel.TTE$SpeciesB_time), origin="1970-01-01")
      Marten.Weasel.TTE$SpeciesB_censor_time <- as.POSIXct(as.numeric(Marten.Weasel.TTE$SpeciesB_censor_time), origin="1970-01-01")
      break
    }  
    
    # CASE 1: Marten/Wolverine
    if (marten.data$Species[j] == 'Marten' & marten.data$Species[j + 1] == 'Short-tailed Weasel'){
      # no censor 
      flag <- 1
      camera.ID.data <- marten.data$Camera_ID[j]
      elevation.data <- marten.data$ELEVATION[j]
      landcover.data <- as.character(marten.data$LANDCOVER1[j])
      tri.data <- as.character(marten.data$TRI[j])
      snow.depth.data <- as.numeric(marten.data$Snow_Depth[j + 1])
      solar.time.data <- as.numeric(marten.data$suntime[j + 1])
      speciesA.data <- marten.data$Species[j]
      speciesA.time.data <- marten.data$date_time[j]
      speciesB.data <- marten.data$Species[j + 1]
      speciesB.time.data <- marten.data$date_time[j + 1]
      speciesB.censor <- "NA"
      speciesB.censor.time <- "0"
      status.data <- flag
      time.event.data <- as.numeric(difftime(speciesB.time.data, speciesA.time.data, units = "hours"))
      # censor data entry if outside time range
      if (time.event.data > 120){
        status.data <- 0
      }
      tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                 speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time, 
                                                 time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
    }
    
    # CASE 2 and 3: Marten/'Species C'...n/Wolverine
    # if first entry = marten and second entry = Species C, look to see if wolverine shows up
    if (marten.data$Species[j] == 'Marten' & marten.data$Species[j + 1] != 'Marten' & marten.data$Species[j + 1] != 'Short-tailed Weasel'){
      for (i in j:nrow(marten.data)){
        
        if (i == nrow(marten.data)){
          break
        }
        
        # look for 'Species C' / Wolverine
        if ((marten.data$Species[i] != 'Marten' & marten.data$Species[i] != 'Short-tailed Weasel') & marten.data$Species[i + 1] == 'Short-tailed Weasel'){
          # censor 
          flag <- 0
          camera.ID.data <- marten.data$Camera_ID[j]
          elevation.data <- marten.data$ELEVATION[j]
          landcover.data <- as.character(marten.data$LANDCOVER1[j])
          tri.data <- as.character(marten.data$TRI[j])
          snow.depth.data <- as.numeric(marten.data$Snow_Depth[i + 1])
          solar.time.data <- as.numeric(marten.data$suntime[i + 1])
          speciesA.data <- marten.data$Species[j]
          speciesA.time.data <- marten.data$date_time[j]
          speciesB.data <- marten.data$Species[i]
          speciesB.time.data <- marten.data$date_time[i]
          speciesB.censor <- marten.data$Species[i + 1]
          speciesB.censor.time <- marten.data$date_time[i + 1]
          status.data <- flag
          time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "hours")
          tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                     speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time,
                                                     time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
        # CASE 4:
        if ((marten.data$Species[i] != 'Marten' & marten.data$Species[i] != 'Short-tailed Weasel') & marten.data$Species[i + 1] == 'Marten'){
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
      }
    }
    j <- j + 1
  }
}


#####################################
# Wolverine v. Marten 
tte.time2.df <- tte.time2.df[0,]
for (k in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  marten.data <- tte.species.df[which(tte.species.df$Camera_ID==current.site), ]
  
  flag <- 1
  j <- 1
  counter <- 0
  while (j <= nrow(marten.data)){
    
    # if reached end of list, end loop.
    if (j == nrow(marten.data)){
      # STEP 5: Sort Data
      tte.time2.df <- tte.time2.df[order(as.numeric(tte.time2.df$time_to_event_hours)),]
      
      # STEP 5.5: Bucket time into 6 hour buckets
      tte.time3.df <- tte.time2.df 
      tte.time3.df$time_to_event_hours <- as.numeric(tte.time3.df$time_to_event_hours)
      
      counter1 <- 6
      counter2 <- 6
      counter3 <- 12
      
      for (t in 1:nrow(tte.time3.df)){
        
        if(t == nrow(tte.time3.df)){
          break
        } 
        
        if (dim(tte.time3.df)[1] != 0) {
          if(as.numeric(tte.time3.df$time_to_event_hours[t]) < counter1){
            tte.time3.df$bucket_6hr[t] <- counter1
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3){
            tte.time3.df$bucket_6hr[t] <- counter3
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 6 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 6){
            tte.time3.df$bucket_6hr[t] <- counter3 + 6
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 12 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 12){
            tte.time3.df$bucket_6hr[t] <- counter3 + 12
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 18 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 18){
            tte.time3.df$bucket_6hr[t] <- counter3 + 18
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 24 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 24){
            tte.time3.df$bucket_6hr[t] <- counter3 + 24
          }
          
          if(as.numeric(tte.time3.df$time_to_event_hours[t+1]) >= counter3){
            counter2 <- counter2 + 6
            counter3 <- counter3 + 6
          }
        }
      }
      
      tte.time4.df <- tte.time3.df 
      
      
      counter1 <- 1
      counter2 <- 1
      counter3 <- 2
      
      for (r in 1:nrow(tte.time4.df)){
        
        if(r == nrow(tte.time4.df)){
          break
        }
        
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$time_to_event_hours[r]) < counter1){
            tte.time4.df$bucket_1hr[r] <- counter1
          } else if (as.numeric(tte.time4.df$time_to_event_hours[r]) >= counter2 & as.numeric(tte.time4.df$time_to_event_hours[r]) < counter3){
            tte.time4.df$bucket_1hr[r] <- counter3
          } else if ((as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3) > 0){
            value <- ceiling(as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3)
            tte.time4.df$bucket_1hr[r] <- counter3 + value
            if (as.numeric(tte.time4.df$time_to_event_hours[r]) == (counter3 + value)){
              tte.time4.df$bucket_1hr[r] <- counter3 + value + 1
            }
          }
          
          if(as.numeric(tte.time4.df$time_to_event_hours[r+1]) >= counter3){
            counter2 <- counter2 + 1
            counter3 <- counter3 + 1
          }
        }
      }
      
      # STEP 5.53
      # ADD WINTER1 and WINTER2
      for (y in 1:nrow(tte.time4.df)){
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$Camera_ID[y]) <= 30){
            tte.time4.df$winter[y] <- '1'
          } else {
            tte.time4.df$winter[y] <- '2'
          }
        }
      }
      
      tte.time4.df <- tte.time4.df[order(as.numeric(tte.time4.df$Camera_ID), tte.time4.df$SpeciesA_time),]
      
      Wolverine.Marten.TTE <- tte.time4.df
      Wolverine.Marten.TTE$SpeciesA_time <- as.POSIXct(as.numeric(Wolverine.Marten.TTE$SpeciesA_time), origin="1970-01-01")
      Wolverine.Marten.TTE$SpeciesB_time <- as.POSIXct(as.numeric(Wolverine.Marten.TTE$SpeciesB_time), origin="1970-01-01")
      Wolverine.Marten.TTE$SpeciesB_censor_time <- as.POSIXct(as.numeric(Wolverine.Marten.TTE$SpeciesB_censor_time), origin="1970-01-01")
      break
    }  
    
    # CASE 1: Marten/Wolverine
    if (marten.data$Species[j] == 'Wolverine' & marten.data$Species[j + 1] == 'Marten'){
      # no censor 
      flag <- 1
      camera.ID.data <- marten.data$Camera_ID[j]
      elevation.data <- marten.data$ELEVATION[j]
      landcover.data <- as.character(marten.data$LANDCOVER1[j])
      tri.data <- as.character(marten.data$TRI[j])
      snow.depth.data <- as.numeric(marten.data$Snow_Depth[j + 1])
      solar.time.data <- as.numeric(marten.data$suntime[j + 1])
      speciesA.data <- marten.data$Species[j]
      speciesA.time.data <- marten.data$date_time[j]
      speciesB.data <- marten.data$Species[j + 1]
      speciesB.time.data <- marten.data$date_time[j + 1]
      speciesB.censor <- "NA"
      speciesB.censor.time <- "0"
      status.data <- flag
      time.event.data <- as.numeric(difftime(speciesB.time.data, speciesA.time.data, units = "hours"))
      # censor data entry if outside time range
      if (time.event.data > 120){
        status.data <- 0
      }
      tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                 speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time, 
                                                 time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
    }
    
    # CASE 2 and 3: Marten/'Species C'...n/Wolverine
    # if first entry = marten and second entry = Species C, look to see if wolverine shows up
    if (marten.data$Species[j] == 'Wolverine' & marten.data$Species[j + 1] != 'Wolverine' & marten.data$Species[j + 1] != 'Marten'){
      for (i in j:nrow(marten.data)){
        
        if (i == nrow(marten.data)){
          break
        }
        
        # look for 'Species C' / Wolverine
        if ((marten.data$Species[i] != 'Wolverine' & marten.data$Species[i] != 'Marten') & marten.data$Species[i + 1] == 'Marten'){
          # censor 
          flag <- 0
          camera.ID.data <- marten.data$Camera_ID[j]
          elevation.data <- marten.data$ELEVATION[j]
          landcover.data <- as.character(marten.data$LANDCOVER1[j])
          tri.data <- as.character(marten.data$TRI[j])
          snow.depth.data <- as.numeric(marten.data$Snow_Depth[j + 1])
          solar.time.data <- as.numeric(marten.data$suntime[j + 1])
          speciesA.data <- marten.data$Species[j]
          speciesA.time.data <- marten.data$date_time[j]
          speciesB.data <- marten.data$Species[i]
          speciesB.time.data <- marten.data$date_time[i]
          speciesB.censor <- marten.data$Species[i + 1]
          speciesB.censor.time <- marten.data$date_time[i + 1]
          status.data <- flag
          time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "hours")
          tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                     speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time,
                                                     time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
        # CASE 4:
        if ((marten.data$Species[i] != 'Wolverine' & marten.data$Species[i] != 'Marten') & marten.data$Species[i + 1] == 'Wolverine'){
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
      }
    }
    j <- j + 1
  }
}


# Wolverine v. Short-tailed Weasel
tte.time2.df <- tte.time2.df[0,]
for (k in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  marten.data <- tte.species.df[which(tte.species.df$Camera_ID==current.site), ]
  
  flag <- 1
  j <- 1
  counter <- 0
  while (j <= nrow(marten.data)){
    
    # if reached end of list, end loop.
    if (j == nrow(marten.data)){
      # STEP 5: Sort Data
      tte.time2.df <- tte.time2.df[order(as.numeric(tte.time2.df$time_to_event_hours)),]
      
      # STEP 5.5: Bucket time into 6 hour buckets
      tte.time3.df <- tte.time2.df 
      tte.time3.df$time_to_event_hours <- as.numeric(tte.time3.df$time_to_event_hours)
      
      counter1 <- 6
      counter2 <- 6
      counter3 <- 12
      
      for (t in 1:nrow(tte.time3.df)){
        
        if(t == nrow(tte.time3.df)){
          break
        } 
        
        if (dim(tte.time3.df)[1] != 0) {
          if(as.numeric(tte.time3.df$time_to_event_hours[t]) < counter1){
            tte.time3.df$bucket_6hr[t] <- counter1
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3){
            tte.time3.df$bucket_6hr[t] <- counter3
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 6 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 6){
            tte.time3.df$bucket_6hr[t] <- counter3 + 6
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 12 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 12){
            tte.time3.df$bucket_6hr[t] <- counter3 + 12
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 18 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 18){
            tte.time3.df$bucket_6hr[t] <- counter3 + 18
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 24 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 24){
            tte.time3.df$bucket_6hr[t] <- counter3 + 24
          }
          
          if(as.numeric(tte.time3.df$time_to_event_hours[t+1]) >= counter3){
            counter2 <- counter2 + 6
            counter3 <- counter3 + 6
          }
        }
      }
      
      tte.time4.df <- tte.time3.df 
      
      
      counter1 <- 1
      counter2 <- 1
      counter3 <- 2
      
      for (r in 1:nrow(tte.time4.df)){
        
        if(r == nrow(tte.time4.df)){
          break
        }
        
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$time_to_event_hours[r]) < counter1){
            tte.time4.df$bucket_1hr[r] <- counter1
          } else if (as.numeric(tte.time4.df$time_to_event_hours[r]) >= counter2 & as.numeric(tte.time4.df$time_to_event_hours[r]) < counter3){
            tte.time4.df$bucket_1hr[r] <- counter3
          } else if ((as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3) > 0){
            value <- ceiling(as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3)
            tte.time4.df$bucket_1hr[r] <- counter3 + value
            if (as.numeric(tte.time4.df$time_to_event_hours[r]) == (counter3 + value)){
              tte.time4.df$bucket_1hr[r] <- counter3 + value + 1
            }
          }
          
          if(as.numeric(tte.time4.df$time_to_event_hours[r+1]) >= counter3){
            counter2 <- counter2 + 1
            counter3 <- counter3 + 1
          }
        }
      }
      
      # STEP 5.53
      # ADD WINTER1 and WINTER2
      for (y in 1:nrow(tte.time4.df)){
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$Camera_ID[y]) <= 30){
            tte.time4.df$winter[y] <- '1'
          } else {
            tte.time4.df$winter[y] <- '2'
          }
        }
      }
      
      tte.time4.df <- tte.time4.df[order(as.numeric(tte.time4.df$Camera_ID), tte.time4.df$SpeciesA_time),]
      Wolverine.Weasel.TTE <- tte.time4.df
      Wolverine.Weasel.TTE$SpeciesA_time <- as.POSIXct(as.numeric(Wolverine.Weasel.TTE$SpeciesA_time), origin="1970-01-01")
      Wolverine.Weasel.TTE$SpeciesB_time <- as.POSIXct(as.numeric(Wolverine.Weasel.TTE$SpeciesB_time), origin="1970-01-01")
      Wolverine.Weasel.TTE$SpeciesB_censor_time <- as.POSIXct(as.numeric(Wolverine.Weasel.TTE$SpeciesB_censor_time), origin="1970-01-01")
      break
    }  
    
    # CASE 1: Marten/Wolverine
    if (marten.data$Species[j] == 'Wolverine' & marten.data$Species[j + 1] == 'Short-tailed Weasel'){
      # no censor 
      flag <- 1
      camera.ID.data <- marten.data$Camera_ID[j]
      elevation.data <- marten.data$ELEVATION[j]
      landcover.data <- as.character(marten.data$LANDCOVER1[j])
      tri.data <- as.character(marten.data$TRI[j])
      snow.depth.data <- as.numeric(marten.data$Snow_Depth[j + 1])
      solar.time.data <- as.numeric(marten.data$suntime[j + 1])
      speciesA.data <- marten.data$Species[j]
      speciesA.time.data <- marten.data$date_time[j]
      speciesB.data <- marten.data$Species[j + 1]
      speciesB.time.data <- marten.data$date_time[j + 1]
      speciesB.censor <- "NA"
      speciesB.censor.time <- "0"
      status.data <- flag
      time.event.data <- as.numeric(difftime(speciesB.time.data, speciesA.time.data, units = "hours"))
      # censor data entry if outside time range
      if (time.event.data > 120){
        status.data <- 0
      }
      tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                 speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time, 
                                                 time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
    }
    
    # CASE 2 and 3: Marten/'Species C'...n/Wolverine
    # if first entry = marten and second entry = Species C, look to see if wolverine shows up
    if (marten.data$Species[j] == 'Wolverine' & marten.data$Species[j + 1] != 'Wolverine' & marten.data$Species[j + 1] != 'Short-tailed Weasel'){
      for (i in j:nrow(marten.data)){
        
        if (i == nrow(marten.data)){
          break
        }
        
        # look for 'Species C' / Wolverine
        if ((marten.data$Species[i] != 'Wolverine' & marten.data$Species[i] != 'Short-tailed Weasel') & marten.data$Species[i + 1] == 'Short-tailed Weasel'){
          # censor 
          flag <- 0
          camera.ID.data <- marten.data$Camera_ID[j]
          elevation.data <- marten.data$ELEVATION[j]
          landcover.data <- as.character(marten.data$LANDCOVER1[j])
          tri.data <- as.character(marten.data$TRI[j])
          snow.depth.data <- as.numeric(marten.data$Snow_Depth[i + 1])
          solar.time.data <- as.numeric(marten.data$suntime[i + 1])
          speciesA.data <- marten.data$Species[j]
          speciesA.time.data <- marten.data$date_time[j]
          speciesB.data <- marten.data$Species[i]
          speciesB.time.data <- marten.data$date_time[i]
          speciesB.censor <- marten.data$Species[i + 1]
          speciesB.censor.time <- marten.data$date_time[i + 1]
          status.data <- flag
          time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "hours")
          tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                     speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time,
                                                     time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
        # CASE 4:
        if ((marten.data$Species[i] != 'Wolverine' & marten.data$Species[i] != 'Short-tailed Weasel') & marten.data$Species[i + 1] == 'Wolverine'){
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
      }
    }
    j <- j + 1
  }
}

#####################################
# Short-tailed Weasel v. Marten 
tte.time2.df <- tte.time2.df[0,]
for (k in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  marten.data <- tte.species.df[which(tte.species.df$Camera_ID==current.site), ]
  
  flag <- 1
  j <- 1
  counter <- 0
  while (j <= nrow(marten.data)){
    
    # if reached end of list, end loop.
    if (j == nrow(marten.data)){
      # STEP 5: Sort Data
      tte.time2.df <- tte.time2.df[order(as.numeric(tte.time2.df$time_to_event_hours)),]
      
      # STEP 5.5: Bucket time into 6 hour buckets
      tte.time3.df <- tte.time2.df 
      tte.time3.df$time_to_event_hours <- as.numeric(tte.time3.df$time_to_event_hours)
      
      counter1 <- 6
      counter2 <- 6
      counter3 <- 12
      
      for (t in 1:nrow(tte.time3.df)){
        
        if(t == nrow(tte.time3.df)){
          break
        } 
        
        if (dim(tte.time3.df)[1] != 0) {
          if(as.numeric(tte.time3.df$time_to_event_hours[t]) < counter1){
            tte.time3.df$bucket_6hr[t] <- counter1
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3){
            tte.time3.df$bucket_6hr[t] <- counter3
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 6 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 6){
            tte.time3.df$bucket_6hr[t] <- counter3 + 6
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 12 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 12){
            tte.time3.df$bucket_6hr[t] <- counter3 + 12
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 18 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 18){
            tte.time3.df$bucket_6hr[t] <- counter3 + 18
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 24 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 24){
            tte.time3.df$bucket_6hr[t] <- counter3 + 24
          }
          
          if(as.numeric(tte.time3.df$time_to_event_hours[t+1]) >= counter3){
            counter2 <- counter2 + 6
            counter3 <- counter3 + 6
          }
        }
      }
      
      tte.time4.df <- tte.time3.df 
      
      
      counter1 <- 1
      counter2 <- 1
      counter3 <- 2
      
      for (r in 1:nrow(tte.time4.df)){
        
        if(r == nrow(tte.time4.df)){
          break
        }
        
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$time_to_event_hours[r]) < counter1){
            tte.time4.df$bucket_1hr[r] <- counter1
          } else if (as.numeric(tte.time4.df$time_to_event_hours[r]) >= counter2 & as.numeric(tte.time4.df$time_to_event_hours[r]) < counter3){
            tte.time4.df$bucket_1hr[r] <- counter3
          } else if ((as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3) > 0){
            value <- ceiling(as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3)
            tte.time4.df$bucket_1hr[r] <- counter3 + value
            if (as.numeric(tte.time4.df$time_to_event_hours[r]) == (counter3 + value)){
              tte.time4.df$bucket_1hr[r] <- counter3 + value + 1
            }
          }
          
          if(as.numeric(tte.time4.df$time_to_event_hours[r+1]) >= counter3){
            counter2 <- counter2 + 1
            counter3 <- counter3 + 1
          }
        }
      }
      
      # STEP 5.53
      # ADD WINTER1 and WINTER2
      for (y in 1:nrow(tte.time4.df)){
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$Camera_ID[y]) <= 30){
            tte.time4.df$winter[y] <- '1'
          } else {
            tte.time4.df$winter[y] <- '2'
          }
        }
      }
      
      tte.time4.df <- tte.time4.df[order(as.numeric(tte.time4.df$Camera_ID), tte.time4.df$SpeciesA_time),]
      Weasel.Marten.TTE <- tte.time4.df
      Weasel.Marten.TTE$SpeciesA_time <- as.POSIXct(as.numeric(Weasel.Marten.TTE$SpeciesA_time), origin="1970-01-01")
      Weasel.Marten.TTE$SpeciesB_time <- as.POSIXct(as.numeric(Weasel.Marten.TTE$SpeciesB_time), origin="1970-01-01")
      Weasel.Marten.TTE$SpeciesB_censor_time <- as.POSIXct(as.numeric(Weasel.Marten.TTE$SpeciesB_censor_time), origin="1970-01-01")
      break
    }  
    
    # CASE 1: Marten/Wolverine
    if (marten.data$Species[j] == 'Short-tailed Weasel' & marten.data$Species[j + 1] == 'Marten'){
      # no censor 
      flag <- 1
      camera.ID.data <- marten.data$Camera_ID[j]
      elevation.data <- marten.data$ELEVATION[j]
      landcover.data <- as.character(marten.data$LANDCOVER1[j])
      tri.data <- as.character(marten.data$TRI[j])
      snow.depth.data <- as.numeric(marten.data$Snow_Depth[j + 1])
      solar.time.data <- as.numeric(marten.data$suntime[j + 1])
      speciesA.data <- marten.data$Species[j]
      speciesA.time.data <- marten.data$date_time[j]
      speciesB.data <- marten.data$Species[j + 1]
      speciesB.time.data <- marten.data$date_time[j + 1]
      speciesB.censor <- "NA"
      speciesB.censor.time <- "0"
      status.data <- flag
      time.event.data <- as.numeric(difftime(speciesB.time.data, speciesA.time.data, units = "hours"))
      # censor data entry if outside time range
      if (time.event.data > 120){
        status.data <- 0
      }
      tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                 speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time, 
                                                 time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
    }
    
    # CASE 2 and 3: Marten/'Species C'...n/Wolverine
    # if first entry = marten and second entry = Species C, look to see if wolverine shows up
    if (marten.data$Species[j] == 'Short-tailed Weasel' & marten.data$Species[j + 1] != 'Short-tailed Weasel' & marten.data$Species[j + 1] != 'Marten'){
      for (i in j:nrow(marten.data)){
        
        if (i == nrow(marten.data)){
          break
        }
        
        # look for 'Species C' / Wolverine
        if ((marten.data$Species[i] != 'Short-tailed Weasel' & marten.data$Species[i] != 'Marten') & marten.data$Species[i + 1] == 'Marten'){
          # censor 
          flag <- 0
          camera.ID.data <- marten.data$Camera_ID[j]
          elevation.data <- marten.data$ELEVATION[j]
          landcover.data <- as.character(marten.data$LANDCOVER1[j])
          tri.data <- as.character(marten.data$TRI[j])
          snow.depth.data <- as.numeric(marten.data$Snow_Depth[i + 1])
          solar.time.data <- as.numeric(marten.data$suntime[i + 1])
          speciesA.data <- marten.data$Species[j]
          speciesA.time.data <- marten.data$date_time[j]
          speciesB.data <- marten.data$Species[i]
          speciesB.time.data <- marten.data$date_time[i]
          speciesB.censor <- marten.data$Species[i + 1]
          speciesB.censor.time <- marten.data$date_time[i + 1]
          status.data <- flag
          time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "hours")
          tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                     speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time,
                                                     time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
        # CASE 4:
        if ((marten.data$Species[i] != 'Short-tailed Weasel' & marten.data$Species[i] != 'Marten') & marten.data$Species[i + 1] == 'Short-tailed Weasel'){
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
      }
    }
    j <- j + 1
  }
}


# Short-tailed Weasel v. Wolverine
tte.time2.df <- tte.time2.df[0,]
for (k in 1:nrow(list.of.sites)) {
  # find all detections for the first site.
  current.site <- list.of.sites$Camera_ID[k]
  marten.data <- tte.species.df[which(tte.species.df$Camera_ID==current.site), ]
  
  flag <- 1
  j <- 1
  counter <- 0
  while (j <= nrow(marten.data)){
    
    # if reached end of list, end loop.
    if (j == nrow(marten.data)){
      # STEP 5: Sort Data
      tte.time2.df <- tte.time2.df[order(as.numeric(tte.time2.df$time_to_event_hours)),]
      
      # STEP 5.5: Bucket time into 6 hour buckets
      tte.time3.df <- tte.time2.df 
      tte.time3.df$time_to_event_hours <- as.numeric(tte.time3.df$time_to_event_hours)
      
      counter1 <- 6
      counter2 <- 6
      counter3 <- 12
      
      for (t in 1:nrow(tte.time3.df)){
        
        if(t == nrow(tte.time3.df)){
          break
        } 
        
        if (dim(tte.time3.df)[1] != 0) {
          if(as.numeric(tte.time3.df$time_to_event_hours[t]) < counter1){
            tte.time3.df$bucket_6hr[t] <- counter1
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3){
            tte.time3.df$bucket_6hr[t] <- counter3
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 6 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 6){
            tte.time3.df$bucket_6hr[t] <- counter3 + 6
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 12 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 12){
            tte.time3.df$bucket_6hr[t] <- counter3 + 12
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 18 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 18){
            tte.time3.df$bucket_6hr[t] <- counter3 + 18
          } else if (as.numeric(tte.time3.df$time_to_event_hours[t]) >= counter2 + 24 & as.numeric(tte.time3.df$time_to_event_hours[t]) < counter3 + 24){
            tte.time3.df$bucket_6hr[t] <- counter3 + 24
          }
          
          if(as.numeric(tte.time3.df$time_to_event_hours[t+1]) >= counter3){
            counter2 <- counter2 + 6
            counter3 <- counter3 + 6
          }
        }
      }
      
      tte.time4.df <- tte.time3.df 
      
      
      counter1 <- 1
      counter2 <- 1
      counter3 <- 2
      
      for (r in 1:nrow(tte.time4.df)){
        
        if(r == nrow(tte.time4.df)){
          break
        }
        
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$time_to_event_hours[r]) < counter1){
            tte.time4.df$bucket_1hr[r] <- counter1
          } else if (as.numeric(tte.time4.df$time_to_event_hours[r]) >= counter2 & as.numeric(tte.time4.df$time_to_event_hours[r]) < counter3){
            tte.time4.df$bucket_1hr[r] <- counter3
          } else if ((as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3) > 0){
            value <- ceiling(as.numeric(tte.time4.df$time_to_event_hours[r]) - counter3)
            tte.time4.df$bucket_1hr[r] <- counter3 + value
            if (as.numeric(tte.time4.df$time_to_event_hours[r]) == (counter3 + value)){
              tte.time4.df$bucket_1hr[r] <- counter3 + value + 1
            }
          }
          
          if(as.numeric(tte.time4.df$time_to_event_hours[r+1]) >= counter3){
            counter2 <- counter2 + 1
            counter3 <- counter3 + 1
          }
        }
      }
      
      # STEP 5.53
      # ADD WINTER1 and WINTER2
      for (y in 1:nrow(tte.time4.df)){
        if (dim(tte.time4.df)[1] != 0) {
          if(as.numeric(tte.time4.df$Camera_ID[y]) <= 30){
            tte.time4.df$winter[y] <- '1'
          } else {
            tte.time4.df$winter[y] <- '2'
          }
        }
      }
      
      tte.time4.df <- tte.time4.df[order(as.numeric(tte.time4.df$Camera_ID), tte.time4.df$SpeciesA_time),]
      Weasel.Wolverine.TTE <- tte.time4.df
      Weasel.Wolverine.TTE$SpeciesA_time <- as.POSIXct(as.numeric(Weasel.Wolverine.TTE$SpeciesA_time), origin="1970-01-01")
      Weasel.Wolverine.TTE$SpeciesB_time <- as.POSIXct(as.numeric(Weasel.Wolverine.TTE$SpeciesB_time), origin="1970-01-01")
      Weasel.Wolverine.TTE$SpeciesB_censor_time <- as.POSIXct(as.numeric(Weasel.Wolverine.TTE$SpeciesB_censor_time), origin="1970-01-01")
      break
    }  
    
    # CASE 1: Marten/Wolverine
    if (marten.data$Species[j] == 'Short-tailed Weasel' & marten.data$Species[j + 1] == 'Wolverine'){
      # no censor 
      flag <- 1
      camera.ID.data <- marten.data$Camera_ID[j]
      elevation.data <- marten.data$ELEVATION[j]
      landcover.data <- as.character(marten.data$LANDCOVER1[j])
      tri.data <- as.character(marten.data$TRI[j])
      snow.depth.data <- as.numeric(marten.data$Snow_Depth[j + 1])
      solar.time.data <- as.numeric(marten.data$suntime[j + 1])
      speciesA.data <- marten.data$Species[j]
      speciesA.time.data <- marten.data$date_time[j]
      speciesB.data <- marten.data$Species[j + 1]
      speciesB.time.data <- marten.data$date_time[j + 1]
      speciesB.censor <- "NA"
      speciesB.censor.time <- "0"
      status.data <- flag
      time.event.data <- as.numeric(difftime(speciesB.time.data, speciesA.time.data, units = "hours"))
      # censor data entry if outside time range
      if (time.event.data > 120){
        status.data <- 0
      }
      tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                 speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time, 
                                                 time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
    }
    
    # CASE 2 and 3: Marten/'Species C'...n/Wolverine
    # if first entry = marten and second entry = Species C, look to see if wolverine shows up
    if (marten.data$Species[j] == 'Short-tailed Weasel' & marten.data$Species[j + 1] != 'Short-tailed Weasel' & marten.data$Species[j + 1] != 'Wolverine'){
      for (i in j:nrow(marten.data)){
        
        if (i == nrow(marten.data)){
          break
        }
        
        # look for 'Species C' / Wolverine
        if ((marten.data$Species[i] != 'Short-tailed Weasel' & marten.data$Species[i] != 'Wolverine') & marten.data$Species[i + 1] == 'Wolverine'){
          # censor 
          flag <- 0
          camera.ID.data <- marten.data$Camera_ID[j]
          elevation.data <- marten.data$ELEVATION[j]
          landcover.data <- as.character(marten.data$LANDCOVER1[j])
          tri.data <- as.character(marten.data$TRI[j])
          snow.depth.data <- as.numeric(marten.data$Snow_Depth[i + 1])
          solar.time.data <- as.numeric(marten.data$suntime[i + 1])
          speciesA.data <- marten.data$Species[j]
          speciesA.time.data <- marten.data$date_time[j]
          speciesB.data <- marten.data$Species[i]
          speciesB.time.data <- marten.data$date_time[i]
          speciesB.censor <- marten.data$Species[i + 1]
          speciesB.censor.time <- marten.data$date_time[i + 1]
          status.data <- flag
          time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "hours")
          tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, 
                                                     speciesB.data, speciesB.time.data, speciesB.censor, speciesB.censor.time,
                                                     time.event.data, status.data, elevation.data, landcover.data, tri.data, snow.depth.data, solar.time.data)
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
        # CASE 4:
        if ((marten.data$Species[i] != 'Short-tailed Weasel' & marten.data$Species[i] != 'Wolverine') & marten.data$Species[i + 1] == 'Short-tailed Weasel'){
          counter <- i - j
          j <- j + counter
          counter <- 0
          break
        }
      }
    }
    j <- j + 1
  }
}

save(Marten.Wolverine.TTE, file = 'file location here')
save(Marten.Weasel.TTE, file = 'file location here')
save(Wolverine.Marten.TTE, file = 'file location here')
save(Wolverine.Weasel.TTE, file = 'file location here')
save(Weasel.Marten.TTE, file = 'file location here')
save(Weasel.Wolverine.TTE, file = 'file location here')


####################
# Solar Time Column FIX (Solar time based on wrong time zone) #
#####################
library(tidyverse)
library(plyr)
library(rgdal)
library(tidyverse)
library(survival)
library(survminer)
library(overlap)
library(sp)
library(maptools)
# Set timezone
Sys.setenv(TZ='GMT')
load(file = 'file location here')
tte.species.df <- apa.df
tte.species.df$Camera_ID <- as.numeric(apa.df$Camera_ID)
test.df <- Weasel.Wolverine.TTE
solar.time.df <- tte.species.df
for (i in 1:nrow(test.df)){
  date.time <- test.df$SpeciesB_time[i]
  camera.id <- test.df$Camera_ID[i]
  species.B <- test.df$SpeciesB[i]
  for (j in 1:nrow(solar.time.df)){
    date.time2 <- solar.time.df$date_time[j]
    camera.id2 <- solar.time.df$Camera_ID[j]
    species.B2 <- solar.time.df$Species[j]
    if ((date.time == date.time2) &
        (camera.id == camera.id2) &
        (species.B == species.B2)){
      test.df$Solar_Time[i] <- solar.time.df$suntime[j]
    }
  }
}

Marten.Wolverine.TTE <- test.df
Marten.Weasel.TTE <- test.df

Wolverine.Marten.TTE <- test.df
Wolverine.Weasel.TTE <- test.df

Weasel.Marten.TTE <- test.df
Weasel.Wolverine.TTE <- test.df

save(Marten.Wolverine.TTE, file = 'file location here')
save(Marten.Weasel.TTE, file = 'file location here')
save(Wolverine.Marten.TTE, file = 'file location here')
save(Wolverine.Weasel.TTE, file = 'file location here')
save(Weasel.Marten.TTE, file = 'file location here')
save(Weasel.Wolverine.TTE, file = 'file location here')