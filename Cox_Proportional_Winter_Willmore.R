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
library(survival)
library(survminer)


# Winter Willmore

# load in data Willmore
load(file = 'file location here')
camera.trap.data <- bfmin
camera.trap.data <- rename(camera.trap.data, c("Site" = "Camera_ID"))
camera.trap.data <- camera.trap.data[!(is.na(camera.trap.data$Camera_ID) | camera.trap.data$Camera_ID=="" | camera.trap.data$Camera_ID=="55?"), ]

# remove data which falls outside the Date Range
camera.trap.data$Date <- as.Date(camera.trap.data$Date, format = "%d-%B-%y")
camera.trap.data <- camera.trap.data[(camera.trap.data$Date >= "2006-12-10" & camera.trap.data$Date <= "2007-03-19")| 
                                       (camera.trap.data$Date >= "2007-12-10" & camera.trap.data$Date <= "2008-03-19"),]

#add status column '1' for now
camera.trap.data$Status <- '1'

# convert species column to character
camera.trap.data$Species <- as.character(camera.trap.data$Species)

# Start wolf pairing process
tte.species.df <- camera.trap.data

# Sort by Camera_ID, then date time
tte.species.df <- tte.species.df[order(as.numeric(tte.species.df$Camera_ID), tte.species.df$date_time),]

# create dataframe to hold time values
tte.time2.df <- data.frame()
tte.time2.df$Camera_ID <- vector(mode="character")
tte.time2.df$SpeciesA <- vector(mode="character")
tte.time2.df$SpeciesA_time <- vector(mode="character")
tte.time2.df$SpeciesB <- vector(mode="character")
tte.time2.df$SpeciesB_time <- vector(mode="character")
tte.time2.df$time_to_event_hours <- vector(mode="character")
tte.time2.df$Status <- vector(mode="character")


# look for wolf/speciesB pairs
for (j in 1:nrow(tte.species.df)){
  
  # if reached end of list, end loop.
  if (j == nrow(tte.species.df)){
    # convert the time_data columns in the final dataframe to a readable format
    tte.time2.df$SpeciesA_time <- as.POSIXct(as.numeric(tte.time2.df$SpeciesA_time), origin="1970-01-01")
    tte.time2.df$SpeciesB_time <- as.POSIXct(as.numeric(tte.time2.df$SpeciesB_time), origin="1970-01-01")
    break
  }  
  
  # if there is a speciesB right after a wolf, and the sites are the same
  if ((tte.species.df$Camera_ID[j] == tte.species.df$Camera_ID[j+1]) & 
      (tte.species.df$Species[j] == 'Wolverine' & tte.species.df$Species[j+1] != 'Wolverine')){
    # add these values to the final dataframe.
    camera.ID.data <- tte.species.df$Camera_ID[j]
    speciesA.data <- tte.species.df$Species[j]
    speciesA.time.data <- tte.species.df$date_time[j]
    speciesB.data <- tte.species.df$Species[j+1]
    speciesB.time.data <- tte.species.df$date_time[j+1]
    status.data <- tte.species.df$Status[j+1]
    time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "hours")
    #time.event.data <- difftime(speciesB.time.data, speciesA.time.data, units = "days")
    # add the above data to a new row in the tte.time2 dataframe
    # IF captured within a week (168 hours)
    if (time.event.data <= 168){
      tte.time2.df[nrow(tte.time2.df) + 1,] <- c(camera.ID.data, speciesA.data, speciesA.time.data, speciesB.data, speciesB.time.data, time.event.data, status.data)
    }
  }
}

# View the tte.time2.df, this is the final time to event data frame
tte.time2.df <- tte.time2.df[order(as.numeric(tte.time2.df$time_to_event_hours)),]

# only species we want (FOR PLOTTING PURPOSES)
tte.time2.df <- tte.time2.df[(tte.time2.df$SpeciesB == 'Marten' | tte.time2.df$SpeciesB == 'Short-tailed Weasel'), ]

tte.data <- tte.time2.df
tte.data$time_to_event_hours <- as.numeric(tte.data$time_to_event_hours)
tte.data$Status <- as.numeric(tte.data$Status)

## start Cox Regression

res.cox <- coxph(Surv(time_to_event_hours, Status) ~ SpeciesB, data = tte.data)
res.cox

summary(res.cox)

res.cox <- coxph(Surv(time_to_event_hours, Status) ~ SpeciesB, data = tte.data)

# Specify the argument data to either survfit()
ggsurvplot(survfit(res.cox, data = tte.data), palette = "#2E9FDF")
