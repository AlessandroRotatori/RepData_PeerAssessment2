## SETTO LA LIBRERIA ###
setwd("/Users/rotatori/Documents/GitHub/RepData_PeerAssessment2")

## DOWNLOAD FILE FROM INTERNET ###
download.file(url ="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              destfile = "repdata_data_StormData.csv.bz2", method = "libcurl")

## LIBRARIES ##
library(dplyr)
library(lubridate)
library(stringr)


## LOAD DATA IN R ##

storm.data.raw <- read.csv2(file = "repdata_data_StormData.csv.bz2", header = TRUE, sep = ",")

## SELECT THE RELEVANT VARIABLES ##
storm.data.filter.raw <- storm.data.raw  %>%
                select(BGN_DATE, STATE, EVTYPE, 
                      FATALITIES, INJURIES, 
                      PROPDMG, PROPDMGEXP,
                      CROPDMG, CROPDMGEXP)

## VARIABLES TRANSFORMATION
storm.data <- storm.data.filter.raw %>%
                mutate(
                  BGN_DATE = mdy_hms(BGN_DATE),
                  FATALITIES = as.numeric(FATALITIES),
                  INJURIES = as.numeric(INJURIES),
                  PROPDMG = as.numeric(PROPDMG),
                  CROPDMG = as.numeric(CROPDMG),
                  STATE = as.character(STATE),
                  EVTYPE = as.character(EVTYPE)
                )

str(storm.data)
## HANDLING OF VARIABLE EVTYPE ##