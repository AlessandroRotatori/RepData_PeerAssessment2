## SETTO LA LIBRERIA ###
## setwd("/Users/rotatori/Documents/GitHub/RepData_PeerAssessment2")
setwd("C:/Users/alessandro.rotatori/Documents/test")

## DOWNLOAD FILE FROM INTERNET ###
download.file(url ="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              destfile = "repdata_data_StormData.csv.bz2", method = "libcurl")
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
              destfile = "repdata.pdf", method = "libcurl")

## LIBRARIES ##
library(dplyr)
library(lubridate)
library(stringr)
library(pdftools)
library(stringdist)
library(xtable)

## LOAD DATA IN R ##

loaded.ds <- read.csv2(file = "repdata_data_StormData.csv.bz2", header = TRUE, sep = ",", stringsAsFactors = FALSE)



set.seed(111)

loaded.ds.sample <- loaded.ds[sample(x = nrow(loaded.ds), size = 10, replace = FALSE),]
loaded.ds.sample

xt <- xtable()
print(xt, type="html")

rm(loaded.ds.sample, xt)

transform.exp <- function(x){
  if (x %in% c("", "-", "+", "?")) {return(0)}
  else if (x %in% c("h", "H")) {return(2)}
  else if (x %in% c("k", "K")) {return(3)}
  else if (x %in% c("m", "M")) {return(6)}
  else if (x %in% c("b", "B")) {return(9)}
  else if (!is.na(as.numeric(x))) {return(as.numeric(x))}
  else if (is.na(as.numeric(x))) {return(0)}
  else {stop("Value Not Applicable")}
}

storm.data.raw <- loaded.ds %>%
  mutate(BGN_DATE = mdy_hms(BGN_DATE),
         FATALITIES = as.numeric(FATALITIES),
         INJURIES = as.numeric(INJURIES),
         PROPDMGEXP = sapply(X = PROPDMGEXP, FUN = transform.exp),
         CROPDMGEXP = sapply(X = CROPDMGEXP, FUN = transform.exp),
         PROPDMG = as.numeric(PROPDMG)*(10^PROPDMGEXP),
         CROPDMG = as.numeric(CROPDMG)*(10^CROPDMGEXP),
         EVTYPE = trimws(str_to_upper(EVTYPE))) %>%
  select(BGN_DATE, STATE, EVTYPE, 
         FATALITIES, INJURIES, 
         PROPDMG, CROPDMG)
#rm(loaded.ds)#

#rm(storm.data.raw.sample, xt)#


## SELECT THE RELEVANT VARIABLES AND  ##
storm.data.filter.raw <- storm.data.raw  %>%
                select(BGN_DATE,
                        STATE,
                        EVTYPE, 
                        FATALITIES, INJURIES, 
                        PROPDMG, PROPDMGEXP,
                        CROPDMG, CROPDMGEXP)
## rm(storm.data.raw)

## FUNCTION TO CONVERT PROPDMGEXP AND CROPDMGEXP ##
exp.norm <- function(x){
  if (x %in% c("", "-", "+", "?")) {return(0)}
  else if (x %in% c("h", "H")) {return(2)}
  else if (x %in% c("k", "K")) {return(3)}
  else if (x %in% c("m", "M")) {return(6)}
  else if (x %in% c("b", "B")) {return(9)}
  else if (!is.na(as.numeric(x))) {return(as.numeric(x))}
  else if (is.na(as.numeric(x))) {return(0)}
  else {stop("Value Not Applicable")}
}

storm.data.filter.raw$PROPDMGEXP <- sapply(X = storm.data.filter.raw$PROPDMGEXP, FUN = exp.norm)
storm.data.filter.raw$CROPDMGEXP <- sapply(X = storm.data.filter.raw$CROPDMGEXP, FUN = exp.norm)

## VARIABLES TRANSFORMATION
storm.data <- storm.data.filter.raw %>%
                mutate(
                  BGN_DATE = mdy_hms(BGN_DATE),
                  FATALITIES = as.numeric(FATALITIES),
                  INJURIES = as.numeric(INJURIES),
                  PROPDMG = as.numeric(PROPDMG)*(10^PROPDMGEXP),
                  CROPDMG = as.numeric(CROPDMG)*(10^CROPDMGEXP),
                  EVTYPE = trimws(str_to_upper(EVTYPE)))
#rm(storm.data.filter.raw)

### CREATE THE VECTOR OF THE KNOWN EVENTS ##

pdf.doc <- pdf_text(pdf = "repdata.pdf")
pdf.table <- pdf.doc[6]
split1 <- str_split(string = pdf.table, pattern = "\r\n")
split2 <- trimws(str_split(string = split1[[1]][9:32], 
                           pattern = regex(pattern = '[:blank:][ZMC][:blank:]'),
                           simplify = TRUE))
known.event <- str_to_upper(c(split2[,1], trimws(str_split(string = split2[,2], 
                                        pattern = regex(pattern = '[:blank:]{2,}[ZMC]'),
                                        simplify = TRUE))[,1]))
rm(pdf.doc, pdf.table, split1, split2)

### FILTER THE TABLE FOR THE KNOWN / NOT KNOWN EVENTS
nk.event.data <- storm.data %>%
                        filter(!EVTYPE %in% known.event)

### IDENTIFY A POSSIBLE PROXY FOR NOT KNOW EVENT


#### REPLICATE THE ANALYSIS FOR ALL THE UNKNOWN VECTORS

nk.vector <- unique(nk.event.data$EVTYPE)
nk.vector.adj <- str_replace(string = nk.vector, pattern="TSTM", replacement = "THUNDERSTORM")
nk.vector.adj <- str_replace(string = nk.vector.adj, pattern="FLD", replacement = "FLOOD")

##EXTRACT THE RECORDS WITH SPELLING ERRORS IN EVENTTYPE
##WE IDENTIFY SPELLING ERROR AS MAXDIST = 2
match.event <-amatch(nk.vector.adj, known.event, maxDist = 2)

##EXTRACT THE RECORDS WITH THE HIGHEST NUMBER OF OBSERVATIONS
##
nk.vector.maxobs <- nk.event.data %>%
  group_by(EVTYPE) %>%
  summarise(total.count=n()) %>%
  filter(total.count  > 500) %>%
  select(EVTYPE)

nk.vector.maxobs.adj <- str_replace(string = nk.vector.maxobs[[1]], pattern="TSTM", replacement = "THUNDERSTORM")
nk.vector.maxobs.adj <- str_replace(string = nk.vector.maxobs.adj, pattern="FLD", replacement = "FLOOD")
nk.vector.maxobs.adj <- str_replace(string = nk.vector.maxobs.adj, pattern="FOG", replacement = "DENSE FOG")
nk.vector.maxobs.adj <- str_replace(string = nk.vector.maxobs.adj, pattern="EXTREME COLD", replacement = "COLD/WIND CHILL")
nk.vector.maxobs.adj <- str_replace(string = nk.vector.maxobs.adj, pattern="THUNDERSTORM WIND/HAIL", replacement = "THUNDERSTORM WIND")
nk.vector.maxobs.adj <- str_replace(string = nk.vector.maxobs.adj, pattern="LANDSLIDE", replacement = "AVALANCHE")

match.event.maxobs <-amatch(nk.vector.maxobs.adj, known.event, maxDist = 15)

## create final conversion table
known.conversion <- data.frame(
  not.known = c(nk.vector, nk.vector.maxobs[[1]]),
  known = c(known.event[match.event], known.event[match.event.maxobs]), stringsAsFactors = FALSE) %>%
  filter(!is.na(known))

storm.data.adj <- storm.data

for (i in 1:length(known.conversion[[1]])){
  w.rows <- which(storm.data.adj$EVTYPE==known.conversion[i,1])
  storm.data.adj$EVTYPE[w.rows] <- known.conversion[i,2]
}


## FREQUENCY OF EVTYPE ##
freq.evtype <- storm.data.adj %>%
                  filter(EVTYPE %in% known.event) %>%
                    group_by(EVTYPE) %>%
                      summarise(total.count=n()) %>%
                        arrange(desc(total.count))

## SORT OF THE TABLE ##
storm.data.sort.fat <- storm.data %>%
                          arrange(desc(FATALITIES))

## DATA INSPECTION ##
## 1. FREQUENCY of FATALITIES
freq.fatalities <- storm.data %>%
                            group_by(FATALITIES) %>%
                              summarise(total.count=n())
## 2. FREQUENCY of INJURIES
freq.injuries <- storm.data %>%
                          group_by(INJURIES) %>%
                              summarise(total.count=n())
## 3. FREQUENCY
## HANDLING OF VARIABLE EVTYPE ##


##### USELESS #########
## ADJUSTING THE BGN_TIME
test <- as.character(storm.data.filter.raw$BGN_TIME)
day_time <- as.period(vector(mode ="numeric",length = length(test)))

for (i in 1:length(test)){
  if (!is.na(test[i])){
    if (nchar(test[i])==4) {
      test[i] <- paste(substr(x = test[i], start = 1, stop = 2),
                       substr(x = test[i], start = 3, stop = 4),
                       "00",
                       sep=":")
    }else{}}else{}}

for (i in 1:length(test)){
  if (!is.na(test[i])){
    if (grepl(pattern = "PM", x = test[i])){day_time[i] <- hms("12:00:00")
    }else{}  
  }else{}}


storm.data.filter.raw$BGN_TIME <- day_time
rm(i, test, day_time)
