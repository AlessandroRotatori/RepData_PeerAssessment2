### CHECK



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

