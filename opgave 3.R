### Opgave 3 - Circling planes over the North Sea

library(httr)
library(jsonlite)
library(leaflet)
source("util.R")   # contains getToken()

# Folder with the circjet.rds files
# "." means the working directory
circfolder <- "."

baseurl   <- "https://opensky-network.org/api"
endpointS <- "/states/all"
endpointT <- "/tracks/all"

# Column names for a track (same as colnv in getCircI.R)
colnv <- c("icao24","callsign","startTime","endTime",
           "time","lat","lng","alt","crs","grd")


#### HELPER FUNCTIONS ##########################################

# Replace a missing value (NULL) with NA
orNA <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  return(x)
}

# Fetch the track of one plane (icao24) and return it as a data frame.
# Returns NULL if OpenSky has no track for the plane.
getTrack <- function(icao, token) {
  turl <- paste0(baseurl, endpointT, "?icao24=", icao, "&time=0")
  res  <- GET(turl, add_headers(Authorization = paste("Bearer", token)))
  
  if (res$status_code != 200) {
    cat("  no track, status code:", res$status_code, "\n")
    return(NULL)
  }
  
  resretval <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  path <- resretval$path
  if (is.null(path) || length(path) == 0) {
    return(NULL)
  }
  
  # Build the data frame column by column. Some planes have no
  # callsign (JSON null), so we use orNA() to put NA there instead.
  # path columns: 1 time, 2 lat, 3 lng, 4 alt, 5 crs, 6 on ground
  trackdf <- data.frame(
    icao24    = orNA(resretval$icao24),
    callsign  = orNA(resretval$callsign),
    startTime = orNA(resretval$startTime),
    endTime   = orNA(resretval$endTime),
    time      = path[, 1],
    lat       = path[, 2],
    lng       = path[, 3],
    alt       = path[, 4],
    crs       = path[, 5],
    grd       = path[, 6]
  )
  
  # så columns er numre
  numcols <- c("startTime","endTime","time","lat","lng","alt","crs")
  for (col in numcols) {
    trackdf[[col]] <- as.numeric(trackdf[[col]])
  }
  return(trackdf)
}

# Course (crs) runs 0-360 degrees. Going from 359 to 1 is a 2-degree
# turn, but diff() says -358. This function fixes that and adds two
# new columns:
#   dcrs    = change in course between two points (-180 to 180)
#   crs_unw = "unwrapped" course, which keeps counting past 360
#             (one full circle to the right = +360)
addCourseChange <- function(df) {
  df <- df[!is.na(df$crs), ]          # drop points without course
  
  dcrs <- diff(df$crs)               
  dcrs[dcrs >  180] <- dcrs[dcrs >  180] - 360   # e.g.  358 -> -2
  dcrs[dcrs < -180] <- dcrs[dcrs < -180] + 360   # e.g. -358 ->  2
  
  df$dcrs    <- c(0, dcrs)            # first point has no change
  df$crs_unw <- df$crs[1] + cumsum(df$dcrs)
  return(df)
}

# Summarise one track in a single row
courseStats <- function(df) {
  df <- addCourseChange(df)
  data.frame(
    icao24      = df$icao24[1],
    callsign    = trimws(df$callsign[1]),
    n_points    = nrow(df),
    sd_crs      = sd(df$crs),         # sd of the raw course
    sd_crs_unw  = sd(df$crs_unw),     # sd of the unwrapped course
    sd_dcrs     = sd(df$dcrs),        # sd of the course changes
    turns       = sum(df$dcrs) / 360  # total turning in full circles
  )
}

myMapPlot <- function(df) {
  leaflet(df) %>%
    addTiles() %>%
    addPolylines(lat = ~lat, lng = ~lng)
}


#### OPGAVE 3.1 - Hent fly og plot #############################

# North Sea bounding box
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954

fullurl <- paste0(baseurl, endpointS,
                  "?lamin=", lamin, "&lomin=", lomin,
                  "&lamax=", lamax, "&lomax=", lomax)

token     <- getToken()
res       <- GET(fullurl, add_headers(Authorization = paste("Bearer", token)))
resretval <- fromJSON(content(res, as = "text"))
statedf   <- as.data.frame(resretval$states)

# V1 = icao24, V2 = callsign, V3 = origin country
icaov <- statedf$V1
cat("Number of planes over the north sea right now:", length(icaov), "\n")

# Count planes per country, largest first
countrytab <- table(statedf$V3)
countrytab <- sort(countrytab, decreasing = TRUE)
print(countrytab)

# Plot
par(mfrow = c(1, 1), mar = c(12, 4, 3, 1), oma = c(1.5, 0, 0, 0))
ymax <- max(countrytab) * 1.15
bp <- barplot(countrytab,
              las  = 2,
              col  = "steelblue",
              main = "Størstedelen af fly over nordsøen er fra de britiske øer",
              ylab = "Number of planes",
              ylim = c(0, ymax))

abline(h = axTicks(2), col = "grey85")
#Så columnsne ligger oven på linjerne
barplot(countrytab, col = "steelblue", ylim = c(0, ymax),
        add = TRUE, axes = FALSE, axisnames = FALSE)

top <- 1:min(5, length(countrytab))
text(x = bp[top], y = countrytab[top], labels = countrytab[top],
     pos = 3, cex = 0.8)
mtext("Source: Data hentet fra OpenSky API, behandlet af os", side = 1, outer = TRUE, adj = 1,
      line = 0.5, cex = 0.8, col = "grey40")

#Genstil
par(mar = c(5, 4, 4, 2), oma = c(0, 0, 0, 0))
par(mar = c(5, 4, 4, 2))                    



#### OPGAVE 3.2 - Start small: one plane #######################

# One "normal" plane from the fresh list
normaldf <- getTrack(icaov[1], token)

# If the first plane has no track, try the next ones
i <- 2
while (is.null(normaldf) && i <= length(icaov)) {
  normaldf <- getTrack(icaov[i], token)
  i <- i + 1
}

# One circling plane from the rds files
circdf <- readRDS(file.path(circfolder, "circjet.rds"))

# Time in minutes since the first point (easier to read than unix time)
normaldf$minutes <- (normaldf$time - normaldf$time[1]) / 60
circdf$minutes   <- (circdf$time   - circdf$time[1])   / 60

par(mfrow = c(2, 2))
plot(normaldf$lng, normaldf$lat, type = "l",
     main = paste("Normal:", trimws(normaldf$callsign[1])),
     xlab = "Longitude", ylab = "Latitude")
plot(circdf$lng, circdf$lat, type = "l",
     main = paste("Circling:", trimws(circdf$callsign[1])),
     xlab = "Longitude", ylab = "Latitude")
plot(normaldf$minutes, normaldf$crs, type = "l", ylim = c(0, 360),
     main = "Normal: course", xlab = "Minutes", ylab = "Course (deg)")
plot(circdf$minutes, circdf$crs, type = "l", ylim = c(0, 360),
     main = "Circling: course", xlab = "Minutes", ylab = "Course (deg)")
par(mfrow = c(1, 1))

# Maps (one at a time in the Viewer)
myMapPlot(normaldf)
myMapPlot(circdf)


#### OPGAVE 3.3 - Move fast: all planes ########################

token    <- getToken()          # fresh token (they expire after 30 min)
tracklist <- list()             # all tracks, saved for later use
statlist  <- list()             # one row of stats per plane

for (i in seq_along(icaov)) {
  icao <- icaov[i]
  cat(i, "of", length(icaov), ":", icao, "\n")
  
  # tryCatch: if anything goes wrong for this plane, print the
  # error and set trackdf to NULL, so the loop continues
  trackdf <- tryCatch(
    getTrack(icao, token),
    error = function(e) {
      cat("  skipped, error:", conditionMessage(e), "\n")
      return(NULL)
    }
  )
  
  if (!is.null(trackdf) && nrow(trackdf) > 2) {
    trackdf <- addCourseChange(trackdf)
    tracklist[[icao]] <- trackdf
    statlist[[icao]]  <- courseStats(trackdf)
  }
  
  Sys.sleep(1)                  # be polite to the API
}

# How many planes did we actually get a track for?
cat("Normal planes with a track:", length(statlist), "of", length(icaov), "\n")
if (length(statlist) == 0) {
  stop("No tracks fetched - check token / API limit (see status codes)")
}

# Stack the one-row data frames into one table
normalstats <- do.call(rbind, statlist)
normalstats$type <- "normal"

# Same stats for the circling planes, for comparison.
# A plain loop so we can see each file's column count.
circfiles <- list.files(path = circfolder,
                        pattern = "^circjet.*\\.rds$",
                        full.names = TRUE)   # full path, so readRDS finds them
print(circfiles)
if (length(circfiles) == 0) {
  stop("No circjet files found in: ", normalizePath(circfolder))
}

circlist <- list()
for (f in circfiles) {
  circdf <- readRDS(f)
  cat(f, ":", ncol(circdf), "columns\n")
  circlist[[f]] <- courseStats(circdf)
}

circstats <- do.call(rbind, circlist)
circstats$type <- "circling"

allstats <- rbind(normalstats, circstats)
allstats <- allstats[order(allstats$sd_crs_unw, decreasing = TRUE), ]
print(allstats)

# Save so you do not have to call the API again
saveRDS(tracklist, "normaltracks.rds")
saveRDS(allstats,  "coursestats.rds")

# Compare the two groups

par(oma = c(1.5, 0, 0, 0))
ytop   <- ceiling(max(allstats$sd_crs_unw) / 50) * 50
yticks <- seq(0, ytop, by = 25)        # a label every 25 degrees

boxplot(sd_crs_unw ~ type, data = allstats,
        main = "Standardafvigelse af unwrapped kurs viser \n forskel mellem cirklende og ikke cirklende",
        ylab = "Degrees", col = c("tomato", "steelblue"),
        ylim = c(0, ytop), yaxt = "n")
axis(2, at = yticks, las = 1, cex.axis = 0.8)
abline(h = yticks, col = "grey90")

boxplot(sd_crs_unw ~ type, data = allstats,
        col = c("tomato", "steelblue"),
        add = TRUE, axes = FALSE)
mtext("Kilde: Data hentet fra OpenSky API, behandlet af os", side = 1, outer = TRUE, adj = 1,
      line = 0.5, cex = 0.8, col = "grey40")

par(oma = c(0, 0, 0, 0))   # reset outer margin




      