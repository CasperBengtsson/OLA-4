#################################################################
# functions.R - shared helper functions for Opgave 3.4 and 3.5
# Load it with: source("functions.R")
#################################################################

library(httr)
library(jsonlite)

baseurl   <- "https://opensky-network.org/api"
endpointS <- "/states/all"
endpointT <- "/tracks/all"


#### FETCHING DATA (same as in opgave_3.R) #####################

# Replace a missing value (NULL) with NA
orNA <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  return(x)
}

# Fetch all planes inside a bounding box right now.
# box is a vector like c(lamin = 54.5, lamax = 57.9, lomin = 7.3, lomax = 12.6)
getStates <- function(box, token) {
  fullurl <- paste0(baseurl, endpointS,
                    "?lamin=", box["lamin"], "&lomin=", box["lomin"],
                    "&lamax=", box["lamax"], "&lomax=", box["lomax"])
  res <- GET(fullurl, add_headers(Authorization = paste("Bearer", token)))
  if (res$status_code != 200) {
    stop("states/all failed, status code: ", res$status_code)
  }
  resretval <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  statedf   <- as.data.frame(resretval$states)
  return(statedf)   # V1 = icao24, V2 = callsign, V3 = country
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

  numcols <- c("startTime","endTime","time","lat","lng","alt","crs")
  for (col in numcols) {
    trackdf[[col]] <- as.numeric(trackdf[[col]])
  }
  return(trackdf)
}


#### COURSE HANDLING ###########################################

# Adds dcrs (change in course, -180 to 180) and crs_unw (unwrapped
# course that keeps counting past 360). See opgave_3.R.
addCourseChange <- function(df) {
  df <- df[!is.na(df$crs), ]

  dcrs <- diff(df$crs)
  dcrs[dcrs >  180] <- dcrs[dcrs >  180] - 360
  dcrs[dcrs < -180] <- dcrs[dcrs < -180] + 360

  df$dcrs    <- c(0, dcrs)
  df$crs_unw <- df$crs[1] + cumsum(df$dcrs)
  return(df)
}


#### THE THREE DETECTION MEASURES ##############################

# Method 1 - sdcourse: standard deviation of the unwrapped course.
# Circling makes the unwrapped course climb or fall a lot -> high sd.
sdCourse <- function(df) {
  df <- addCourseChange(df)
  return(sd(df$crs_unw))
}

# Method 2 - lmres: how well does a straight line fit the route?
# We fit lat ~ lng and take R-squared (0 = no fit, 1 = perfect line).
# A plane flying straight gives R-squared close to 1.
lmR2 <- function(df) {
  model <- lm(lat ~ lng, data = df)
  r2    <- summary(model)$r.squared
  # If lat or lng never changes, R-squared cannot be calculated (NaN).
  # That only happens for a perfectly straight east-west or
  # north-south line, so we call it a straight line (1).
  if (is.na(r2)) {
    r2 <- 1
  }
  return(r2)
}

# Method 3 - myOwnAlg: the largest turn the plane makes within any
# window of 'window_min' minutes.
# For every point we look at the points in the next window_min
# minutes and measure how far the unwrapped course has moved
# (largest value minus smallest). We keep the largest of these.
# A full circle = 360 degrees within the window.
maxTurn <- function(df, window_min = 20) {
  df <- addCourseChange(df)
  if (nrow(df) < 3) {
    return(0)
  }

  best <- 0
  for (i in 1:nrow(df)) {
    start <- df$time[i]
    inwindow <- df$time >= start & df$time <= start + window_min * 60
    turn <- max(df$crs_unw[inwindow]) - min(df$crs_unw[inwindow])
    if (turn > best) {
      best <- turn
    }
  }
  return(best)
}

# Calculate all three measures for one track -> one row
measureTrack <- function(df, window_min = 20) {
  data.frame(
    icao     = df$icao24[1],
    callsign = trimws(df$callsign[1]),
    sd_unw   = sdCourse(df),
    r2       = lmR2(df),
    maxturn  = maxTurn(df, window_min)
  )
}
