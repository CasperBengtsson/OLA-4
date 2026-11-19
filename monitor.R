#################################################################
# Opgave 3.5 - Automate: monitor an area for circling planes
# Each run:
#   1. fetches all planes in the area
#   2. fetches their tracks and runs myOwnAlg (maxTurn)
#   3. saves the list of suspicious planes in a csv file
#   4. emails the list if there are any suspicious planes
#   5. writes one line in a log file, so you can see it ran
# Run it by hand, or let Windows Task Scheduler run it (see chat).
#################################################################

source("util.R")        # getToken()
source("functions.R")   # getStates(), getTrack(), maxTurn() ...
library(emayili)        # sending email

#### SETTINGS ###################################################

# Area to monitor (Over Danmark)
box <- c(lamin = 54.546494, lamax = 57.912978,
         lomin = 7.294922,  lomax = 12.568359)

turn_limit <- 300       # same thresholds as in Opgave 3.4
window_min <- 20

# TEST MODE: also runs the teacher's circling planes through the
# algorithm, so there is always something to find and email.
# Set to FALSE for real monitoring.
testmode   <- FALSE
circfolder <- "."

# Email
mail_from <- "helene.signe.bengtsson@gmail.com"     # your Gmail
mail_to   <- "helene.signe.bengtsson@gmail.com"     # recipient
send_mail <- TRUE

outfolder <- "monitor_output"             # where the csv files go
if (!dir.exists(outfolder)) {
  dir.create(outfolder)
}

runtime <- format(Sys.time(), "%Y-%m-%d_%H%M")   # e.g. 2026-11-15_1845


#### STEP 1: Collect tracks #####################################

token   <- getToken()
statedf <- getStates(box, token)
icaov   <- statedf$V1
cat("Planes in the area now:", length(icaov), "\n")

tracks <- list()
for (i in seq_along(icaov)) {
  icao <- icaov[i]
  cat(i, "of", length(icaov), ":", icao, "\n")
  trackdf <- tryCatch(
    getTrack(icao, token),
    error = function(e) {
      cat("  skipped, error:", conditionMessage(e), "\n")
      return(NULL)
    }
  )
  if (!is.null(trackdf) && nrow(trackdf) > 2) {
    tracks[[icao]] <- trackdf
  }
  Sys.sleep(1)
}

if (testmode) {
  circfiles <- list.files(path = circfolder, pattern = "^circjet.*\\.rds$",
                          full.names = TRUE)
  for (f in circfiles) {
    tracks[[paste0("TEST_", basename(f))]] <- readRDS(f)
  }
}


#### STEP 2: Run our algorithm on every track ###################

checklist <- list()
for (name in names(tracks)) {
  df <- tracks[[name]]
  lastrow <- nrow(df)
  checklist[[name]] <- data.frame(
    icao     = df$icao24[1],
    callsign = trimws(df$callsign[1]),
    maxturn  = round(maxTurn(df, window_min)),
    last_lat = df$lat[lastrow],                  # latest position
    last_lng = df$lng[lastrow],
    last_seen = format(as.POSIXct(df$time[lastrow], origin = "1970-01-01"),
                       "%Y-%m-%d %H:%M"),
    test     = startsWith(name, "TEST_")
  )
}
checked <- do.call(rbind, checklist)
rownames(checked) <- NULL

suspicious <- checked[checked$maxturn >= turn_limit, ]
cat("Suspicious planes:", nrow(suspicious), "\n")
print(suspicious)


#### STEP 3: Save the list in a file ############################

outfile <- file.path(outfolder, paste0("suspicious_", runtime, ".csv"))
write.csv(suspicious, outfile, row.names = FALSE)
cat("Saved:", outfile, "\n")


#### STEP 4: Email the list #####################################

mailstatus <- "not sent (no suspicious planes)"

if (send_mail && nrow(suspicious) > 0) {
  password <- Sys.getenv("GMAIL_APP_PW")    # read from .Renviron
  if (password == "") {
    mailstatus <- "not sent (GMAIL_APP_PW missing)"
  } else {
    # One line of text per suspicious plane
    lines <- paste(suspicious$callsign, suspicious$icao,
                   "- turned", suspicious$maxturn, "deg in", window_min, "min",
                   "- last seen", suspicious$last_seen,
                   "at", round(suspicious$last_lat, 3), round(suspicious$last_lng, 3))
    body <- paste0("Suspicious (circling) planes found ", runtime, ":\n\n",
                   paste(lines, collapse = "\n"))

    email <- envelope(from = mail_from, to = mail_to,
                      subject = paste("Circling planes", runtime),
                      text = body)
    email <- attachment(email, path = outfile)

    smtp <- server(host = "smtp.gmail.com", port = 465,
                   username = mail_from, password = password)

    mailstatus <- tryCatch({
      smtp(email)
      "sent"
    }, error = function(e) {
      paste("FAILED:", conditionMessage(e))
    })
  }
}
cat("Email:", mailstatus, "\n")


#### STEP 5: Log line ###########################################

logline <- paste(runtime, "| planes:", length(icaov),
                 "| tracks:", length(tracks),
                 "| suspicious:", nrow(suspicious),
                 "| email:", mailstatus)
cat(logline, "\n", file = file.path(outfolder, "monitor_log.txt"), append = TRUE)
