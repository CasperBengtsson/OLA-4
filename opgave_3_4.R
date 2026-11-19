#################################################################
# Opgave 3.4 - Aim high: our own algorithm
# Compares three ways of spotting circling planes:
#   sdcourse - standard deviation of the unwrapped course is high
#   lmres    - a straight line fits the route badly (low R-squared)
#   myOwnAlg - the plane turns (almost) a full circle within 20 min
# Uses the tracks saved in Opgave 3.3, so no API calls are needed.
#################################################################

library(leaflet)
source("functions.R")

circfolder <- "."   # folder with the circjet*.rds files

# Thresholds - a plane is flagged as circling (1) when:
sd_limit   <- 120   # sd of unwrapped course is above this
r2_limit   <- 0.8   # R-squared of the straight-line fit is below this
turn_limit <- 300   # it turns at least this many degrees ...
window_min <- 20    # ... within this many minutes


#### STEP 1: Collect all tracks ################################

# Normal planes: the fresh tracks saved in Opgave 3.3
normaltracks <- readRDS("normaltracks.rds")

# Circling planes: the teacher's rds files
circfiles <- list.files(path = circfolder,
                        pattern = "^circjet.*\\.rds$",
                        full.names = TRUE)
circtracks <- list()
for (f in circfiles) {
  circtracks[[f]] <- readRDS(f)
}

cat("Normal tracks:", length(normaltracks),
    " Circling tracks:", length(circtracks), "\n")


#### STEP 2: Measure every track ###############################

measurelist <- list()

for (name in names(normaltracks)) {
  row <- measureTrack(normaltracks[[name]], window_min)
  row$isOff <- 0                       # fresh planes = assumed normal
  measurelist[[name]] <- row
}

for (name in names(circtracks)) {
  row <- measureTrack(circtracks[[name]], window_min)
  row$isOff <- 1                       # teacher's planes = circling
  measurelist[[name]] <- row
}

measures <- do.call(rbind, measurelist)
rownames(measures) <- NULL             # tidy row numbers 1, 2, 3 ...

# The raw numbers - useful for choosing the thresholds
print(measures[order(measures$maxturn, decreasing = TRUE), ])


#### STEP 3: Turn the measures into 0/1 verdicts ###############

# as.integer(TRUE) = 1 and as.integer(FALSE) = 0
results <- data.frame(
  icao     = measures$icao,
  sdcourse = as.integer(measures$sd_unw  >  sd_limit),
  lmres    = as.integer(measures$r2      <  r2_limit),
  myOwnAlg = as.integer(measures$maxturn >= turn_limit),
  isOff    = measures$isOff
)

# Same layout as the table in the assignment
print(results)
View(results)


#### STEP 4: How good is each method? ##########################

# For each method: how many circling planes did it catch,
# and how many normal planes did it wrongly flag?
evaluate <- function(name, verdict, truth) {
  caught      <- sum(verdict == 1 & truth == 1)
  falsealarms <- sum(verdict == 1 & truth == 0)
  correct     <- sum(verdict == truth)
  cat(sprintf("%-9s caught %d of %d circling | false alarms: %d | correct overall: %d of %d\n",
              name, caught, sum(truth == 1), falsealarms,
              correct, length(truth)))
}

evaluate("sdcourse", results$sdcourse, results$isOff)
evaluate("lmres",    results$lmres,    results$isOff)
evaluate("myOwnAlg", results$myOwnAlg, results$isOff)

# The full picture for our own algorithm: rows = verdict, cols = truth
table(verdict = results$myOwnAlg, truth = results$isOff)


#### STEP 5: Plots ##############################################

# One box per group for each measure, with the threshold as a line
par(mfrow = c(1, 3))
boxplot(sd_unw ~ isOff, data = measures, names = c("normal", "circling"),
        main = "sdcourse", ylab = "SD of unwrapped course (deg)",
        col = c("steelblue", "tomato"))
abline(h = sd_limit, lty = 2)
boxplot(r2 ~ isOff, data = measures, names = c("normal", "circling"),
        main = "lmres", ylab = "R-squared of lat ~ lng",
        col = c("steelblue", "tomato"))
abline(h = r2_limit, lty = 2)
boxplot(maxturn ~ isOff, data = measures, names = c("normal", "circling"),
        main = "myOwnAlg", ylab = paste("Max turn within", window_min, "min (deg)"),
        col = c("steelblue", "tomato"))
abline(h = turn_limit, lty = 2)
par(mfrow = c(1, 1))

# Look closer at the normal planes our algorithm flags (false alarms?)
flagged <- results$icao[results$myOwnAlg == 1 & results$isOff == 0]
print(flagged)
if (length(flagged) > 0) {
  df <- normaltracks[[flagged[1]]]
  print(leaflet(df) %>% addTiles() %>% addPolylines(lat = ~lat, lng = ~lng))
}

saveRDS(results, "results_3_4.rds")
