# MethaneCalibMarkerV2


# This list will count the number of solenoid values of
# a[[i+1]] for i \in {0, 1, 2, 3, 4, 5}
a <- list()
# Initialize the list
a[[1]] <- a[[2]] <- a[[3]] <- a[[4]] <- a[[5]] <- a[[6]] <- 0
sol.vals <- SolValues(dataframe, sol.vals)
# Input a list with the previous solenoid values,
# and a dataframe with the data from the hour we want to know about
 


# Output a re-marked dataframe
SolValues <- function(dataframe, sol.vals) {
  # Create a table of all observed solenoid valve values
  end.times <- dataframe[1:2,1]
  tabled <- table(dataframe[, 2])
  values <- as.numeric(names(table))
  tabled <- as.vector(tabled)
  j <- 0
  for (i in 1:length(sol.vals)) {
    k <- length(sol.vals[[i]])
    # sol.vals started with a zero, this corrects that!
    if (k == 1) {
      j <- 1
    }
    if ((i-1) %in% values) {
      sol.vals[[i]][k+1-j] <- tabled[which(values == (i-1))]
    }
    else {
      sol.vals[[i]][k+1-j] <- 0
    }
  }
  return(sol.vals)
 }   
 
 # Put in the dataframe in question, and a vector with the solenoid values
 # we want to exclude from our averaging 
MethCalib <- function(dataframe, bad.sols) {
  values <- as.numeric(levels(as.factor(dataframe[,2])))
  k <- length(which(values%in%bad.sols))
  if(k == 0) {
    cat("\nThere are no precision checks in the dataset for", 
        as.Date(dataframe[1, 1]), sep = " ")
  }
  else {
    indices <- as.numeric(row.names(dataframe[dataframe[, 2]%in%bad.sols, ]))
    diffs <- diff(indices) 
    # Indexes of final precision check values
    big.diffs <- indices[which(diffs > 1)]
    # The final time(s) of the precision check(s)
    end.times <- strptime(c(paste(dataframe[big.diffs, 1], tz = "GMT"), 
                            paste(dataframe[max(indices), 1], tz = "GMT")),
                            format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    dataframe[,2][dataframe[,1] >= end.times & dataframe[,1] <= end.times + 300] <- 9
    dataframe[,2][dataframe[,2]%in%bad.sols] <- 9
  }
  dataframe
}
  
    
    


###########################
# Calibration Period Marker#
###########################

# This omits solenoid values from the hourly averages coded with anything other than "0",
# And 5 minutes after them.
CalibMarker <- function(dataframe) {

  # The possible Calibrations are those that are not marked "0", "9", or "2"
  possibles <- dataframe[dataframe[, 2] != 0
                         & dataframe[, 2] != 9
                         & dataframe[, 2] != 2, ]

  # This makes sure there is at least one thing in the dataset not coded "0"
  calibs <- dim(possibles)[1]
  if(calibs > 1) {
    # What solenoid values do we observe in this particular dataframe?
    sol_values <- as.numeric(levels(as.factor(possibles[, 2])))
    separated <- list()

    # We separate the dataframe based on the observed solenoid values.
    for(i in 1:length(sol_values)) {
      separated[[i]] <- possibles[possibles[, 2] == sol_values[i], ]
      N1 <- length(separated[[i]][, 1])
      # If there is a Precision check somewhere in there, or a really low water value...
      if( N1 > 1 &  N1 < nrow(dataframe)) {

        # There should not be too many H20 Values greater than 0.4 for Precision checks...
        # This takes care of cases when column 2 is not coded as "0" but it is also not Precision checking
        firsts <- min(as.numeric(row.names(separated[[i]])))
        lasts <- 0
        diffs <- 0 * (1 : (N1 - 1))

        # We want to find the separation points for the calibration periods
        diffs <- difftime(separated[[i]][ (2 : N1), 1],
                          separated[[i]][(1 : (N1 - 1)), 1],
                          units = "secs")

        # Since these should be separated Precision checks, we find the large
        # differences to figure out where the different Precision checks are separated
        big_diffs <- as.numeric(row.names(separated[[i]][which(diffs > 60) + 1, ]))
        N <- length(big_diffs)

        # One of the places we're going to definitely remove 5 minutes after is
        # The final point in this separated frame.
        ends <- max(as.numeric(row.names(separated[[i]])))

        # If there is at least one big difference (more than one Precision check)
        if (N > 0) {
          firsts <- c(firsts, big_diffs)

          # This is like finding a supremum (least upper bound) on a Precision check period
          for (k in (1 : length(firsts))) {
            if (k == length(firsts)) {
              lasts[k] <- max(as.numeric(row.names(separated[[i]])))
            }
            else {
              lasts[k] <- max(as.numeric(row.names(separated[[i]][as.numeric(row.names(separated[[i]])) < firsts[k+1],])))
            }
            dataframe[,2][(as.numeric(row.names(dataframe)) >= firsts[k])
                          & (dataframe[,1] < (dataframe[lasts[k], 1] + 300))] <- 9
          }
        }

        # If there is only one Precision check in the time period
        else {
          lasts <- nrow(separated[[i]])

          cat("There is only one Precision check in this time period\n",
              sol_values[i], as.character(separated[[i]][1,1]),
              as.character(separated[[i]][lasts,1]), "\n", sep = " ")

          dataframe[,2][(as.numeric(row.names(dataframe)) >= firsts)
                        & (dataframe[,1] < (dataframe[lasts, 1] + 300))] <- 9
        }
      }

      # If there is something weird going on, we tell the person running the program
      else {
        cat("The machine coded", sol_values[i], N1, "times.\n", sep = " ")
      }
    }
  }

  # If there are no Precision checks
  else {
    cat("All of the solenoid valve readings are 0 for this time period!\n",
         as.character(dataframe[1,1]), as.character(dataframe[nrow(dataframe), 1]), "\n", sep = " ")
  }

  dataframe
}
