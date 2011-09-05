# Methane_For_GUI_2.0

# 7/31/2011: Fixed error where rowSums whould try to add a single column
Import <- function(directory = FALSE, file = TRUE, pattern = NULL, 
                   imp.file = NULL, good = FALSE, trim = FALSE, check.cols = 2, nrow = 10000000) {
  # Import the entire directory, by "Pattern"
  if (directory == TRUE) {
    # So it does not go into the file.choose procedure afterward.
    file <- FALSE
    data <- list()
    dir2 <- getwd()
    dir <- choose.dir()
    setwd(dir)
    filez <- list.files(recursive = TRUE, pattern = pattern)
    for (i in files) {
      data[[i]] <- Import(imp.file = i)
    }
    setwd(dir2)
  }
  # Import the file, giving the user the option to select a file
  # if they have not provided one.
  else if (file == TRUE) {
    if (is.null(imp.file)) {
      imp.file <- file.choose()
    }
    data <- read.csv(imp.file, nrow = nrow)
    # Make sure the time is in R Format
    if (("POSIXct" %in% class(data[, 1])) == FALSE) {
      c <- as.POSIXct(data[, 1])
      if(is.na(c[1]) | nchar(c[1]) < 13) {
        data <- RFormat(data) 
      }
      else {
        data[, 1] <- c
      }
    }
    if (good == TRUE) {
      data <- GoodData(data, check.cols = check.cols, trim = trim)
    }
  }
  return(data)
}


RFormat <- function(dataframe) {

  if("--" %in% gsub("[0-9]", "", as.character(dataframe[1, 1]))) {
    dataframe[, 1] <- as.POSIXct(strptime(
                                 paste(dataframe[, 1], dataframe[, 2], sep=" "),
                                 format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  }

  else {
    dataframe[, 1] <- as.POSIXct(strptime(
                                 paste(dataframe[, 1], dataframe[, 2], sep=" "),
                                 format = "%m/%d/%y %H:%M:%S", tz = "GMT"))
  }

  dataframe[, 2] <- NULL
  names(dataframe)[1] <- "DATE"
  dataframe
}

# Filter dataframes for the good data in them.
# 1. Remove "NA" rows
# 2. Remove rows with measurements of "0"
# 3. Remove rows with very few observations
# 4. 
GoodData <- function(dataframe, check.cols, toofew = 1000,
                     total.col = which(names(dataframe) == "Total"), 
                     total.check = FALSE, trim = FALSE, trim.num = 5) {
  dataframe <- dataframe[which(complete.cases(dataframe[, check.cols])), ]
  for (i in 1:length(check.cols)) {
    if (0 %in% dataframe[, check.cols[i]])
      dataframe <- dataframe[-which(dataframe[, check.cols[i]] == 0), ]
  }
  if (total.check == TRUE) {
    for (i in 1:length(total.col)) {
      ind <- which(dataframe[, total.col] < toofew)
      if (length(ind) > 0) {
        dataframe <- dataframe[-ind, ]
      }
    }
  }
  if (trim == TRUE) {
    dataframe <- TrimEnds(dataframe, column = check.cols[1], number = trim.num)
  }
    
  return(dataframe)
}

TrimEnds <- function(dataframe, number = 5, column) {
  dataframe <- dataframe[order(dataframe[,column]),]
  # Trim off the largest and smallest entries
  dataframe <- dataframe[(number + 1) : (nrow(dataframe) - number), ]
  dataframe <-dataframe[order(dataframe[, 1]),]
  return(dataframe)
}
# THIS IS ALMOST ALL GOOD
#The problem is with creating the extra column for the solenoid valve values.
# The old dataframe currently has one more column than the new one.  Fix this,
#and you win.
SingleMethCorrect <- function(dataframe) {
 # These weird numbers are constants taken from a table
  l <- dataframe[, 5]/(1 - 0.00982 * dataframe[, 6] -
                            2.393 * (1 / 10000) * (dataframe[, 6] ^ 2))
  dataframe <- data.frame(dataframe, CH4_H20_CORR = l)
  dataframe
}

library(reshape)

ChooseFiles <- function(directory, oldfile) {
  x <- list()
  dir2 <- getwd()
  setwd(directory)
  if (nchar(oldfile) > 2) {
    olddata <- RFormat(read.csv(oldfile))
    # Find the last day in the old file, change the format to work in FileChooser
    olddata <- olddata[is.na(olddata[, 1]) == FALSE, ]
    row.names(olddata) <- 1 : nrow(olddata)
    max.old <- as.character(as.Date(olddata[nrow(olddata), 1]) - 1)
    max.old <- strsplit(max.old, split = "-")
    max.old <- paste(max.old[[1]][1], max.old[[1]][2], max.old[[1]][3], sep = "")

  }
  else {
    max.old <- 0
  }
  files <- FileChooser(directory, max.old)
  files
}

ImportRaw <- function(filename, type.of.file) {
  columns <- list()
  columns[[1]] <- c(1, 2, 6, 9, 10, 11, 12)
  columns[[2]] <- c(1, 2, 11, 15, 16, 17, 18)
  x <- read.table(filename, sep = "", nrows = 50000, header = TRUE)
  x <- subset(x, select = columns[[type.of.file]])
  x <- x[is.na(x[, 1]) == FALSE, ]
  flags <- 0*(1:nrow(x))
# If there is a problem in the script, it's HERE
  x <- RFormat(x)
  x <- SingleMethCorrect(x)
  # Add in column for flags
  x <- cbind(x, flags)
  x
}

timez <- function(min.time, max.filename) {
  max.time <- strsplit(max.filename, split = "/")[[1]][1]
  year <- substring(max.time, 1, 4)
  month <- substring(max.time, 5, 6)
  day <- substring(max.time, 7, 8)
  max.date <- paste(month, day, year, sep = "/")
  max.time <- as.POSIXct(strptime(paste(max.date, "23:00:00", sep = " "),
                                  format = "%m/%d/%Y %H:%M:%S", tz = "GMT"))
  seq(min.time, max.time, by = "hour")
}


###################
#Excel Format Time#
###################

#Converts the time/dates to a more convenient format for excel, given a dataframe
#with a time/date measurement in the first column #
ExcelFormat <- function(dataframe) {
  library(reshape)
  time.cols <- colsplit(as.character(dataframe[,1]), split = " ", 
                        names = c("DATE", "TIME"))
  time.cols[,1] <- as.POSIXct(time.cols[,1], format = "%Y-%m-%d", tz = "GMT")
  hourly <- cbind(time.cols, dataframe[,2:length(dataframe)])
  hourly
}


#############################
# Calibration Period Marker #
#############################

# MethaneCalibMarkerV2


# This list will count the number of solenoid values of
# a[[i+1]] for i \in {0, 1, 2, 3, 4, 5}

# Input a list with the previous solenoid values,
# and a dataframe with the data from the hour we want to know about
 
# output a vector with values we wish to exclude from our averaging

# Output a re-marked dataframe
SolValues <- function(dataframe, sol.vals, ind) {
  # Create a table of all observed solenoid valve values
  tabled <- table(dataframe[, 2])
  values <- as.numeric(names(tabled))
  tabled <- as.vector(tabled)
  j <- 0
  for (i in 1:length(sol.vals)) {
    if ((i-1) %in% values) {
      sol.vals[[i]][ind] <- tabled[which(values == (i-1))]
    }
    else {
      sol.vals[[i]][ind] <- 0
    }
  }
  return(sol.vals)
 }  
 
 # Put in the dataframe in question, and a vector with the solenoid values
 # we want to exclude from our averaging 
MethCalib <- function(dataframe, bad.sols = c(1, 3)) {
  values <- as.numeric(levels(as.factor(dataframe[,2])))
  k <- length(which(values %in% bad.sols))
  if(k == 0) {
    cat("\nThere are no precision checks in the dataset for", 
        as.character(as.Date(dataframe[1, 1])), "\n", sep = " ")
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
    for (i in 1:length(end.times)) {
      dataframe$flags[dataframe[,1] >= end.times[i] & 
                      dataframe[,1] <= (end.times[i] + 300)] <- 9
    }
    dataframe$flags[dataframe[,2]%in%bad.sols] <- 9
  }
  return(dataframe)
}
  


# Bind Methane
BindMethane <- function(data1, data2) {
  output <- merge(data1, data2, all.x = TRUE, all.y = TRUE)
  a <- which(duplicated(output[,1]))
  while (length(a) > 0) {
      k <- which(output[,1] == output[a[1],1])
      # Find which one has fewer observations, throw it out
      r <- which(output[k,]$Total == min(output[k,]$Total))
      output <- output[-k[r],]
      row.names(output) <- 1:nrow(output)
      a <- which(duplicated(output[,1]))
  }
  output[is.na(output)] <- 0
  while (names(output)[length(output)]!= "Center") {
    output <- output[, c(1:(length(output)-3), length(output), length(output)-2, length(output)-1)]
  }
  return(output)
}
      
      

################
# File Chooser #
################

# This function is called within "Importer" to decide which files to import


# SEE ERRORS

# input the old dates that we already have averages for,
# and the directory to find new files from

FileChooser <- function(directory, max.old) {
  files <- list()
  
  # NOTE: there is an assumption here that is false in the way we subset the file string#
  files1 <- list.files(recursive = TRUE, pattern = "UserLog.dat")
  l <- as.numeric(unlist(lapply(files1, substring, first = 1, last = 8)))
  l <- l[is.na(l) == FALSE]

  files2 <- list.files(recursive = TRUE, pattern = "Data.dat")
  w <- as.numeric(unlist(lapply(files2, substring, first = 1, last = 8)))
  w <- w[is.na(w) == FALSE]
  if(max.old == 0) {
    files[[1]] <- files1[1]
    k <- l
    m <- w
  }
  else if(max.old > max(l, w)){
    cat("There is no new data in the directory specified!\n")
    return(0)
  }
  else{
    # Find the maximum date in the old file.
    # Convert it to the format of the dates in the files

    k <- l[is.na(l) == FALSE]
    k <- k[order(rank(k))]
    k <- k[k >= max.old]

    m <- w[is.na(w) == FALSE]
    m <- m[order(rank(m))]
    m <- m[m >= max.old]

    files[[1]] <- files1[1]
  }
  files[[2]] <- 0
  # Start with the file with the minimum number
  j <- 1
  second <- min(k, m)
  while(second <= max(k, m)) {
    # Best case scenerio...
    if(second%in%k) {
      files[[1]][j] <- files1[max(which(l == second))]
      files[[2]][j] <- 1
    }
    # otherwise...
    else {
      files[[1]][j] <- files2[max(which(w == second))]
      files[[2]][j] <- 2
    }
    j <- j+1
    if(second == max(k, m)) {
      second <- second + 5
    }
    else{
    # This is like a supremum
      second <- min(k[k > second], m[m > second])
    }
  }
  files
}




MethaneAvg <- function(directory, oldfile = "", site = "", combine = FALSE, writeit = FALSE,
                       bad.sols = c(1, 3), output.dir = getwd()){
  if (nchar(directory) > 1) {
    library(xts)
    rawdata <- list()
    # Choose files to import
    files <- ChooseFiles(directory, oldfile)
    N <- length(files[[1]])
    for (i in 1:N) {
      cat("Importing and averaging file", i, "of", N, "\n")
      rawdata[[i]] <- ImportRaw(files[[1]][i], files[[2]][i])
      rawdata[[i]] <- MethCalib(rawdata[[i]], bad.sols)
      if (i == 1) {
        # Create our time sequence
        min.time <- round(min(rawdata[[i]][, 1]) - 1800, "hours")
        times1 <- timez(min.time, files[[1]][N])
        NT <- length(times1)
        # Assign all variables
        Number <- Number2 <- Other <- Total <- CO2_sd <- CO2_CORR_sd <-
        CH4_sd <- CH4_H20_CORR_sd <- H2O_sd <- MAX_CH4_DIFF <- 
        MAX_CO2_DIFF <- AVG_CO2_DIFF <- AVG_CH4_DIFF <- MAX_CH4 <- 
        MAX_CO2 <-  0 * (1 : NT)
        sol.vals <- lapply(vector("list", 10), function(i) i <- 0 * (1 : NT))
        hourly <- rawdata[[1]][1 : NT, ]
        hourly[, 1] <- times1
        Center <- times1
        hourly[, 2 : ncol(hourly)] <- 0
      }
      if (i > 1) {
        rawdata[[i]] <- rbind(rawdata[[i-1]], rawdata[[i]])
        rawdata[[i]] <- rawdata[[i]][is.na(rawdata[[i]][, 1]) == FALSE,]
        row.names(rawdata[[i]]) <- 1:nrow(rawdata[[i]])
        rawdata[[i-1]] <- integer(0)
      }
      ends <- endpoints(rawdata[[i]][, 1], "hours")
      # Sometimes we get weird time measurments that are actually good data. 
      # This ignores the time recorded, and assumes continuity in our times.
  
      times2 <- as.POSIXct(round(rawdata[[i]][, 1][ends[2 : length(ends)]] - 1800,
                               "hour"), tz = "GMT")
      bad.check <- abs(diff(times2))
      if (length(which(bad.check > 1)) > 0) {
        if (max(bad.check) > 24) {
          cat("The machine made a timestamp error in the dataset for ", 
              as.character(as.Date(times2[5])), "\n")
        }
        rawdata[[i]]$flags[abs(as.Date(rawdata[[i]][, 1])- 
                           median(as.Date(rawdata[[i]][, 1]))) > 2] <- 8
        rawdata[[i]] <- rawdata[[i]][rawdata[[i]]$flags != 8, ]
        row.names(rawdata[[i]]) <- 1:nrow(rawdata[[i]])
        ends <- endpoints(rawdata[[i]][, 1], "hours")
        times2 <- as.POSIXct(round(rawdata[[i]][, 1][ends[2 : length(ends)]] - 1800,
                             "hour"), tz = "GMT")
       }
      ind <- which(times1 %in% times2 == TRUE)
      
      if (i == N){
        p <- length(ind) - 1
      }
      else {
        p <- length(ind) - 2
      }
      for (k in 1:p) {
          hour <- rawdata[[i]][(ends[k] + 1) : ends[k + 1], ]
          sol.vals <- SolValues(hour, sol.vals, ind[k])
          to.avg <- hour[hour$flags != 9, ]
          if (nrow(to.avg) > 0) {
          # Statistics
            CH4_diff <- diff(to.avg$CH4_H20_CORR)
            CO2_diff <- diff(to.avg$CO2_CORR)
            MAX_CH4_DIFF[ind[k]] <- max(abs(CH4_diff))
            MAX_CO2_DIFF[ind[k]] <- max(abs(CO2_diff))
            AVG_CH4_DIFF[ind[k]] <- mean(abs(CH4_diff))
            AVG_CO2_DIFF[ind[k]] <- mean(abs(CO2_diff))
            MAX_CH4[ind[k]] <- max(to.avg$CH4_H20_CORR)
            MAX_CO2[ind[k]] <- max(to.avg$CO2_CORR)
            CH4_sd[ind[k]] <- sd(to.avg$CH4)
            CH4_H20_CORR_sd[ind[k]] <- sd(to.avg$CH4_H20_CORR)
            CO2_sd[ind[k]] <- sd(to.avg$CO2)
            CO2_CORR_sd[ind[k]] <- sd(to.avg$CO2_CORR)
            Center[ind[k]] <- mean(to.avg[, 1])
            H2O_sd[ind[k]] <- sd(to.avg$H2O)
            hourly[ind[k],(3 : length(hourly))] <- mean(to.avg[, 3 : length(hourly)])
          }
      }
      rawdata[[i]] <- rawdata[[i]][-(ends[1]:ends[k+1]), ]
     }
    sol.vals <- data.frame(sol.vals)
    names(sol.vals) <- paste("Number", 0:5, sep = "")
    sol.vals <- sol.vals[,-which(colSums(sol.vals) == 0)]
    
    # Find the total number of measurements in each hour regardless of 
    # solenoid value.
    if(ncol(sol.vals) > 1) {
      Total <- rowSums(sol.vals)
    }
    else {
      Total <- sol.vals[, 1]
    }
    hourly[, 2] <- NULL
    hourly <- cbind(hourly, CO2_sd, CH4_sd, CO2_CORR_sd, CH4_H20_CORR_sd, 
                    H2O_sd, MAX_CH4_DIFF, MAX_CO2_DIFF, MAX_CH4, MAX_CO2, 
                    AVG_CH4_DIFF, AVG_CO2_DIFF, sol.vals, Total, Center)
    hourly$flags <- NULL
    if (combine == TRUE) {
      hourly <- BindMethane(RFormat(read.csv(oldfile)), hourly)
    }
  
    if (writeit == TRUE) {
      setwd(output.dir)
      write.csv(ExcelFormat(hourly), 
                file = paste(Sys.Date(), site, ".csv", sep = ""), 
                row.names = FALSE)
    }
    cat("\nFinished averaging CO data for", site,
        "\nYou will find the output",
        paste(Sys.Date(), site, ".csv", sep = ""),
        "in\n", getwd(), "\n", sep = " ")
    hourly
  }
}


