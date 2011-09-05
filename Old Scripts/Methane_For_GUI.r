#Erik Gregory, egregory2007@yahoo.com,
#Jan 13 2011#
#Fixed up R_format, improved code readability, added column to output of
#HourlyAverage representing other measurments
#Fixed Bug in HourlyAverage where it wasn't marking calibrations for the first
#dataset!!

#Added comments, sped up some loops 21 Dec 2010#



#TO DO#
#NEXT: WRITE FUNCTION TO JOIN OLD HOURLY AVERAGE FILE WITH NEW ONES#
#16 November 2010#
#All of the Functions for Methane Analysis#

library(reshape)





##############
#File Chooser#
##############

#This function is called within "Importer" to decide which files to import#


#SEE ERRORS#

#input the old dates that we already have averages for,
#and the directory to find new files from#

FileChooser <- function(directory, max.old) {
  files <- list()

  #NOTE: there is an assumption here that is false in the way we subset the file string#
  files1 <- list.files(recursive = TRUE, pattern = "UserLog.dat")
  l <- as.numeric(unlist(lapply(files1, substring ,first = 1, last = 8)))
  l <- l[is.na(l) == FALSE]

  files2 <- list.files(recursive = TRUE, pattern = "Data.dat")
  w <- as.numeric(unlist(lapply(files2, substring ,first = 1, last = 8)))
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
    #Find the maximum date in the old file.#
    #Convert it to the format of the dates in the files#

    k <- l[is.na(l) == FALSE]
    k <- k[order(rank(k))]
    k <- k[k >= max.old]

    m <- w[is.na(w) == FALSE]
    m <- m[order(rank(m))]
    m <- m[m >= max.old]

    files[[1]] <- files1[1]
  }
  files[[2]] <- 0
  #Start with the file with the minimum number#
  j <- 1
  second <- min(k, m)
  while(second <= max(k, m)) {
    #Best case scenerio...#
    if(second%in%k) {
      files[[1]][j] <- files1[max(which(l == second))]
      files[[2]][j] <- 1
    }
    #otherwise...#
    else {
      files[[1]][j] <- files2[max(which(w == second))]
      files[[2]][j] <- 2
    }
    j <- j+1
    if(second == max(k, m)) {
      second <- second + 5
    }
    else{
    #This is like a supremum#
      second <- min(k[k>second], m[m>second])
    }
  }
  files
}



########################
#Methane H20 Correction#
########################

#This is to adjust the measured value of the methane to remove the saturation bias#
meth_correct <- function(dataframe) {
  for(j in 1:length(dataframe)) {
    #These weird numbers are constants taken from a table#
    l <- dataframe[[j]][,5]/(1-0.00982*dataframe[[j]][,6] - 2.393*(1/10000)*(dataframe[[j]][,6])^2)
    dataframe[[j]] <- data.frame(dataframe[[j]], CH4_H20_CORR = l)
  }
  dataframe
}






#######################
#Methane data importer#
#######################

#Input should be the directory for the site that we want to import the data from#
Importer <- function(directory, oldfile) {
  columns <- list()
  columns[[1]] <- c(1, 2, 6, 9, 10, 11, 12)
  columns[[2]] <- c(1, 2, 11, 15, 16, 17, 18)
  x <- list()
  dir2 <- getwd()
  setwd(directory)
  if(nchar(oldfile)> 2) {
    olddata <- R_format(read.csv(oldfile))
    #Find the last day in the old file, change the format to work in file.chooser()#
    olddata <- olddata[is.na(olddata[,1]) == FALSE,]
    row.names(olddata) <- 1:nrow(olddata)
    max.old <- as.character(as.Date(olddata[nrow(olddata), 1])-1)
    max.old <- strsplit(max.old, split = "-")
    max.old <- paste(max.old[[1]][1], max.old[[1]][2], max.old[[1]][3], sep = "")

  }
  else {
    max.old <- 0
  }
  files <- FileChooser(directory, max.old)
  N <- length(files[[1]])
  for(j in 1:N) {
    cat("Importing file", j, "of", N, "\n")
    x[[j]] <- read.table(files[[1]][j], sep = "", nrows = 50000, header = TRUE)
    x[[j]] <- subset(x[[j]], select = columns[[files[[2]][j]]])
    x[[j]] <- x[[j]][is.na(x[[j]][,1]) == FALSE,]
    #add citation, borrowed#
    x[[j]]$DATE <- as.POSIXct(strptime(paste(x[[j]]$DATE,x[[j]]$TIME,sep=":"),format="%m/%d/%y:%H:%M:%S", tz = "GMT"))
    x[[j]]$TIME <- NULL
    j <- j+1
  }
  x <- meth_correct(x)
  x
}


###########################
#Calibration Period Marker#
###########################

#This omits solenoid values from the hourly averages coded with anything other than "0",
#And 5 minutes after them.
calib_marker <- function(dataframe) {

  #The possible Calibrations are those that are not marked "0", "9", or "2"
  possibles <- dataframe[dataframe[,2] != 0 & dataframe[,2] != 9 & dataframe[,2] != 2,]

  #This makes sure there is at least one thing in the dataset not coded "0"#
  calibs <- dim(possibles)[1]
  if(calibs > 1) {
    #What solenoid values do we observe in this particular dataframe?#
    sol_values <- as.numeric(levels(as.factor(possibles[,2])))
    separated <- list()

    #We separate the dataframe based on the observed solenoid values.#
    for (i in 1:length(sol_values)) {
      separated[[i]] <- possibles[possibles[,2] == sol_values[i],]
      N1 <- length(separated[[i]][,1])
      #If there is a precision check somewhere in there, or a really low water value...#
      if( N1 > 1 &  N1 < length(dataframe[,1])) {

        #There should not be too many H20 Values greater than 0.4 for precision checks...#
        #This takes care of cases when column 2 is not coded as "0" but it is also not precision checking#
        firsts <- min(as.numeric(row.names(separated[[i]])))
        lasts <- 0
        diffs <- 0*(1:(N1-1))

        #We want to find the separation points for the calibration periods#
        diffs <- difftime(separated[[i]][2:N1,1], separated[[i]][1:(N1-1),1], units = "secs")

        #Since these should be separated precision checks, we find the large
        #differences to figure out where the different precision checks are separated
        big_diffs <- as.numeric(row.names(separated[[i]][which(diffs > 60) + 1,]))
        N <- length(big_diffs)

        #One of the places we're going to definitely remove 5 minutes after is
        #The final point in this separated frame.
        ends <- max(as.numeric(row.names(separated[[i]])))

        #If there is at least one big difference (more than one precision check)#
        if(N > 0) {
          firsts <- c(firsts, big_diffs)

          #This is like finding a supremum (least upper bound) on a precision check period#
          for(k in 1:length(firsts)) {
            if(k == length(firsts)) {
              lasts[k] <- max(as.numeric(row.names(separated[[i]])))
            }
            else {
              lasts[k] <- max(as.numeric(row.names(separated[[i]][as.numeric(row.names(separated[[i]])) < firsts[k+1],])))
            }
            dataframe[,2][(as.numeric(row.names(dataframe)) >= firsts[k]) & (dataframe[,1] < (dataframe[lasts[k], 1] + 300))] <- 9
          }
        }

        #If there is only one precision check in the time period#
        else {
          lasts <- as.numeric(max(row.names(separated[[i]])))
          cat("There is only one precision check in this time period", sol_values[i], separated[[i]][firsts[1],1], separated[[i]][lasts[1],1], "\n", sep = " ")
          dataframe[,2][(as.numeric(row.names(dataframe)) >= firsts) & (dataframe[,1] < (dataframe[lasts, 1] + 300))] <- 9
        }
      }

      #If there is something weird going on, we tell the person running the program#
      else {
        cat("The machine coded", sol_values[i], N1, "times.\n", sep = " ")
      }
    }
  }

  #If there are no precision checks#
  else {
    cat("All of the solenoid valve readings are 0 for this time period!", dataframe[1,1], dataframe[nrow(dataframe),1], "\n", sep = " ")
  }

  dataframe
}








########################
#Hourly Average Methane#
########################


#Input is a list with all of the datasets (usually a month or longer)
#in the format of "Importer"
HourlyAverage <- function(dataframelist) {

  N <- length(dataframelist)

  #Create a sequence from the minimum hour to maximum hour of all of the data#
  min.time <- round(min(dataframelist[[1]][,1]) - 1800, units = "hour") + 1
  max.time <- round(max(dataframelist[[length(dataframelist)]][,1]) + 1800, units = "hour")
  times <- seq(min.time, max.time, 3600)
  NT <- length(times)


  #num is the number of observations being averaged#
  Number <- 0*(1:NT)
  Number2 <- 0*(1:NT)
  Other <- 0*(1:NT)
  Total <- 0*(1:NT)
  CO2_sd <- 0*(1:NT)
  CO2_CORR_sd <- 0*(1:NT)
  CH4_sd <- 0*(1:NT)
  CH4_H20_CORR_sd <- 0*(1:NT)
  Center <- times
  hourly <- dataframelist[[1]][1:NT,]
  hourly[,1] <- times
  hourly[,2:length(hourly)] <- 0

  i <- 1
  dataframelist[[1]] <- calib_marker(dataframelist[[1]])
  cat("Averaging dataset 1 of", N, "\n", sep = " ")
  #j indexes the dataset#
  for(j in 1:length(dataframelist)){
    #i indexes the hour#
    while(i<= NT) {
      q <- max(dataframelist[[j]][,1])

      #If the last time in this list element is in this time interval,
      #carry the remaining things over and continue the calculation
      if((((q  >= times[i]) && (q < times[i+1])) || ((i+1) > NT)) && ((j+1) <= length(dataframelist))) {
        carry <- dataframelist[[j]][dataframelist[[j]][,1] >= times[i],]
        dataframelist[[j+1]] <- merge(dataframelist[[j+1]], carry, all.x = TRUE, all.y = TRUE)
        dataframelist[[j+1]] <- dataframelist[[j+1]][order(dataframelist[[j+1]][,1]),]
        dataframelist[[j+1]] <- dataframelist[[j+1]][is.na(dataframelist[[j+1]][,1]) == FALSE,]
        row.names(dataframelist[[j+1]]) <- 1:length(dataframelist[[j+1]][,1])
        dataframelist[[j]] <- integer(j)
        gc()
        j <- j+1
        cat("Averaging dataset", j, "of", N, "\n")

        #Mark the Calibration periods, and 5 minutes after them#
        dataframelist[[j]] <- calib_marker(dataframelist[[j]])
      }
      else {

        #Subset the dataframe by things in the hour we want, and remove all data
        #we cannot include in the average#
        w <- dataframelist[[j]][(dataframelist[[j]][,1] >= times[i]) & (dataframelist[[j]][,1] < times[i+1]),]
        Other[i] <- nrow(w[w[,2] == 9,])
        k <- w[w[,2] != 9,]

        #Statistics#
        CH4_sd[i] <- sd(k$CH4)
        CH4_H20_CORR_sd[i] <- sd(k$CH4_H20_CORR)
        CO2_sd[i] <- sd(k$CO2)
        CO2_CORR_sd[i] <- sd(k$CO2_CORR)
        Number[i] <- length(w[,1][w[,2] == 0])
        Number2[i] <- length(w[,1][w[,2] == 2])
        Center[i] <- mean(k[,1])
        hourly[i,(3:length(hourly))] <- mean(k[,3:length(hourly)])
        i <- i+1
      }

    }
  }
  Total <- Number + Number2 + Other
  hourly[,2] <- NULL
  hourly <- cbind(hourly, CO2_sd, CH4_sd, CO2_CORR_sd, CH4_H20_CORR_sd, Number, Number2, Other, Total, Center)
  hourly
}





##########################
#Methane Precision Checks#
##########################

#Input is a list with all of the data we averaged#

#This function extracts all of the precision checks#
#If this stops working, the problem is the part where we added the part to extract "3s"

precision <- function(dataframelist, halved) {
  average <- dataframelist[[1]][1:2,2:length(dataframelist[[1]])]
  names(average)[1] <- "Time"
  dev <- dataframelist[[1]][1:2,3:length(dataframelist[[1]])]
  names(dev) <- paste(names(dev), "_sd", sep = "")
  calibrations <- list()
  begins <- list()
  ends <- list()

  l <- 1
  times <- 0
  class(times) <- class(dataframelist[[1]][,1])
  for(i in 1:length(dataframelist)) {
    dataframelist[[i]] <- dataframelist[[i]][is.na(dataframelist[[i]][,1]) == FALSE,]

    #We assume "1"s and "3"s denote precision checks#
    if(length(dataframelist[[i]][,1][dataframelist[[i]][,2] == 1 | dataframelist[[i]][,2] == 3]) > 0){
      #Subset out the precision checks#
      k <- dataframelist[[i]][dataframelist[[i]][,2] == 1 | dataframelist[[i]][,2] == 3,]
      N <- length(k[,1])
      diffs <- 0*(1:(N-1))

      #Find large differences between the precision checks#
      diffs <- difftime(k[2:N,1], k[1:(N-1), 1], units = "secs")
      m <- which(diffs > 60, arr.ind = TRUE)
      if(length(m) >= 1) {
        begins[[i]] <- c(1, m+1)
        ends[[i]] <- c(m, length(k[,1]))
      }
      else {
        begins[[i]] <- 1
        ends[[i]] <- length(k[,1])
      }

      #We only want the second half of the precision checks usually, so we pull them out#
      for(p in 1:length(begins[[i]])) {
        calibrations[[l]] <- k[begins[[i]][p]:ends[[i]][p],]
        if(halved == TRUE) {
          hold <- length(calibrations[[1]])
          q <- length(calibrations[[l]][,1])
          calibrations[[l]] <- calibrations[[l]][(as.integer(round(q/2)):q),]
        }
        l <- l+1
      }
    }
  }

  dfs <- rbind.fill(calibrations)
  dfs

}






###################
#Excel Format Time#
###################

#Converts the time/dates to a more convenient format for excel, given a dataframe
#with a time/date measurement in the first column #
ExcelFormat <- function(dataframe) {
  separated <- colsplit(as.character(dataframe[,1]), split = " ", names = c("DATE", "TIME"))
  separated[,1] <- as.POSIXct(separated[,1], format = "%Y-%m-%d", tz = "GMT")
  hourly <- cbind(separated, dataframe[,2:length(dataframe)])
  hourly
}









###############
#R Format Time#
###############


#Changes the time column of a dataframe to something that works better in R#
RFormat <- function(dataframe) {

  if("--" %in% gsub("[0-9]", "", as.character(dataframe[1, 1]))) {
    dataframe[, 1] <- as.POSIXct(strptime(
                                 paste(dataframe[, 1], dataframe[, 2], sep=" "),
                                 format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  }

  else {
    dataframe[, 1] <- as.POSIXct(strptime(
                                 paste(dataframe[, 1], dataframe[, 2], sep=" "),
                                 format = "%m/%d/%Y %H:%M:%S", tz = "GMT"))
  }

  dataframe[, 2] <- NULL
  names(dataframe)[1] <- "DATE"
  dataframe
}







#This is sort of Obsolete....#
#If we already have an hourly average file for a site, sometimes we
#may want to add to that file instead of creating a new one.#

#The filename should be the file name of the file that we want to add to,
#The dataframe is what we want to add to it, and it should NOT be in excel format#
appender <- function(filename, dataframe) {
  master <- read.csv(filename)
  #Once we've appended these files once, this if/then will be unnecessary...#
  master[,1] <- as.POSIXct(strptime(paste(master[,1], master[,2], sep=" "),format="%Y-%m-%d %H:%M:%S", tz = "GMT"))
  master[,2] <- NULL
  names(master)[1] <- "DATE"

  max.time.master <- max(master[,1])
  min.time.new <- min(dataframe[,1])

  #If there is some overlap in our datasets, we'll append the master file#
  #starting at its row#
  if(max.time.master > min.time.new) {
    merge.point <- which(dataframe[,1] == max.time.master)
    master <- rbind(master[1:(length(master[,1])-1),], dataframe[merge.point:length(dataframe[,1]),])
    write.csv(ExcelFormat(master), file = filename, row.names = FALSE)
    master
  }

  #If there is no overlap in our datasets...we need to add an exception#
  else {
    cat("Tell Erik to build in support for this problem")
  }

}

Merger <- function(newdata, oldfile) {
  olddata <- R_format(read.csv(oldfile))
  
  oldmax <- max(olddata[,1])
  newmin <- min(newdata[,1])
  N <- nrow(olddata)
  
  #We will construct this to be true...#
  if(oldmax > newmin) {
    newdata <- merge(olddata, newdata, all= TRUE, by = names(olddata))
  }
  else{
    cat("We dont have a way to do this yet, since oldmax < newmin\n")
  }
  newdata
}

AllAnalysis <- function(directory, filename , site, combine) {
  datas <- importer(directory, filename)
  hourly <- HourlyAverage(datas)
  if(combine == TRUE) {
    hourly <- Merger(hourly, filename)
  }
  write.csv(ExcelFormat(hourly),file = paste(Sys.Date(), site, ".csv", sep = ""), row.names = FALSE)
  cat("\nFinished averaging CO data for", "%(site)s \n", sep = " ")
}


memory.limit(size = 2000)

