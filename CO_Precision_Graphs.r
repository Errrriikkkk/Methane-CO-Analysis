#CalibGraphsV3
#Erik Gregory, egregory2007@yahoo.com, 28 September 2010
#UPDATED: Fixed the format of the input, 1/4/2011
# + added exceptions to deal with weird data that isn't really precision check data

#This function is used to graph the CO levels per calibration period, #
#and also the average CO level per calibration period over the length of 
#the dataset#

#It also outputs two datasets: One of the average ppb values per precision check;
#one with the average ppb values for the second half of each precision check




#DATASET MUST BE R FORMAT
PrecisionGraphs <- function(dataframe, site)
{
#Dataframe with only calibration values
  calib <- dataframe[dataframe$notes == 1,]
  N <- length(calib[,1])
  change <- diff(as.numeric(row.names(calib)))
  a <- list()
  lasts <- which(change > 1)
  firsts <- c(1, lasts + 1)
  lasts <- c(lasts, length(calib[,1]))

#This creates a list, each element of the list is a sequence indexing
#the rownames for a single calibration period
  i <- 1
  j <- 1
  ymin <- 2000
  ymax <- 0
  while(i <= length(firsts)) {
    a[[j]] <- firsts[i]:lasts[i]
    #We assume if there are fewer than 7 measurments, its not a precision check...
    if(length(a[[j]]) < 7 || length(a[[j]]) > 15) {
      a[[j]] <- NULL
    }
    else {
      ymin <- min(ymin, min(calib$C0ppb[a[[j]]]))
      ymax <- max(ymax, max(calib$C0ppb[a[[j]]]))
      j <- j+1
    }

    i <- i + 1
  }
  bottom <- round(ymin / 10) * 10
  top <- round(ymax / 10) * 10

  my.ticks <- pretty(c(bottom, top), n = 5)
  #Pdf file of our plots
  i <- 1
  N1 <- length(a)
  y <- 0 * (1 : N1)
  r <- y
  z <- seq(strptime("01-01-2010 00:00", format = "%m-%d-%Y %H:%M", tz = "GMT"),
           strptime("01-01-2010 00:00", format = "%m-%d-%Y %H:%M", tz = "GMT")
           + N1 - 1, 1)
  w <- z
  pdf(paste(site, "COPrecision",
      max(strptime(calib$DATE, format = "%Y-%m-%d", tz = "GMT")),
      ".pdf", sep = ""))
  par(mfrow = c(3, 3))

  for (i in 1 : N1) {
    y[i] <- mean(calib$C0ppb[a[[i]][round(length(a[[i]]) / 2 - 0.5 )
                 : length(a[[i]])]])
    z[i] <- calib$DATE[a[[i]][1]]
    r[i] <- mean(calib$C0ppb[a[[i]]])
    w[i] <- calib$DATE[a[[i]][1]]
    #w[i] <- mean(calib$DATE[a[[i]]])
    plot(calib$DATE[a[[i]]], calib$C0ppb[a[[i]]],
         yaxt = "n", ylim = c(bottom, top),
         xlab = paste("Minutes After",
         substr(round(calib$DATE[a[[i]][1]] - 1800, "hours"),
         12, 22), sep = " "), ylab = "CO (ppb)",
         main = paste(calib$DATE[a[[i]][1]]))
    axis(side = 2, at = my.ticks)
  }
  par(mfrow = c(1, 1))
  plot(z, y, yaxt = "n", xlab = "Time",
      ylab = "Avg.ppb", ylim = c(bottom, top),
      main = "CO Avg (ppb) during Precision checks")
  axis(side = 2, at = my.ticks)
  plot(w, r, yaxt = "n", xlab = "Time",
      ylab = "Avg.ppb", ylim = c(bottom, top),
      main = "CO Avg (ppb) during second half of Precision Check")
  axis(side = 2, at = my.ticks)
  dev.off()

  #Create and write the datasets with the averages per precision check#
  calibavgs <- data.frame(DATE = z, Avg.ppb.EntireCheck = y)
  calibavgs1 <- data.frame(DATE = w, Avg.ppb.SecondHalv = r)

  write.table(calibavgs,
              file = paste(site, "SecondHalfPrecisonAvg",
                           max(strptime(dataframe$DATE, format = "%Y-%m-%d", 
                           tz = "GMT")), ".csv", sep = ""), 
                           sep = ",", row.names = FALSE)
  write.table(calibavgs1,
              file = paste(site, "AllPrecisionAvg",
                           max(strptime(dataframe$DATE, format = "%Y-%m-%d", 
                           tz = "GMT")), ".csv", sep = ""),
                           sep = ",", row.names = FALSE)


}


###############
#R Format Time#
###############

#Changes times of a dataframe to R format#
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

