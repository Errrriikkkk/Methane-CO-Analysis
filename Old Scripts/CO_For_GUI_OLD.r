# Datafixer V4#
#Erik Gregory, egregory2007@yahoo.com, 21 September 2010#


#############
#CO Importer#
#############

library(foreign)
library(reshape)
#This program takes CO data and makes it into a nice, marked, ordered dataframe.#
CO_importer <- function(directory) {

  dir2 <- getwd()
  setwd(directory)
  x <- list()

  #ASSUMPTION: All CO files have the string "CONC" in their title#
  a <- list.files(recursive = TRUE, pattern = "CONC")
  for ( i in a) {
    x[[i]] <- read.csv(i, header = TRUE)
    cat("Reading file", i, "\n")

  }
  setwd(dir2)

  #merge the lists into a master#
  master <- rbind.fill(x)

  #Change time format#

  names(master)[1] <- "Date_Time"
  master$Date_Time <- strptime(master$Date_Time, format = "%m/%d/%Y %H:%M")

  #Change ppm to ppb#
  names(master)[2] <- "C0ppb"
  master[,2] <- master[,2]*1000

  #Order the master dataframe by Date_Time#
  master <- master[order(master$Date_Time),]

  #Delete duplicate rows#
  master <- master[duplicated(master) == FALSE,]
  row.names(master) <- 1:nrow(master)

  master
}






##############
#Datafixer V5#
##############

#Mark periods of data we do not want included in our hourly average#
########
#Codes:#
########
#1. Periods we believe to be precision checks.  For now, the only criteria is that
#the CO measurement is between 900 and 1200 ppb#
#3. Low measurments (<60ppb)#
#5. High measurements(>=1200ppb)#
datafixer <- function(master, site)
{
  N <- nrow(master)

  #Convert data from ppm to ppb#


  notes <- 0*(1:N)
  master <- data.frame(master, notes)

  #"1" denotes calibration data#
  master$notes[master$C0ppb > 800] <- 5

  cat("Determining precision check periods...\n")
  #This is to mark the periods where the machine produces the same CO ppb#
  #for more than one minute in a row.  We mark these as "2"#
  tdiffs <- 0*(1:N)
  tdiffs[1] <- 0
  tdiffs[2:N] <- difftime(master[2:N,1], master[1:(N-1),1], units = "mins")


  #diffs keeps track of the change in ppb from one measurement to the next#
  diffs <- 0*(1:N)
  diffs[1] <- 0
  diffs[2:N] <- diff(master$C0ppb)

  #Differences larger than 50 probably mean that a precision check has begun#
  #Likewise, those less than -50...#
  positives <- which(diffs>50)
  negatives <- which(diffs<(-50))

  #We assume that precision checks dont start right when the machine starts up
  #after being off for a while#
  positives <- positives[tdiffs[positives]<5]

  #Precision checks usually have ppb values between 800 and 1200.  The first one might not though!#
  positives <- positives[master$C0ppb[positives+3]>=800 & master$C0ppb[positives+3] < 2000]

  #Diffs2 keeps track of how close two of our possible precision check beginnings are#
  diffs2 <- 0*(1:length(positives))
  diffs2[1] <- 11
  diffs2[2:length(positives)] <- diff(positives)

  #If two of the "positives" are pretty close, theyre probably not both precision checks#
  positives[which(diffs2<10)] <- 0

  #We find the end of a calibration period by seeing if there is some cooresponding
  #value of "negatives" which occurs soon after "positives."
  #NOTE: This could fail if precision checks can be less than 9 measurements, or more than 20#
  endcalib <- 0*(1:length(positives))
  for(i in 8:19) {
    x <- which((positives+i)%in%negatives)
    endcalib[x] <- positives[x]+(i-1)
  }
  positives[which(endcalib == 0)] <- 0

  #We additionally assume that precision checks are relatively consistent periods of measurment#
  #That is, when we add up the differences they're small!#
  for(i in 1:length(positives)) {
    if(sum(abs(master$C0ppb[(positives[i]+1):endcalib[i]]-master$C0ppb[positives[i]:(endcalib[i]-1)])) < 250) {
      master$notes[positives[i]:endcalib[i]] <- 1
    }
  }

  #We dont care about the small differences in times#
  tdiffs[tdiffs <= 2] <- 0

  cat("Marking Machine self-checks... \n")
  #First we find when the machine marks the same value twice in a row.#
  check1 <- 0*(1:(N-1))
  check1 <- diff(master[,2])

  #Next, we check to see if the machine marks that same value 5 measurements later#
  check2 <- 0*(1:N-5)
  check2 <- master[5:length(master[,1]), 2] - master[1:(length(master[,1]) - 4), 2]

  #When did it record the same measurments?#
  truths1 <- which(check1 == 0)
  truths2 <- which(check2 == 0)

  #If it marked the same measurement twice in a row and then 5 measurements later, we assume
  #it was a machine self-check period.
  truths <- truths1[truths1%in%truths2]

  i <- 1
  N1 <- length(truths)
  #Mark the self-check periods by finding all ppb measurments in the next
  #20 measurments that are the same as the first one we suspected#
  while(i <= N1){
    master$notes[truths[i]:(truths[i] + 20)][(master[truths[i]:(truths[i]+20),2] == master[truths[i], 2]) & master[truths[i]:(truths[i] + 20),3] == 0] <- 2
    j <- truths[i]
    while(truths[i] < 25+j & i<= N1) {
      i <- i+1
    }
    if(i%%50 == 0) {
      cat(100*i/N1, "% complete \n")
    }
  }

  #3 denotes low measurements#
  master$notes[master$C0ppb <= 60] <- 3

  #Add in extra column to indicate missing minutes#
  master <- data.frame(master, missingmins = tdiffs)
  master <- master[is.na(master[,1]) == FALSE,]
  row.names(master) <- 1:length(master[,1])

  #Write the new table to a file#
  write.table(excel_format(master), file = paste(site, "CO", max(as.Date(master[,1])), ".csv", sep = ""), sep = ",", row.names = FALSE)
  write.table(master[master[,3] == 1,], file = paste(site, "CO_Precision_Checks", max(as.Date(master[,1])), ".csv", sep = ""), sep = ",", row.names = FALSE)

  #Return the dataframe#
  master
}






###################
#Excel Format Time#
###################
excel_format <- function(dataframe) {
  separated <- colsplit(as.character(dataframe[,1]), split = " ", names = c("DATE", "TIME"))
  separated[,1] <- as.POSIXct(separated[,1], format = "%Y-%m-%d")
  hourly <- cbind(separated, dataframe[,2:length(dataframe)])
  hourly
}





###################
#CO Hourly Average#
###################

CO_hourly_average <- function(dataframe, site) {
  dataframe <- dataframe[,-(4:6)]
  min.time <- round(min(dataframe[,1]) - 1800, units = "hour") + 1
  max.time <- round(max(dataframe[,1]) + 1800, units = "hour") + 1
  times <- seq(min.time, max.time, 3600)
  N <- length(times)

  #num is the number of observations being averaged#
  Number <- 0*(1:N)
  CO_sd <- 0*(1:N)
  Other <- 0*(1:N)

  #center is the mean time of the averages#
  Center <- times

  hourly <- dataframe[1:N,1:2]
  hourly[,1] <- times
  hourly[,2] <- 0
  cat("\nAveraging data for", N, "hours\n", sep = " ")
      i <- 1
      while(i <= length(times)) {
          k <- dataframe[(dataframe[,1] >= times[i]) & (dataframe[,1] < times[i+1]),]
          w <- k[k[,3] == 0,]
          if(nrow(w) != 0) {
            Other[i] <- nrow(k)-nrow(w)
            CO_sd[i] <- sd(w[,2])
            Number[i] <- length(w[,1])
            Center[i] <- mean(w[,1])

            hourly[i,2] <- mean(w[,2])
          }
          i <- i+1
          if(i%%50 == 0){
            cat(100*i/N, "% complete \n")
          }
      }
  hourly <- cbind(hourly, CO_sd, Number, Other, Center)

  write.table(excel_format(hourly), file = paste(site, "CO_Hourly_Average", max(strptime(hourly[,1], format = "%Y-%m-%d")), ".csv", sep = ""), sep = ",", row.names = FALSE)
  hourly
}






###############
#R Format Time#
###############

#Changes times of a dataframe to R format#
#Changes the time column of a dataframe to something that works better in R#
R_format <- function(dataframe) {

  if("--"%in%gsub("[0-9]","", as.character(dataframe[1,1]))) {
    dataframe[,1] <- as.POSIXct(strptime(paste(dataframe[,1], dataframe[,2], sep=" "),format="%Y-%m-%d %H:%M:%S"))
  }

  else {
    dataframe[,1] <- as.POSIXct(strptime(paste(dataframe[,1], dataframe[,2], sep=" "),format="%m/%d/%Y %H:%M:%S"))
  }

  dataframe[,2] <- NULL
  names(dataframe)[1] <- "DATE"
  dataframe
}
