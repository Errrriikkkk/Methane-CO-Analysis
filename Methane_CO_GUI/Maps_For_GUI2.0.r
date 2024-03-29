# Output either a googlemap, or a ggplot map of Hysplit-processed data
# Args:
#   datas: (dataframe) Hysplit formatted dataset
#   bin.length: (double) Number of degrees for the map grid
#   start.month, start.year, ....: (integer) Beginning and end
#       of the period to be plotted
#   plotit: (bool) Should the binned dataset be plotted?
#   zeros: (bool) Should cells in which no points fall be included in
#       the output dataset?
#   back: (int) How many hours backward should be included in the plot?
#   site: "Madera", "Arvin", "Tranquillity", or "Wilson".
#   too.few: (int) How many data points is too few for a gridcell? 
#   gg: (bool) Use ggplot maps?
#   g.maps: (bool) Use google maps?
#   gmap.type: What type of google map to use?
#   colors: Which RColorBrewer pallette to use?
#   points: (not used) Should the points be plotted without binning?
#   start.time, end.time: (int) Which hour range would do we want to 
#      subset the data by?
#   order.output: (bool) Should the data be ordered by the probability 
#      of being a high measurement?
#   cutoff: (int) What percentage of the data should be labled "high"?
# TODO: Add the ability to add reference points, other than just a single point
#       for the site.
library(maps, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(RgoogleMaps, quietly = TRUE)
library(RColorBrewer, quietly = TRUE)
library(fields, quietly = TRUE)
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
                                 format = "%m/%d/%Y %H:%M:%S", tz = "GMT"))
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
HysplitMaps <- function(datas = NULL, bin.length = 0.05,
                  map.type = "state", start.month = 0, start.year = 0, start.day = 0,
                  end.month = 0, end.year = 0, end.day = 0,
                  plotit = TRUE, zeros = TRUE, back = 6, site = "", 
                  too.few = NULL, gg = FALSE, g.maps = TRUE, 
                  gmap.type = "mobile", colors = "Reds", points = FALSE,
                  start.time = NULL, end.time = NULL, order.output = TRUE,
                  cutoff = 25, output.map = FALSE, output.data = FALSE,
                  rm.site = TRUE, MyMap = NULL, return.map = FALSE,
                  filter = TRUE, numeric.cutoff = NULL, output.cutoff = FALSE,
                  map.name = NULL) {
  # Make sure a dataset was entered.
  stopifnot(!is.null(datas))
  
  # Make sure the user didn't enter any crazy values for the year.
  start.year <- max(start.year, 2010)
  end.year <- min(end.year, 2020)
  
  # Make sure the dates entered are consistent
  if (start.month > end.month) {
    if (start.year == end.year) {
      end.year <- end.year + 1
    }
  }
  # Logical value indicating whether the data are to be filtered by a time
  # period.
  by.period <- all(c(start.year, start.month, start.day, 
                     end.year, end.month, end.day) != 0)
  # Obtain the latitude/longitude coordinates of the site entered.
  coor <- Coordinates(site)
  coor <- as.numeric(unlist(strsplit(coor, " ")))
  
  # Filter the data if it isn't already in the correct form.
  if (filter == TRUE) {
    datas <- FilterData(datas = datas, bin.length = bin.length,
                        start.month = start.month, start.year = start.year, start.day = start.day,
                        end.month = end.month, end.year = end.year, end.day = end.day,
                        zeros = zeros, back = back, too.few = too.few, points = points,
                        start.time = start.time, end.time = end.time, cutoff = cutoff, 
                        rm.site = rm.site, numeric.cutoff = numeric.cutoff, 
                        output.cutoff = output.cutoff)
  }
  # Add an entry to the dataset with the coordinates for the site entered.
  datas[(nrow(datas) + 1), ] <- datas[nrow(datas), ]
  datas[nrow(datas), ]$LAT <- coor[1]
  datas[nrow(datas), ]$LON <- coor[2]
  row.names(datas) <- 1:nrow(datas)
  
  # If a map name is not supplied, produce one.
  if(is.null(map.name)) {
    map.name <- FileName(site = site, start.year = start.year, start.month = start.month, 
                         start.day = start.day, end.year = end.year, end.month = end.month, 
                         end.day = end.day, back = back, bin.length = bin.length, 
                         start.time = start.time, end.time = end.time, 
                         cutoff = cutoff, by.period = by.period)
  }

  # For a ggplot of the dataset
  if (gg == TRUE) {
    # Make a map of the area we care about
    max.lat <- max(datas$LAT)
    min.lat <- min(datas$LAT)
    max.lon <- max(datas$LON)
    min.lon <- min(datas$LON)
    mapped <- data.frame(map(map.type, plot = FALSE, 
                         xlim = c(min.lon, max.lon), 
                         ylim = c(min.lat, max.lat))[c("x", "y")])
    p <- ggplot(datas) + 
         geom_point(data = datas, aes(x = LON, y = LAT, color = WT)) + 
         geom_path(data = mapped, aes(x = x, y = y)) + 
         geom_point(data = data.frame(coor))
    if (output.map == TRUE) {
      pdf(paste(map.name, ".pdf", sep = ""))
    }
    print(p)
  }
  
  # For a non-ggplot of the data
  if (g.maps == TRUE) {
    GoogleMapIt(datas = datas, output.map = output.map, MyMap = MyMap, 
                map.name = map.name, colors = colors, gmap.type = gmap.type)
  }
  if (output.map == TRUE) {
    dev.off()
  }
  # Should the data be ordered by the percentage of high outputs?
  if (order.output == TRUE) {
    datas <- datas[order(datas$WT), ]
  }
  if (output.data == TRUE) {
    write.csv(datas, row.names = FALSE, file = paste(map.name, ".csv", sep = ""))
  }
  if (return.map == TRUE) {
    return(MyMap)
  }
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

# This function "bins" the data by latitude and longitude.
# Output: Dataset with latitude and longitude coordinates (separated by
#       "bin.length"), the total number of data points in "datas" which are close
#       to those coordinates, and the number of those data points which are 
#       "high" measurements.
# Args:
#      datas: (data.frame) Should have entries for latitude, longitude,
#             and whether the observation is "high" or "low".
Matrixed <- function(datas, bin.length = 0.05) {
  place <- list()
  print(nrow(datas))

  # Determine the grid limits. 
  max.lat <- round(max(datas$LAT) + 0.5)
  min.lat <- round(min(datas$LAT) - 0.5)
  max.lon <- round(max(datas$LON) + 0.5)
  min.lon <- round(min(datas$LON) - 0.5)
  lats <- seq(min.lat, max.lat, by = bin.length)
  lons <- seq(min.lon, max.lon, by = bin.length)
  
  # Find the center of the grid cells
  lat.coors <- seq(lats[1] + abs(lats[1] - lats[2]) / 2, 
                   lats[length(lats)] - 
                   abs(lats[length(lats) - 1] - lats[length(lats)]) / 2, 
                   by = bin.length)
  lon.coors <- seq(lons[1] + abs(lons[1] - lons[2]) / 2, 
                   lons[length(lons)] - 
                   abs(lons[length(lons) - 1] - lons[length(lons)]) / 2, 
                   by = bin.length)
                   
  points <- matrix(nrow = length(lat.coors) * length(lon.coors), ncol = 2, 0)
  # It is important to notice that "temp" and "temp1" have the same number of 
  # entries as "points" has rows.  
  temp <- matrix(nrow = length(lat.coors), ncol = length(lon.coors), 0)
  temp1 <- temp
  k <- 1
  # Create all ordered pairs for the centers of the grid cells in the form 
  # (LON, LAT).
  for (i in 1 : length(lon.coors)) {
      points[k:(k + length(lat.coors) - 1), 1:2] <- cbind(lon.coors[i], lat.coors) 
      k <- k + length(lat.coors)
  }
  # Figure out which interval each of the latitude/longitude measurments of 
  # the data fall into.
  lon.brk <- cut(datas$LON, breaks = lons, labels = FALSE)
  lat.brk <- cut(datas$LAT, breaks = lats, labels = FALSE)
  
  # Now we have ordered pairs with integer coefficients
  place <- data.frame(lat.brk, lon.brk, datas$color)
  # Create a table representing how many data points have each ordered pair.
  totalz <- as.matrix(table(place[, 1:2]))
  
  # Coerce the table information to a usable format
  rowz <- as.numeric(row.names(totalz))
  colz <- as.numeric(names(totalz[1, ]))
  
  # Also, if we thing of "lon.coors" as the columns of the 
  # temp matrix, and "lat.coors" as the rows, we see that if we trace 
  # the matrix starting from the left side, top to bottom, by column, 
  # the resulting vector cooresponds exactly to the "points" matrix.
  # This is, cooincidently, exactly how R converts matricies to vectors!
  temp[rowz, colz] <- totalz
  high <- place[place[, 3] == "High", ]
  high <- as.matrix(table(high[ , 1:2]))
  rowz <- as.numeric(row.names(high))
  colz <- as.numeric(names(high[1, ]))
  temp1[rowz, colz] <- high
  points <- data.frame(cbind(points[, 2], points[, 1], c(temp1), 
                        c(temp)))

  names(points) <- c("LAT", "LON", "High", "Total")
  return(points)
}

Coordinates <- function(site) {
  madera <- "36.867 -120.010"
  arvin <- "35.239 -118.789"
  tranquil <- "36.634 -120.382"
  wilson <- "34.223 -118.063"
  if (site == "Madera"){ 
    coor <- madera
  }
  else if (site == "Arvin") {
    coor <- arvin
  }
  else if (site == "Tranquillity") {
    coor <- tranquil
  }
  else if (site == "Wilson") { 
    coor <- wilson
  }
  # Default Madera
  else {
    coor <- madera
  }
  return(coor)
}

# Create a filename, based on the inputs.
FileName <- function(site, start.year, start.month, start.day, 
                     end.year, end.month, end.day, back, bin.length, 
                     start.time, end.time, cutoff, by.period = NULL) {
  if(is.null(by.period)) {
    by.period <- all(c(start.year, start.month, start.day, 
                       end.year, end.month, end.day) != 0)
  }
  if(by.period == TRUE) {
    date <- paste(paste(start.year, start.month, start.day, sep = "-"),
                  "to", paste(end.year, end.month, end.day, sep = "-"))
    if(class(start.time) == "numeric") {
      name <- paste(site, "_", back, "Back_", date, "_", 
                    start.time, "to", end.time, "_", 
                    bin.length, "degrees", "_",
                    cutoff, "percent",  sep = "")
    }
    else {
      name <- paste(site, "_", back, "Back_", date, "_", 
                    "AllHours", "_", 
                     bin.length, "degrees", "_",
                     cutoff, "percent",  sep = "")
    }
  }
  else {
    if(class(start.time) == "numeric") {
      name <- paste(site, "_", back, "Back_", "AllDays", "_", 
                    start.time, "to", end.time, "_", 
                    bin.length, "degrees", "_",
                    cutoff, "percent",  sep = "")
    }
    else {
      name <- paste(site, "_", back, "Back_", "AllDays", "_", 
                    "AllHours", "_",
                    bin.length, "degrees", "_",
                    cutoff, "percent",  sep = "")
    }
  }
  return(name)
}





# datas should simply be the dataframe with all of the data we want to make a common
# map from (binded already).

# The reference map will be the FIRST map entered after
# clicking the "compare" button.
CompareMap <- function(datas, gmap.type) {
  boxt <- qbbox(lat = datas$LAT, lon = datas$LON)
  MyMap <- GetMap.bbox(boxt$lonR, boxt$latR, destfile = "Arvin12Map.png", 
                       maptype = gmap.type)
  return(MyMap)
}

# TODO: right now, this function outputs a methane value as the cutoff for "high..."
# Filter the data based on parameters, return the subsetted data.
FilterData <- function(datas = NULL, bin.length = 0.05,
                       start.month = NULL, start.year = 0, start.day = 0,
                       end.month = 0, end.year = 0, end.day = 0,
                       zeros = TRUE, back = 6, too.few = NULL, points = FALSE,
                       start.time = NULL, end.time = NULL, cutoff = 25, 
                       rm.site = TRUE, numeric.cutoff = NULL, output.cutoff = FALSE,
                       weight.col = which(names(datas) == "CH4_H20_CORR")) {
  # Deal with the case where they supply a number for a 
  # weight column, or a column name.
  if (class(weight.col) == "character") {
    try(weight.col <- which(names(datas) == weight.col))
    if (class(weight.col) == "character") {
      print(paste("\nCould not find column ", weight.col, 
                  ", defaulting to CH4_H20_CORR", sep = ""))
      weight.col <- which(names(datas) == "CH4_H20_CORR")
    }
  }
  # Filter data by maximum number of hours backward
  datas <- datas[datas$BACK > -back, ]
  by.period <- all(c(start.year, start.month, start.day, 
                     end.year, end.month, end.day) != 0)
  # Filter data by days.
  if (by.period == TRUE) {
    time.sub <- seq(as.Date(paste(start.year, start.month, start.day, sep = "-")),
                    as.Date(paste(end.year, end.month, end.day, sep = "-")), by = "days")
    # Subset by months, possibly a vector
    datas <- datas[as.Date(datas$DATE) %in% time.sub, ]
  }
  
  # Filter data by hours
  if (class(start.time) == "numeric") {
    zero.back <- which(datas$BACK == 0)
    if (is.null(end.time)) {
      end.time <- start.time
      times <- seq(start.time, end.time)
    }
    else if (end.time <= start.time) {
      times <- c(seq(start.time, 23), 0, end.time)
    }
    else {
      times <- seq(start.time, end.time)
    }
    hourz <- as.numeric(substr(datas[zero.back, 1], 12, 13))
    hour.subset <- which(hourz %in% times)
    good.ind <- zero.back[hour.subset]
    # The C02_CORR and CH4_H20_CORR values for the good indicies will be 
    # assumed to be unique pairs.  This is a problem if there are (0, 0) pairs!
    subsetted <- which(datas[, 7] %in% datas[good.ind, 7] & 
                       datas[, 8] %in% datas[good.ind, 8])
    datas <- datas[subsetted, ]
  }
  if (rm.site == TRUE) {
    datas <- datas[datas$BACK != 0, ]
  }
  
  # Add acolumn to the data denoting whether the concentration
  # is high or low.
  datas <- data.frame(datas, color = "Low")
  datas$color <- as.character(datas$color)
  
  datas <- MarkHigh(datas, numeric.cutoff = numeric.cutoff, cutoff = cutoff, 
                    output.cutoff = output.cutoff, weight.col = weight.col)
  
  if (points == FALSE) {
    # Bin the data by latitude/longitude.
    datas <- Matrixed(datas, bin.length)
    # Omit empty cells from the data
    datas <- datas[datas$Total > 0, ]
    # Omit High cells with zeros from the data
    if (zeros == FALSE) {
      datas <- datas[datas$High > 0, ]
    }
    # Remove cells with very few measurements
    if (class(too.few) == "numeric") {
      datas <- datas[datas$Total > too.few, ]
    }
    # Weight the data's color by the number of high/total 
    datas <- data.frame(datas, WT = datas$High/(datas$Total))
  }
  else {
    changeit <- which(names(datas) == "CH4_H20_CORR")
    names(datas)[changeit] <- "WT"
  }
  return(datas)
}

GoogleMapIt <- function(datas, map.name = "", MyMap = NULL, output.map = FALSE, 
                        colors = "Reds", gmap.type = "mobile") {
  My.pal <- brewer.pal(9, colors)[3:9]
  if(is.null(MyMap)) {
    boxt <- qbbox(lat = datas$LAT, lon = datas$LON)
    MyMap <- GetMap.bbox(boxt$lonR, boxt$latR, destfile = "Arvin12Map.png", 
                         maptype = gmap.type)
  }
  binz2 <- seq(0, 1, 1/7)
  binz2[1] <- binz2[1] - 0.00000000001
  binz2[8] <- binz2[8] + 0.00000000001
  binz3 <- binz2
  binz2 <- round(binz2, digits = 3) * 100
  leg.txt <- c(paste(binz2[1], "-", binz2[2], sep = " "), 
               paste(binz2[2], "-", binz2[3], sep = " "),
               paste(binz2[3], "-", binz2[4], sep = " "),
               paste(binz2[4], "-", binz2[5], sep = " "),
               paste(binz2[5], "-", binz2[6], sep = " "),
               paste(binz2[6], "-", binz2[7], sep = " "),
               paste(binz2[7], "-", 100, sep = " "))
  
  cols2 <- My.pal[cut(datas$WT, binz3, labels = FALSE)]
  cols2[length(cols2)] <- "black"
  if (output.map == TRUE) {
    pdf(paste(map.name, ".pdf", sep = ""))
  }
  PlotOnStaticMap(MyMap, lat = datas$LAT, lon = datas$LON, col = cols2, 
                  pch = 19, cex = 0.5) # axes = TRUE, mar = rep(4, 4))
  legend("bottomleft", legend = leg.txt, fill = My.pal, 
         title = "High Observations (%)", bg = "white")

  
}

# Input a pre-formatted dataset with latitude/longitude,
# and all of the data which must be included in the map area.
MasterMap <- function(datas, gmap.type = "mobile") {
  boxt <- qbbox(lat = datas$LAT, lon = datas$LON)
  MyMap <- GetMap.bbox(boxt$lonR, boxt$latR, destfile = "CompareMap.png", 
                       maptype = gmap.type)
  return(MyMap)
}

MarkHigh <- function(datas = NULL, cutoff = 25, weight.col, numeric.cutoff = NULL,
                     output.cutoff = FALSE) {
  if (is.null(numeric.cutoff)) {
    datas$color[datas[, weight.col] > quantile(datas[, weight.col], 
                                                prob = 1 - cutoff/100)] <- "High"
    if(output.cutoff == TRUE) {
      abs.cutoff <<- quantile(datas[, weight.col], prob = 1 - cutoff/100)
    }
  }
  # If a numeric cutoff is supplied, cut there.
  else {
    datas$color[datas[, weight.col] > abs.cutoff] <- "High"
  }
  return(datas)
}
