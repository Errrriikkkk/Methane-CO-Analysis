is.installed <- function(mypkg) {
	is.element(mypkg, installed.packages()[, 1]) 
}
pkgs = c("maps", "ggplot2", "RgoogleMaps", "RColorBrewer", "fields", 
		 "rgdal", "foreign", "reshape", "xts", "tcltk")
repos = "http://lib.stat.cmu.edu/R/CRAN"
for (i in pkgs) {
	if(!is.installed(i)) {
		print(paste("Installing", i))
		install.packages(i, repos = repos)
	}
}