################################################################################
# Produces animated .gif of Landsat zoomed image in desired band combination and
# synchronised time series of index of a demonstration site. Uses ggplot methods
# to display Landsat stretched raster with overlaid vectors (boundaries, demo 
# site, etc) with corrsponding time series below.
#
# Require:
# 1. Suncorrected Landsat scenes in processed USGS date folders
# 2. Shape files
#       a. Extent of zoom required 
#       b. Demonstration site 
#       c. Other vectors if required (boundaries etc)
# 3. Extracted index/band values for demo site (use Landsat_stackR script)
#
# Special Notes
# 1. For time series and raster animation to synchronise both must contain the
# identical number of steps (Landsat image or data point). Whatever logical
# indexing to weed out L7 images or bad data must be applied to both the list
# of pre.ers and image dates for site data.





# Clear workspace
rm(list=ls())

# Load required libraries
library(ggplot2)
library(animation)
library(grid)
library(gridExtra)
library(raster)
library(rgdal)
library(maptools)
library(tools)
library(plyr)

# Required directory paths
imdir <- "W:\\usgs\\110078"#path/row folder in USGS
anidir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation"#working dir: location of shape files

# Read in shape files
setwd(anidir)
ext <- extent(readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                      "aoi_LG_extent_polygons"))#get extent only of aoi
# Special workflow to fortify for use in ggplot
enclosure <- readOGR(dsn = ".", layer = "Enclosure")
enclosure@data$id <- rownames(enclosure@data)
enclosure.points <- fortify(enclosure, region = "id")
enclosure.df <- join(enclosure.points, enclosure@data, by = "id")

site <- readOGR(dsn = ".", layer = "LG_monitor_sites_example")
site@data$id <- rownames(site@data)
site.points <- fortify(site, region = "id")
site.df <- join(site.points, site@data, by = "id")

# Read in time series data for demo site
d <- read.csv("11078_i35_test_ALL.csv", header = TRUE)
d[,2] <- as.Date(d[,2], "%Y-%m-%d")
d$dnum <- as.numeric(d[,2])
df <- d[-1]

# Code to trim to match animation equence (remove Display folder and bad L7's)
setwd("W:\\usgs\\110078")#imdir
allfiles <- list.files(recursive = TRUE)
result <- allfiles[grepl("*pre.ers", allfiles)]
result <- result[!grepl("Display*", result)]#remove display folder
fold <- substr(result, 1, 8)#get just folders
imdate <- as.Date(fold, "%Y%m%d")#get just image date
sensor <- substr(result, 10, 11)#get just sensor
#make df for indexing only
ind.df <- data.frame(s = sensor, d = imdate, stringsAsFactors = FALSE)
#create index to and remove l7 scan error images
nL7.index <- (ind.df$s == "l7") & (ind.df$d > as.Date("2003-05-30"))
df2 <- df[which(!nL7.index), ]#use which!
folds.no.7 <- fold[!nL7.index]#this used in images - CHECK lengths same













