
# Clear workspace
rm(list=ls())

# Load required libraries
library(ggplot2)
library(animation)
library(grid)
library(gridExtra)
library(raster)
library(gpclib)
library(rgeos)
library(rgdal)
library(maptools)
library(tools)
library(plyr)

# Required directory paths
imdir <- "W:\\usgs\\110078"#path/row folder in USGS
anidir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation"#working dir: location of shape files
tmpdir <- "C:\\Users\\barth\\AppData\\Local\\Temp\\R_raster_barth"
pathrow <- substr(imdir, 9, 14)

setwd(anidir)
folder <- paste0(pathrow, "_ani_tifs_", Sys.Date())
if(!file.exists(folder)){ dir.create(folder)} # create another folder
tifdir <- paste0(anidir, "\\", folder)

# Read in shape files
ext <- extent(readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                      "aoi_LG_extent_polygons"))#get extent only of aoi
# Special workflow to fortify for use in ggplot
enclosure <- readOGR(dsn = ".", layer = "Enclosure")
enclosure@data$id <- rownames(enclosure@data)
enclosure.points <- fortify(enclosure, region = "id")
enclosure.df <- join(enclosure.points, enclosure@data, by = "id")

# LGsite1
site1 <- readOGR(dsn = ".", layer = "LG_site1")
site1@data$id <- rownames(site1@data)
site1.points <- fortify(site1, region = "id")
site1.df <- join(site1.points, site1@data, by = "id")

# LGsite2
site2 <- readOGR(dsn = ".", layer = "LG_site2")
site2@data$id <- rownames(site2@data)
site2.points <- fortify(site2, region = "id")
site2.df <- join(site2.points, site2@data, by = "id")

# LGsites 1 and 2
site3 <- readOGR(dsn=".", layer="LG_site_1_2")
site3@data$id <- rownames(site3@data)
#site3@data$NAME <- as.character(site3@data$NAME)
site3.points <- fortify(site3, region = "id")
site3.df <- join(site3.points, site3@data, by = "id")

# site <- readOGR(dsn = ".", layer = "LG_monitor_sites_example_sml")
# site@data$id <- rownames(site@data)
# site.points <- fortify(site, region = "id")
# site.df <- join(site.points, site@data, by = "id")

# Read in time series data for demo site
d <- read.csv("11078_i35_test_ALL.csv", header = TRUE)
d[,2] <- as.Date(d[,2], "%Y-%m-%d")
d$dnum <- as.numeric(d[,2])
df <- d[-1]

# Code to trim to match animation sequence (remove Display folder and bad L7's)
setwd("W:\\usgs\\110078")#imdir#
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
sites.df <- df[which(!nL7.index), ]#use which!
folds.no.7 <- fold[!nL7.index]#this used in images - CHECK lengths same

#for (i in 1:5){
for (i in 6: length(folds.no.7)){
        setwd(paste0(imdir, "\\", folds.no.7[i]))
        imname <- list.files(pattern = "pre.ers")
        gtname <- paste0("L_str_", as.Date(substr(imname, 12,17), "%d%m%y"), 
                         "_", pathrow, ".tif")
        #rdate <- format(as.Date(substr(imname, 12,17), "%d%m%y"), "%b %Y")
        rawsat <- stack(imname)
        
        aoi.rawsat <- crop(x=rawsat, ext)#crop it
        aoi.str.sat <- stretch(x=aoi.rawsat, minv=0, maxv=255)
        
        setwd(tifdir)
        writeRaster(x=aoi.str.sat, filename=gtname, format="GTiff", overwrite=T)
        
        ##Clean up "C:\\Users\\barth\\AppData\\Local\\Temp\\R_raster_barth"
        tmp.list <- list.files(path = tmpdir, full.names = TRUE)
        file.remove(tmp.list)#Dangerous be careful
        
}

for (i in length(folds.no.7)){
        setwd(tifdir)
        t.name <- list.files(pattern = ".tif")
        t.stack <- stack(t.name)
        t.df <- raster::as.data.frame(t.stack, xy=T)
        t.df <- data.frame(x=t.df[,1], y=t.df[,2], b1=t.df[,3], b2=t.df[,4], 
                            b3=t.df[,5], b4=t.df[,6], b5=t.df[,7],
                            b6=t.df[,8])
        t.df <- t.df[complete.cases(t.df),]#remove NA's
        
        ## Create Plots objects
        
        
        
}






