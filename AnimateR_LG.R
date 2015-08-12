# Clear workspace
rm(list=ls())

# The purpose of this code is to create a gif animation of a Landsat sequence to aid in
# presentations. At present it can produce a gif but hoping to develop a later step for
# conversion to mpeg for greater control.

# By Bart Huntley
# 07/08/2015

############################STAGE 1####################################################
## Set up work environment with required common objects and tasks for later stages

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

## Read in shape files
ext <- extent(readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                      "aoi_LG_extent_polygons"))#get extent of aoi

# Special workflow to fortify for use in ggplot for polygon shape files
enclosure <- readOGR(dsn = ".", layer = "Enclosure")
enclosure@data$id <- rownames(enclosure@data)
enclosure.points <- fortify(enclosure, region = "id")
enclosure.df <- join(enclosure.points, enclosure@data, by = "id")

# Create stand alone .csv for site data with lat long column
# LGsite1 pt
site1.df <- read.csv("site1_pt.csv", header = TRUE)
site3.df <- read.csv("site3_pt.csv", header = TRUE)

# Read in time series data for demo site from StackR
d <- read.csv("11078_i35_test_sites1_3clean.csv", header = TRUE, stringsAsFactors = FALSE)
d[,2] <- as.Date(d[,2], "%d/%m/%Y")
d$dnum <- as.numeric(d[,2])
df <- d[,-1]

# Code to trim to match animation sequence (remove Display folder and bad L7's)
setwd(imdir)
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
#sites.df <- df[which(!nL7.index), ]#use which!
sites.df <- df
folds.no.7 <- fold[!nL7.index]#this used in images - CHECK lengths same

############################STAGE 2####################################################
## Create stretched geotiffs for later import (runs for 2-3mins per image)

for (i in 1: length(folds.no.7)){
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
############################STAGE 3####################################################
## Create individual png files with polygons, points and incremental graphs

#special case due to manual deletion of cloudy images
setwd(tifdir)
new.folds <- as.numeric(length(list.files(pattern = ".tif")))
t.names <- list.files(pattern = ".tif")

for (i in 1:new.folds){#beware of change from folds.no.7
        #setwd(tifdir)
        #setwd("Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation\\110078_ani_tifs_2015-08-07")
        
        t.name <- t.names[i]
        t.stack <- stack(t.name)
        t.df <- raster::as.data.frame(t.stack, xy=T)
        t.df <- data.frame(x=t.df[,1], y=t.df[,2], b1=t.df[,3], b2=t.df[,4], 
                           b3=t.df[,5], b4=t.df[,6], b5=t.df[,7],
                           b6=t.df[,8])
        t.df <- t.df[complete.cases(t.df),]#remove NA's
       
        rdate <- format(as.Date(substr(t.name, 7, 16), "%Y-%m-%d"), "%b %Y")
        print(paste("processing: ", t.name))
        
        ## Create Plot objects
        # Base image with shape files
        map <- ggplot() +
                coord_equal() + theme_bw() + 
                geom_tile(data=t.df, aes(x=x, y=y, fill=rgb(b5,b4,b3, 
                                                             maxColorValue = 255))) + 
                scale_fill_identity() +
                scale_x_continuous(breaks=c(325000, 345000),
                                   labels=c(325000, 345000), expand = c(0,0)) +
                scale_y_continuous(breaks=c(7095000, 7105000), 
                                   labels=c(7095000, 7105000), expand = c(0,0)) +
                theme(panel.grid=element_blank(), plot.title = element_text(size = 25))+
                xlab("")+ ylab("")+
                labs(title=rdate)
        
        p1 <- map + geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
                              colour="yellow", size=1)+#enclosure
                geom_point(data=site1.df, aes(x=long,y=lat), 
                          colour="red", size=2.5)+#site 1
                geom_point(data=site3.df, aes(x=long,y=lat), 
                          colour="blue", size=2.5)#site 2
        
        ## Create df for site 1 index plot
        #x.1 <- sites.df[, 5]
        x.1 <- sites.df[, 4]
        y.1 <- sites.df[, 2]
        dfall.1 <- data.frame(x.1=x.1, y.1=200-y.1)##Flipped i35
        dfsub.1 <- dfall.1[i, ]
        # Plot site 1 index graph
        p2 <- ggplot()+
                geom_point(data = dfsub.1, aes(x=x.1, y=y.1), colour = "red", 
                           shape = 10, size =5)+
                geom_line(data = dfall.1, aes(x=x.1, y=y.1))+
                geom_vline(xintercept = dfsub.1[,1], colour = "red")+
                theme_bw()+
                xlab("")+
                ylab("index")+
                
                coord_cartesian(xlim = c(6000, 16700),
                                ylim = c(0, 200))+
                theme(axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      legend.position = "none")
        
        ## Create df for site 2 index plot
        #x.2 <- sites.df[, 5]
        x.2 <- sites.df[, 4]
        y.2 <- sites.df[, 3]
        dfall.2 <- data.frame(x.2=x.2, y.2=200-y.2)##Flipped i35
        dfsub.2 <- dfall.2[i, ]
        # Plot site 2 index graph
        p3 <- ggplot()+
                geom_point(data = dfsub.2, aes(x=x.2, y=y.2), colour = "blue", 
                           shape = 10, size =5)+
                geom_line(data = dfall.2, aes(x=x.2, y=y.2))+
                geom_vline(xintercept = dfsub.2[,1], colour = "blue")+
                theme_bw()+
                xlab("")+
                ylab("index")+
                coord_cartesian(xlim = c(6000, 16700),
                                ylim = c(0, 200))+
                theme(axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      legend.position = "none")
        
        pngname <- paste0(substr(t.name, 7, 16), "_LG.png")#name for png file
        
        # Create png file using grids to organise layout
        png(filename = pngname, width = 1000, height = 1000)#, width = 842, height = 250
        grid.newpage() # Open a new page on grid device
        pushViewport(viewport(layout = grid.layout(9, 5)))
        print(p1, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 1:5))
        print(p2, vp = viewport(layout.pos.row = 6:7, layout.pos.col = 2:4))#site 1
        print(p3, vp = viewport(layout.pos.row = 8:9, layout.pos.col = 2:4))#site 2
        dev.off()
        
}

############################STAGE 4####################################################
## png files need to be unambiguously renamed 0001 to (n) to ensure correct sequence in 
## gif animation creation. Ensure that you make a copy of the original png's before running

#Rename png files to numbers to order correctly
png.list <- list.files(pattern = '*.png')
nname <- sprintf("img-%d.png", seq(png.list))
file.rename(png.list, nname)

############################STAGE 5####################################################
## Using animation package and pre-installed ImageMagick to compile gif

ani.options(convert = 'C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe',
            ani.width = 1800, ani.height = 750, interval = 0.7, ani.dev = "png",
            ani.type = "png", loop = 0)
im.convert("*.png", output = "LG-i35f_graph_fixed_ts.gif")

#######################################################################################
## If you want a mp4 instead you need to have FFmpeg installed and the ffmpeg.exe in 
## your working directory. Open a cmd window and run

# > ffmpeg -f image2 -r 2 -i img-%d.png -crf 5 your name here.mp4

# r- 2 = is for speed between static images (low is slow)
# -crf 5 = quality (low is high quality)
 


