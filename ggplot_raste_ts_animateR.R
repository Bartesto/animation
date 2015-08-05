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
site2.df <- join(site2.points, site1@data, by = "id")

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
sites.df <- df[which(!nL7.index), ]#use which!
folds.no.7 <- fold[!nL7.index]#this used in images - CHECK lengths same

############################################################################
# Manipulate raster data for image plot

i=102
for (i in 1: length(folds.no.7)){
        setwd(paste0(imdir, "\\", folds.no.7[i]))
        imname <- list.files(pattern = "pre.ers")
        gtname <- paste0("L_", as.Date(substr(imname, 12,17), "%d%m%y"), 
                         "_", pathrow, ".tif")
        rdate <- format(as.Date(substr(imname, 12,17), "%d%m%y"), "%b %Y")
        rawlt <- stack(imname)
       
        ngn <- crop(x=rawlt, ext)
        setwd(tifdir)
        writeRaster(x=ngn, filename=gtname, format="GTiff", overwrite=T)
        t.name <- list.files(pattern = ".tif")
        mb_ras <- stack(t.name)
        mb_ras <- stretch(x=mb_ras, minv=0, maxv=255)
        mb_df <- raster::as.data.frame(mb_ras, xy=T)
        mb_df <- data.frame(x=mb_df[,1], y=mb_df[,2], b1=mb_df[,3], b2=mb_df[,4], 
                            b3=mb_df[,5], b4=mb_df[,6], b5=mb_df[,7],
                            b6=mb_df[,8])
        mb_df <- mb_df[complete.cases(mb_df),]#remove NA's
        #create raster ggplot object
        map <- ggplot() +
                coord_equal() + theme_bw() + 
                geom_tile(data=mb_df, aes(x=x, y=y, fill=rgb(b5,b4,b3, 
                                                             maxColorValue = 255))) + 
                scale_fill_identity() +
                scale_x_continuous(breaks=c(304000, 350000),
                                   labels=c(304000, 350000), expand = c(0,0)) +
                scale_y_continuous(breaks=c(7090000, 7130000), 
                                   labels=c(7090000, 7130000), expand = c(0,0)) +
                theme(panel.grid=element_blank(), plot.title = element_text(size = 10))+
                xlab("")+ ylab("")+
                labs(title=rdate)
        p1 <- map + geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
                              colour="yellow", size=1)+
                geom_path(data=site.df, aes(x=long,y=lat,group=group), 
                          colour="red", size=2)
        
        #create ts ggplot object ####CHANGES!!!!!!!!!
        x <- sites.df[, 4]
        y <- sites.df[, 3]
        dfall <- data.frame(x=x, y=200-y)##Flipped i35
        dfsub <- dfall[i, ]
        p2 <- ggplot()+
                #geom_point(data = dfall, aes(x=x, y=y))+
                geom_point(data = dfsub, aes(x=x, y=y), colour = "red", shape = 10, size =5)+
                geom_line(data = dfall, aes(x=x, y=y))+
                geom_vline(xintercept = dfsub[,1], colour = "red")+
                theme_bw()+
                xlab("")+
                ylab("index")+
                
                coord_cartesian(xlim = c(6000, 16700),
                                ylim = c(0, 200))+
                theme(axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      legend.position = "none")
        png(filename = "testY.png", width = 1000, height = 1000)#, width = 842, height = 250
        grid.newpage() # Open a new page on grid device
        pushViewport(viewport(layout = grid.layout(7, 5)))
        print(p1, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 1:5))
        print(p2, vp = viewport(layout.pos.row = 6:7, layout.pos.col = 2:4)) 
        dev.off()
        
        ##Clean up "C:\\Users\\barth\\AppData\\Local\\Temp\\R_raster_barth"
        tmp.list <- list.files(path = tmpdir, full.names = TRUE)
        file.remove(tmp.list)#Dangerous be careful
        
}











