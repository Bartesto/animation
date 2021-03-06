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

############################################################################
# Manipulate raster data for image plot
test1=c(1:5)
for (i in 1:5){
#for (i in 1: length(folds.no.7)){
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
                theme(panel.grid=element_blank(), plot.title = element_text(size = 25))+
                xlab("")+ ylab("")+
                labs(title=rdate)
        #plots only last site and enclosure
        p1 <- map + geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
                              colour="yellow", size=1)+#enclosure
                   geom_path(data=site1.df, aes(x=long,y=lat,group=group), 
                              colour="red", size=4)+#green site 1
                   geom_path(data=site2.df, aes(x=long,y=lat,group=group), 
                              colour="blue", size=4)#hot pink site 2
        #plots all 3 but no map
#         ggplot() + geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
#                              colour="yellow", size=1)+#enclosure
#                 geom_path(data=site1.df, aes(x=long,y=lat,group=group), 
#                           colour="#00FF00", size=2)+#green site 1
#                 geom_path(data=site2.df, aes(x=long,y=lat,group=group), 
#                           colour="#FF00CC", size=2)#hot pink site 2
        #
#         map + #geom_path(data=site3.df, aes(long, lat, group=NAME, color=NAME), size=2)+
#                 geom_path(data=site1.df, aes(long, lat, group=NAME), color="#00FF00", size =2)+
#                 geom_path(data=site2.df, aes(long, lat, group=NAME), color="#FF00CC", size =2)+
#                 geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
#                           colour="yellow", size=1)
        
        #create ts ggplot object ####CHANGES!!!!!!!!!
        ##Site 1 Green
        x.1 <- sites.df[, 5]
        y.1 <- sites.df[, 2]
        dfall.1 <- data.frame(x.1=x.1, y.1=200-y.1)##Flipped i35
        dfsub.1 <- dfall.1[i, ]
        p2 <- ggplot()+
                #geom_point(data = dfall, aes(x=x, y=y))+
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
        
        ##Site 2 Hot Pink
        x.2 <- sites.df[, 5]
        y.2 <- sites.df[, 3]
        dfall.2 <- data.frame(x.2=x.2, y.2=200-y.2)##Flipped i35
        dfsub.2 <- dfall.2[i, ]
        p3 <- ggplot()+
                #geom_point(data = dfall, aes(x=x, y=y))+
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
        
       
        name.i <- paste0(substr(gtname[i], 3, 19), "_LG.png")
        
        png(filename = name.i, width = 1000, height = 1000)#, width = 842, height = 250
        grid.newpage() # Open a new page on grid device
        pushViewport(viewport(layout = grid.layout(9, 5)))
        print(p1, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 1:5))
        print(p2, vp = viewport(layout.pos.row = 6:7, layout.pos.col = 2:4))#site 1
        print(p3, vp = viewport(layout.pos.row = 8:9, layout.pos.col = 2:4))#site 2
        dev.off()
        
        ##Clean up "C:\\Users\\barth\\AppData\\Local\\Temp\\R_raster_barth"
        tmp.list <- list.files(path = tmpdir, full.names = TRUE)
        file.remove(tmp.list)#Dangerous be careful
        
}











