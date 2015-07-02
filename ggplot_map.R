rm(list=ls())
############################################################
# Starting point for this script is geotiffs that have been 
# batch stretched and cropped to area of interest


imdir <- "W:\\usgs\\110078\\LG_display_tiff"
pathrow <- substr(imdir, 9, 14)
anidir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation"
setwd(imdir)

library(ggplot2)
library(animation)
library(grid)
library(gridExtra)
library(raster)
library(rgdal)
library(maptools)
library(animation)
library(tools)
library(plyr)

#read suncorrected image
imname <- list.files(pattern = "pre.ers")
gtname <- paste0("L_", as.Date(substr(imname, 12,17), "%d%m%y"), "_", pathrow, ".tiff")
rawlt <- stack(imname)
nlayers(rawlt)

setwd(anidir)
e <- extent(readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                    "aoi_LG_extent_polygons"))
ngn <- crop(x=rawlt, e)
writeRaster(x=ngn, filename=gtname, format="GTiff", overwrite=T)

mb_ras <- stack("L_1986-09-12_110078.tif")
mb_ras <- stretch(x=mb_ras, minv=0, maxv=255)
mb_df <- raster::as.data.frame(mb_ras, xy=T)

mb_df <- data.frame(x=mb_df[,1], y=mb_df[,2], b1=mb_df[,3], b2=mb_df[,4], 
                    b3=mb_df[,5], b4=mb_df[,6], b5=mb_df[,7],
                    b6=mb_df[,8])


mb_df[!complete.cases(mb_df),]
mb_df <- mb_df[complete.cases(mb_df),]
mb_df[!complete.cases(mb_df),]

##create rgb fills outside of ggplot
fill543 <- rgb(mb_df$b5,mb_df$b4,mb_df$b3, maxColorValue = 255)
fill321 <- rgb(mb_df$b3,mb_df$b2,mb_df$b1, maxColorValue = 255)

#shapes
enclosure = readOGR(dsn=".", layer="Enclosure")
enclosure@data$id = rownames(enclosure@data)
enclosure.points = fortify(enclosure, region="id")
enclosure.df = join(enclosure.points, enclosure@data, by="id")

site = readOGR(dsn=".", layer="LG_monitor_sites_example")
site@data$id = rownames(site@data)
site.points = fortify(site, region="id")
site.df = join(site.points, site@data, by="id")

###################################################
#For ts data
setwd(anidir)
d <- read.csv("11078_i35_test.csv", header = TRUE)
d$date <- as.Date(d$date, "%Y-%m-%d")
d$dnum <- as.numeric(d$date)
#d$label <- factor(rep("ts", by=length(df[,1])))
df <- d[-1]
# sname <- colnames(df)
# sname <- sname[2:3]
#df2.i <- df
#date = df2.i$date
x = df[,4]
y = df[,2]
dfall <- data.frame(x,y)
ind <- length(x)
ndate <- d$date
##################################################

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
        labs(title="date of image here")


map2 <- map + geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
                  colour="yellow", size=1)+
        geom_path(data=site.df, aes(x=long,y=lat,group=group), 
                  colour="red", size=2)
i=100
dfsub <- dfall[i, ]
ts <- ggplot()+
        #geom_point(data = dfall, aes(x=x, y=y))+
        geom_point(data = dfsub, aes(x=x, y=y), colour = "red", shape = 10, size =5)+
        geom_line(data = dfall, aes(x=x, y=y))+
        geom_vline(xintercept = dfsub[,1], colour = "red")+
        theme_bw()+
        xlab("")+
        ylab("i35 index")+
        
        coord_cartesian(xlim = c(6000, 16700),
                        ylim = c(20, 200))+
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none")+
        coord_equal(ratio = 12)
grid.arrange(map2, ts, nrow=2)




#aspect ratio for plots!
qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1)
qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 5)
qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1/3)
