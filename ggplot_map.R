rm(list=ls())


imdir <- "W:\\usgs\\110078\\19860912"
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


map <- ggplot(data=mb_df, aes(x=x, y=y, fill=fill543)) +
                coord_equal() + theme_bw() + geom_tile() + scale_fill_identity() +
                scale_x_continuous(breaks=c(304000, 350000),
                                   labels=c(304000, 350000), expand = c(0,0)) +
                scale_y_continuous(breaks=c(7090000, 7130000), 
                                   labels=c(7090000, 7130000), expand = c(0,0)) +
                #geom_polygon(data=enclosure.df, aes(x = long, y= lat, group = id)) +
                #geom_path(color="white") +
                theme(panel.grid=element_blank(), plot.title = element_text(size = 10))+
                xlab("")+ ylab("")+
                labs(title="date of image here")
map +
        geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
                  colour="yellow", alpha=0.5)
geom_polygon(data=enclosure.df, aes(x = long, y= lat, group = ID)) +
geom_path(color="white") 
ggplot(enclosure.df, aes(long, lat)) + geom_polygon(colour='black', fill='white')

utah = readOGR(dsn=".", layer="eco_l3_ut")
utah@data$id = rownames(utah@data)
utah.points = fortify(utah, region="id")
utah.df = join(utah.points, utah@data, by="id")


