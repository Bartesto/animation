rm(list=ls())
############################################################
# Starting point for this script is geotiffs that have been 
# batch stretched and cropped to area of interest


imdir <- "W:\\usgs\\110078"#\\LG_display_tiff
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
gtname <- paste0("L_", as.Date(substr(imname, 12,17), "%d%m%y"), "_", pathrow, ".tif")
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
# fill543 <- rgb(mb_df$b5,mb_df$b4,mb_df$b3, maxColorValue = 255)
# fill321 <- rgb(mb_df$b3,mb_df$b2,mb_df$b1, maxColorValue = 255)

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


p1 <- map + geom_path(data=enclosure.df, aes(x=long,y=lat,group=group), 
                  colour="yellow", size=1)+
        geom_path(data=site.df, aes(x=long,y=lat,group=group), 
                  colour="red", size=2)
i=100
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
                        ylim = c(20, 200))+
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none")
        #coord_equal(ratio = 12)
#grid.arrange(map2, ts, nrow=2)
png(filename = "test1.png", width = 1000, height = 1000)#, width = 842, height = 250
grid.newpage() # Open a new page on grid device
pushViewport(viewport(layout = grid.layout(7, 5)))
print(p1, vp = viewport(layout.pos.row = 1:5, layout.pos.col = 1:5))
print(p2, vp = viewport(layout.pos.row = 6:7, layout.pos.col = 2:4)) 
dev.off()




#aspect ratio for plots!
qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1)
qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 5)
qplot(mpg, wt, data = mtcars) + coord_equal(ratio = 1/3)


anidir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation"
setwd(anidir)
d <- read.csv("11078_i35_test_ALL.csv", header = TRUE)
d[,2] <- as.Date(d[,2], "%Y-%m-%d")
d$dnum <- as.numeric(d[,2])
df <- d[-1]
##Code for 11078 example to match animation equence
setwd("W:\\usgs\\110078")#imdir
allfiles <- list.files(recursive = TRUE)
#get only those that end in .pre
result <- allfiles[grepl("*pre.ers", allfiles)]
#get only image date folders file paths
result <- result[!grepl("Display*", result)]#remove display folder
#get just folders
fold <- substr(result, 1, 8)
#get just image date
imdate <- as.Date(fold, "%Y%m%d")
#get just sensor
sensor <- substr(result, 10, 11)
#make df of everything for indexing
ind.df <- data.frame(s = sensor, d = imdate, stringsAsFactors = FALSE)
#create index to and remove l7 scan error images
nL7.index <- (ind.df$s == "l7") & (ind.df$d > as.Date("2003-05-30"))
df2 <- df[which(!nL7.index), ]#use which!
folds.no.7 <- fold[!nL7.index]#this used in images - CHECK lengths same


date <- as.Date(folds.no.7, "%Y%m%d")

#for LGS2
x = df[,4]
y = df[,2]
dfall <- data.frame(x,y)
ind <- length(x)
ndate <- d$date


i = 100

#For LGS2 90m plot
x = df[,4]
y = df[,3]
dfsml <- data.frame(x,y)
dfsub <- dfsml[i, ]
p1 <- ggplot()+
        #geom_point(data = dfall, aes(x=x, y=y))+
        geom_point(data = dfsub, aes(x=x, y=y), colour = "red", shape = 10, size =5)+
        geom_line(data = dfsml, aes(x=x, y=y))+
        geom_vline(xintercept = dfsub[,1], colour = "red")+
        theme_bw()+
        xlab("")+
        ylab("index")+
        
        coord_cartesian(xlim = c(6000, 16700),
                        ylim = c(20, 200))+
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none")
#For LGRS landscape plot
x1 = df[,4]
y1 = df[,2]
dflge <- data.frame(x1,y1)
dfsub1 <- dflge[i, ]
p2 <- ggplot()+
        #geom_point(data = dfall, aes(x=x, y=y))+
        geom_point(data = dfsub1, aes(x=x1, y=y1), colour = "red", shape = 10, size =5)+
        geom_line(data = dflge, aes(x=x1, y=y1))+
        geom_vline(xintercept = dfsub[,1], colour = "red")+
        theme_bw()+
        xlab("")+
        ylab("index")+
        
        coord_cartesian(xlim = c(6000, 16700),
                        ylim = c(20, 200))+
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none")

grid.arrange(p1, p2, nrow=2)

for (i in 1:ind){
        dfsub <- dfall[i, ]
        name.i <- paste0(ndate[i], "_LG.png")
        ggobj <- ggplot()+
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
                      legend.position = "none")
        png(filename = name.i, width = 842, height = 250)
        print(ggobj)
        dev.off()
}
