# This work seeks to combine a stretched, zoomed plotRGB object
# with a "moving" time series trace of a site.
rm(list=ls())

#Lots of libraries
library(ggplot2)
library(animation)
library(grid)
library(gridExtra)
library(raster)
library(rgdal)
library(maptools)
library(animation)
library(tools)

##directories
#here <- "Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\Working\\animation"
imdir <- "W:\\usgs\\110078"
tmpdir <- "C:\\Users\\barth\\AppData\\Local\\Temp\\R_raster_barth"
anidir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation"

##user inputs
red=5
green=4
blue=3
project="LG"#for use in gif name for providence
combo="543"#for use in gif name for providence
index.name="200-i35"#for use in gif name for providence

###################################################
#For ggplot data
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
##For raster
setwd(imdir)

#Function to return folder names - source HELPER functions
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
        # use full.names=TRUE to pass to file.info
        all <- list.files(path, pattern, all.dirs,
                          full.names=TRUE, recursive=FALSE, ignore.case)
        dirs <- all[file.info(all)$isdir]
        # determine whether to return full names or just dir names
        if(isTRUE(full.names))
                return(dirs)
        else
                return(basename(dirs))
}

#get all files then only .pre.ers - BEWARE and check file paths. They should
#all start with: date folder/...pre.ers. If it has collected extra folders (e.g.
#perhaps suncorrected inside the processing folder) then these need to be removed
#from the "result" object.

#get everything
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
#make df
ind.df <- data.frame(s = sensor, d = imdate, stringsAsFactors = FALSE)
#create index to and remove l7 scan error images
nL7.index <- (ind.df$s == "l7") & (ind.df$d > as.Date("2003-05-30"))
folds.no.7 <- fold[!nL7.index]

setwd(anidir)
#Obtain extents from are of interest shape file
e <- extent(readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                    "aoi_LG_extent_polygons"))
enclosure <- readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                     "Enclosure")
LG <- readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
              "LornaGlen")
site <- readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation",
                "LG_monitor_sites_example")


i=100
resetPar <- function() {
        dev.new()
        op <- par(no.readonly = TRUE)
        dev.off()
        op
}
par(resetPar())

for(i in 1:length(folds.no.7)){
        setwd(paste0(imdir, "\\", folds.no.7[i]))
        f <- list.files(pattern = '*pre.ers')
        date <- as.Date(substring(f, 12, 17),"%d%m%y")
        pname <- paste0(date, "-", combo, ".png")
        fname <- paste0(here, "\\", "allstretch", pname)
        plab <- format(date, "%b %Y")
        br <- raster(f, band = red)
        br1 <- stretch(br)
        bg <- raster(f, band = green)
        bg1 <- stretch(bg)
        bb <- raster(f, band = blue)
        bb1 <- stretch(bb)
        b <- brick(br1, bg1, bb1)
        bsc <- crop(b, e)
        png(filename = fname, width = 842, height = 870)
        layout(matrix(c(1,2), 1, 1, byrow = TRUE),
               height = c(3,1))
        #par(mfrow=c(1,1))
        #par(mar=c(8,6,4,2)+0.1)
        plotRGB(bsc, 1, 2, 3, axes = TRUE, 
                main = paste0(plab, " ", "Lorna Glen"))
        plot(enclosure, add = TRUE, lwd = 2, border = "green")
        plot(LG, add= TRUE, lwd = 2, border = "yellow")
        plot(site, add= TRUE, lwd = 2, border = "red")
        
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
        print(ggobj)
        dev.off()
        tmp.list <- list.files(path = tmpdir, full.names = TRUE)
        file.remove(tmp.list)#Dangerous be careful
        
}
##################################################
##create plots
for (i in 1:ind){
        dfsub <- dfall[1:i, ]
        name.i <- paste0(ndate[i], "_LG.png")
        ggobj <- ggplot()+
                        geom_point(data = dfsub, aes(x=x, y=y))+
                        geom_line(data = dfsub, aes(x=x, y=y), colour = "red", size = 1)+
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

png.list <- list.files(pattern = '*LG.png')
nname <- sprintf("%.4d.png", seq(png.list))
file.rename(png.list, nname)

#Create .gif animation
ani.options(convert = 'C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe',
            ani.width = 1800, ani.height = 750, interval = 0.7, ani.dev = "png",
            ani.type = "png", loop = 0)
im.convert("*.png", output = paste0(project,"_", combo, "_",
           index.name, ".gif"))


#Combine 2 png in 1
setwd(".\\test")
rl = lapply(z, png::readPNG)
gl = lapply(rl, grid::rasterGrob)
do.call(gridExtra::grid.arrange, gl) 

##ggplot with moving line and fixed ts
dfsub <- dfall[i, ]
ggplot()+
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
z = 1:20
i = 13
q = sprintf("%.4d", seq(z))

for(i in 1:length(z)){
l = list.files(pattern = paste0("*",q[i],".png"))

rl = lapply(l, png::readPNG)
gl = lapply(rl, grid::rasterGrob)
png(filename=paste0("test_combo_", i, ".png"))
do.call(gridExtra::grid.arrange, gl) 
dev.off()
}

ani.options(convert = 'C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe',
            ani.width = 1800, ani.height = 750, interval = 0.7, ani.dev = "png",
            ani.type = "png", loop = 0)
im.convert("*.png", output = "LG-i35_graph_fixed_ts.gif")
