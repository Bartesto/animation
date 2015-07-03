##Code to zoom to area, create .png and create animation. This code
##performs linear stretch PRIOR to zooming.

rm(list=ls())

library(raster)
library(rgdal)
library(maptools)
library(animation)
library(tools)


#Working location
here <- "Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\Working\\animation"
#Image folders location
imdir <- "W:\\usgs\\110078"

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


setwd(here)
#Obtain extents from are of interest shape file
e <- extent(readOGR(dsn = "Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\Working\\animation", 
                    "aoi_LG_extent_polygons"))
enclosure <- readOGR(dsn = "Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\Working\\animation", 
                     "Enclosure")
LG <- readOGR(dsn = "Z:\\DEC\\LornaGlenVegetationChange_15122C03\\DATA\\Working\\animation", 
              "LornaGlen")
#Set options



red=5
green=4
blue=3
combo="543"


#timer
start = Sys.time()
for(i in 1:length(folds.no.7)){
        setwd(paste0(imdir, "\\", folds.no.7[i]))
        f <- list.files(pattern = '*pre.ers')
        date <- as.Date(substring(f, 12, 17),"%d%m%y")
        pname <- paste0(date, "-", combo, ".png")
        fname <- paste0(here, "\\", "allstretch", pname)
        plab <- format(date, "%b %Y")
        br <- raster(f, band = red)
        br <- stretch(br)
        bg <- raster(f, band = green)
        bg <- stretch(bg)
        bb <- raster(f, band = blue)
        bb <- stretch(bb)
        b <- brick(br, bg, bb)
        bsc <- crop(b, e)
        png(filename = fname, width = 842, height = 870)
        par(mar=c(8,6,4,2)+0.1)
        plotRGB(bsc, 1, 2, 3, axes = TRUE, 
                main = paste0(plab, " ", "Lorna Glen"))
        plot(enclosure, add = TRUE, lwd = 2, border = "green")
        plot(LG, add= TRUE, lwd = 2, border = "yellow")
        dev.off()
        
}
end = Sys.time() - start
end
#Back to working directory
setwd(here)

#Rename png files to leading zero numbers to order correctly
png.list <- list.files(pattern = '*543.png')
nname <- sprintf("%.4d.png", seq(png.list))
file.rename(png.list, nname)

#Create .gif animation
ani.options(convert = 'C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe',
            ani.width = 1800, ani.height = 750, interval = 0.7, ani.dev = "png",
            ani.type = "png", loop = 0)
im.convert("*.png", output = "wal-animation-543-Jan88-Jun15.gif")

end = Sys.time() - start
end
