## Attempts to animate timeseries RGB's

##After creating the band combo algorithm in ERMapper, run a batch process to create
##.tif files. These will be converted to png prior to animation.

library(sp)
library(raster)
library(rgdal)
library(maptools)
library(rasterVis)
library(ggplot2)
library(png)
library(grid)
library(dismo)
library(animation)


#a <- raster("l8ut11174m_300515_USG_utm51pre_543_display.tif")

#read .tif as stack to get all 3 layers (RGB)
b <- stack("l8ut11174m_300515_USG_utm51pre_543_display.tif")

#obtain premade extent to crop image if necessary

e <- extent(readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                    "walyarta_display_extent"))

#obtain list of .tif files for loop
files <- list.files(pattern = ".tif")

#crop and plot
rc <- crop(b, e)
plotRGB(rc, r=1, g=2, b=3)

outpath <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation\\out"
dir.create(outpath)

outfiles <- paste0(outpath, files)


for(i in 1:length(files)) {
        
        r <-stack(files[i])
        rc <- crop(r, e)
        rc <- writeRaster(rc, outfiles[i])
}

d <- stack("outl8ut11174m_070215_USG_utm51pre_543_display.tif")
plotRGB(f, r=1, g=2, b=3)
f <- brick(d)

files2 <- list.files(pattern="out.*.tif$")
#for name of png
files3 <- files2
extension(files3) <- '.png'
png.name <- paste0(as.character(as.Date(substring(files3, 15, 20), "%d%m%y")), ".png")

for(i in 1:length(files2)) {
        fname <- files2[i]
        r.i <- brick(files2[i])
        png(filename = png.name[i])
        plotRGB(r.i, r=1, g=2, b=3)
        dev.off()
}
        
file.rename(list.files(pattern=".png"), paste0("walyarta_", 1:length(png.name), ".png"))


##This works!!
ani.options(convert = 'C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe',
            ani.width = 700, ani.height = 800, interval = 1, ani.dev = "png",
            ani.type = "png")
im.convert("wal*.png", output = "wal-animation.gif")


imdir <- "W:\\usgs\\111074\\20150615"
setwd(imdir)
setwd("Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation")

f <- list.files(pattern = "*pre.ers")
b1 <- brick(f)
b2 <- crop(b1, e)
plotRGB(b2, 3,2,1, stretch = 'lin')
plotRGB(b2, 5, 4, 3, stretch = 'lin')
