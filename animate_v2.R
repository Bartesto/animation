library(raster)
library(rgdal)
library(maptools)
library(animation)
library(tools)

rm(list=ls())

##USE FOR TESTING

#Working location
here <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation"
#Image folders location
imdir <- "W:\\usgs\\111074"

setwd(here)

#hard coded folder names
folders <- c("20140612", "20140628", "20140714", "20140730", "20140815",
             "20140831", "20140916", "20141002", "20141018", "20141103",
             "20141119", "20141205", "20141221", "20150122", "20150207",
             "20150223", "20150311", "20150327", "20150412", "20150514",
             "20150615", "20150530")

#folders <- c("20140612", "20140628")

#Obtain extents from are of interest shape file
e <- extent(readOGR(dsn = "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation", 
                    "walyarta_display_extent"))

#Set options

# red=3
# green=2
# blue=1
# combo="-321"

red=5
green=4
blue=3
combo="543"

for(i in 1:length(folders)){
        setwd(paste0(imdir, "\\", folders[i]))
        f <- list.files(pattern = '*pre.ers')
        pname <- paste0(as.character(as.Date(substring(f, 12, 17), 
                                             "%d%m%y")), "-", combo, ".png")
        date <- as.Date(substring(f, 12, 17),"%d%m%y")
        fname <- paste0(here, "\\", pname)
        plab <- format(date, "%b %Y")
        b <- crop(brick(f), e)
        png(filename = fname, width = 1800, height = 600)
        par(mar=c(8,6,4,2)+0.1)
#         plotRGB(b, red, green, blue, stretch = 'lin', axes = TRUE, 
#                 main = file_path_sans_ext(pname))
        plotRGB(b, red, green, blue, stretch = 'lin', axes = TRUE, 
                main = plab)
        dev.off()
        
}



#Back to working directory
setwd(here)

#Rename png files to leading zero numbers to order correctly
png.list <- list.files(pattern = '*543.png')
nname <- sprintf("%.4d.png", seq(png.list))
file.rename(png.list, nname)

#Create .gif animation
ani.options(convert = 'C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe',
            ani.width = 1800, ani.height = 600, interval = 1.5, ani.dev = "png",
            ani.type = "png", loop = 0)
im.convert("*.png", output = "test.gif")


