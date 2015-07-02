

wd= "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\animation"
imdir= "W:\\usgs\\111074"
ext.shp= "walyarta_display_extent"#no ext
red=5
green=4
blue=3
combo="543"
gifname= "wal-animation-543-Jan88-Jun15.gif"

animateR <- function(wd, imdir, ext.shp, red, green, blue, combo, gifname){
        library(raster)
        library(rgdal)
        library(maptools)
        library(animation)
        library(tools)
        
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
        #allfiles <- list.files(recursive = TRUE)
        allfiles <- c("20150530\\l8ut11174m_300515_USG_utm51pre.ers", 
                      "20150607\\l7ut11174m_070615_USG_utm51pre.ers", 
                      "20150615\\l8ut11174m_150615_USG_utm51pre.ers")
        result <- allfiles[grep("*pre.ers", allfiles)]
        
        
        #get dates from .pre.ers not folders (discrepencies due to leap years)
        fdate <- as.Date(substr(result, 21, 26), "%d%m%y")
        
        #get sensor
        sensor <- substr(result, 10, 11)
        
        #make df for to help with index
        ind.df <- data.frame(date = fdate, sens = sensor, file = result,
                             stringsAsFactors = FALSE)
        #index to find Landsat 7's post scan line error
        nL7.index <- (ind.df$sens == "l7") & (ind.df$date > as.Date("2003-05-30"))
        
        
        ##INDEXED data from here
        #grab folder names only
        #folds <- list.dirs(pattern = "[[:digit:]]")
        folds <- c("20150530", "20150607", "20150615")
        folds.no.7 <- folds[!nL7.index]#remove L7's
        #create index for L1G folders - BEWARE folder date/image date discrepancy for
        #leap years (e.g. 2000 folders 'day'will be out) 
        #L1G.index <- folds.no.7 == c("19950928", "19951201", "20001121")
        #folds.no.7 <- folds.no.7[!L1G.index]
        
        setwd(wd)
        #Obtain extents from are of interest shape file
        e <- extent(readOGR(dsn = wd, ext.shp))
        
        for(i in 1:length(folds.no.7)){
                setwd(paste0(imdir, "\\", folds.no.7[i]))
                f <- list.files(pattern = '*pre.ers')
                date <- as.Date(substring(f, 12, 17),"%d%m%y")
                pname <- paste0(date, "-", combo, ".png")
                fname <- paste0(wd, "\\", pname)
                plab <- format(date, "%b %Y")
                b <- crop(brick(f), e)
                png(filename = fname, width = 1800, height = 750)
                par(mar=c(8,6,4,2)+0.1)
                plotRGB(b, red, green, blue, stretch = 'lin', axes = TRUE, 
                        main = plab)
                dev.off()
                
        }
        
        setwd(wd)
        
        #Rename png files to leading zero numbers to order correctly
        png.list <- list.files(pattern = '*543.png')
        nname <- sprintf("%.4d.png", seq(png.list))
        file.rename(png.list, nname)
        
        #Create .gif animation
        ani.options(convert = 'C:/Program Files/ImageMagick-6.9.1-Q16/convert.exe',
                    ani.width = 1800, ani.height = 750, interval = 0.7, ani.dev = "png",
                    ani.type = "png", loop = 0)
        im.convert("*.png", output = "wal-animation-543-Jan88-Jun15.gif")
 

}

x=folds.no.7[1]

testd <- c("20150530", "20150607", "20150615")
testd <- paste0(imdir, "\\", testd)

testfn <- function(x){
        #setwd(paste0(imdir, "\\", x))
        f <- list.files(path = x, pattern = '*pre.ers')
        date <- as.Date(substring(f, 12, 17),"%d%m%y")
        pname <- paste0(date, "-", combo, ".png")
        fname <- paste0(wd, "\\", pname)
        plab <- format(date, "%b %Y")
        b <- crop(brick(f), e)
        png(filename = fname, width = 1800, height = 750)
        par(mar=c(8,6,4,2)+0.1)
        plotRGB(b, red, green, blue, stretch = 'lin', axes = TRUE, 
                main = plab)
        dev.off()
        
}
sapply(testd, testfn)

