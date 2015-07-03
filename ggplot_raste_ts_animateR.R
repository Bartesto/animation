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

# Read in 