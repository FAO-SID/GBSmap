##### join soil data with covariates #####################################################
library(mapview)
library(sf)
library(tidyverse)
library(raster)
library(snowfall)

rm(list = ls())

# options for raster package (define working directory)
rasterOptions(tmpdir="/run/media/marcos/_home/marcos/tmp/", # set a temporal directory
              progress= "text",
              timer=TRUE,
              overwrite = T,
              chunksize=2e8,
              maxmemory=1e8)

### Load point data ######################################################################
# read the data created in the first script
s <- read_csv("input_data/bs.csv") 

# we convert all BS (bs1 and bs2) into 1
s <- s %>% 
  mutate(z = if_else(z>=1, 1, 0))
# and then we convert the table into a spatial object (with sf package)
s <- s %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  as_Spatial()

mapview(s["z"], cex = 2,lwd = 0, color = c("black", "green"))


# load area of interest if needed
# aoi <- read_sf("input_data/aoi.shp")

### load covariates ######################################################################
# load rasters
files <- list.files(path = "input_data/covs/", pattern = "tif", full.names = TRUE)
r <- stack(files)

plot(r)

# Now, we have to extract the values of the rasters at each point location. This process 
# can be very heavy computationally, so we do it in parallel processing 
# from https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points

# Extract values to a data frame - multicore approach
# First, convert raster stack to list of single raster layers
r.list <- unstack(r)
names(r.list) <- names(r)

# Now, create a R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sp)
# Run parallelized 'extract' function and stop cluster
e.df <- sfSapply(r.list, extract, y=s, method='simple')
sfStop()
e.df

colSums(is.na(e.df))[colSums(is.na(e.df))>1]
# Fix resulting data frame
s <- st_as_sf(s)
DF <- st_bind_cols(s, e.df) 
DF <- cbind(DF,st_coordinates(DF))
DF <- DF %>% st_drop_geometry()
s <- DF %>% na.omit()

write_csv(s, "input_data/soil_&_covariates.csv")



