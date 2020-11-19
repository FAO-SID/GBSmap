# join soil data with covariates

library(sf)
library(tidyverse)
library(raster)
library(snowfall)

rm(list = ls())

# options for raster package (define working directory)
rasterOptions(tmpdir="/run/media/marcos/_home/marcos/tmp/",
              progress= "text",
              timer=TRUE,
              overwrite = T,
              chunksize=2e8,
              maxmemory=1e8)

s <- read_csv("input_data/bs.csv") 

# we convert all BS (bs1 and bs2) to 1
s <- s %>% 
  mutate(z = if_else(z>=1, 1, 0))

s <- s %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  as_Spatial()

plot(s["z"])

class(s)

# load area of interest
aoi <- read_sf("input_data/aoi.shp")

# load rasters
files <- list.files(path = "input_data/covs/", pattern = "tif", full.names = TRUE)
r <- stack()

for (i in seq_along(files)) {
  x <- raster(files[i])
  x <- crop(x, y = aoi)
  x <- mask(x, aoi)
  r <- stack(x, r)
  print(i)
}
r
# extract in parallel 
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
class(e.df)


# Fix resulting data frame
s <- st_as_sf(s)
DF <- st_bind_cols(s, e.df) 
DF <- DF %>% st_drop_geometry()
s <- read_csv("input_data/bs.csv")
s <- dplyr::left_join(s,DF) %>% na.omit()

write_csv(s, "input_data/soil_&_covariates.csv")



