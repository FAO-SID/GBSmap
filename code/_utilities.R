library(sf)
library(raster)
library(tidyverse)
library(mapview)

rm(list = ls())

pts =as.data.frame(matrix(c(-58.91,-62,-62,-58.91,-58.91,
                            -34.67,-34.67,-32,-32,-34.67),,2))
aoi <- pts %>%
  st_as_sf(coords = c("V1", "V2"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
mapview(aoi)
st_write(aoi, "input_data/aoi.shp")

# CARGAR DATOS EDITADOS POR DARÍO
#
sat <- read_delim("input_data/datos_saturacion_revisados_3_nov_2020.csv", delim = "\t")

sat %>% pivot_longer(cec:na) %>%
  ggplot(aes(y = value)) + facet_wrap("name", scales = "free") + geom_boxplot()


sat <- sat %>% group_by(idh) %>%
  mutate(bsat2 = if_else(condition = !is.na(cec) & !is.na(ca) &
                           !is.na(mg) & !is.na(k) & !is.na(na),
                         true = ((ca+mg+k+na)/cec)*100,
                         false = bsat)) %>%
  mutate(bsat = if_else(condition = bsat > 5000,
                         true = bsat2,
                         false = bsat)) %>%
  mutate(bsat = if_else(condition = is.na(ca) &
                           is.na(mg) & !is.na(k) & !is.na(na),
                         true = 100,
                         false = bsat)) %>%
  mutate(bsat = if_else(condition = is.na(bsat) & !is.na(bsat2) & bsat2 < 125,
                         true = bsat2,
                         false = bsat))
sat <- sat %>%
  transmute(bsat = if_else(bsat>100, 100, bsat)) %>%
  filter(bsat>=1)

library(tidyverse)
dat <- read_csv("input_data/datos_revisados_26_sep_20.csv") %>%
  dplyr::select(-bsum, -bsat) %>%
  mutate(cec = cec/1000) %>%
  left_join(sat) %>%
  unique() %>%
  dplyr::select(-clay, -ph, -cols, -colh)

dat <- dat %>%
  rename(w_chroma=chroma_humedo, w_hue=h_humedo, w_value=value_humedo,
         d_chroma=chroma_seco,   d_hue=h_seco,   d_value=value_seco) %>%
  filter(x>=-62 & x<=-58.91 & y>=-34.67 & y<=-32) %>%
  filter(top<25)
dat$cec[dat$cec>150 & !is.na(dat$cec)] <- 21.42
write_csv(dat, "input_data/soil_profiles.csv")

library(fs)
library(raster)
library(tidyverse)
# load area of interest
aoi <- read_sf("input_data/aoi.shp")

# load rasters
files <- list.files(path = "input_data/covs/", 
                    pattern = "tif$", full.names = TRUE, recursive = TRUE)
names <- list.files(path = "input_data/covs/", 
                         pattern = "tif$", full.names = FALSE, recursive = TRUE) %>% 
  tolower()  %>% 
  str_remove(".tif")
  
# names <- paste0(str_sub(names, 1,3),
#                 "_",
#                 abbreviate(str_replace_all(str_remove(str_sub(names, 5), "sdat"),
#                                            "[^[:alnum:]]", " "),
#                            minlength = 8, strict = TRUE)) %>% 
#   str_replace(" ","_")

r <- stack()

for (i in seq_along(files)) {
  x <- raster(files[i])
  x <- crop(x, y = aoi)
  x <- mask(x, aoi)
  r <- stack(x, r)
  print(i)
}
plot(r)
writeRaster(r, filename = paste0("input_data/covs/",names, ".tif"), bylayer = TRUE)
