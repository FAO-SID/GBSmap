library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())

## load soil profile data
dat <- read_csv("input_data/soil_profiles.csv")
# Numbers
paste("number of soil horizons", length(unique(dat$idh)))
paste("number of soil profiles" ,length(unique(dat$idp)))

## Explore the data
# histograms 
dat %>% 
  dplyr::select(top, bot, cec, oc, w_value, w_chroma, bsat) %>% 
  pivot_longer(top:bsat) %>% 
  na.omit() %>% 
  ggplot(aes(x = value, color = name)) + 
  geom_histogram() + 
  facet_wrap(name~., scales = "free")

# soil profile distribution          
dat %>% 
  dplyr::select(x, y) %>% 
  unique() %>% 
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mapview(cex = 2, lwd = 0)

# Set up conditions for black soils
(x <- dat %>%
  mutate(bs_oc = if_else(condition = oc >= 1.2, true = 1, false = 0),
         bs_w_chroma = if_else(condition = w_chroma <= 3, true = 1, false = 0),
         bs_w_value = if_else(condition = w_value <= 3, true = 1, false = 0),
         bs_bot = if_else(condition = bot >= 25, true = 1, false = 0),
         bs_bsat = if_else(condition = bsat >= 50, true = 1, false = 0),
         bs_cec = if_else(condition = cec >= 25, true = 1, false = 0)))

# show points in a map
x %>% 
  dplyr::select(x, y, bs_oc) %>% 
  unique() %>% 
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mapview(cex = 3, lwd = 0)

# select useful columns
(x <- x %>% dplyr::select(idh:bot, bs_oc:bs_cec))


# Classify the profiles as BS or no-BS
# 1st. We create two columns bs1 and bs2 to classify horizons
x <- x %>% 
  mutate(bs1 = ifelse(bs_oc == 1 & bs_w_chroma == 1 & bs_w_value == 1 & 
                        bs_cec == 1 & bs_bsat == 1,
                      yes = 1, 
                      no = 0),
         bs2 = ifelse(bs_oc == 1 & bs_w_chroma == 1 & bs_w_value == 1,
                      yes = 1,
                      no = 0)) 
# then, a column with three values are created: 0 for non-BS, 1 for 1st cat. BS, and
# 2 for 2nd cat. BS
x <- x %>% 
  group_by(idp) %>% 
  summarise(x = first(x),
            y = first(y),
            n = n(),
            bs1 = sum(bs1)/n,
            bs2 = sum(bs2)/n,
            bottom = sum(bs_bot))

x <- x %>% 
  transmute(idp = idp,
            x = x,
            y = y,
            z = ifelse(bs1 == 1 & bottom == 1, 1, 
                       ifelse(bs2 == 1 & bottom == 1, 2, 0)))

# Finally we visualise the data in a map and save it
x %>% 
  dplyr::select(x, y, z) %>% 
  na.omit() %>% 
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  mapview(cex = 3, lwd = 0)

# how many samples the classes have?
table(x$z) # note that 1 and 2 are both BS

write_csv(x, "input_data/bs.csv")
