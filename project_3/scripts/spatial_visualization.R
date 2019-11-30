# Alfredo Rojas
# BIOS 611: Project 3
# 11/27/2019

library(sf)
library(tidyverse)
require(rgdal)
library(tmap)

#########
# From: 
# https://gis.stackexchange.com/questions/151613/reading-feature-class-in-file-geodatabase-using-r
# 
# How to read an ESRI file geodatabase to sf in R:

# The input file geodatabase
fgdb = "ACS_2017_5YR_TRACT_37_NORTH_CAROLINA.gdb"

# List all feature classes in a file geodatabase
subset(st_drivers(), grepl("GDB", name))
fc_list = st_layers(fgdb)
print(fc_list) # print list of layers

nc_shp = st_read(dsn = fgdb, layer = "ACS_2017_5YR_TRACT_37_NORTH_CAROLINA")
race_data = st_read(dsn = fgdb, layer = "X02_RACE")
count_data = st_read(dsn = fgdb, layer = "X00_COUNTS")
meta_data = st_read(dsn = fgdb, layer = "TRACT_METADATA_2017")

############

# get long names and corresponding codes from meta_data for race_data
row_names = colnames(race_data)
long_names = data.frame()
for(i in row_names){
  temp = meta_data[meta_data$Short_Name == i, ]
  long_names = rbind(long_names, temp)
}

# select variables of interest: White and Afr. American populations
race_data2 <- race_data %>%
  select(GEOID, RACE_TOTAL = "B02001e1", RACE_TOTAL_ME = "B02001m1",
         WHITE_ALONE = "B02001e2", WHITE_ALONE_ME = "B02001m2",
         BLACK_AFRAM = "B02001e3", BLACK_AFRAM_ME = "B02001m3",
         WHITE_COMBO = "B02008e1", WHITE_COMBO_ME = "B02008m1",
         BLACK_COMBO = "B02009e1", BLACK_COMBO_ME = "B02009m1") %>%
  mutate(WHITE_PRCNT = WHITE_ALONE / RACE_TOTAL) %>%
  mutate(BLACK_PRCNT = BLACK_AFRAM / RACE_TOTAL) 

# helps avoid warning message and missing values during left_join
nc_shp$GEOID = as.character(nc_shp$GEOID)
race_data2$GEOID = as.character(nc_shp$GEOID)
nc_race = left_join(nc_shp, race_data2, by = "GEOID")

### Part 2 ###
# Use NC Shapefil from GADM for scale
usa_shp = readRDS("gadm36_USA_2_sf.rds")

durham = usa_shp %>% 
  filter(NAME_1 == "North Carolina", NAME_2 == "Durham")

plot(st_geometry(durham))

st_crs(durham) # EPSG: 4326
nc_race_proj = st_transform(nc_race, crs = 4326)
### ###

### Part 3 ###
# Visualize

# Crop map to Durham's extent (gives weird warning)
nc_race_crop = st_crop(nc_race_proj, st_bbox(durham))

# White percentage
map_white = tm_shape(nc_race_crop) +
  tm_fill(col = "WHITE_PRCNT",
              palette = "Purples",
              contrast = c(0, 0.7)) +
  tm_layout(main.title = "Durham White Percentage \nby Census Tract, 2017",
            main.title.size = 1,
            frame = FALSE,
            legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.7) +
  tm_borders() +
  tm_shape(durham) +
  tm_borders(col = "red") +
  tm_shape(nc_race_crop[nc_race_crop$TRACTCE == "002200", ]) +
  tm_borders(col = "green") 


# Black percentage
map_black = tm_shape(nc_race_crop) +
  tm_fill(col = "BLACK_PRCNT",
          palette = "Blues",
          contrast = c(0, 0.7)) +
  tm_borders() +
  tm_layout(main.title = "Durham Black Percentage \nby Census Tract, 2017",
            main.title.size = 1,
            frame = FALSE,
            legend.position = c("right", "bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.7) +
  tm_shape(durham) +
  tm_borders(col = "red") +
  tm_shape(nc_race_crop[nc_race_crop$TRACTCE == "002200", ]) +
  tm_borders(col = "green")
  
reference = tm_shape(nc_race_crop[nc_race_crop$TRACTCE == "002200", ]) +
  tm_layout(title = "Contains UMD", title.size = 1.5, frame = FALSE, bg.color = NA) +
  tm_borders(col = "green")


# Now, make NC state map with Durham as a reference
nc_state = usa_shp %>%
  filter(NAME_1 == "North Carolina")

nc_map = tm_shape(nc_state) +
  tm_polygons() +
  tm_shape(durham) +
  tm_borders(col = "red") +
  tm_layout(main.title = "North Carolina (Durham outline in red)",
            main.title.size = 1)

png("map.png")
tmap_arrange(map_white, map_black, outer.margins = 0.1)
print(reference, vp = grid::viewport(0.5, 0.1, width = 0.17, height = 0.17))
dev.off()




