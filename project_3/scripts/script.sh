#!/bin/bash

curl -o ACS_2017_5YR_TRACT_37.gdb.zip https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_TRACT_37.gdb.zip

unzip ACS_2017_5YR_TRACT_37.gdb.zip

# get USA shape files for NC
curl -o gadm36_USA_2_sf.rds https://data.biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_USA_2_sf.rds
