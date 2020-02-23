## Data
The data comes from Urban Ministries of Durham (UMD). It is about patient demgoraphics, income, and health insurance.This project also relies on data from the U.S. Census Bureau. 

This project focues on two kinds of data: (1) Client-based data on demographics, specifically race from UMD; and (2) spatial data, the percentage of black and white populations per census tract from U.S. census data.

## Concept
This project will apply concepts and technologies learned in BIOS 611 to analyze demographic data from Urban Ministries of Durham data. This analysis will rely on Python to clean and wrangle data, R to visualize data, Docker to contain requisite dependencies, and Make for reproducitility.  

## Methods
This project analyzes demographic data provided by UMD to describe the demographic makeup of UMD's clients. Additionally, external data will be used to explore demographic information across Durham county using spatial data. The analysis includes descriptive statistics to visualize data. Currently, no statistical analyses have been performed.

## Conclusion
Clients at UMD are primarily African-American/Black, with White the second largest population. Spatially, White populations predominate across Durham county by percentage and occupy central areas of Durham county. Black populations by percentage are fewer and exist in surrounding areas.

## Note for Peer Feedback
The Makefile is contained in the project_3 directory. The makefile was used to produce a visualizaiton of the spatial data using an Rscript and a Docker container from rocker/geospatial. 

The final make command is: `make scripts/map.png` (NOTE: Still undergoing some debugging to run correctly.)
