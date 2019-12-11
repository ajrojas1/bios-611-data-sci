scripts/gadm36_USA_2_sf.rds:
	cd scripts/ && \
	curl -o gadm36_USA_2_sf.rds  https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_USA_2_sf.rds

scripts/ACS_2017_5YR_TRACT_37.gdb.zip: scripts/gadm36_USA_2_sf.rds
	cd scripts/ &&\
	curl -o ACS_2017_5YR_TRACT_37.gdb.zip https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_TRACT_37.gdb.zip && \
	unzip ACS_2017_5YR_TRACT_37.gdb.zip

scripts/build-docker: scripts/ACS_2017_5YR_TRACT_37.gdb.zip
	cd scripts/ &&\
	docker build -t ajr . && touch build-docker

scripts/map.png: scripts/build-docker
	docker run -v $(shell pwd)/scripts:/workdir -w=/workdir --rm -it rocker/geospatial Rscript spatial_visualization.R                                    
