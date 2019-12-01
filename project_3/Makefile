scripts/ACS_2017_5YR_TRACT_37.gdb:
        cd scripts/ && \
        curl -o ACS_2017_5YR_TRACT_37.gdb.zip https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_TRACT_37.gdb.zip && \
        unzip ACS_2017_5YR_TRACT_37.gdb.zip     

build-docker: scripts/ACS_2017_5YR_TRACT_37.gdb
        docker build -t ajr . && touch $@

map.png: Dockerfile
        cd scripts/ && \
        docker run -it --rm \
        -v $(shell pwd):/workdir \
        -w /workdir rocker/tidyverse \
        Rscript spatial_visualization.R                                     
