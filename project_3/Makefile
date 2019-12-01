scripts/ACS_2017_5YR_TRACT_37.gdb:
        cd scripts/ && \
        curl -o ACS_2017_5YR_TRACT_37.gdb.zip https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_TRACT_37.gdb.zip && \
        unzip ACS_2017_5YR_TRACT_37.gdb.zip     

map.png: scripts/ACS_2017_5YR_TRACT_37.gdb
        cd scripts/ && \
        Rscript spatial_visualization.R
~                                        
