download-data: script.sh
	chmod 755 script.sh &&
	./script.sh

map.png: download-data
	Rscript spatial_vizualization.R
