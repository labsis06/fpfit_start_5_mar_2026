#!/usr/bin/env bash
set -euo pipefail

# --- Area Campi Flegrei
R="14.03/14.23/40.77/40.88"

# --- 1) Costa da GSHHG -> GMT ASCII
gmt coast -R$R -Df -M -G -W0.25p > coast_land.gmt   # dump poligoni terra (GSHHG)  [13](https://docs.generic-mapping-tools.org/latest/coast)

# --- 2) Laghi OSM: presuppone che tu abbia esportato laghi_osm.geojson da Overpass Turbo
#     (se non disponibile, vedi commento più sotto)
ogr2ogr -f "GMT" laghi_osm.gmt laghi_osm.geojson     # GeoJSON -> GMT ASCII         [6](https://overpass-turbo.eu/)[7](https://dges.carleton.ca/CUOSGwiki/index.php/Extracting_OpenStreetMap_data_using_Overpass_Turbo_and_managing_the_data_in_QGIS)

# --- 3) (Opzione alternativa se NON usi OSM): estrai laghi da GSHHG livello 2
# gmt coast -R$R -Df -M -C -A0/2/2 > laghi_gshhg.gmt   # dump laghi (livello 2)       [13](https://docs.generic-mapping-tools.org/latest/coast)

# --- 4) Caldera: digitalizza con QGIS da mappa INGV e salva "caldera_cf.gmt"
#     (se mi autorizzi, preparo io il file accurato)                                    [10](https://www.ingv.it/campi-flegrei)

# --- 5) Crateri (facoltativo): "crateri_cf.gmt" con punti (Solfatara/Astroni/Monte Nuovo)
#     coords di riferimento su GVP                                                     [12](https://volcano.si.edu/volcano.cfm?vn=211010)

# --- 6) Clip (opzionale) e unione
gmt spatial coast_land.gmt -R$R > coast_land_clip.gmt
gmt spatial laghi_osm.gmt   -R$R > laghi_osm_clip.gmt

cat coast_land_clip.gmt \
    laghi_osm_clip.gmt  \
    caldera_cf.gmt      \
    crateri_cf.gmt      \
    > campi_flegrei_ALL.gmt

# --- 7) GMT ASCII -> BF2
gmt convert campi_flegrei_ALL.gmt -Fb > campi_flegrei_ALL.bf2           #              [14](https://docs.generic-mapping-tools.org/dev/gmtconvert.html)

echo "OK: creato campi_flegrei_ALL.bf2"
