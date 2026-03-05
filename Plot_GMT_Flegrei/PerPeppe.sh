#!/bin/bash

# --- Locale “pulita” per evitare simboli UTF-8 strani
export LC_ALL=C
export LANG=C
# --- Assi: gradi decimali, senza simboli ° ' "
gmt set FORMAT_GEO_MAP=ddd.xx
gmt set MAP_DEGREE_SYMBOL=none
gmt set PS_CHAR_ENCODING=Standard
gmt set FONT_ANNOT_PRIMARY=10p,Helvetica
gmt set FONT_LABEL=12p,Helvetica
#
#
#
# --- imposta estremi in longitudine e latitudine dell'area da plottare, così da settarli una volta per tutte
#
lo0="14.03"
lo1="14.23"
la0="40.77"
la1="40.88"
#
# ---File topografia Tinitaly 1.1 (10m) o SRTM01s (1s) (di default viene scaricato SRTM01s dalle repo web, cambiare il commento se necessario cambiare)
#
topo="@srtm_relief_01s"
#topo="./Data/tinitaly11.hdf4=gd:hdf4"
#
# caratteristiche linee di livello
#
gmt set PS_LINE_CAP round
gmt set PS_LINE_JOIN round
#
# --- inizia la procedura. Creerà alla fine un file mechanism.pdf
#
# nome di base del file pdf (pdfnamebase="mechanism" genera il file mechanism.pdf)
#
pdfnamebase="mechanism"
#
gmt begin $pdfnamebase #mechanism
    gmt psbasemap -R$lo0/$lo1/$la0/$la1 -Jm60 -Bxa0.04f0.02 -Bya0.02f0.01 -BWSne -Ln0.9/0.075+w2k+at+l"km"+f #-Y15 -X2 jBR
#    gmt makecpt -C200 -T-10000,10000 -N ; gmt grdimage $topo -R$lo0/$lo1/$la0/$la1 -Jm60 -I+d    # decommentare per aggiungere shaded relief
#    gmt coast -R$lo0/$lo1/$la0/$la1 -Jm60 -Df -Wthick,black                                      # colinee di costa a full resolution [deve essere installato sul sistema GSSH full] (default usa gli shapefile locali)
    gmt grdcontour $topo -C50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000,1050,1100,1150,1200,1250,1300  -A -R$lo0/$lo1/$la0/$la1 -Ba0 -Jm60 -Wthinnest,black
    gmt plot ./Data/land_polygons_osm_campania.bf2 -bi2f -Wthick,black -R$lo0/$lo1/$la0/$la1 -Jm60
    gmt plot ./Data/LagoPatria.gmt  -Wthick,black -R$lo0/$lo1/$la0/$la1 -Jm60
    gmt plot ./Data/LagoFusaro.gmt  -Wthick,black -R$lo0/$lo1/$la0/$la1 -Jm60
    gmt plot ./Data/LagoMiseno.gmt  -Wthick,black -R$lo0/$lo1/$la0/$la1 -Jm60
    gmt plot ./Data/LagoLucrino.gmt  -Wthick,black -R$lo0/$lo1/$la0/$la1 -Jm60
    gmt plot ./Data/LagoAverno.gmt  -Wthick,black -R$lo0/$lo1/$la0/$la1 -Jm60

    # --- prepara i dati meccanismi in un file per psmeca (NO pipe, perché conda run non passa stdin)
    MECA_DAT="meca.dat"
    MECA_PSMECA="meca_psmeca.txt"
    #
    # crea un file di meccanismi focali fittizi, da commentare quando vengono assorbiti dallo script
    mag="5.0" #magnitudo dell'evento, influisce sulle dimensioni del simbolo
    echo "14.10 40.80 2.0 30.0 60.0 270.0 $mag" >  meca_psmeca.txt
    echo "14.13 40.84 3.0 700.0 80.0 20.0 $mag" >>  meca_psmeca.txt
    #
    gmt psmeca $MECA_PSMECA -R$lo0/$lo1/$la0/$la1 -Jm60 -Ba0 -Sa1.0
gmt end
#
# --- per produrre in aggiunta un file png a 600dpi dal precedente pdf, decommentare la seguente
#
#pdftoppm -png -r 600 -cropbox -singlefile  $pdfnamebase.pdf $pdfnamebase
