#! /bin/bash

nome=$1

cp $nome file.loc.h71

echo "ttl   1 'none'
hyp 'file.loc.h71'
out 'fpfit.out'
pol 'fpfit.pol'
sum 'fpfit.fps'
for   1
mag      0.    
obs  6
dis  0.1000E+06
res   100.0    
ain      0.      180.0    
amp   0
bst   0
fin   1
rep   1
cmp   0
hdr  0.1000E-01 0.2000E-01 0.5000E-01 0.1000    
mcr  0.2000      1.000      1.000      1.000    
dir    0.00      360.0      20.00      5.000    
dip    0.00      90.00      20.00      5.000    
rak  -180.0      180.0      20.00      10.00 " >   h71.inp



/home/anna/Programmi/fpfit/fpfit << EOD
@h71.inp
fps
sto
EOD

/home/anna/Programmi/fpfit/fpplot << EOD
fpfit.pol
y
y
y
a
0
2
stop
EOD
rm h71.inp

ps2pdf LaserWriter.ps $nome.pdf
rm LaserWriter.ps


TOPO=~/Dati/NLLoc/topotot.grd
gmt psbasemap -R14.03/14.23/40.77/40.88 -Ba2m::WsNe  -Jm60 -Y15 -X2 -P -K > mechanism.ps
gmt grdcontour $TOPO -C'cont' -A -R -Ba0 -Jm -K -O >> mechanism.ps
gmt grdcontour $TOPO -C'cont0' -R -Ba0 -W2 -Jm -K -O >> mechanism.ps
sed -n '2p' file.loc.h71 | awk '{print $1 " " $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $8}' > text.txt
#gmt pstext text.txt -R -Jm0.1i -F+f0.01p,Helvetica,-=5.5p,red -B5 >> mechanism.ps

#/home/anna/Programmi/fpfit/fpfit2gmt < fpfit.fps | awk '{print -$2, $1, 0., $4, $5, $6,5., 0., 0.}' |sed -n 1p| gmt psmeca -R14.03/14.23/40.77/40.88 -Jm60 -Ba0 -Sa1.0 -K -O >> mechanism.ps
/home/anna/Programmi/fpfit/fpfit2gmt < fpfit.fps | awk '{print -$2, $1, 0., $4, $5, $6,5., 0., 0.}' |sed -n 1p| gmt psmeca -R14.03/14.23/40.77/40.88 -Jm60 -Ba0 -Sa1.0 -K -O >> mechanism.ps
/home/anna/Programmi/fpfit/fpfit2gmt < fpfit.fps | awk '{print -$2+0.02, $1, 0., $4, $5, $6,5., 0., 0.}' |sed -n 2p| gmt psmeca -R14.03/14.23/40.77/40.88 -Jm60 -Ba0 -Sa1.0 -K -O >> mechanism.ps
/home/anna/Programmi/fpfit/fpfit2gmt < fpfit.fps | awk '{print -$2+0.04, $1, 0., $4, $5, $6,5., 0., 0.}' |sed -n 3p| gmt psmeca -R14.03/14.23/40.77/40.88 -Jm60 -Ba0 -Sa1.0 -K -O >> mechanism.ps


#| gmt psxy -Skmeca/5  -R14.03/14.23/40.77/40.88 -Ba0  -Jm60 -K -O > mechanism.ps 
#ps2pdf mechanism.ps mechanism.pdf
#	write (6, 200) lat, lon, depth, strike, dip, rake, mag, '0. 0.'
/home/anna/Programmi/fpfit/fpfit2gmt < fpfit.fps | awk '{print -$2, $1, 0., $4, $5, $6}'
ps2pdf mechanism.ps $nome.gmt.pdf
rm mechanism.ps
rm file.loc.h71
echo $nome
mv fpfit.out $nome.out
mv fpfit.pol $nome.pol
mv fpfit.fps $nome.fps

