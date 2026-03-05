\
#!/bin/bash
set -e
set -u
set -o pipefail

# ============================================================
# runfpfit1.sh (mechanism-style map)
# - Esegue fpfit su <evento>.grid0.loc.h71
# - Genera PDF: <evento>.gmt.pdf con mappa "linework" tipo mechanism.pdf
#   (cornice fancy, contorni, coste/laghi in nero spesso) + beachball
#
# NOTE IMPORTANTI:
# - Usiamo GMT in CLASSIC MODE (ps*) perché con `conda run` ogni comando
#   è un processo separato e `gmt begin/end` (modern mode) non è affidabile.
# ============================================================

nome="${1:?Uso: $0 <evento_senza_estensione>}"

FPFIT_DIR="/etc/software/fpfit"
DATA_DIR="/etc/software/fpfit/dati"

# Conda (solo per eseguire GMT senza activate)
CONDA="/etc/software/miniconda/miniconda3/bin/conda"
GMT_ENV="/srv/fpfitweb/conda-envs/gmt66"

# --- helper: esegue GMT nell'env conda (non dipende dal PATH)
gmt_run() {
  "$CONDA" run -p "$GMT_ENV" gmt "$@"
}

# --- check rapidi
if [ ! -x "$CONDA" ]; then
  echo "ERRORE: conda non trovato/eseguibile in: $CONDA"
  exit 2
fi

if ! "$CONDA" run -p "$GMT_ENV" gmt --version >/dev/null 2>&1; then
  echo "ERRORE: GMT non disponibile nell'env: $GMT_ENV"
  exit 2
fi

if [ ! -f "${nome}.grid0.loc.h71" ]; then
  echo "ERRORE: file input mancante: ${nome}.grid0.loc.h71"
  exit 2
fi

# --- Input hypo71
cp "${nome}.grid0.loc.h71" file.loc.h71
printf '\n' >> file.loc.h71

# --- fpfit input
cat > h71.inp <<'EOF'
ttl   1 'none'
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
rak  -180.0      180.0      20.00      10.00
EOF

# --- run fpfit
"${FPFIT_DIR}/fpfit" <<EOD
@h71.inp
fps
sto
EOD

# ------------------------------------------------------------
# GMT map (stile mechanism.pdf)
# ------------------------------------------------------------

export LC_ALL=C
export LANG=C

# Formato coordinate come in mechanism.pdf (14.04, 40.78, ecc.)
gmt_run set FORMAT_GEO_MAP=ddd.xx
gmt_run set MAP_DEGREE_SYMBOL=none
gmt_run set PS_CHAR_ENCODING=Standard
gmt_run set FONT_ANNOT_PRIMARY=10p,Helvetica
gmt_run set FONT_LABEL=12p,Helvetica
gmt_run set PS_LINE_CAP=round
gmt_run set PS_LINE_JOIN=round

# Cornice "fancy" come mechanism.pdf
gmt_run set MAP_FRAME_TYPE=fancy
gmt_run set MAP_FRAME_PEN=thick
gmt_run set MAP_TICK_PEN_PRIMARY=thin

# --- Area Campi Flegrei (come mechanism.pdf)
lo0="14.03"
lo1="14.23"
la0="40.77"
la1="40.88"

R="-R${lo0}/${lo1}/${la0}/${la1}"
J="-Jm60"

# --- Topografia:
#     - Se hai internet: @srtm_relief_01s (come stai usando)
#     - Se NON hai internet: decommenta la griglia locale
topo="@srtm_relief_01s"
#topo="$DATA_DIR/topotot.grd"

if [[ "$topo" != @* ]] && [[ ! -f "$topo" ]]; then
  echo "ERRORE: griglia topografica mancante: $topo"
  exit 2
fi

# --- Land polygons: preferisci no-islands se presente
if [[ -f "$DATA_DIR/land_polygons_osm_campiflegrei_no_islands.bf2" ]]; then
  LAND_BF2="$DATA_DIR/land_polygons_osm_campiflegrei_no_islands.bf2"
else
  LAND_BF2="$DATA_DIR/land_polygons_osm_campania.bf2"
fi

if [[ ! -f "$LAND_BF2" ]]; then
  echo "ERRORE: land polygons bf2 non trovato in: $DATA_DIR"
  exit 2
fi

# --- prepara i dati meccanismo in un file per psmeca
MECA_DAT="meca.dat"
MECA_PSMECA="meca_psmeca.txt"

"${FPFIT_DIR}/fpfit2gmt" < fpfit.fps > "$MECA_DAT"
if [ ! -s "$MECA_DAT" ]; then
  echo "ERRORE: fpfit2gmt non ha prodotto dati (meca.dat vuoto)."
  exit 3
fi

# fpfit2gmt: lat lon depth strike dip rake ...
# lon esce NEGATIVA (es -14.1143) ma la regione è su lon POSITIVE -> correggo: lon_corr = -lon
# psmeca vuole anche una magnitudine -> metto 5.0 costante
awk 'NF>=6 { printf "%.6f %.6f %.3f %.1f %.1f %.1f %.1f\n", -$2, $1, $3, $4, $5, $6, 5.0 }' "$MECA_DAT" > "$MECA_PSMECA"
if [ ! -s "$MECA_PSMECA" ]; then
  echo "ERRORE: meca_psmeca.txt vuoto (parsing fallito)."
  exit 3
fi

# --- Output finale
pdfbase="${nome}.gmt"     # psconvert creerà ${pdfbase}.pdf == ${nome}.gmt.pdf
PSFILE="${pdfbase}.ps"
rm -f "$PSFILE" "${pdfbase}.pdf"

# 1) Basemap + scalebar (come mechanism.pdf)
#    Tick/annotazioni coerenti: x ~0.04, y ~0.02
gmt_run psbasemap $R $J -Bxa0.04f0.02 -Bya0.02f0.01 -BWSen -Ln0.9/0.075+w2k+at+l"km"+f -K > "$PSFILE"

# 2) Contorni topografici (solo linee, niente shaded relief)
gmt_run grdcontour "$topo" $R $J -C50 -Wthinnest,black -O -K >> "$PSFILE"

# 3) Coste/terre emerse (OSM bf2) — solo contorno spesso
gmt_run psxy "$LAND_BF2" -bi2f $R $J -Wthick,black -O -K >> "$PSFILE"

# 4) Laghi (se presenti)
for lake in LagoPatria.gmt LagoFusaro.gmt LagoMiseno.gmt LagoLucrino.gmt LagoAverno.gmt; do
  if [[ -f "$DATA_DIR/$lake" ]]; then
    gmt_run psxy "$DATA_DIR/$lake" $R $J -Wthick,black -O -K >> "$PSFILE"
  fi
done

# 5) Beachball (ultimo layer)
gmt_run psmeca "$MECA_PSMECA" $R $J -Sa1.0 -W0.8p,black -O >> "$PSFILE"

# 6) Converti PS -> PDF
gmt_run psconvert "$PSFILE" -Tf -A -F"$pdfbase"

# --- rename outputs (come prima)
mv fpfit.out "${nome}.out"
mv fpfit.pol "${nome}.pol"
mv fpfit.fps "${nome}.fps"

# --- cleanup temporanei
rm -f file.loc.h71 h71.inp "$MECA_DAT" "$MECA_PSMECA" "$PSFILE"

exit 0
