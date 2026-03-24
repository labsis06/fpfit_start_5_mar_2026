#!/bin/bash
set -e
set -u
set -o pipefail

mode="${1:?Uso: $0 <direct|hypo71> <evento_senza_estensione>}"
nome="${2:?Uso: $0 <direct|hypo71> <evento_senza_estensione>}"

FPFIT_DIR="/etc/software/fpfit"
HYPO71_DIR="/etc/software/hypo71"
HYPO71_EXE="/etc/software/hypo71/Hypo71PC"
DATA_DIR="/etc/software/fpfit/dati"

CONDA="/etc/software/miniconda/miniconda3/bin/conda"
GMT_ENV="/srv/fpfitweb/conda-envs/gmt66"

gmt_run() {
  "$CONDA" run -p "$GMT_ENV" gmt "$@"
}

# --- Check rapidi
if [ ! -x "$CONDA" ]; then
  echo "ERRORE: conda non trovato/eseguibile in: $CONDA"
  exit 2
fi

# verifica che GMT funzioni
if ! "$CONDA" run -p "$GMT_ENV" gmt --version >/dev/null 2>&1; then
  echo "ERRORE: GMT non disponibile nell'env: $GMT_ENV"
  exit 2
fi

# --- prepara file.loc.h71 in base alla modalità scelta
case "$mode" in
  direct)
    if [ -f "${nome}.grid0.loc.h71" ]; then
      cp "${nome}.grid0.loc.h71" file.loc.h71
    elif [ -f "${nome}.loc.h71" ]; then
      cp "${nome}.loc.h71" file.loc.h71
    elif [ -f "${nome}.prt" ]; then
      cp "${nome}.prt" file.loc.h71
    else
      echo "ERRORE: nessun file diretto trovato per base ${nome}"
      exit 2
    fi
    ;;

         hypo71)
    if [ ! -f "${nome}.p01" ]; then
      echo "ERRORE: file input mancante: ${nome}.p01"
      exit 2
    fi

    if [ ! -f "${HYPO71_DIR}/flegrei.sta" ]; then
      echo "ERRORE: file stazioni non trovato: ${HYPO71_DIR}/flegrei.sta"
      exit 2
    fi

    rm -f HYPO71PC.INP HYPO71PC.PRT HYPO71PC.PUN HYPO71PC.RES HYPO71PC.REL \
          file.loc.h71 hypo71.cmd hypo71.stdout hypo71.stderr

    # costruzione input Hypo71 corretto
cp "${HYPO71_DIR}/flegrei.sta" HYPO71PC.INP

sed -e 's/\r$//' \
    -e 's/^/ /' \
    "${nome}.p01" >> HYPO71PC.INP

printf '\n' >> HYPO71PC.INP

echo "[INFO] creato HYPO71PC.INP (formato corretto)"

    # 2) file di controllo per stdin
    cat > hypo71.cmd << 'EOF'
HYPO71PC.INP
HYPO71PC.PRT
HYPO71PC.PUN
HYPO71PC.RES

HYPO71PC.REL
EOF

    # 3) eseguo Hypo71
    "${HYPO71_EXE}" < hypo71.cmd > hypo71.stdout 2> hypo71.stderr || true

    # 4) recupero il file utile
    if [ -f "HYPO71PC.PRT" ]; then
      cp "HYPO71PC.PRT" file.loc.h71
      echo "[INFO] trovato HYPO71PC.PRT"
    else
      echo "ERRORE: Hypo71 non ha prodotto HYPO71PC.PRT"
      echo "----- hypo71.stdout -----"
      cat hypo71.stdout 2>/dev/null || true
      echo "----- hypo71.stderr -----"
      cat hypo71.stderr 2>/dev/null || true
      exit 2
    fi
    ;;

  *)
    echo "ERRORE: modalità non valida: $mode"
    exit 2
    ;;
esac

# evita casi EOF strani
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
# GMT map
# ------------------------------------------------------------

# --- Locale “pulita” per evitare simboli UTF-8 strani
export LC_ALL=C
export LANG=C

# --- Assi: gradi decimali, senza simboli ° ' "
gmt_run set FORMAT_GEO_MAP=ddd.xx
gmt_run set MAP_DEGREE_SYMBOL=none
gmt_run set PS_CHAR_ENCODING=Standard
gmt_run set FONT_ANNOT_PRIMARY=10p,Helvetica
gmt_run set FONT_LABEL=12p,Helvetica
gmt_run set PS_LINE_CAP=round
gmt_run set PS_LINE_JOIN=round
gmt_run set MAP_FRAME_TYPE=fancy
gmt_run set MAP_FRAME_PEN=thick
gmt_run set MAP_TICK_PEN_PRIMARY=thin

# --- Area Campi Flegrei
lo0="14.03"
lo1="14.23"
la0="40.77"
la1="40.88"

R="-R${lo0}/${lo1}/${la0}/${la1}"
J="-Jm60"
# ---File topografia Tinitaly 1.1 (10m) o SRTM01s (1s) 
# di default viene scaricato SRTM01s dalle repo web
# Se si vuole usare un file locale, decommentare la riga corrispondente e commentare quella di SRTM

topo="@srtm_relief_01s"
#topo="./Data/tinitaly11.hdf4=gd:hdf4"
#topo="$DATA_DIR/topotot.grd"

# Se topo NON è remoto, verifica che esista davvero
if [[ "$topo" != @* ]] && [[ ! -f "$topo" ]]; then
  echo "ERRORE: griglia topografica mancante: $topo"
  exit 2
fi

  LAND_BF2="$DATA_DIR/land_polygons_osm_campania.bf2"

if [[ ! -f "$LAND_BF2" ]]; then
  echo "ERRORE: land polygons bf2 non trovato in: $DATA_DIR"
  exit 2
fi

# --- Output finale: ${nome}.gmt.pdf (tutto dentro la stessa pagina)
pdfnamebase="${nome}.gmt"

# --- prepara i dati meccanismi in un file per psmeca (NO pipe, perché conda run non passa stdin)
MECA_DAT="meca.dat"
MECA_PSMECA="meca_psmeca.txt"

"${FPFIT_DIR}/fpfit2gmt" < fpfit.fps > "$MECA_DAT"
if [ ! -s "$MECA_DAT" ]; then
  echo "ERRORE: fpfit2gmt non ha prodotto dati (meca.dat vuoto)."
  exit 3
fi

# fpfit2gmt produce: lat lon depth strike dip rake ...
# lon esce NEGATIVA (es: -14.1143) ma la regione è su lon POSITIVE (14.x)
# Quindi correggo: lon_corr = -lon
# Inoltre psmeca in convenzione "aki" vuole anche una magnitudine: metto 5 come valore costante.
awk 'NF>=6 { printf "%.6f %.6f %.3f %.1f %.1f %.1f %.1f\n", -$2, $1, $3, $4, $5, $6, 5.0 }' "$MECA_DAT" > "$MECA_PSMECA"

if [ ! -s "$MECA_PSMECA" ]; then
  echo "ERRORE: meca_psmeca.txt vuoto (parsing fallito)."
  exit 3
fi

\
# ------------------------------------------------------------
# Output mappa in CLASSIC MODE (PS -> PDF)
#
# Motivo: con `conda run` ogni chiamata a GMT è un processo separato,
# quindi `gmt begin/end` (modern mode) non riesce a mantenere la sessione.
# Qui genero un unico file PostScript dove vengono sovrapposti i diversi layer e lo converto in PDF.
# ------------------------------------------------------------

PSFILE="${pdfnamebase}.ps"
rm -f "$PSFILE"

# 1) Basemap + scalebar
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
gmt_run psconvert "$PSFILE" -Tf -A -F"$pdfnamebase"

# --- rename outputs 
mv fpfit.out "${nome}.out"
mv fpfit.pol "${nome}.pol"
mv fpfit.fps "${nome}.fps"

# --- cleanup
rm -f file.loc.h71 h71.inp "$MECA_DAT" "$MECA_PSMECA" "$PSFILE"

exit 0
