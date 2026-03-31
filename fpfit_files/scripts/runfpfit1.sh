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


clean_b01_to_pun() {
  local src="$1"
  awk '
    {
      gsub(/\r/, "")
      gsub(/\<EV\>/, "")
      gsub(/\<TRACES\>/, "")
      gsub(/\<PHASES\>/, "")
      gsub(/[[:space:]]+/, " ")
      sub(/^ /, "")
      sub(/ $/, "")
      if (length($0) > 0) print
    }
  ' "$src"
}



b01_to_fixed_cards() {
  local b01="$1"

  awk '
    NR == 1 { next }
    NF == 0 { next }

    {
      line = $0
      gsub(/\r/, "", line)

      # toglie eventuali etichette testuali
      gsub(/EV/, "", line)
      gsub(/TRACES/, "", line)
      gsub(/PHASES/, "", line)

      # normalizza cose tipo "14- 8.69" -> "14-8.69"
      gsub(/- +/, "-", line)

      # normalizza spazi multipli
      gsub(/[[:space:]]+/, " ", line)
      sub(/^ /, "", line)
      sub(/ $/, "", line)

      split(line, a, " ")

      # atteso circa:
      # a[1]=YYMMDD
      # a[2]=HHMM
      # a[3]=SEC
      # a[4]=LAT es. 40-49.88
      # a[5]=LON es. 14-8.69
      # a[6]=DEPTH
      hhmm = a[2]
      sec  = a[3]
      lat  = a[4]
      lon  = a[5]
      dep  = a[6]

      split(lat, la, "-")
      split(lon, lo, "-")

      minute = substr(hhmm, 3, 2) + 0

      # instruction card:
      # KNST=6  -> usa S + first motion
      # INST=9  -> fixed hypocenter da card successiva
      printf "%17s69\n", ""

      # additional fixed card:
      # ORG1 ORG2 LAT1 LAT2 LON1 LON2 Z
      printf "%5d%5.2f%5d%5.2f%5d%5.2f%5.2f\n",
             minute,
             sec + 0,
             la[1] + 0,
             la[2] + 0,
             lo[1] + 0,
             lo[2] + 0,
             dep + 0
      exit
    }
  ' "$b01"
}



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
    echo "[DEBUG] pwd=$(pwd)"
    echo "[DEBUG] contenuto job dir:"
    ls -l

    if [ ! -f "${nome}.p01" ]; then
      echo "ERRORE: file input mancante: ${nome}.p01"
      exit 2
    fi

    if [ ! -f "${nome}.b01" ]; then
      echo "ERRORE: file input mancante: ${nome}.b01"
      exit 2
    fi

    if [ ! -f "${HYPO71_DIR}/flegrei.sta" ]; then
      echo "ERRORE: file stazioni non trovato: ${HYPO71_DIR}/flegrei.sta"
      exit 2
    fi

    rm -f HYPO71PC.INP HYPO71PC.PRT HYPO71PC.PUN HYPO71PC.RES HYPO71PC.REL \
          phase.tmp fixed.tmp hypo71.cmd hypo71.stdout hypo71.stderr file.loc.h71

    # ------------------------------------------------------------
    # 1) Costruzione HYPO71PC.INP
    # ------------------------------------------------------------
    cp "${HYPO71_DIR}/flegrei.sta" HYPO71PC.INP || {
      echo "ERRORE: impossibile copiare ${HYPO71_DIR}/flegrei.sta"
      exit 2
    }

    python3 /etc/software/fpfit/scripts/p01_to_hypo71_phase.py \
      "${nome}.p01" \
      phase.tmp || {
      echo "ERRORE: conversione ${nome}.p01 -> phase.tmp fallita"
      exit 2
    }

    echo "[DEBUG] prime righe phase.tmp"
    sed -n '1,10p' phase.tmp | cat -vet
    echo "12345678901234567890123456789012345678901234567890"
    sed -n '1,5p' phase.tmp

    cat phase.tmp >> HYPO71PC.INP || {
      echo "ERRORE: impossibile accodare phase.tmp"
      exit 2
    }

    # fixed cards dal .b01
    b01_to_fixed_cards "${nome}.b01" > fixed.tmp || {
      echo "ERRORE: conversione ${nome}.b01 -> fixed.tmp fallita"
      exit 2
    }

    echo "[DEBUG] fixed.tmp"
    cat fixed.tmp | cat -vet

    cat fixed.tmp >> HYPO71PC.INP || {
      echo "ERRORE: impossibile accodare fixed.tmp"
      exit 2
    }

    # blank finale
    printf '\n' >> HYPO71PC.INP

    echo "[INFO] creato HYPO71PC.INP (phase list + fixed cards da b01)"
    ls -l HYPO71PC.INP

    # ------------------------------------------------------------
    # 2) Costruzione HYPO71PC.PUN dal .b01 ripulito
    # ------------------------------------------------------------
    clean_b01_to_pun "${nome}.b01" > HYPO71PC.PUN || {
      echo "ERRORE: impossibile creare HYPO71PC.PUN da ${nome}.b01"
      exit 2
    }

    echo "[DEBUG] HYPO71PC.PUN ricavato da b01"
    cat HYPO71PC.PUN | cat -vet

    # ------------------------------------------------------------
    # 3) File di controllo per Hypo71
    # ------------------------------------------------------------
    cat > hypo71.cmd << 'EOF'
HYPO71PC.INP
HYPO71PC.PRT
HYPO71PC.PUN
HYPO71PC.RES

HYPO71PC.REL
EOF

    echo "[DEBUG] hypo71.cmd"
    cat hypo71.cmd | cat -vet

    # ------------------------------------------------------------
    # 4) Esecuzione Hypo71
    # ------------------------------------------------------------
    "${HYPO71_EXE}" < hypo71.cmd > hypo71.stdout 2> hypo71.stderr || true

    echo "[DEBUG] dimensioni file Hypo71 prodotti"
    ls -l HYPO71PC.* 2>/dev/null || true

    echo "[DEBUG] prime righe HYPO71PC.PRT"
    head -n 120 HYPO71PC.PRT 2>/dev/null || true

    echo "[DEBUG] hypo71.stdout"
    cat hypo71.stdout 2>/dev/null || true

    echo "[DEBUG] hypo71.stderr"
    cat hypo71.stderr 2>/dev/null || true

    # ------------------------------------------------------------
    # 5) Recupero file utile per fpfit
    # ------------------------------------------------------------
    if [ -f "HYPO71PC.PRT" ]; then
      cp "HYPO71PC.PRT" file.loc.h71
      echo "[INFO] trovato HYPO71PC.PRT"
    else
      echo "ERRORE: Hypo71 non ha prodotto HYPO71PC.PRT"
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
