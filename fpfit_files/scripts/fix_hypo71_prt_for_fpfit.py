#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import sys

if len(sys.argv) != 3:
    print("Uso: fix_hypo71_prt_for_fpfit.py input.PRT output.loc.h71")
    sys.exit(1)

src = sys.argv[1]
dst = sys.argv[2]

sep_re = re.compile(r'^\s*-{5,}\s*$')
junk_num_re = re.compile(r'^\s*[-+0-9.Ee]+\s+[-+0-9.Ee]+\s+[-+0-9.Ee]+')
station_re = re.compile(r'^\s*[A-Z0-9]{3,4}\s+')
sline_re = re.compile(r'^\s+S\s+')

with open(src, "r", encoding="utf-8", errors="ignore") as f:
    lines = [x.rstrip("\n") for x in f]

out = []
in_station_block = False
i = 0

while i < len(lines):
    line = lines[i]

    # finche non arrivo alla tabella stazioni, non copio niente
    if not in_station_block:
        if "Sta   Dist  Az Inc P" in line:
            in_station_block = True
        i += 1
        continue

    # fine blocco evento
    if line.strip().startswith("***"):
        break

    # elimina header e righe descrittive
    if (
        "Date   Heure Minute Seconde" in line
        or "Latitude" in line
        or "Longitude" in line
        or "Profondeur / Ref" in line
        or "RMS" in line
        or "Sta   Dist  Az Inc P" in line
    ):
        i += 1
        continue

    # elimina righe vuote, separatori e blocchi numerici
    if not line.strip():
        i += 1
        continue

    if sep_re.match(line) or junk_num_re.match(line):
        i += 1
        continue

    # se non è una riga stazione, scarta
    if not station_re.match(line):
        i += 1
        continue

    p_line = line.rstrip()
    s_line = ""

    # eventuale riga S subito sotto
    if i + 1 < len(lines) and sline_re.match(lines[i + 1]):
        s_line = lines[i + 1].strip()
        i += 1

    if s_line:
        parts = s_line.split()
        # atteso: S sec calc oc wt
        if len(parts) >= 5:
            merged = (
                p_line
                + "                    S         "
                + f"{parts[1]} {parts[2]} {parts[3]} {parts[4]}"
            )
        else:
            merged = p_line
    else:
        merged = p_line

    # shift di 1 carattere a destra
    merged = " " + merged
    out.append(merged)

    i += 1

with open(dst, "w", encoding="utf-8") as f:
    for line in out:
        f.write(line.rstrip() + "\n")