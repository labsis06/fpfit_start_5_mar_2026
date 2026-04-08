#!/usr/bin/env python3
import re
import sys

if len(sys.argv) != 3:
    print("Uso: fix_hypo71_prt_for_fpfit.py input.PRT output.loc.h71")
    sys.exit(1)

src = sys.argv[1]
dst = sys.argv[2]

sep_re = re.compile(r'^\s*-{5,}\s*$')
junk_num_re = re.compile(r'^\s*[-+0-9.Ee]+\s+[-+0-9.Ee]+\s+[-+0-9.Ee]+')
station_re = re.compile(r'^\s*([A-Z0-9]{3,4})\s+')
sline_re = re.compile(r'^\s+S\s+')

def normalize_num(tok: str) -> str:
    tok = tok.strip()
    tok = re.sub(r'^(-?)0\.', r'\1.', tok)
    return tok

def is_station_line(line: str) -> bool:
    if sep_re.match(line):
        return False
    if junk_num_re.match(line):
        return False
    if sline_re.match(line):
        return False
    return bool(station_re.match(line))

with open(src, "r", encoding="utf-8", errors="ignore") as f:
    lines = [x.rstrip("\n") for x in f]

out = []
in_station_block = False
i = 0

while i < len(lines):
    line = lines[i]

    # prima del blocco stazioni: conserva tutto
    if not in_station_block:
        out.append(line)
        if "Sta   Dist  Az Inc P" in line:
            in_station_block = True
        i += 1
        continue

    # fine blocco evento
    if line.strip().startswith("***"):
        out.append(line)
        break

    # scarta separatori e blocchi numerici
    if sep_re.match(line) or junk_num_re.match(line):
        i += 1
        continue

    # se non è riga stazione, scarta
    if not is_station_line(line):
        i += 1
        continue

    p_line = line.rstrip()

    # guarda se la riga successiva è una riga S
    s_line = ""
    if i + 1 < len(lines):
        nxt = lines[i + 1]
        if sline_re.match(nxt):
            s_line = nxt
            i += 1

    # senza S: conserva la riga P così com'è
    if not s_line:
        out.append(p_line)
        i += 1
        continue

    # parse della riga S
    parts = s_line.split()
    # atteso: S sec calc oc wt
    if len(parts) >= 5 and parts[0] == "S":
        s_sec  = normalize_num(parts[1]).rjust(5)
        s_calc = normalize_num(parts[2]).rjust(5)
        s_oc   = normalize_num(parts[3]).rjust(5)
        s_wt   = normalize_num(parts[4]).rjust(5)

        # costruisce UNA SOLA riga, accodando la parte S in coda
        merged = (
            p_line.rstrip()
            + "                    S         "
            + f"{s_sec} {s_calc} {s_oc} {s_wt}"
        )
        out.append(merged)
    else:
        out.append(p_line)

    i += 1

with open(dst, "w", encoding="utf-8") as f:
    for line in out:
        f.write(line.rstrip() + "\n")