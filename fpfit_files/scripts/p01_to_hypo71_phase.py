#!/usr/bin/env python3
import re
import sys
from typing import Optional


def build_phase_line(sta: str, pol: str, dt14: str, s_sec: Optional[str]) -> str:
    """
    Costruisce una phase card HYP071 a colonne fisse.

    Colonne usate:
    1-4   station
    5     onset (blank)
    6     P
    7     first motion
    8     weight (blank)
    9     blank
    10-15 date YYMMDD
    16-17 hour
    18-19 minute
    20-24 P seconds F5.2
    25-31 blank
    32-36 S seconds F5.2 (se presente)
    resto blank
    """
    yy = dt14[0:2]
    mm = dt14[2:4]
    dd = dt14[4:6]
    hh = dt14[6:8]
    mi = dt14[8:10]
    sec = float(dt14[10:])

    # colonna 7: U/D/+/-/N/blank
    # dal tuo .p01 usi soprattutto P+ o P
    if pol == "P+":
        fm = "+"
    elif pol == "P-":
        fm = "-"
    elif pol == "P":
        fm = " "
    else:
        fm = " "

    psec = f"{sec:5.2f}"

    line = [" "] * 80

    # 1-4
    sta4 = sta[:4].ljust(4)
    line[0:4] = list(sta4)

    # 5 blank
    # 6 P
    line[5] = "P"

    # 7 first motion
    line[6] = fm

    # 8 blank
    # 9 blank

    # 10-15 date
    line[9:15] = list(yy + mm + dd)

    # 16-17 hour
    line[15:17] = list(hh)

    # 18-19 minute
    line[17:19] = list(mi)

    # 20-24 P seconds
    line[19:24] = list(psec)

    # 32-36 S seconds
    if s_sec is not None:
        ssec = f"{float(s_sec):5.2f}"
        line[31:36] = list(ssec)

    return "".join(line).rstrip()


def main():
    if len(sys.argv) != 3:
        print("Uso: p01_to_hypo71_phase.py input.p01 output.phase", file=sys.stderr)
        sys.exit(1)

    src = sys.argv[1]
    dst = sys.argv[2]

    out = []

    with open(src, "r", encoding="utf-8", errors="ignore") as f:
        for raw in f:
            line = raw.rstrip("\r\n")

            if not line.strip():
                continue

            # terminatore finale del .p01
            if line.strip() == "10":
                break

            # rimuovo eventuale FQ finale
            line = re.sub(r"\s+FQ\s*$", "", line)

            # formato atteso:
            # CAAM P+  260228101908.47
            # CBAC P+  260228101909.43        11.3 S
            m = re.match(
                r"^\s*([A-Za-z0-9]{4})\s+(P[+\-]?)\s+(\d{12}\.\d{2})(?:\s+(\d+(?:\.\d+)?)\s+S)?\s*$",
                line,
            )
            if not m:
                continue

            sta = m.group(1).upper()
            pol = m.group(2)
            dt14 = m.group(3)
            s_sec = m.group(4)

            out.append(build_phase_line(sta, pol, dt14, s_sec))

    with open(dst, "w", encoding="utf-8") as f:
        for row in out:
            f.write(row + "\n")
        # instruction card: blank line
        f.write("\n")


if __name__ == "__main__":
    main()