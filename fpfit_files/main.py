from __future__ import annotations

import os
import re
import shutil
import subprocess
from pathlib import Path
from uuid import uuid4

from fastapi import FastAPI, File, UploadFile, HTTPException
from fastapi.responses import HTMLResponse, FileResponse
from fastapi.templating import Jinja2Templates
from starlette.requests import Request

APP_ROOT = Path("/srv/fpfitweb")
JOBS_DIR = APP_ROOT / "jobs"
TEMPLATES_DIR = APP_ROOT / "templates"

RUN_SCRIPT = Path("/etc/software/fpfit/scripts/runfpfit1.sh")
CONTOUR_DIR = Path("/etc/software/fpfit/dati")
CONTOUR_FILES = ["cont", "cont0"]

SAFE_BASE_RE = re.compile(r"^[A-Za-z0-9._-]{1,200}$")
TIMEOUT = 300  # secondi

app = FastAPI()
templates = Jinja2Templates(directory=str(TEMPLATES_DIR))


def base_from_filename(fn: str) -> str:
    if fn.endswith(".grid0.loc.h71"):
        base = fn[:-len(".grid0.loc.h71")]
    elif fn.endswith(".loc.h71"):
        base = fn[:-len(".loc.h71")]
    else:
        raise ValueError("Carica un file .grid0.loc.h71 (o .loc.h71).")

    if not SAFE_BASE_RE.match(base):
        raise ValueError("Nome file non valido (consenti solo lettere/numeri/._-).")
    return base


def copy_contours(job_dir: Path) -> None:
    for name in CONTOUR_FILES:
        src = CONTOUR_DIR / name
        if src.exists():
            shutil.copy2(src, job_dir / name)


@app.get("/", response_class=HTMLResponse)
def home(request: Request):
    return templates.TemplateResponse("index.html", {"request": request})


@app.post("/run", response_class=HTMLResponse)
async def run(h71_file: UploadFile = File(...), request: Request = None):
    if not RUN_SCRIPT.exists():
        raise HTTPException(500, f"Script non trovato: {RUN_SCRIPT}")

    try:
        base = base_from_filename(h71_file.filename or "input.grid0.loc.h71")
    except ValueError as e:
        raise HTTPException(400, str(e))

    job_id = uuid4().hex
    job_dir = JOBS_DIR / job_id
    job_dir.mkdir(parents=True, exist_ok=True)

    # salva input con nome atteso dallo script
    input_path = job_dir / f"{base}.grid0.loc.h71"
    data = await h71_file.read()
    if len(data) > 50 * 1024 * 1024:
        raise HTTPException(400, "File troppo grande (>50MB).")
    input_path.write_bytes(data)

    # copia file necessari per GMT
    copy_contours(job_dir)

    # esegui script
    log_path = job_dir / "run.log"
    try:
        proc = subprocess.run(
            ["bash", str(RUN_SCRIPT), base],
            cwd=str(job_dir),
            capture_output=True,
            text=True,
            timeout=TIMEOUT,
            env={**os.environ, "LANG": "C", "LC_ALL": "C"},
        )
        log_path.write_text(
            f"CMD: bash {RUN_SCRIPT} {base}\n\nSTDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}\n",
            encoding="utf-8",
        )
    except subprocess.TimeoutExpired:
        raise HTTPException(500, "Timeout: esecuzione troppo lunga.")
    except Exception as e:
        raise HTTPException(500, f"Errore: {e}")

    out_pdf = job_dir / f"{base}.gmt.pdf"   # quello GMT (il grafico)
    ok = (proc.returncode == 0) and out_pdf.exists()

    return templates.TemplateResponse(
        "result.html",
        {
            "request": request,
            "ok": ok,
            "job_id": job_id,
            "base": base,
            "returncode": proc.returncode,
        },
    )


@app.get("/file/{job_id}/{filename}")
def download_file(job_id: str, filename: str):
    job_dir = JOBS_DIR / job_id
    if not job_dir.exists():
        raise HTTPException(404, "Job non trovato")

    if not re.match(r"^[A-Za-z0-9._-]{1,260}$", filename):
        raise HTTPException(400, "Nome file non valido")

    allowed_ext = (".pdf", ".out", ".pol", ".fps", ".log", ".ps")
    if not filename.endswith(allowed_ext):
        raise HTTPException(403, "Tipo file non consentito")

    path = job_dir / filename
    if not path.exists():
        raise HTTPException(404, "File non trovato")

    return FileResponse(str(path), filename=path.name)
