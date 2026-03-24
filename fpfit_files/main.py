from __future__ import annotations

import os
import re
import shutil
import subprocess
from pathlib import Path
from uuid import uuid4
from typing import Optional
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


def safe_base(base: str) -> str:
    if not SAFE_BASE_RE.match(base):
        raise ValueError("Nome file non valido (consenti solo lettere/numeri/._-).")
    return base


def base_from_direct_filename(fn: str) -> str:
    if fn.endswith(".grid0.loc.h71"):
        return safe_base(fn[:-len(".grid0.loc.h71")])
    elif fn.endswith(".loc.h71"):
        return safe_base(fn[:-len(".loc.h71")])
    elif fn.lower().endswith(".prt"):
        return safe_base(fn[:-len(".prt")])
    else:
        raise ValueError("Carica un file .grid0.loc.h71, .loc.h71 oppure .prt.")


def base_from_pair_filename(fn: str, ext: str) -> str:
    if not fn.lower().endswith(ext):
        raise ValueError(f"File non valido: atteso {ext}")
    return safe_base(fn[:-len(ext)])


def copy_contours(job_dir: Path) -> None:
    for name in CONTOUR_FILES:
        src = CONTOUR_DIR / name
        if src.exists():
            shutil.copy2(src, job_dir / name)


@app.get("/", response_class=HTMLResponse)
def home(request: Request):
    return templates.TemplateResponse("index.html", {"request": request})


@app.post("/run", response_class=HTMLResponse)
async def run(
    request: Request,
    h71_file: Optional[UploadFile] = File(None),
    p01_file: Optional[UploadFile] = File(None),
    b01_file: Optional[UploadFile] = File(None),
):
    if not RUN_SCRIPT.exists():
        raise HTTPException(500, f"Script non trovato: {RUN_SCRIPT}")

    # modalità:
    # 1) file diretto: .loc.h71 / .grid0.loc.h71 / .prt
    # 2) coppia: .p01 + .b01
    direct_mode = h71_file is not None and (h71_file.filename or "").strip() != ""
    pair_mode = (
        p01_file is not None and (p01_file.filename or "").strip() != "" and
        b01_file is not None and (b01_file.filename or "").strip() != ""
    )

    if direct_mode and pair_mode:
        raise HTTPException(400, "Carica o un file diretto oppure la coppia .p01 + .b01, non entrambi.")
    if not direct_mode and not pair_mode:
        raise HTTPException(400, "Carica un file .loc.h71/.grid0.loc.h71/.prt oppure la coppia .p01 + .b01.")

    job_id = uuid4().hex
    job_dir = JOBS_DIR / job_id
    job_dir.mkdir(parents=True, exist_ok=True)

    copy_contours(job_dir)

    try:
        if direct_mode:
            filename = h71_file.filename or "input.grid0.loc.h71"
            base = base_from_direct_filename(filename)

            data = await h71_file.read()
            if len(data) > 50 * 1024 * 1024:
                raise HTTPException(400, "File troppo grande (>50MB).")

            
            if filename.endswith(".grid0.loc.h71"):
                input_path = job_dir / f"{base}.grid0.loc.h71"
            elif filename.endswith(".loc.h71"):
                input_path = job_dir / f"{base}.loc.h71"
            elif filename.lower().endswith(".prt"):
                input_path = job_dir / f"{base}.prt"
            else:
                raise HTTPException(400, "Formato non supportato.")

            input_path.write_bytes(data)

            mode = "direct"

        else:
            p01_name = p01_file.filename or "input.p01"
            b01_name = b01_file.filename or "input.b01"

            base_p01 = base_from_pair_filename(p01_name, ".p01")
            base_b01 = base_from_pair_filename(b01_name, ".b01")

            if base_p01 != base_b01:
                raise HTTPException(400, "I file .p01 e .b01 devono avere lo stesso nome base.")

            base = base_p01

            p01_data = await p01_file.read()
            b01_data = await b01_file.read()

            if len(p01_data) > 50 * 1024 * 1024 or len(b01_data) > 50 * 1024 * 1024:
                raise HTTPException(400, "File troppo grande (>50MB).")

            (job_dir / f"{base}.p01").write_bytes(p01_data)
            (job_dir / f"{base}.b01").write_bytes(b01_data)

            mode = "hypo71"

    except ValueError as e:
        raise HTTPException(400, str(e))

    log_path = job_dir / "run.log"
    try:
        proc = subprocess.run(
            ["bash", str(RUN_SCRIPT), mode, base],
            cwd=str(job_dir),
            capture_output=True,
            text=True,
            timeout=TIMEOUT,
            env={**os.environ, "LANG": "C", "LC_ALL": "C"},
        )
        log_path.write_text(
            f"CMD: bash {RUN_SCRIPT} {mode} {base}\n\nSTDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}\n",
            encoding="utf-8",
        )
    except subprocess.TimeoutExpired:
        raise HTTPException(500, "Timeout: esecuzione troppo lunga.")
    except Exception as e:
        raise HTTPException(500, f"Errore: {e}")

    out_pdf = job_dir / f"{base}.gmt.pdf"
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
