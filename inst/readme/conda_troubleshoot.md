# Conda + R (reticulate) Troubleshooting Guide (Copy/Paste Ready)

This guide helps you confirm **which Conda** and **which Python** are being used by:
- your **terminal** (CMD/PowerShell/macOS/Linux shell)
- **R** (via `reticulate::py_config()`)

It also shows **expected outputs** and what to do if the outputs are wrong.

---

## 0) Before you start (important)

### What “correct” looks like
You want **Terminal Python** and **R/reticulate Python** to point to the **same Conda env**.

Example env name:
- `<your_env>`

Example correct Python paths:
- **Windows:** `C:\Users\...\anaconda3\envs\<your_env>\python.exe`
- **macOS/Linux:** `/Users/.../miniconda3/envs/<your_env>/bin/python`

---

## 1) Terminal checks (NOT inside R)

Pick the section for your OS/shell.

---

### 1A) Windows — CMD

Run:

```bat
conda --version
where conda
where python
python --version
python -c "import sys; print(sys.executable); print(sys.version)"
conda env list
conda activate <your_env>
where python
python -c "import sys; print(sys.executable); print(sys.version)"
```

**Expected output (examples)**

- `where conda`:
  - ✅ `C:\Users\YOU\anaconda3\condabin\conda.bat`
- Before activation, `where python` might show system Python:
  - `C:\Windows\...` or `C:\Users\YOU\AppData\...`
- After `conda activate <your_env>`, `where python` should include:
  - ✅ `C:\Users\YOU\anaconda3\envs\<your_env>\python.exe`
- `python -c "import sys; print(sys.executable)"` should print the SAME env Python:
  - ✅ `C:\Users\YOU\anaconda3\envs\<your_env>\python.exe`

---

### 1B) Windows — PowerShell

Run:

```powershell
conda --version
Get-Command conda
Get-Command python
python --version
python -c "import sys; print(sys.executable); print(sys.version)"
conda env list
conda activate <your_env>
Get-Command python
python -c "import sys; print(sys.executable); print(sys.version)"
```

**Expected output (examples)**

- `Get-Command python` after activation should show:
  - ✅ `...\anaconda3\envs\<your_env>\python.exe`

---

### 1C) macOS / Linux (bash/zsh)

Run:

```bash
conda --version
which conda
which python
python --version
python -c "import sys; print(sys.executable); print(sys.version)"
conda env list
conda activate <your_env>
which python
python -c "import sys; print(sys.executable); print(sys.version)"
```

**Expected output (examples)**

- After activation:
  - ✅ `which python` → `/.../miniconda3/envs/<your_env>/bin/python`
  - ✅ `sys.executable` prints the same path

---

## 2) R checks (inside R)

Run this in R:

```r
cat("R version:\n"); print(R.version.string)
cat("\nPlatform:\n"); print(Sys.info())

cat("\nPATH:\n"); print(Sys.getenv("PATH"))
cat("\nwhich python:\n"); print(Sys.which("python"))
cat("\nwhich conda:\n"); print(Sys.which("conda"))

if (requireNamespace("reticulate", quietly = TRUE)) {
  cat("\nreticulate version:\n"); print(packageVersion("reticulate"))
  cat("\npy_config():\n"); print(reticulate::py_config())
} else {
  cat("\nreticulate not installed\n")
}
```

**Expected output (what to look for)**

- `Sys.which("python")` should point to:
  - ✅ Windows: `...\envs\<your_env>\python.exe`
  - ✅ macOS/Linux: `.../envs/<your_env>/bin/python`

- `reticulate::py_config()` should show something like:
  - `python: .../envs/<your_env>/python`
  - `pythonhome: .../envs/<your_env>`
  - `version: 3.x.x`
  - `numpy: ...` (if installed)

If `py_config()` shows a different Python than your env → reticulate is using the wrong interpreter.

---

## 3) Fix: Force R/reticulate to use the correct Conda env

### 3A) Best method: set `RETICULATE_PYTHON` (strongest)

Find your env python path:

- Windows example:
  - `C:/Users/YOU/anaconda3/envs/<your_env>/python.exe`
- macOS/Linux example:
  - `/Users/YOU/miniconda3/envs/<your_env>/bin/python`

In R:

```r
Sys.setenv(RETICULATE_PYTHON = "PATH/TO/YOUR/ENV/PYTHON")
```

Restart R:
- RStudio: **Session → Restart R**

Verify:

```r
library(reticulate)
py_config()
```

✅ Expected: `py_config()` shows that exact python.

---

### 3B) Use `use_python()` (must run early)

```r
library(reticulate)

use_python("PATH/TO/YOUR/ENV/PYTHON", required = TRUE)
py_config()
```

⚠️ If you already imported Python in that R session, restart R first.

---

### 3C) Use `use_condaenv()` (by env name)

```r
library(reticulate)

use_condaenv("<your_env>", required = TRUE)
py_config()
```

If this fails to find Conda, fix Conda PATH or use Method 3A.

---

## 4) Installing missing Python packages into the same env

### 4A) Install in terminal

```bash
conda activate <your_env>
python -m pip install <pkg>
# or
conda install <pkg>
```

### 4B) Verify in R

```r
library(reticulate)
py_config()
py_module_available("<pkg>")
```

**Expected output**
- `py_module_available("<pkg>")` → `TRUE`

If `FALSE`, you installed it into a different env than reticulate is using.

---

## 5) Common error cases (and what they mean)

### Case 1: `py_config()` shows a different Python than terminal
**Cause:** R session PATH differs from your terminal PATH.  
**Fix:** Set `RETICULATE_PYTHON` and restart R (Section 3A).

### Case 2: `ModuleNotFoundError: No module named ...`
**Cause:** module not installed in that env.  
**Fix:** Install it in the same env (Section 4), restart R, re-check `py_config()`.

### Case 3 (Windows): `DLL load failed`
**Cause:** incompatible binary libs or wrong env.  
**Fix:** Force correct Python (3A), reinstall in env:

```bash
conda activate <your_env>
python -m pip uninstall <pkg>
python -m pip install <pkg>
# or conda install -c conda-forge <pkg>
```

### Case 4 (Linux): `GLIBCXX` / `libstdc++` errors
**Cause:** system libs vs conda libs mismatch.  
**Fix:** Prefer conda-forge and avoid mixing random pip wheels:

```bash
conda install -c conda-forge <pkg>
```

---

## 6) Recommended workflow (prevents most problems)

1) Activate env in terminal:
```bash
conda activate <your_env>
```

2) Launch R from that same terminal (best):
```bash
R
```

3) In R:
```r
library(reticulate)
py_config()
```

If wrong Python appears:
- Use Section 3A (`RETICULATE_PYTHON`) and restart.

---

## 7) Copy/paste bundle for reporting an issue (share this output)

### Terminal output
- Windows CMD:
  - `where conda`
  - `where python`
  - `python -c "import sys; print(sys.executable); print(sys.version)"`
- macOS/Linux:
  - `which conda`
  - `which python`
  - `python -c "import sys; print(sys.executable); print(sys.version)"`

### R output
```r
Sys.which("python")
Sys.which("conda")
reticulate::py_config()
```

This is enough to identify the mismatch quickly.
