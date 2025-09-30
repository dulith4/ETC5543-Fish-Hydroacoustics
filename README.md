# ETC5543-Fish-Hydroacoustics

## ⚠️ Important:

The Quickstart only covers the bare minimum.
👉 Make sure to read the full instructions below once, especially if you are:

- setting up Python (Keras/TensorFlow)

- adding new packages

- or collaborating with others.

## 🚀 Quickstart

**Run these commands after cloning**:


#### 1. Clone with Git LFS
```bash
git clone <REPO-URL>
cd ETC5543-Fish-Hydroacoustics
git lfs install
git lfs pull
```
#### 2. In R (restore pinned packages)

```r
install.packages("renv")
renv::restore(prompt = FALSE)
```
#### 3. Sanity check

```bash
Rscript 00_smoke_test.R
```

✅ You should see TensorFlow 2.15, Keras 2.15, H2O loaded (but not running), and “All checks passed”.

## ⚠️ Environment Setup – Do This First

This project uses **renv (R)** and a dedicated **Python virtualenv** for TensorFlow/Keras.  
To ensure reproducibility, everyone (new or existing collaborator) must follow these steps.

### 1. Git LFS (Large File Storage)

The dataset (~60 MB) is stored via Git LFS.  
If you don’t set up LFS, you’ll only get tiny pointer files.

- Install LFS once per computer:  
  ```bash
  git lfs install
After cloning or pulling:
  git lfs pull
  ```

### 2. R Environment (renv)

We pin all R packages with `renv`.

After cloning or pulling:  

```r
install.packages("renv")
renv::restore(prompt = FALSE)
```
This will install the exact R package versions defined in `renv.lock`.

  **Important**: Do not manually upgrade/downgrade packages.
  If you add a new package, first add it to 00_dependencies.R, then run:
  
```r
source("00_dependencies.R")
renv::snapshot(type = "explicit")
```
Commit the updated `renv.lock`.

### 3. Python (TensorFlow/Keras via reticulate)

Some scripts (e.g., RNNs) require Python. We pin **TensorFlow 2.15.0** and **Keras 2.15.0**.
Each collaborator must create this environment locally (not tracked in Git).

Create the venv:

```bash
# Windows (PowerShell)
python -m venv "%USERPROFILE%\.virtualenvs\r-keras"

# macOS/Linux
python3 -m venv ~/.virtualenvs/r-keras
```

**Activate it and install packages:**

```bash
# Windows
%USERPROFILE%\.virtualenvs\r-keras\Scripts\activate

# macOS/Linux
source ~/.virtualenvs/r-keras/bin/activate

pip install "numpy<2" tensorflow==2.15.0 keras==2.15.0 tf-keras==2.15.1
```

**Tell R to use it (already set up in code):**

Our `00_dependencies.R` and `00_smoke_test.R` automatically search for the venv in:

  - `~/Documents/.virtualenvs/r-keras` 

  - `~/OneDrive/Documents/.virtualenvs/r-keras`

So as long as you created it there, **no manual change needed**.
Advanced users can override via:

```r
options(fishhydro.rkeras.path = "C:/custom/path/to/r-keras")
```

### 4. Quick Health Check

Run this after setup (and after pulling updates):

```bash
Rscript 00_smoke_test.R
```
✅ Expected:

- R version printed

- ggplot2 OK

- reticulate OK

- TensorFlow version 2.15.0

- Keras version 2.15.0

- H2O loaded but not running (that’s normal)

- All checks passed

If this fails → check Python venv and rerun the install commands above.


### 5. Collaborator Workflow

- **Daily use (after pull):**

```r
source("00_dependencies.R")
```

This activates renv, checks TF/Keras venv, and loads packages.

- **If you add a new R package**:
  Add it to `00_dependencies.R` → run `renv::snapshot(type = "explicit")` → commit `renv.lock`.

- **Do NOT commit**:

  - Your Python venv (`.virtualenvs/`)

  - `renv/library/` (local cache; ignored by Git)

  - Any temp logs or cache files

- Safe to commit:

  - Updated R scripts

  - Updated `00_dependencies.R`

  - Updated `renv.lock` (when packages change)
