# ETC5543-Fish-Hydroacoustics

## Before Cloning or Pulling – Read This First

This repository’s initial dataset is ~60 MB.  
Large files are stored using **Git LFS** (Large File Storage).  
If you do not have Git LFS set up correctly, you may only get a small placeholder file (~100 bytes) instead of the real dataset.


### 1. Install Git LFS (only once per computer)

🔗 Official site: [https://git-lfs.com/](https://git-lfs.com/)

Choose your OS:

- **Windows**: Download and run the installer from [https://git-lfs.com/](https://git-lfs.com/)  
  (Recent Git for Windows installers may already include LFS, if so go to next step.)
- **macOS**:  
  ```bash
  brew install git-lfs
  ```
  
- **Linux (Debian/Ubuntu):**

  ```bash
  sudo apt install git-lfs
  ```
- **Linux (Fedora/RHEL):**

  ```bash
  sudo dnf install git-lfs
  ```


First-time setup (once per computer, in git bash or terminal (MUST DO)):

```bash
git lfs install
```

## Cloning the repository (with large files)

```bash
git clone <REPO-URL>
cd ETC5543-Fish-Hydroacoustics
git lfs pull
```

**If you cloned before LFS was enabled (2025‑08‑10), please delete your local copy and reclone.**

```bash
rm -rf ETC5543-Fish-Hydroacoustics
git clone <REPO-URL>
cd ETC5543-Fish-Hydroacoustics
git lfs pull

```

## Troubleshooting LFS Files

Symptoms:

- `file.size()` in R is ~100–200 bytes
- `readRDS()` says “unknown input format”

```bash
git lfs pull
git lfs checkout data/TSresponse_clean.RDS

```

## OneDrive/Dropbox Users

If your repo is inside OneDrive, Dropbox, or iCloud:
Mark the repo folder as "Always keep on this device" so that large files stay downloaded.

## Workflow for Contributing

If you change or add large data files (e.g., .RDS, .rds, .csv > 50 MB):

Save the file into the data/ folder.

If new type, track it with:

```bash
git lfs track "*.RDS" "*.rds"
git add .gitattributes
git commit -m "Track RDS files in LFS"

```
(Already set for .RDS/.rds.)

Commit and push as usual:

```bash
git add data/<filename>
git commit -m "Update dataset"
git push

```

## Reproducibility (renv)

This repo pins package versions with **renv**.

### Restore the environment
```r
install.packages("renv")
renv::restore()   # installs the exact package versions from renv.lock

## How to run (end-to-end)

```bash
git lfs install
git lfs pull
R -e "install.packages('renv'); renv::restore()"

Rscript Analysis/03_classification_original.R


## Python (Keras/TensorFlow) Setup

Some scripts (e.g. `Analysis/03_rnn_classification.R`) use Keras/TensorFlow via **reticulate**.  
You need a dedicated Python virtual environment (not tracked in Git).  
We pin TensorFlow/Keras to **v2.15** for compatibility with the R `keras` interface.

---

### 1. Create a Python virtual environment

```bash
# Windows (PowerShell):
python -m venv "%USERPROFILE%\.virtualenvs\r-keras"

# Mac/Linux:
python3 -m venv ~/.virtualenvs/r-keras

```
### 2. Activate the virtual environment

```bash
# Windows:
%USERPROFILE%\.virtualenvs\r-keras\Scripts\activate

# Mac/Linux:
source ~/.virtualenvs/r-keras/bin/activate
```

### 3. Install the required Python packages

```bash

pip install "numpy<2" tensorflow==2.15 keras==2.15 tf-keras==2.15.1

```

### 4. Tell R to use this environment

At the start of your R session (or in `.Rprofile`):

```r
reticulate::use_virtualenv("C:/Users/<your-username>/Documents/.virtualenvs/r-keras", required = TRUE)
```
(Adjust the path for Mac/Linux: ~/.virtualenvs/r-keras)

### 5. Sanity check

```r
library(tensorflow)
tensorflow::tf_config()   # should show TensorFlow v2.15.0

library(keras)
keras::is_keras_available()   # should return TRUE
```

### Notes

- The R keras package is deprecated; we use it intentionally with TensorFlow 2.15 for compatibility.

- Do not upgrade to TensorFlow 2.16+ or Keras 3+, as the R interface is not yet stable with those versions.

- Each collaborator must create this virtual environment locally. The .virtualenvs/ folder is ignored by Git.

### Troubleshooting

- **Error:** `AttributeError: module 'tensorflow' has no attribute 'VERSION'`  
  → Cause: wrong TF/Keras version.  
  → Fix: Reinstall pinned versions inside the virtualenv:  
  ```bash
  pip install --upgrade --force-reinstall "tensorflow==2.15.0" "keras==2.15.0" "tf-keras==2.15.1"
```
- Error: `keras::is_keras_available()` returns `FALSE`
→ Make sure you called `reticulate::use_virtualenv(".../r-keras", required = TRUE)` before loading keras in R.

## Quick Health Check

**Run this check once after cloning the repo or updating dependencies to confirm everything works.**

After installing R and Python dependencies, run the smoke test:

```bash
Rscript 00_smoke_test.R
```
### Expected output:

- R version is printed

- ggplot2 OK

- reticulate OK

- TensorFlow version 2.15.0

- Keras available: TRUE

- **All checks passed ✅**

#### Notes for OneDrive users

If your `.virtualenvs/r-keras` is inside **OneDrive**, paths may vary (e.g., `C:/Users/<name>/OneDrive/Documents/.virtualenvs/r-keras`).  
The provided `00_smoke_test.R` script automatically checks both standard `Documents/` and `OneDrive/Documents/` locations and uses whichever exists.

If this script runs without errors, your environment is correctly set up.
