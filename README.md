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