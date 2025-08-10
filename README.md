# ETC5543-Fish-Hydroacoustics

## BEFORE DOING INITIAL PULL PLEASE READ THIS,

THIS REPO's INTIAL DATASET IS CLOSE TO 60mb AS A RESULT OF THAT I HAVE USED GIT LFS TO STORE THE DATASET.
IF YOU ARE USING GIT LFS FOR THE FIRST TIME PLEASE FOLLOW THE STEPS BELOW.

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


First-time setup (once per computer, in git bash or terminal):

```bash
git lfs install
```

## Cloning the repository

```bash
git clone <REPO-URL>
cd ETC5543-Fish-Hydroacoustics
git lfs pull
```

**If you cloned before LFS was enabled (2025‑08‑10), please delete your local copy and reclone.**
