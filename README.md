# Parcelas-permanentes
Reproducible R project for permanent plots analysis (Bosque Pehuén, FMA).

## 🔧 Requirements
- **R** ≥ 4.2 (recommended)
- **RStudio** *or* **VS Code** with R extension + **radian** (optional but nice)
- Internet access to install packages on first run

> Some R packages may require **system libraries** on Linux (see “System libraries” below).

## 🚀 First-time setup (any OS)
1. Install `{renv}` once:
   ```r
   install.packages("renv")

🐧 Linux: system libraries (only if you see build errors)

Some packages (e.g., sf, terra, curl, xml2) need OS libraries. Use:

# helper to list system reqs (install 'pak' if needed)
install.packages("pak")
pak::pkg_sysreqs(c("sf","terra","curl","xml2"))


Then install via your package manager (e.g., Ubuntu/Debian):

sudo apt-get update
sudo apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev \
                        gdal-bin libgdal-dev libgeos-dev libproj-dev
