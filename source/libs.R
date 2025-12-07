# ===============================
# 00_packages_setup.R
# ===============================
# Purpose: Install (if missing) and load all required packages
# for hydrological, geospatial, and statistical data handling
# ===============================

# -------- DEFINE PACKAGE LISTS --------
cran_pkgs <- c(
  # Data handling
  "data.table", "dplyr", "tidyr", "readr",
  
  # Visualization
  "ggplot2", "ggpubr", "gridExtra","cowplot",
  
  # Spatial data
  "sf", "sp", "raster", "terra",
  
  # NetCDF handling
  "ncdf4",
  
  # Date and time
  "lubridate",
  
  # Statistical tools
  "stats", "scales", "zoo","modifiedmk",
  
  # File & string utilities
  "tools", "stringr", "showtext","scico"
)

# GitHub packages (latest versions)
gh_pkgs <- c(
  "AkbarR1184/evapoRe",  # your evapoRe package
  "imarkonis/twc"        # latest twc
)

# -------- INSTALL CRAN PACKAGES --------
installed <- rownames(installed.packages())
to_install <- setdiff(cran_pkgs, installed)
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

# -------- INSTALL GITHUB PACKAGES --------
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

for (pkg in gh_pkgs) {
  repo_name <- strsplit(pkg, "/")[[1]][2]
  if (!requireNamespace(repo_name, quietly = TRUE)) {
    remotes::install_github(pkg, upgrade = "always", dependencies = TRUE)
  }
}

# -------- LOAD ALL PACKAGES QUIETLY --------
all_pkgs <- c(cran_pkgs, "evapoRe", "twc")
suppressPackageStartupMessages({
  lapply(all_pkgs, require, character.only = TRUE)
})

cat("✅ All required packages (CRAN + GitHub) installed and loaded successfully.\n")
