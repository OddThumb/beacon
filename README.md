# BEACON: Burst Event Anomaly Clustering and Outlier Notification

[![License: GPL v2+](https://img.shields.io/badge/License-GPL%20v2+-blue.svg)](LICENSE)

BEACON is a fully data-driven pipeline designed to detect unmodeled gravitational wave (GW) transients. By combining sequential autoregressive modeling ([seqARIMA](https://doi.org/10.1103/PhysRevD.109.102003)) and anomaly clustering, BEACON provides a low-latency and model-agnostic framework for robust burst detection.

## Key Features

-   **Denoising** via seqARIMA modeling (`seqarima`)
-   **Anomaly Detection** using robust IQR-based statistical thresholds
-   **Clustering** of temporal outliers with DBSCAN
-   **Statistical Evaluation** via Poisson and Exponential models
-   **Coincidence Analysis** across multiple detectors
-   Fully compatible with streaming or batch-based analysis

## **Pipeline Overview**

``` text
                  ┌─────────────────────────┐
                  │        seqARIMA         │◀─── Denoising (seqarima)
                  └───────────┬─────────────┘
                              ↓
                  ┌─────────────────────────┐
                  │    Anomaly Detection    │◀─── IQR method
                  └───────────┬─────────────┘
                              ↓
                  ┌─────────────────────────┐
                  │       Clustering        │◀─── DBSCAN clustering
                  └───────────┬─────────────┘
                              ↓
                  ┌─────────────────────────┐
                  │ Significance Evaluation │◀─── Significance (λₐ, λ꜀)
                  └───────────┬─────────────┘
                              ↓
                  ┌─────────────────────────┐
                  │  Coincidence Analysis   │◀─── Across detectors
                  └─────────────────────────┘
```

## Installation

``` shell
# In R
# If `devtools` is not installed
# install.packages(“devtools”)
devtools::install_github(
    repo = "https://github.com/OddThumb/beacon.git",
    upgrade = "never"
)
```

or

``` bash
git clone https://github.com/OddThumb/beacon.git
cd beacon

# In R
devtools::install(".")
```

## Usage

``` r
# R Example
library(beacon)

# Load GW strain data
ts_H1 <- read_H5("H1.hdf5", sampling.freq = 4096)
ts_L1 <- read_H5("L1.hdf5", sampling.freq = 4096)
ts_list <- list("H1" = ts_H1, "L1" = ts_L1)

# Data batch preparation
batch_set <- batching.network(ts_list)

# Configure pipeline
cfg <- config_pipe()

# Run detection pipeline
result <- stream(batch_set = batch_set, arch_params = cfg)

# In console, 
# 1-th batch:
#   H1: λ_c=6.403, λ_N=3.333
#   L1: λ_c=6.445, λ_N=3.337
# 2-th batch:
#   H1: λ_c=6.403, λ_N=3.333
#   L1: λ_c=6.445, λ_N=3.337
# ...
```

> ⚠️ **Note1:** If you plan to use the **PyCBC backend (e.g. functions in R/use_pycbc.R)**, make sure to link your environment with PyCBC installed. You can do so by specifying the path to your conda environment as shown below:

``` r
# Install reticulate to incorporate python package
install.packages("reticulate")

# Register your conda envrionment path
reticulate::use_condaenv(condaenv = "path/to/your/conda/env")
```

> ⚠️ **Note2**: The sophisticated vignettes for detailed usage will be delivered soon.

## Python Version

A **Python implementation** of BEACON is now available at [beacon-py](https://github.com/OddThumb/beacon-py).

## **Documentation**

-   Repository: <https://github.com/OddThumb/beacon>

-   For detail usages, See GitHub Pages: <https://oddthumb.github.io/beacon/articles/>

## **Example Datasets**

See [GWOSC](https://www.gw-openscience.org/) compatible tools to fetch real event data.

Or data can be downloaded via `list_event()` and `download_event()`.

For demo data, you can find it by:

```{r}
system.file("extdata", "demo_4kHz.h5", package = "beacon")
```

(The demo data was created by using Python's `pycbc` package)

## **Publications**

If you use BEACON in your work, please cite: Kim et al., “Autoregressive Search of Gravitational Waves: Design of low-latency search pipeline for unmodeled transients — BEACON”, submitted.

If you use only `seqarima` in your work, please cite: [Kim et al., PRD, 2024, “Autoregressive Search of Gravitational Waves: Denoising”](https://doi.org/10.1103/PhysRevD.109.102003).
