# `data-raw` Folder: Data Preparation

> [!NOTE]
> The `data-raw` folder is for raw data files and scripts to prepare them.
> This is where you should put scripts that download data from the web, scripts that process raw data files, etc.
> The idea is that the `data-raw` folder contains the data preparation process, while the `data` folder contains
> the final processed data.

## Contents

The folder contains the following folders and files:

- `cache/`: Folder for cached data files.
- `data/`: Folder for the actual data files, split out by type:
  - `original/`: Folder for original (raw) data files, typically from the client.
  - `working/`: Folder for working data files, which are intermediate files that are created during the data preparation process.
  - `metadata/`: Folder for metadata files, which contain information about the data files.
- `scripts/`: Folder for R scripts that prepare the data for the package, split out into atomic, hierarchical scripts.

- `internal.R`: Main data preparation script that creates all internally packaged data included in [`R/sysdata.rda`](../R/sysdata.rda).
- `exported.R`: Main data preparation script that creates all exported data included in [`data/`](../data/). These datasets are also documented under [`R/data.R`](../R/data.R) via `roxygen2` comments.
- `README.md`: This file.

## Data Preparation Process

The data preparation process is split into two main scripts:

1. `internal.R`: This script prepares all internally packaged data included in [`R/sysdata.rda`](../R/sysdata.rda).
2. `exported.R`: This script prepares all exported data included in [`data/`](../data/). These datasets are also documented under [`R/data.R`](../R/data.R) via `roxygen2` comments.

Each of these scripts sources the scripts in the `scripts/` folder to prepare the data.

## Data Preparation Scripts

The `scripts/` folder contains the following scripts:

- `entrata.R`: Top-level entrata data preparation script that sources all other entrata data preparation scripts located
  under `scripts/entrata/`.
