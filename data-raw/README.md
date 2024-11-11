# Raw Data Preparation

> [!NOTE]
> The `data-raw` folder is for raw data files and scripts to prepare them. 
> This is where you should put scripts that download data from the web, scripts
> that process raw data files, etc. 
> The idea is that the `data-raw` folder contains the data preparation process, 
> while the `data` folder contains the final processed data.

## Contents

The folder contains the following folders and files:

- `cache/`: Folder for cached data files.
- `src/`: Folder for R scripts that prepare the data for the package.

- `dataprep.R`: Main data preparation script that sources other scripts in
  the `src/` folder.

## Data Preparation
