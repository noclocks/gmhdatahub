
# Market Survey

For the database, SQL DDL and Seeding and Schemas are under `inst/database/schemas/survey/`.

For example, in this folder there would be:

- `survey.schema.sql`
- `survey.schema.dbml`
- `seed.survey.schema.sql`
- `tables/`
- `triggers/`
- `seed/`
- `functions/`
- `archive/`
- `etc.`


For the R package data preparations, these are split out amongst a variety of data (market survey is just one component of many other areas in this package). 

But for market survey in particular the data-raw folder has:

- `data-raw/data/original/market_survey/*`
- `data-raw/cache/`

- `data-raw/R/utils_excel.R`
- `data-raw/R/utils_survey.R`

- `data-raw/scripts/survey/survey_prep.R`
- `data-raw/scripts/survey.R`
- `data-raw/internal.R`
- `data-raw/exported.R`

prepped, working datasets (typically CSVs) are saved in the `data-raw/data/working/survey/` folder and also can be included under `inst/extdata/survey/` for use in the package.

The `data-raw/scripts/survey/survey_prep.R` script is the main script that runs the data preparation for the market survey. It sources the utils functions and then runs the data preparation steps. The final output is saved in the `data-raw/data/working/survey/` folder.

The `data-raw/scripts/survey.R` script is the main script that runs the data preparation for all the surveys. It sources the utils functions and then runs the data preparation steps for each survey. The final output is saved in the `data-raw/data/working/survey/` folder.

The `data-raw/internal.R` script is the main script that runs the data preparation for all the internal data. It sources the utils functions and then runs the data preparation steps for each internal data. The final output is saved in the `data-raw/data/working/survey/` folder.

The `data-raw/exported.R` script is the main script that runs the data preparation for all the exported data. It sources the utils functions and then runs the data preparation steps for each exported data. The final output is saved in the `data-raw/data/working/survey/` folder.

The `data-raw/cache/` folder is used to store intermediate files that are used in the data preparation process. These files are not included in the final output and are only used for debugging purposes. The `data-raw/cache/` folder is not included in the package and is not used in the final output.

The `data-raw/data/original/market_survey/` folder is used to store the original data files that are used in the data preparation process. These files are not included in the final output and are only used for debugging purposes. The `data-raw/data/original/market_survey/` folder is not included in the package and is not used in the final output.

The `data-raw/data/working/survey/` folder is used to store the final output files that are used in the data preparation process. These files are included in the final output and are used in the package. The `data-raw/data/working/survey/` folder is included in the package and is used in the final output.
