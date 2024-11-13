source("data-raw/R/utils_brand.R")
source("data-raw/R/utils_cache.R")

gmh_brand <- fetch_brand(
  domain = "gmhcommunities.com"
)

write_cache(gmh_brand)
download_brand_logos(gmh_brand)

noclocks_brand <- fetch_brand(
  domain = "noclocks.dev"
)


usethis::use_data(brand, overwrite = TRUE)
