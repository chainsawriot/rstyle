require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)
require(modules)
require(fs)

cfg <- modules::use("config.R")

## We should package the functions in helpers.R into a package.
source('helpers.R')

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = cfg$PATH_CODE_DB)
cran_code <- tbl(con, "cran_code")

## NAMESPACE was introduced in R version 1.7.0 (Jan 2003)
## https://developer.r-project.org/170update.txt

cran_code %>% group_by(pkg_name, pub_year) %>% summarize(NS = sum(filename == "NAMESPACE")) %>% ungroup %>% mutate(has_ns = NS != 0) %>% collect -> all_pkgs

plan(multiprocess)

all_pkgs %>% filter(pub_year <= cfg$INCLUDE_YR) %>% mutate(yr = pub_year) %>% group_by(yr) %>% dplyr::group_nest() -> all_pkgs_nest

### DETECTING MISSING YRS

dir_ls("data") %>% str_subset(cfg$FX_DATA_PREFIX) %>% str_extract("[0-9]{4}") %>% as.numeric %>% setdiff(seq(1998,cfg$INCLUDE_YR), .) -> missing_yrs

##all_pkgs_nest$data

extract_fx <- function(pkgsdata, cfg) {
    pkgsdata %>% mutate(functions = map2(pkg_name, pub_year, extract_exported_functions, dbname = cfg$PATH_CODE_DB, verbose = FALSE)) -> res
    yr <- unique(pkgsdata$pub_year)
    saveRDS(res, paste0(cfg$FX_DATA_PREFIX, yr, cfg$FX_DATA_SUFFIX))
    return(yr)
}

all_pkgs_nest %>% filter(yr %in% missing_yrs) %>% pull(data) %>% future_map(extract_fx, .progress = TRUE, cfg = cfg)
