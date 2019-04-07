require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)

## We should package the functions in helpers.R into a package.
source('helpers.R')

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "code.db")
cran_code <- tbl(con, "cran_code")



## NAMESPACE was introduced in R version 1.7.0 (Jan 2003)
## https://developer.r-project.org/170update.txt

cran_code %>% group_by(pkg_name, pub_year) %>% summarize(NS = sum(str_detect(filename, "NAMESPACE"))) %>% ungroup %>% mutate(has_ns = NS != 0) %>% collect -> all_pkgs

### Benchmark

## all_pkgs %>% sample_n(20) -> test_cases
## system.time({
##     test_cases %>% mutate(functions = future_map2(pkg_name, pub_year, extract_exported_functions, dbname = 'code.db', verbose = FALSE, .progress = TRUE)) -> test_cases
## })

## system.time({
##     test_cases %>% mutate(functions = future_map2(pkg_name, pub_year, extract_exported_functions_old, dbname = 'code.db', verbose = FALSE, .progress = TRUE)) -> test_cases
## })

## 39 secs versus 122.79 -> about 3-fold reduction...

plan(multiprocess)

all_pkgs %>% mutate(functions = future_map2(pkg_name, pub_year, extract_exported_functions, dbname = 'code.db', verbose = FALSE, .progress = TRUE)) -> all_pkgs
saveRDS(all_pkgs, "pkgs_functions.RDS")

## inlinedocs 2013
## uniah 2015
## coin 2009
## coin 2006

## 'EBMAforecast', 2016

## Amsterdam style test case
# pheatmap 2012

## no ns
# ade4 2010

## Other test case
## Rpad 2006
## giRaph 2007 <- I give up on this
## Excecessive warnings
## Nlp 2017

## using exportPattern

## Genius style
# weirs 2012



## NOTE: some smart people put `` around function names
