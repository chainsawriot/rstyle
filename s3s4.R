require(tidyverse)
require(modules)
cfg <- modules::use("config.R")
source('helpers.R')

pkg_functions <- readRDS(cfg$PATH_TARGET_META)

dbname <- "code.db"
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)
cran_code <- tbl(con, "cran_code")
cran_code %>% filter(filename %in% c("NAMESPACE", "DESCRIPTION")) %>% collect() -> pkg_ns

check_oo <- function(target_pkg_name, target_pub_year, pkg_ns) {
    pkg_ns %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename %in% c("NAMESPACE", "DESCRIPTION")) %>% select(filename, code) -> pkg_ns2
    pkg_ns2 %>% filter(filename == "NAMESPACE") %>% pull(code) %>% str_detect("^S3method") %>% any -> s3
    pkg_ns2 %>% filter(filename == "NAMESPACE") %>% pull(code) %>% str_detect("^exportMethods") %>% any -> s4
    pkg_ns2 %>% filter(filename == "DESCRIPTION") %>% pull(code) %>% textConnection() %>% read.dcf(all = TRUE) -> des_file
    "R6" %in% des_file$Imports -> r6
    tibble(pkg_name = target_pkg_name, pub_year = target_pub_year, s3, s4, r6)
}

require(furrr)
plan(multiprocess)

res <- future_map2(pkg_functions$pkg_name, pkg_functions$year, safely(check_oo), pkg_ns = pkg_ns, .progress = TRUE)

saveRDS(res, cfg$OO_DATA)
