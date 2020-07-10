require(tidyverse)
require(fs)
require(baaugwo)
require(lubridate)

vread <- function(path) {
    print(path)
    return(read_tarball_meta(path))
}

tarballs <- dir_info("./cran/contrib/main/", regexp = "tar\\.gz$", recursive = TRUE) %>% select(path, modification_time)
pkg_name <- map(tarballs$path, safely(vread))

transpose(pkg_name)$result %>% map_chr(~ ifelse(is.null(.), NA, .)) -> tarballs$pkg_name

saveRDS(tarballs, 'pkg_meta.RDS')

### Re-runing the extraction for those failed packages
pkg_meta <- readRDS("pkg_meta.RDS")
pkg_meta %>% filter(is.na(pkg_name)) -> missing_pkg_name_meta

missing_pkg_name <- map(missing_pkg_name_meta$path, safely(vread))
transpose(missing_pkg_name)$result %>% map_chr(~ ifelse(is.null(.), NA, .)) -> missing_pkg_name_meta$pkg_name

pkg_meta %>% filter(!is.na(pkg_name)) %>% bind_rows(missing_pkg_name_meta) %>% filter(!is.na(pkg_name) & !grepl(" ", pkg_name)) -> final_meta

saveRDS(final_meta, "final_meta.RDS")


### sampling: for each package, one random submission per year.
## This one is NOT reproducible because randomization is involved.
## Update: 24-07-2019: saved random seed.
final_meta <- readRDS("final_meta.RDS")
set.seed(46709394)
final_meta  %>% mutate(year = year(modification_time)) %>% group_by(pkg_name, year) %>% sample_n(size = 1) -> target_meta
saveRDS(target_meta, 'target_meta.RDS')
