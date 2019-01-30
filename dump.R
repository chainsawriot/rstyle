require(baaugwo)
require(tidyverse)
require(fs)
require(dbplyr)
require(RSQLite)

cran_meta <- readRDS('target_meta.RDS')

cran_meta %>% ungroup -> cran_meta

con <- dbConnect(RSQLite::SQLite(), "code.db")

read_pkg <- function(i, cran_meta, con) {
    print(paste(i, cran_meta$path[i]))
    x <- read_tarball(cran_meta$path[i])
    x$pub_year <- cran_meta$year[i]
    x$tarball <- cran_meta$path[i]
    x$pkg_name <- cran_meta$pkg_name[i]
    dbWriteTable(con, 'cran_code', x, append = TRUE)
    return(cran_meta$path[i])
}

map_chr(1:nrow(cran_meta), read_pkg, cran_meta = cran_meta, con = con)
dbDisconnect(con)
