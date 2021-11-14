require(dbplyr)
require(tidyverse)
require(modules)

cfg <- modules::use("config.R")

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = cfg$PATH_CODE_DB)

cran_code <- tbl(con, "cran_code")

##cran_code %>% filter(filename != "NAMESPACE" & filename != "DESCRIPTION" & code %like% "#") %>% mutate(n_chars = nchar(code)) %>% group_by(pub_year, n_chars) %>% tally -> res


cran_code %>% filter(filename != "NAMESPACE" & filename != "DESCRIPTION") %>% mutate(n_chars = nchar(code), comment = str_sub(code, 1, 1)) %>% mutate(comment = comment != "#") %>% group_by(pub_year, n_chars, comment) %>% tally -> res
comment_dist <- collect(res)
saveRDS(comment_dist, cfg$PATH_COMMENT_DIST)
