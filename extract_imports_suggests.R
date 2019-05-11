require(tidyverse)
require(fs)
require(baaugwo)
require(lubridate)
require(purrr)


target_meta <- readRDS('target_meta.RDS')

vread <- function(path, fields = c("Imports")) {
    print(path)
    return(read_tarball_meta(path, fields))
}

descs <- map(target_meta$path, safely(vread))

#todo: there is no ./cran folder here
transpose(descs)$result %>% map_chr(~ ifelse(is.null(.), NA, .)) -> target_meta$imports

saveRDS(target_meta, "target_meta.RDS")
