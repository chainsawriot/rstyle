require(tidyverse)
require(fs)
require(baaugwo)
require(lubridate)


target_meta <- readRDS('target_meta.RDS')

vread <- function(path, fields = c("Description")) {
    print(path)
    return(read_tarball_meta(path, fields))
}

descs <- map(target_meta$path, safely(vread))

transpose(descs)$result %>% map_chr(~ ifelse(is.null(.), NA, .)) -> target_meta$desc

saveRDS(target_meta, "target_meta.RDS")
