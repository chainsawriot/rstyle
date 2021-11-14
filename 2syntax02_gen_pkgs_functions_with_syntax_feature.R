require(tidyverse)
require(rex)
require(fs)
require(modules)
require(furrr)

cfg <- modules::use("config.R")

dir_ls("data") %>% str_subset(cfg$SYNTAX_DATA_PREFIX) -> syntax_data_rds


pkg_functions <- purrr::map_dfr(syntax_data_rds[str_extract(syntax_data_rds, "[0-9]{4}") %in% 1998:cfg$INCLUDE_YR], ~ readRDS(.))

saveRDS(pkg_functions, cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)
