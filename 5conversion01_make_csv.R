require(modules)
require(fs)
require(tidyverse)

cfg <- modules::use("config.R")

readRDS(cfg$PATH_TARGET_META) %>% write.csv(fs::path("data", "target_meta.csv"))

## Run this if you have 1.2G to spare

## readRDS(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE) %>% as_tibble -> x

## x %>% mutate(id = paste(pkg_name, pub_year)) %>% unnest(function_feat) %>% mutate(null_col = map_lgl(function_feat, ~!is.null(.))) %>% filter(null_col) %>% unnest(function_feat) %>% select(-null_col) %>% write.csv(fs::path("data", "flatten_pkgs_functions_with_syntax_feature.csv"))
