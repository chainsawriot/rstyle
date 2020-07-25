rm(list=ls())
require(tidyverse)
require(igraph)
require(rex)
require(datasets)
require(dplyr)
require(purrr)
require(stringr)
require(iterators)
cfg <- modules::use("config.R")

# local variables
pkgs <- readRDS(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)
comm <- readRDS(cfg$PATH_COMM)
comm_size <- read_rds(cfg$PATH_COMM_SIZE)
style_regexes <- list(
    "alllowercase"   = rex(start, one_or_more(rex(one_of(lower, digit))), end),
    "ALLUPPERCASE"   = rex(start, one_or_more(rex(one_of(upper, digit))), end),
    "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
    "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
    "snake_case"     = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more("_", one_or_more(rex(one_of(lower, digit)))), end),
    "dotted.case"    = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more(dot, one_or_more(rex(one_of(lower, digit)))), end)
)

### functions
conv_style <- function(x, style_regexes) {
    x <- x[!is.na(x) & !is.null(x)]
    styles <- map_chr(x, match_function_style, style_regexes = style_regexes)
}

match_function_style <- function(x, style_regexes) {
    res <- map_lgl(style_regexes, ~ str_detect(x, .))
    if (sum(res) == 0) {
        return("other")
    }
    names(style_regexes)[min(which(res))]
}

get_target_pkgs <- function(x) {
    membership(comm)[membership(comm) == x] %>% names -> target_pkgs
    return(target_pkgs)
}

ratio <- function(x) {
    return(sum(x) / length(x))
}

get_feature_table_from_pkgs <- function(comm_id, return_res = FALSE) {
    print(comm_id)
    target_pkgs <- get_target_pkgs(comm_id)
        pkgs %>% filter(pub_year <= 2018) %>% 
        filter(pkg_name %in% target_pkgs) %>% 
        group_by(pkg_name) %>% 
        top_n(1, wt = pub_year) %>% 
        ungroup %>% select(function_feat) %>%
        pull %>% map("result") %>% Filter(Negate(is.null), .) %>%
        bind_rows -> res
    if (return_res) {
        return(res)
    }
    res %>% summarise_at(vars(fx_assign:fx_tab), ratio) -> binratio
    res %>% mutate(styles = map(fx_name, conv_style, style_regexes = style_regexes)) %>%
        summarise(alllower = ratio(unlist(styles) == "alllowercase"),
                  allupper = ratio(unlist(styles) == "ALLUPPERCASE"),
                  upcamel = ratio(unlist(styles) == "UpperCamelCase"),
                  lowcamel = ratio(unlist(styles) == "lowerCamelCase"),
                  snake = ratio(unlist(styles) == "snake_case"),
                  dotted = ratio(unlist(styles) == "dotted.case"),
                  other = ratio(unlist(styles) == "other")) -> nameratio
    bind_cols(tibble(comm_id = comm_id), binratio, nameratio)
}


### main
### build two fx feature tables for all communities
### df_total: counting
### df_ratio_total: ratio


largest_comm <- comm_size %>% filter(rank <= cfg$MAX_NUM_COMM_TO_ANALYZE)

res <- map_dfr(largest_comm$comm_id, get_feature_table_from_pkgs)
res$comm_name <- largest_comm$top
res$n_mem <- largest_comm$n_mem
saveRDS(res, cfg$PATH_COMM_LANG_FEATURES)
