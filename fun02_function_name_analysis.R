require(tidyverse)
require(stingr)
require(rex)
require(fs)
require(modules)
require(furrr)

cfg <- modules::use("config.R")


match_function_style <- function(x, style_regexes) {
    res <- map_lgl(style_regexes, ~ str_detect(x, .))
    if (sum(res) == 0) {
        return("other")
    }
    names(style_regexes)[min(which(res))]
}

dir_ls("data") %>% str_subset(cfg$FX_DATA_PREFIX) -> fx_data_rds


pkg_functions <- purrr::map_dfr(fx_data_rds[str_extract(fx_data_rds, "[0-9]{4}") %in% 1998:cfg$INCLUDE_YR], ~ readRDS(.))

conv_style <- function(x, style_regexes) {
    x <- x[!is.na(x) & !is.null(x)]
    styles <- map_chr(x, match_function_style, style_regexes = style_regexes)
}

## Stole the regexes from the lintr package.
## rearrange the order for precedence.

style_regexes <- list(
    "alllowercase"   = rex(start, one_or_more(rex(one_of(lower, digit))), end),
    "ALLUPPERCASE"   = rex(start, one_or_more(rex(one_of(upper, digit))), end),
    "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
    "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
    "snake_case"     = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more("_", one_or_more(rex(one_of(lower, digit)))), end),
    "dotted.case"    = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more(dot, one_or_more(rex(one_of(lower, digit)))), end)
)
plan(multiprocess)


### R IS SLOW!!!!
pkg_functions %>% group_by(pub_year) %>%
    mutate(styles = future_map(functions, conv_style, style_regexes = style_regexes, .progress = TRUE)) %>%
    summarise(total = length(unlist(styles)),
              alllower = sum(unlist(styles) == "alllowercase"),
              allupper = sum(unlist(styles) == "ALLUPPERCASE"),
              upcamel = sum(unlist(styles) == "UpperCamelCase"),
              lowcamel = sum(unlist(styles) == "lowerCamelCase"),
              snake = sum(unlist(styles) == "snake_case"),
              dotted = sum(unlist(styles) == "dotted.case"),
              other = sum(unlist(styles) == "other")) -> style_by_year

saveRDS(style_by_year, cfg$PATH_FX_STYLE_BY_YEAR)
