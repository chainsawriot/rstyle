require(tidyverse)
require(igraph)
pkgs <- readRDS("pkgs_functions_with_syntax_feature.RDS")
comm <- readRDS("cran_community_20190518.RDS")

membership(comm)[membership(comm) == 15] %>% names -> target_pkgs

pkgs %>% filter(pub_year <= 2018) %>% 
    filter(pkg_name %in% target_pkgs) %>% 
    group_by(pkg_name) %>% 
    top_n(1, wt = pub_year) %>% 
    ungroup %>% select(function_feat) %>%
    pull %>% map("result") %>% Filter(Negate(is.null), .) %>%
    map_dfr(function(x) x) %>% 
    summarise_at(vars(fx_assign:fx_tab), sum) %>% t -> res1


membership(comm)[membership(comm) == 36] %>% names -> target_pkgs

pkgs %>% filter(pub_year <= 2018) %>% 
    filter(pkg_name %in% target_pkgs) %>% 
    group_by(pkg_name) %>% 
    top_n(1, wt = pub_year) %>% 
    ungroup %>% select(function_feat) %>%
    pull %>% map("result") %>% Filter(Negate(is.null), .) %>%
    map_dfr(function(x) x) %>% 
    summarise_at(vars(fx_assign:fx_tab), sum) %>% t -> res2

x <- cbind((res2 / 4237) * 100, (res1 / 129891) * 100)
colnames(x) <- c('text', 'rstudio')
require(rex)

style_regexes <- list(
    "alllowercase"   = rex(start, one_or_more(rex(one_of(lower, digit))), end),
    "ALLUPPERCASE"   = rex(start, one_or_more(rex(one_of(upper, digit))), end),
    "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
    "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
    "snake_case"     = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more("_", one_or_more(rex(one_of(lower, digit)))), end),
    "dotted.case"    = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more(dot, one_or_more(rex(one_of(lower, digit)))), end)
)


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

pkgs %>% filter(pub_year <= 2018) %>% 
    filter(pkg_name %in% target_pkgs) %>% 
    group_by(pkg_name) %>% 
    top_n(1, wt = pub_year) %>% 
    ungroup %>% select(function_feat) %>%
    pull %>% map("result") %>% Filter(Negate(is.null), .) %>%
    map_dfr(function(x) x) %>%
    mutate(styles = map(fx_name, conv_style, style_regexes = style_regexes)) %>%
    summarise(total = length(unlist(styles)),
              alllower = sum(unlist(styles) == "alllowercase"),
              allupper = sum(unlist(styles) == "ALLUPPERCASE"),
              upcamel = sum(unlist(styles) == "UpperCamelCase"),
              lowcamel = sum(unlist(styles) == "lowerCamelCase"),
              snake = sum(unlist(styles) == "snake_case"),
              dotted = sum(unlist(styles) == "dotted.case"),
              other = sum(unlist(styles) == "other"))


