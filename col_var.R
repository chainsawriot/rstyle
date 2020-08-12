require(tidyverse)
require(modules)

cfg <- modules::use("config.R")

pkg <- readRDS(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)

pkg$function_feat[[8]]$result %>% select() %>% summarise_all(var) %>% t -> res

mean(res[,1])

max(table(factor(c(1,2,3,4,5,1))) / 6)

conv_style <- function(x, style_regexes) {
    x <- x[!is.na(x) & !is.null(x)]
    styles <- map_chr(x, match_function_style, style_regexes = style_regexes)
}

## Stole the regexes from the lintr package.
## rearrange the order for precedence.
require(rex)
style_regexes <- list(
    "alllowercase"   = rex(start, one_or_more(rex(one_of(lower, digit))), end),
    "ALLUPPERCASE"   = rex(start, one_or_more(rex(one_of(upper, digit))), end),
    "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
    "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
    "snake_case"     = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more("_", one_or_more(rex(one_of(lower, digit)))), end),
    "dotted.case"    = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more(dot, one_or_more(rex(one_of(lower, digit)))), end)
)


match_function_style <- function(x, style_regexes) {
    res <- map_lgl(style_regexes, ~ str_detect(x, .))
    if (sum(res) == 0) {
        return("other")
    }
    names(style_regexes)[min(which(res))]
}


x <- conv_style(pkg$function_feat[[8]]$result$fx_name, style_regexes)

entro <- function(x) {
    -sum(sapply(as.vector(table(x) / length(x)), function(z) z * log(z)))
}

pkg$function_feat[[11212]]$result %>% mutate(fx_name = conv_style(fx_name, style_regexes)) %>% summarise_all(entro) -> res

cal_entro <- function(function_feat) {
    function_feat$result %>% mutate(fx_name = conv_style(fx_name, style_regexes)) %>% summarise_all(entro) -> res
    return(res)
}
require(furrr)
plan(multiprocess)
future_map(pkg$function_feat, safely(cal_entro), .progress = TRUE) -> entro_res
saveRDS(entro_res, "entro_res.RDS")
pkg$entro_res <- entro_res

pkg %>% filter(map_lgl(entro_res, ~is.null(.$error))) %>% pull(entro_res) %>% map("result") -> res

res %>% do.call("rbind", .) -> res2
