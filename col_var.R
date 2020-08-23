require(tidyverse)
require(modules)
require(rex)
require(furrr)

### local variables
cfg <- modules::use("config.R")
pkg <- readRDS(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)
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


### functions
match_function_style <- function(x, style_regexes) {
    res <- map_lgl(style_regexes, ~ str_detect(x, .))
    if (sum(res) == 0) {
        return("other")
    }
    style <- names(style_regexes)[min(which(res))]
    return(style)
}

conv_style <- function(x, style_regexes) {
    x <- x[!is.na(x) & !is.null(x)]
    styles <- map_chr(x, match_function_style, style_regexes = style_regexes)
}

entro <- function(x) {
    -sum(sapply(as.vector(table(x) / length(x)), function(z) z * log(z)))
}

cal_entro <- function(function_feat) {
    res <- function_feat$result %>% mutate(fx_name = conv_style(fx_name, style_regexes)) %>% summarise_all(entro)
    return(res)
}

### Main
plan(multiprocess)
entro_res <- future_map(pkg$function_feat, safely(cal_entro), .progress = TRUE)
saveRDS(entro_res, cfg$PATH_PKG_ENTROPY)

pkg$entro_res <- entro_res
avg_entro_pkg <- pkg %>% 
    filter(map_lgl(entro_res, ~is.null(.$error))) %>% pull(entro_res) %>% map("result") %>% 
    do.call("rbind", .) %>% summarise_all(mean) %>% 
    mutate_at(vars(fx_name), ~ ./ log(length(style_regexes)+1)) %>% 
    mutate_at(vars(fx_assign:fx_tab), ~ ./log(2))


data_entro <- avg_entro_pkg %>% 
    gather(key = "feature", value = "entropy") %>%
    arrange(desc(entropy)) %>% 
    mutate(feature = fct_reorder(feature, entropy))
data_entro

filename <- str_glue(cfg$FOLDER_FUNC_OUTPUT, "within_pkg_feature_entropy.png")
g_entro <- ggplot(data =  data_entro, aes(feature, entropy)) + 
    geom_col() + 
    ylim(0, 1) + xlab("") + 
    geom_text(aes(label = sprintf("%s", round(entropy, 2))), position = position_dodge(width=1), hjust = 0) + 
    coord_flip() 
ggsave(filename, plot = g_entro, width = 10, height = 6, units = "in", bg = "transparent")
