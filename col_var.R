require(tidyverse)
require(modules)
require(rex)
require(furrr)
require(igraph)

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
naming_conv <- tibble(feature = c("dotted.case", "ALLUPPERCASE", "UpperCamelCase", "other", "alllowercase", "lowerCamelCase", "snake_case"),
                      long_name = c("dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel ", "lower_snake"))
poster_theme <- theme(plot.title = element_text(size = 24, face = "bold"), 
                      plot.subtitle =  element_text(size = 10), 
                      axis.text = element_text(size = 15), 
                      axis.title=element_text(size=14,face="bold"),
                      rect = element_rect(fill = "transparent")) 

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

plot_naming_among_pkg <- function(pkg_feat){
    data_naming <- pkg_feat %>% ungroup() %>% 
        mutate(rank_by_snake = snake_case) %>%
        pivot_longer(c(-pkg_name, -rank_by_snake), names_to = "feature", values_to = "percentage") %>% 
        mutate(percentage = percentage * 100) %>% 
        left_join(naming_conv, by = "feature") %>% 
        mutate(pkg_name = fct_reorder(pkg_name, rank_by_snake),
               long_name = fct_relevel(long_name, naming_conv$long_name))
    
    g_naming <- ggplot(data_naming, aes(y = percentage, x = pkg_name, fill = long_name)) + 
        geom_bar(stat="identity") + 
        labs(x = "package (years since first release)", y = "%") + 
        theme(legend.title = element_blank()) +
        coord_flip() + scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
        poster_theme 
    return(g_naming)
}

### Main
plan(multiprocess)
entro_res <- future_map(pkg$function_feat, safely(cal_entro), .progress = TRUE)
saveRDS(entro_res, cfg$PATH_PKG_ENTROPY)

# compute average entropy within packages
pkg$entro_res <- entro_res
pkg_ext <-  pkg %>% group_by(pkg_name) %>% mutate(earliest_release = min(pub_year), latest_release = max(pub_year))
pkg_latest <- pkg_ext %>% group_by(pkg_name) %>% filter(pub_year == latest_release) %>% mutate(age = 2019 - earliest_release) %>% ungroup()
avg_entro_pkg <- pkg_latest %>% 
    filter(map_lgl(entro_res, ~is.null(.$error))) %>% pull(entro_res) %>% map("result") %>% 
    do.call("rbind", .) %>% summarise_all(mean) %>% 
    mutate_at(vars(fx_name), ~ ./ log(length(style_regexes)+1)) %>% 
    mutate_at(vars(fx_assign:fx_tab), ~ ./log(2))

# plot within-package variation
data_entro <- avg_entro_pkg %>% 
    pivot_longer(everything(), names_to = "feature", values_to = "entropy") %>%
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

# plot the variation of names within packages of largest pagerank
pagerank <- read_rds(cfg$PATH_CRAN_GRAPH) %>% page_rank() %>% pluck("vector") %>% sort(decreasing = TRUE) %>% 
    tibble(pkg_name = names(.), pagerank = .) 
pkg_feat <- pkg_latest %>% 
    left_join(pagerank, by = c("pkg_name")) %>%  
    arrange(desc(pagerank)) %>% head(21) %>% 
    mutate(pkg_name = sprintf("%s (%s)", pkg_name, age)) %>% 
    select(pkg_name, function_feat) %>% 
    mutate(function_feat = map(function_feat, "result")) %>% 
    unnest(cols = c(function_feat)) %>% 
    mutate(fx_name = conv_style(fx_name, style_regexes)) %>% 
    group_by(pkg_name, fx_name) %>% summarise(n = n()) %>% 
    group_by(pkg_name) %>% mutate(ratio = n / sum(n)) %>% 
    pivot_wider(id_cols = c(pkg_name), names_from = fx_name, values_from = ratio) %>% 
    mutate_if(is.numeric, ~ replace(., is.na(.), 0)) 

filename <- str_glue(cfg$FOLDER_FUNC_OUTPUT, "within_pkg_naming_variation.png")
g_naming_most_popular <- plot_naming_among_pkg(pkg_feat)
ggsave(filename, plot = g_naming_most_popular, width = 10, height = 6, units = "in", bg = "transparent")

