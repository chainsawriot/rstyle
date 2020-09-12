require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)
require(here)
require(modules)
require(rex)

cfg <- modules::use(here::here("config.R"))

#########
# Tab 1
#########
tab1 <- read.csv(here::here("rjournal_submission", "tab1.csv"), header = TRUE) %>% 
    knitr::kable(format = 'latex', caption = 'Three major style-guides: Google, Tidyverse and Bioconductor')
tab1

#########
# Tab 2
#########
comm_size <- readRDS(here::here(cfg$PATH_COMM_SIZE))
comm_name <- read.csv(here::here(cfg$PATH_COMM_NAME), header = TRUE)
tab2 <- comm_size %>% 
    left_join(comm_name, by = c("comm_id")) %>% 
    select(comm_name, n_mem, top) %>% 
    rename(Community = comm_name, Number_of_Packages = n_mem, Top_3_Packages = top) %>% 
    head(20) %>% 
    knitr::kable(format = 'latex', caption = 'The largest 20 communities and their top 3 packages according to PageRank')
tab2

source(here::here('helpers.R'))

#########
# Fig 1
#########

ent_cal <- function(x) {
    base <- length(x)
    group <- c(sum(x) / base, sum(!x) / base)
    group <- Filter(function(x) x != 0, group)
    res <-  sum(sapply(group, function(x) x * log(x)))
    return(-res)
}

ent_cal <- function(x) {
    base <- length(x)
    group <- c(sum(x) / base, sum(!x) / base)
    group <- Filter(function(x) x != 0, group)
    res <-  sum(sapply(group, function(x) x * log(x)))
    return(-res)
}

ratio <- function(x) {
    return(sum(x) / length(x))
}

cal_entro <- function(yr, data) {
    data %>% filter(pub_year == yr) %>% pull(function_feat) %>% map("result") %>% Filter(Negate(is.null), .) %>% bind_rows() %>% summarise_at(vars(fx_assign:fx_tab), list("entropy" = ent_cal, "ratio" = ratio)) %>% mutate(pub_year = yr)
}

fx_name_trans <- tibble(fx_name = c("fx_assign_ratio", "fx_opencurly_ratio", "fx_infix_ratio",  "fx_integer_ratio", "fx_singleq_ratio", "fx_commas_ratio", "fx_semi_ratio", "fx_t_f_ratio", "fx_closecurly_ratio", "fx_tab_ratio"), full_name = c("= as assignment", "{ on own line", "Infix no spaces", "Not type integers", "' for strings", "No space after ,", "; to terminate lines", "Use T/F", "} not on own line", "Tab to indent"))

data1 <- readRDS(here::here(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)) %>% 
    map_dfr(1998:cfg$INCLUDE_YR, cal_entro, data = .) %>% 
    gather(key = 'feature', value = 'entropy', -pub_year) %>% 
    filter(str_detect(feature, "ratio$")) %>% rename(share = 'entropy') %>% 
    left_join(fx_name_trans, by = c('feature' = 'fx_name')) %>% 
    mutate(full_name = paste0(str_replace(feature, "_ratio$", ""), ": ", full_name)) %>% 
    select(-feature) %>% rename(feature = "full_name") %>% 
    mutate(share = share * 100) 

fig1 <- ggplot(data=data1, aes(x = pub_year, y = share)) + geom_line() + facet_wrap(~feature, ncol = 2) + scale_color_brewer(palette="Dark2") + xlab("Year") + ylab("Share of all functions (%)") +  theme(plot.title = element_text(size = 24), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 8, angle = 90), axis.title=element_text(size=10)) + theme(rect = element_rect(fill = "transparent")) + theme(legend.position = "none") 

ggsave(here::here("rjournal_submission", "fig1.pdf"), figure1, width = 5, height = 5)

#########
# Fig 2
#########

require(tidyverse)
require(fs)
require(rex)
require(furrr)

### What are the "others"?

match_function_style <- function(x, style_regexes) {
    res <- map_lgl(style_regexes, ~ str_detect(x, .))
    if (sum(res) == 0) {
        return("other")
    }
    names(style_regexes)[min(which(res))]
}

fx_data_rds <- dir_ls(here::here("data")) %>% str_subset(cfg$FX_DATA_PREFIX) 

set.seed(42)
pkg_functions <- purrr::map_dfr(fx_data_rds[str_extract(fx_data_rds, "[0-9]{4}") %in% 1998:cfg$INCLUDE_YR], ~ readRDS(.)) %>% sample_n(150)

## Stole the regexes from the lintr package.
## rearrange the order for precedence.

match_function_style <- function(x, style_regexes) {
    res <- map_lgl(style_regexes, ~ str_detect(x, .))
    if (sum(res) == 0) {
        return("other")
    }
    names(style_regexes)[min(which(res))]
}

style_regexes <- list(
    "alllowercase"   = rex(start, one_or_more(rex(one_of(lower, digit))), end),
    "ALLUPPERCASE"   = rex(start, one_or_more(rex(one_of(upper, digit))), end),
    "UpperCamelCase" = rex(start, upper, zero_or_more(alnum), end),
    "lowerCamelCase" = rex(start, lower, zero_or_more(alnum), end),
    "snake_case"     = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more("_", one_or_more(rex(one_of(lower, digit)))), end),
    "dotted.case"    = rex(start, one_or_more(rex(one_of(lower, digit))), zero_or_more(dot, one_or_more(rex(one_of(lower, digit)))), end)
)

## ### adherence to Zeitgeist

## test %>% filter(pub_year == 2019) %>% pull(function_feat) %>% map("result") %>% bind_rows %>% as_tibble %>% mutate(styles = future_map_chr(fx_name, match_function_style, style_regexes = style_regexes, .progress = TRUE)) -> data2019
## saveRDS(data2019, here::here("rjournal_submission", "data2019.RDS"))

####################
# PROBLEMATIC
# TODO: Hong please check where the bug is

readRDS(here::here("rjournal_submission", "data2019.RDS")) %>% 
    count(styles == "snake_case" & !fx_assign & !fx_opencurly & !fx_infix & fx_integer & !fx_singleq & !fx_commas & !fx_semi & !fx_t_f & !fx_closecurly & !fx_tab) %>% 
    add_count(wt = n) %>% 
    mutate(p = n / nn)

plan(multiprocess)

pkg_functions <- pkg_functions %>%
    mutate(styles = future_map(functions, conv_style, style_regexes = style_regexes, .progress = TRUE)) 

pkg_sample <- pkg_functions %>% mutate(fxl = map_int(styles, length)) %>% 
    filter(fxl > 0) 

tibble(fun_name = unlist(pkg_sample$functions), fun_style = unlist(pkg_sample$styles)) %>% filter(fun_style == "other") %>% sample_n(10) %>% pull(fun_name) %>% paste(collapse = ", ")
####################


fx_style <- readRDS(here::here(cfg$PATH_FX_STYLE_BY_YEAR))

naming_conv <- tibble(style = c('dotted', 'allupper', 'upcamel', 'other', 'alllower', 'lowcamel', 'snake'), 
       long_name = c("dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake"))

fx_style %>% mutate(alllower = alllower / total,
                    allupper = allupper / total,
                    upcamel = upcamel / total,
                    lowcamel = lowcamel / total,
                    snake = snake / total,
                    dotted = dotted / total,
                    other = other / total) %>%
    select(-total) %>% filter(pub_year == cfg$INCLUDE_YR)

fig2 <- fx_style %>% 
    mutate(alllower = alllower / total,
           allupper = allupper / total,
           upcamel = upcamel / total,
           lowcamel = lowcamel / total,
           snake = snake / total,
           dotted = dotted / total,
           other = other / total) %>%
    select(-total) %>% 
    gather(key = 'style', value = 'share', -pub_year) %>% filter(pub_year <= cfg$INCLUDE_YR) %>%
    left_join(naming_conv, by = 'style') %>% 
    mutate(opacity = ifelse(style %in% c('dotted', 'snake', 'lowcamel', 'upcamel'), 0.8, 0.4)) %>%
    mutate(long_name = fct_relevel(long_name, 
                                   "dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake")) %>%
    rename("Naming" = long_name) %>%
    mutate(percentage = share * 100) %>%
    ggplot(aes(x = pub_year, y = share, col = Naming)) + 
    geom_line() + scale_color_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
    xlab("Year") + ylab("Share of all functions") + 
    theme(plot.title = element_text(size = 24), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 10), axis.title=element_text(size = 10)) + 
    theme(rect = element_rect(fill = "transparent")) + theme(strip.text.x = element_text(size = 10)) 

ggsave(here::here("rjournal_submission", "fig2.pdf"), fig2, width = 5, height = 4)

#########
# Fig 3
#########

comment_dist <- readRDS(here::here(cfg$PATH_COMMENT_DIST))

### Number of lines analyzed by this project
comment_dist %>% filter(pub_year <= cfg$INCLUDE_YR) %>% ungroup %>% summarise(sum(n))

fig3 <- comment_dist %>% mutate(comment = ifelse(comment == 0, "Yes", "No")) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline)) %>% 
    select(pub_year, n_chars, prob, comment) %>%
    filter(n_chars > 40 & n_chars < 100 & pub_year %in% c(2003, 2008, 2013, 2019)) %>% mutate(prob = prob * 100) %>%
    ggplot(aes(x = n_chars, y = prob, color = comment)) + 
    geom_line(stat = 'identity') +
    geom_vline(xintercept = 80, alpha = 0.3) + facet_wrap(~ pub_year, ncol = 2) +
    xlab('Number of characters') + ylab('Share of all lines (%)') + scale_color_brewer(palette="Dark2") +
    theme(plot.title = element_text(size = 24), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 10), axis.title=element_text(size = 10)) + 
    theme(rect = element_rect(fill = "transparent")) + theme(strip.text.x = element_text(size = 10)) 

ggsave(here::here("rjournal_submission", "fig3.pdf"), fig3, width = 5, height = 4)

#########
# Fig 4
#########

require(igraph)

plot_naming_among_comm <- function(comm_feat, naming_conv){
    poster_theme <- theme(plot.title = element_text(size = 24, face = "bold"), 
                          plot.subtitle =  element_text(size = 10), 
                          axis.text = element_text(size = 15), 
                          axis.title=element_text(size=14,face="bold"),
                          rect = element_rect(fill = "transparent")) 
    
    data_naming <- comm_feat %>% select(comm_id, comm_name, alllower:other) %>% 
        mutate(rank_by_snake = snake) %>%
        gather("feature", "proportion", -comm_id, -comm_name, -rank_by_snake) %>%
        left_join(naming_conv, by = "feature") %>% 
        mutate(comm_name = fct_reorder(comm_name, rank_by_snake),
               long_name = fct_relevel(long_name, naming_conv$long_name)) %>%
        mutate(percentage = proportion * 100) 
    
    g_naming <- ggplot(data_naming, aes(y = percentage, x = comm_name, fill = long_name)) + 
        geom_bar(stat="identity") + 
        labs(x = "", y = "%") + 
        theme(legend.title = element_blank()) +
        coord_flip() + scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
        poster_theme 
    return(g_naming)
}

comm_feat <- readRDS(here::here(cfg$PATH_COMM_LARGEST_FEATURES))
naming_conv <- read.csv(here::here(cfg$PATH_NAMING_CONVENTION), stringsAsFactors = FALSE)
fig4 <- plot_naming_among_comm(comm_feat, naming_conv)
ggsave(here::here("rjournal_submission", "fig4.pdf"), plot = fig4, width = 5, height = 5)

#########
# Fig 5
#########
fig5 <- comm_feat %>% select(comm_name, fx_assign:fx_tab) %>% 
    mutate(fx_opencurly2 = fx_opencurly) %>%
    gather('feature', 'proportion', -comm_name, -fx_opencurly2) %>%
    mutate(comm_name = fct_reorder(comm_name, fx_opencurly2)) %>%
    mutate(feature = fct_relevel(feature, "fx_opencurly")) %>%
    ggplot(aes(x = comm_name, y = proportion)) + geom_bar(stat = 'identity') +
    facet_grid(feature ~ ., switch="y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text.y=element_text(angle=180)) + labs(x = "") + 
    scale_y_continuous("Share of all functions", position="right") 

ggsave(here::here("rjournal_submission", "fig5.pdf"), fig5, width = 5, height = 7)

#########
# Fig 6
#########
pkg <- readRDS(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)
pkg$entro_res <- entro_res <- readRDS(cfg$PATH_PKG_ENTROPY)
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

fig6 <- ggplot(data =  data_entro, aes(feature, entropy)) + 
    geom_col() + 
    ylim(0, 1) + xlab("") + 
    geom_text(aes(label = sprintf("%s", round(entropy, 2))), position = position_dodge(width=1), hjust = 0) + 
    coord_flip() 
ggsave(here::here("rjournal_submission", "fig6.pdf"), fig6, width = 5, height = 5)

#########
# Fig 7
#########
plot_naming_among_pkg <- function(pkg_feat){
    poster_theme <- theme(plot.title = element_text(size = 24, face = "bold"), 
                          plot.subtitle =  element_text(size = 10), 
                          axis.text = element_text(size = 15), 
                          axis.title=element_text(size=14,face="bold"),
                          rect = element_rect(fill = "transparent")) 
    
    naming_conv <- tibble(feature = c("dotted.case", "ALLUPPERCASE", "UpperCamelCase", "other", "alllowercase", "lowerCamelCase", "snake_case"),
                          long_name = c("dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel ", "lower_snake"))
    
    data_naming <- pkg_feat %>% ungroup() %>% 
        mutate(rank_by_snake = snake_case) %>%
        pivot_longer(c(-pkg_name, -rank_by_snake), names_to = "feature", values_to = "percentage") %>% 
        mutate(percentage = percentage * 100) %>% 
        left_join(naming_conv, by = "feature") %>% 
        mutate(pkg_name = fct_reorder(pkg_name, rank_by_snake),
               long_name = fct_relevel(long_name, naming_conv$long_name))
    
    g_naming <- ggplot(data_naming, aes(y = percentage, x = pkg_name, fill = long_name)) + 
        geom_bar(stat="identity") + 
        labs(x = "", y = "%") + 
        theme(legend.title = element_blank()) +
        coord_flip() + scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
        poster_theme 
    return(g_naming)
}
pagerank <- read_rds(cfg$PATH_CRAN_GRAPH) %>% page_rank() %>% pluck("vector") %>% sort(decreasing = TRUE) %>% 
    tibble(pkg_name = names(.), pagerank = .) 
pkg_feat <- pkg_latest %>% 
    left_join(pagerank, by = c("pkg_name")) %>%  
    arrange(desc(pagerank)) %>% head(21) %>% 
    select(pkg_name, function_feat) %>% 
    mutate(function_feat = map(function_feat, "result")) %>% 
    unnest(cols = c(function_feat)) %>% 
    mutate(fx_name = conv_style(fx_name, style_regexes)) %>% 
    group_by(pkg_name, fx_name) %>% summarise(n = n()) %>% 
    group_by(pkg_name) %>% mutate(ratio = n / sum(n)) %>% 
    pivot_wider(id_cols = c(pkg_name), names_from = fx_name, values_from = ratio) %>% 
    mutate_if(is.numeric, ~ replace(., is.na(.), 0)) 

fig7 <- plot_naming_among_pkg(pkg_feat)
ggsave(here::here("rjournal_submission", "fig7.pdf"), fig7, width = 5, height = 5)

