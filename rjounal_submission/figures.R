require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)
require(here)
require(modules)

cfg <- modules::use(here::here("config.R"))

read.csv(here::here("rjounal_submission", "tab1.csv"), header = TRUE) %>% knitr::kable(format = 'latex', caption = 'Three major style-guides: Google, Tidyverse and Bioconductor')

source(here::here('helpers.R'))

test <- readRDS(here::here(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE))

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

map_dfr(1998:cfg$INCLUDE_YR, cal_entro, data = test) %>% gather(key = 'feature', value = 'entropy', -pub_year) %>% filter(str_detect(feature, "ratio$")) %>% rename(share = 'entropy') %>% left_join(fx_name_trans, by = c('feature' = 'fx_name')) %>% select(-feature) %>% rename(feature = "full_name") %>% mutate(share = share * 100) %>% ggplot(aes(x = pub_year, y = share)) + geom_line() + facet_wrap(~feature) + scale_color_brewer(palette="Dark2") + xlab("Year") + ylab("Share of all functions (%)") +  theme(plot.title = element_text(size = 24), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 8, angle = 90), axis.title=element_text(size=10)) + theme(rect = element_rect(fill = "transparent")) + theme(legend.position = "none") -> figure1

ggsave(here::here("rjounal_submission", "fig1.pdf"), figure1, width = 5, height = 5)

######################

require(tidyverse)

fx_style <- readRDS(here::here(cfg$PATH_FX_STYLE_BY_YEAR))

tibble(style = c('dotted', 'allupper', 'upcamel', 'other', 'alllower', 'lowcamel', 'snake'), 
       long_name = c("dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake")) ->
    naming_conv

fx_style %>% mutate(alllower = alllower / total,
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
    theme(rect = element_rect(fill = "transparent")) + theme(strip.text.x = element_text(size = 10)) -> fig2

ggsave(here::here("rjounal_submission", "fig2.pdf"), fig2, width = 5, height = 4)

####

comment_dist <- readRDS(here::here(cfg$PATH_COMMENT_DIST))

### Number of lines analyzed by this project
comment_dist %>% filter(pub_year <= cfg$INCLUDE_YR) %>% ungroup %>% summarise(sum(n))

comment_dist %>% mutate(comment = ifelse(comment == 0, "Yes", "No")) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline)) %>% 
    select(pub_year, n_chars, prob, comment) %>%
    filter(n_chars > 40 & n_chars < 100 & pub_year %in% c(2003, 2008, 2013, 2019)) %>% mutate(prob = prob * 100) %>%
    ggplot(aes(x = n_chars, y = prob, color = comment)) + 
    geom_line(stat = 'identity') +
    geom_vline(xintercept = 80, alpha = 0.3) + facet_wrap(~ pub_year, ncol = 2) +
    xlab('Number of characters') + ylab('Share of all lines (%)') + scale_color_brewer(palette="Dark2") +
    theme(plot.title = element_text(size = 24), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 10), axis.title=element_text(size = 10)) + 
    theme(rect = element_rect(fill = "transparent")) + theme(strip.text.x = element_text(size = 10)) -> fig3

ggsave(here::here("rjounal_submission", "fig3.pdf"), fig3, width = 5, height = 4)

#####

require(igraph)
cran_graph <- read_rds("../cran_graph.RDS")
cran_wc <- membership(read_rds("../cran_community_20190518.RDS"))
cran_event <- evcent(cran_graph, direct = TRUE)$vector
node_info <- tibble(pkg_name = names(cran_wc), cran_wc, cran_event)
comm_lab <- tibble(community_ids = c(15, 9, 4, 
                      60, 14, 35, 
                      # 7,# error, shall be 1
                      36, 25, 39, 
                      23, 19, 31, 
                      8, 64, 73, 
                      18, 20, 120),
cols = c("RStudio-related", "base", "Image Plotting",
          "RCpp", "GPS and Geography", "Machine learning",
          # "public health and Statistics", # error
          "Text Analysis", "Social Network Analysis", "Graphics", 
          "Graph data structure", "Genetics", "Finance", 
          "Insurance and Actuary", "Numerical Optimization", "Sparse Matrix", 
          "Java", "Time, Date, and Money", "Neuroscience"))

node_info %>% filter(cran_wc %in% comm_lab$community_ids) %>% left_join(comm_lab, c('cran_wc' = 'community_ids')) %>% group_by(cols) %>% summarise(n = n()) %>% left_join((node_info %>% filter(cran_wc %in% comm_lab$community_ids) %>% left_join(comm_lab, c('cran_wc' = 'community_ids')) %>% group_by(cols) %>% filter(rank(desc(cran_event), ties.method = "first") <= 3) %>% arrange(cran_wc, desc(cran_event)) %>% summarise(top_pkg = paste(pkg_name, collapse = ", "))), by = 'cols') %>% arrange(desc(n)) %>% rename("Community" = cols, "Number of Packages" = n, "Top 3 Packages" = top_pkg) %>% knitr::kable(format = 'latex', caption = 'The largest 18 communities and their top 3 packages according to eigenvector centrality')

#####

comm_feat <- readRDS('../comm_lang_feature.RDS') %>% filter(comm_id != 1)
tibble(comm_id = membership(read_rds("../cran_community_20190518.RDS"))) %>% 
    group_by(comm_id) %>% summarise(n_mem = n()) %>% ungroup -> n_member
comm_feat %>% left_join(n_member, by = 'comm_id') -> comm_feat
tibble(feature = c('dotted', 'allupper', 'upcamel', 'other', 'alllower', 'lowcamel', 'snake'), 
       long_name = c("dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake")) ->
    naming_conv

comm_feat %>% select(alllower:comm_name) %>% 
    mutate(snake2 = snake) %>%
    gather("feature", "proportion", -comm_name, 
           -snake2) %>% left_join(naming_conv, by = 'feature') %>% 
    mutate(comm_name = fct_reorder(comm_name, snake2)) %>%
    mutate(long_name = fct_relevel(long_name, 
                                   "dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake")) %>%
    mutate(percentage = proportion * 100) %>%
    ggplot(aes(y = proportion, x = comm_name, fill = long_name)) + 
    geom_bar(stat="identity") + 
    labs(x = "", y = "Share of all functions") + 
    theme(legend.title = element_blank()) +
    coord_flip() + scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
    theme(plot.title = element_text(size = 24), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 10), axis.title=element_text(size = 10)) + 
    theme(rect = element_rect(fill = "transparent")) + theme(strip.text.x = element_text(size = 10)) -> fig4

ggsave('fig4.pdf', fig4, width = 5, height = 5)

####

comm_feat %>% select(comm_name, fx_assign:fx_tab) %>% 
    mutate(fx_opencurly2 = fx_opencurly) %>%
    gather('feature', 'proportion', -comm_name, -fx_opencurly2) %>%
    mutate(comm_name = fct_reorder(comm_name, fx_opencurly2)) %>%
    mutate(feature = fct_relevel(feature, "fx_opencurly")) %>%
    ggplot(aes(x = comm_name, y = proportion)) + geom_bar(stat = 'identity') +
    facet_grid(feature ~ ., switch="y", scales="free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          strip.text.y=element_text(angle=180)) + labs(x = "") + 
    scale_y_continuous("Share of all functions", position="right") -> fig5

ggsave('fig5.pdf', fig5, width = 5, height = 7)
