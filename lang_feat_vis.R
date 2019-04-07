require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)

source('helpers.R')

test <- readRDS('pkgs_functions_with_syntax_feature.RDS')

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
    data %>% filter(pub_year == yr) %>% pull(function_feat) %>% map("result") %>% Filter(Negate(is.null), .) %>% bind_rows() %>% summarise_at(vars(fx_assign:fx_tab), funs("entropy" = ent_cal, "ratio" = ratio)) %>% mutate(pub_year = yr)
}

res_entropy  <- map_dfr(1998:2018, cal_entro, data = test)

res_entropy %>% gather(key = 'feature', value = 'entropy', -pub_year) %>% filter(str_detect(feature, "entropy$")) %>% ggplot(aes(x = pub_year, y = entropy)) + geom_line() + facet_wrap(~feature)
ggsave('lang_feature.png')

res_entropy %>% gather(key = 'feature', value = 'entropy', -pub_year) %>% filter(str_detect(feature, "ratio$")) %>% ggplot(aes(x = pub_year, y = entropy)) + geom_line() + facet_wrap(~feature)
ggsave('lang_feature_ratio.png')