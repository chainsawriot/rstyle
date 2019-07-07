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
    data %>% filter(pub_year == yr) %>% 
        pull(function_feat) %>% map("result") %>% 
        Filter(Negate(is.null), .) %>% bind_rows() %>% 
        summarise_at(vars(fx_assign:fx_tab), 
                     funs("entropy" = ent_cal, "ratio" = ratio)) %>% 
        mutate(pub_year = yr)
}

cal_total <- function(yr, data) {
    data %>% filter(pub_year == yr) %>% 
        pull(function_feat) %>% map("result") %>% 
        Filter(Negate(is.null), .) %>% bind_rows() %>% nrow() -> n
    tibble(yr = yr, total = n)
}


res_entropy  <- map_dfr(1998:2018, cal_entro, data = test)


res_total  <- map_dfr(1998:2018, cal_total, data = test)



test %>% filter(pub_year <= 2018) %>% group_by(year) %>% 
    mutate()
res_entropy %>% gather(key = 'feature', value = 'entropy', -pub_year) %>% filter(str_detect(feature, "entropy$")) %>% ggplot(aes(x = pub_year, y = entropy)) + geom_line() + facet_wrap(~feature)
ggsave('lang_feature.png')

res_entropy %>% gather(key = 'feature', value = 'entropy', -pub_year) %>% filter(str_detect(feature, "ratio$")) %>% rename(share = 'entropy') %>% ggplot(aes(x = pub_year, y = share)) + geom_line() + facet_wrap(~feature)
ggsave('lang_feature_ratio.png')


res_entropy %>% gather(key = 'feature', value = 'entropy', -pub_year) %>% filter(str_detect(feature, "ratio$")) %>% 
    rename(share = 'entropy') %>% filter(feature == "fx_assign_ratio") %>% 
    mutate(share = share * 100) %>%
    ggplot(aes(x = pub_year, y = share)) + geom_line() + scale_color_brewer(palette="Dark2") + xlab("Year") + ylab("Share of all functions (%)") + 
    theme(plot.title = element_text(size = 24, face = "bold"), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 15), axis.title=element_text(size=14,face="bold")) + 
    theme(rect = element_rect(fill = "transparent")) +
    theme(legend.position = "none") -> amsterdam
ggsave('lang_amsterdam__ratio.png', amsterdam ,width = 6, height = 5, units = 'in', bg = "transparent")



res_entropy %>% gather(key = 'feature', value = 'entropy', -pub_year) %>% filter(str_detect(feature, "ratio$")) %>% filter(pub_year == 2018)
