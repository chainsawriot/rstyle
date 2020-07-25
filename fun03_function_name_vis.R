require(tidyverse)
require(modules)

cfg <- modules::use("config.R")

fx_style <- readRDS(cfg$PATH_FX_STYLE_BY_YEAR)

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
    gather(key = 'style', value = 'share', -pub_year) %>%
    left_join(naming_conv, by = 'style') %>% 
    mutate(opacity = ifelse(style %in% c('dotted', 'snake', 'lowcamel', 'upcamel'), 0.8, 0.4)) %>%
    mutate(long_name = fct_relevel(long_name, 
                                   "dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake")) %>%
    mutate(percentage = share * 100) %>%
    ggplot(aes(x = pub_year, y = percentage, col = long_name, alpha = opacity)) + 
    geom_line() + scale_color_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
    xlab("Year") + ylab("Share of all functions (%)") + 
    theme(plot.title = element_text(size = 24, face = "bold"), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 15), axis.title=element_text(size=14,face="bold")) + 
    theme(rect = element_rect(fill = "transparent")) +
    theme(legend.position = "none") -> prob_plot
ggsave('visualization_fun/func_prob_plot.png', prob_plot, width = 6, height = 5, units = 'in', bg = "transparent") 




fx_style %>% mutate(alllower = alllower / total,
                    allupper = allupper / total,
                    upcamel = upcamel / total,
                    lowcamel = lowcamel / total,
                    snake = snake / total,
                    dotted = dotted / total,
                    other = other / total) %>%
  select(-total) %>% mutate(entropy = -
                                 (alllower * log(alllower) +
                                        allupper * log(allupper) +
                                        upcamel * log(upcamel) +
                                        lowcamel * log(lowcamel) +
                                        ifelse(snake != 0, snake * log(snake), 0) +
                                        dotted * log(dotted) +
                                        other * log(other))) %>% 
  ggplot(aes(x = pub_year, y = entropy)) + geom_line() -> entropy_plot

ggsave('visualization_fun/func_entropy_plot.png', entropy_plot)

fx_style %>% mutate(alllower = alllower / total,
                    allupper = allupper / total,
                    upcamel = upcamel / total,
                    lowcamel = lowcamel / total,
                    snake = snake / total,
                    dotted = dotted / total,
                    other = other / total) %>%
  select(-total) %>% mutate(entropy = -
                                 (alllower * log(alllower) +
                                        allupper * log(allupper) +
                                        upcamel * log(upcamel) +
                                        lowcamel * log(lowcamel) +
                                        ifelse(snake != 0, snake * log(snake), 0) +
                                        dotted * log(dotted) +
                                        other * log(other))) %>% mutate(type = "function_name_style") %>%  select(pub_year, entropy, type) %>% saveRDS('visualization_fun/entropy_fx_name.RDS')
