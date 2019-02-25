require(tidyverse)

fx_style <- readRDS('fx_style_by_year.RDS')
fx_style %>% mutate(alllower = alllower / total,
                    allupper = allupper / total,
                    upcamel = upcamel / total,
                    lowcamel = lowcamel / total,
                    snake = snake / total,
                    dotted = dotted / total,
                    other = other / total) %>%
  select(-total) %>% 
  gather(key = 'style', value = 'share', -pub_year) %>% filter(pub_year <= 2018) %>%
  ggplot(aes(x = pub_year, y = share, col = style)) + geom_line() + scale_color_brewer(palette="Dark2") -> prob_plot

ggsave('func_prob_plot.png', prob_plot)


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

ggsave('func_entropy_plot.png', entropy_plot)

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
                                        other * log(other))) %>% mutate(type = "function_name_style") %>%  select(pub_year, entropy, type) %>% saveRDS('entropy_fx_name.RDS')
