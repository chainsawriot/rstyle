require(tidyverse)

fx_name <- readRDS('entropy_fx_name.RDS')
line_length <- readRDS('entropy_linelength.R')

bind_rows(fx_name, line_length) %>% group_by(type) %>% mutate(entropy_98 = entropy[pub_year == 1998], 
                                                              normalized_entropy = entropy / entropy_98) %>%
  filter(pub_year < 2019) %>% ungroup %>% 
  ggplot(aes(x = pub_year, y = normalized_entropy)) + geom_line() + facet_wrap(~type, scales = 'free')
