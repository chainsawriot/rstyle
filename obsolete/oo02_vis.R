require(tidyverse)
require(modules)
cfg <- modules::use("config.R")
source('helpers.R')

oo_data <- readRDS(cfg$OO_DATA) %>% discard(~!is.null(.$error)) %>% map_dfr("result")

oo_data %>% filter(!is.na(s3) & !is.na(s4) & !is.na(r6)) %>% group_by(pub_year) %>% summarise(s3 = sum(s3) / n(), s4 = sum(s4) / n(), r6 = sum(r6) / n()) %>% pivot_longer(-pub_year, names_to = "oo", values_to = "prop") %>% mutate(prop = prop * 100) %>% filter(pub_year <= cfg$INCLUDE_YR) %>% ggplot(aes(x = pub_year, y = prop, color = oo)) + geom_line() + xlab("year") + ylab("Share of packages (%)") + scale_color_brewer(palette="Dark2") -> ggobj
ggsave('visualization_fun/oo_trend.png', ggobj)


