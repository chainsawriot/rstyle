require(tidyverse)

fx_name <- readRDS('entropy_fx_name.RDS')
line_length <- readRDS('entropy_linelength.RDS')

bind_rows(fx_name, line_length) %>% group_by(type) %>% 
    mutate(entropy_98 = entropy[pub_year == 1998], normalized_entropy = entropy / entropy_98) %>% ungroup %>% 
    group_by(pub_year) %>% summarise(entropy = mean(normalized_entropy)) %>% filter(pub_year < 2019) %>% 
  ggplot(aes(x = pub_year, y = entropy)) + geom_line() + theme(plot.title = element_text(size = 24, face = "bold"), plot.subtitle =  element_text(size = 10), 
                                                               axis.text = element_text(size = 15), axis.title=element_text(size=14,face="bold")) + 
    theme(rect = element_rect(fill = "transparent")) +
    theme(legend.position = "none") + xlab("Year") + ylab("Level of disagreement") + geom_vline(xintercept = 2015, alpha = 0.3) -> entropy

ggsave('master_entropy.png', entropy ,width = 9, height = 3, units = 'in', bg = "transparent")

