
comment_dist <- readRDS('comment_dist.RDS')



comment_dist %>% mutate(comment = ifelse(comment == 1, "Yes", "No")) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline)) %>% 
    select(pub_year, n_chars, prob, comment) %>% filter(pub_year %in% c(2010, 2018)) %>%
    filter(n_chars > 40 & n_chars < 100 & pub_year < 2019) %>% 
    mutate(prob = prob * 100) %>%
    ggplot(aes(x = n_chars, y = prob, color = comment)) + 
    geom_line(stat = 'identity') +
    geom_vline(xintercept = 80, alpha = 0.3) + facet_wrap(~ pub_year, nrow = 3) +
    xlab('Number of characters') + ylab('Share of all lines (%)') + scale_color_brewer(palette="Dark2") +
    theme(plot.title = element_text(size = 24, face = "bold"), plot.subtitle =  element_text(size = 10), axis.text = element_text(size = 15), axis.title=element_text(size=14,face="bold")) + 
    theme(rect = element_rect(fill = "transparent")) +
    theme(legend.position = "none") + theme(strip.text.x = element_text(size = 15, face = "bold")) -> line_compare
ggsave('line_length_2010_2018.png', line_compare, width = 6, height = 5, units = 'in', bg = "transparent")

require(gganimate)
comment_dist %>% mutate(comment = comment == 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline) * 100) %>% select(pub_year, n_chars, prob, comment) %>% filter(n_chars < 150 & pub_year < 2019) %>% 
  ggplot(aes(x = n_chars, y = prob, color = comment)) + 
  geom_line(stat = 'identity') +
  geom_vline(xintercept = 80, alpha = 0.1) + facet_wrap(~ pub_year, nrow = 3)

require(glue)
comment_dist %>% mutate(comment = comment == 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline) * 100) %>% 
  select(pub_year, n_chars, prob, comment) %>% filter(n_chars < 150 & pub_year < 2019) %>%
  ungroup %>% ggplot(aes(x = n_chars, y = prob, color = comment)) + 
  geom_line(stat = 'identity') +
  geom_vline(xintercept = 80, alpha = 0.1) +
  labs(title = 'Year: {frame_time}', x = 'Number of Characters', y = 'Proportion') +
  transition_time(pub_year)

comment_dist %>% mutate(comment = comment == 1) %>% filter(n_chars > 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline) * 100) %>% 
  select(pub_year, n_chars, prob, comment) %>% filter(n_chars < 110 & pub_year < 2019) %>%
  ungroup %>% ggplot(aes(x = n_chars, y = prob, color = comment)) + 
  geom_line(stat = 'identity') +
  geom_vline(xintercept = 80, alpha = 0.1) +
  labs(title = 'Year: {frame_time}', x = 'Number of Characters', y = 'Proportion') +
  transition_time(pub_year)

## information entropy

comment_dist %>% mutate(comment = comment == 1) %>%  group_by(pub_year, comment) %>% mutate(totalline = sum(n), prob = (n / totalline), plogp = prob * log(prob)) %>% summarise(entropy = - sum(plogp)) %>% ungroup %>% mutate(type = paste0('linelength_', ifelse(comment, "comment", "code"))) %>% select(pub_year, entropy, type) %>% saveRDS('entropy_linelength.RDS')
