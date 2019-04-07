require(dbplyr)
require(tidyverse)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "code.db")

cran_code <- tbl(con, "cran_code")

##cran_code %>% filter(filename != "NAMESPACE" & filename != "DESCRIPTION" & code %like% "#") %>% mutate(n_chars = nchar(code)) %>% group_by(pub_year, n_chars) %>% tally -> res


cran_code %>% filter(filename != "NAMESPACE" & filename != "DESCRIPTION") %>% mutate(n_chars = nchar(code), comment = str_detect(code, "#")) %>% group_by(pub_year, n_chars, comment) %>% tally -> res
comment_dist <- collect(res)


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
