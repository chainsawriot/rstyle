require(tidyverse)

df_total <- readRDS('community_df_total.RDS')
df_ratio_total <- readRDS('community_df_ratio_total.RDS')
df_naming_total <- readRDS('community_df_naming_total.RDS')
df_naming_ratio_total <- readRDS('community_df_naming_ratio_total.RDS')

key <- 'alllower'



df_naming_ratio_long <- df_naming_ratio_total %>% 
    t() %>% as.data.frame() %>% 
    rownames_to_column('community') %>% 
    arrange(snake) %>% 
    gather('naming', 'ratio', -community) %>% 
    mutate(percentage = ratio*100, 
           community=factor(community, levels = unique(community)),
           naming=factor(naming, levels = c('dotted', 'allupper', 'alllower', 'other', 'upcamel', 'lowcamel', 'snake'))) 

g_naming <- ggplot() + 
    geom_bar(aes(y = percentage, x = community, fill = naming), 
             data = df_naming_ratio_long, 
             stat="identity") + 
    coord_flip() 

png("naming_among_community.png", width = 9*96, height = 6*96)
g_naming
dev.off()
