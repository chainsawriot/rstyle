require(tidyverse)

df_total <- readRDS('community_df_total.RDS')
df_ratio_total <- readRDS('community_df_ratio_total.RDS')
df_naming_total <- readRDS('community_df_naming_total.RDS')
df_naming_ratio_total <- readRDS('community_df_naming_ratio_total.RDS')


wide_to_long <- function(df, id_var, key_var, val_var, sort_by){
    df %>% 
        t() %>% as.data.frame() %>% 
        rownames_to_column(id_var) %>% 
        arrange(UQ(sort_by)) %>% 
        gather(!!key_var, !!val_var, -(!!id_var)) 
}

df_naming_ratio_long <- df_naming_ratio_total %>% 
    wide_to_long(id_var = 'community', key_var = 'naming', val_var = 'ratio', sort_by = quo(snake)) %>% 
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


# cake plot
df_ratio_long <- df_ratio_total %>% 
    wide_to_long(id_var = 'community', key_var = 'feature', val_var = 'ratio', sort_by = quo(fx_assign)) 
