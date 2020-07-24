rm(list=ls())
require(tidyverse)
require(ggthemes)
require(igraph)
require(ggthemes)
cfg <- modules::use("config.R")

### local variables
pkgs <- readRDS(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)
cran_graph <- read_rds(cfg$PATH_CRAN_GRAPH)
comm <- read_rds(cfg$PATH_COMM)
comm_size <- read_rds(cfg$PATH_COMM_SIZE)
comm_feat <- readRDS(cfg$PATH_COMM_LANG_FEATURES)
community_ids <- comm_size %>% filter(rank <= cfg$MAX_NUM_COMM_TO_ANALYZE) %>% pull(comm_id)
poster_theme <- theme(plot.title = element_text(size = 24, face = "bold"), 
                      plot.subtitle =  element_text(size = 10), 
                      axis.text = element_text(size = 15), 
                      axis.title=element_text(size=14,face="bold")) +  
    theme(rect = element_rect(fill = "transparent")) 


### main
avg_comm_feat <- comm_feat %>% 
    summarise_at(vars(fx_assign:other), list(~weighted.mean(., n_mem))) 


cal_dist <- function(i, feat, avg) {
    target <- select(feat[i,], fx_assign:other)
    res <- bind_rows(target, avg) %>% as.matrix %>% dist 
    res[1]
}

selected_comm_feat <- comm_feat %>% 
    bind_rows(bind_cols(tibble(comm_name = "Average"), avg_comm_feat)) %>% 
    mutate(dist = c(map_dbl(1:nrow(comm_feat), cal_dist, feat = comm_feat, avg = avg_comm_feat), 0)
)%>%
    select(comm_name, fx_opencurly, fx_integer, fx_infix, fx_assign, dist, n_mem) 

#############
# simplied Fig. 5
g_features <- selected_comm_feat %>% 
    gather("feature", "proportion", -comm_name, -dist, -n_mem) %>% 
    mutate(percentage = proportion * 100, avg = (comm_name == "Average")) %>% 
    mutate(feature = factor(feature), comm_name = factor(comm_name)) %>% 
    mutate(feature = fct_relevel(feature, 
                                 'fx_opencurly', 'fx_integer', 
                                 'fx_infix', 'fx_assign')) %>%
    mutate(comm_name = fct_reorder(comm_name, dist)) %>% 
    ggplot(aes(y = percentage, x = comm_name, fill = avg)) + 
    geom_bar(stat="identity", position="dodge") +
    facet_grid(. ~ feature) + labs(x="", y = "") + 
    coord_flip() + scale_x_discrete(position="top") + 
    scale_y_continuous(trans = 'reverse',  limits = c(110, 0)) + 
    geom_text(aes(label=sprintf("%s%s", 
                                round(percentage, 0), "%")), 
              position = position_dodge(width=0.9), hjust = 1.1) +
    xlab("") + ylab("") + 
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) + poster_theme + 
    scale_fill_brewer(palette = 'Dark2') 

ggsave("visualization_community/features_among_community2.png", plot = g_features, width = 12, height = 6, units = "in", bg = "transparent")

#############
# poster: distance-to-average  
dist_summ <- selected_comm_feat %>% filter(comm_name != "Average") %>% 
    summarise(mu_dist = weighted.mean(dist, n_mem), 
              sigma_dist = sqrt(sum((n_mem / sum(n_mem))* (dist - mu_dist)^2))) 
mu_dist <- dist_summ %>% pull(mu_dist)
sigma_dist <- dist_summ %>% pull(sigma_dist)

g_dist <- selected_comm_feat %>% 
    mutate(comm_name = fct_reorder(comm_name, dist)) %>% 
    ggplot(aes(x = comm_name, y = dist)) + 
    geom_point() + 
    geom_segment(aes(x=comm_name, xend = comm_name, y = 0, 
                     yend = dist))  +
    labs(x = "", y = "distance") + 
    geom_hline(yintercept = mu_dist, color = "#E7298A", alpha = 0.4) + 
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = mu_dist + sigma_dist * 1, ymax = mu_dist + sigma_dist * 2, alpha = 0.2, fill = "#562457") + 
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = mu_dist + sigma_dist * 2, ymax = mu_dist + sigma_dist * 3, alpha = 0.4, fill = "#562457") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = mu_dist + sigma_dist * 3, ymax = Inf, alpha = 0.7, fill = "#562457") +
    geom_text(aes(label = round(dist, 2)), position = position_dodge(width=0.9), hjust = -0.40) + 
    poster_theme + 
    # poster_scale_color + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank()) +
    coord_flip() 

ggsave("visualization_community/feature_distance2.png", plot = g_dist, width = 8, height = 6, units = "in", bg = "transparent")

#############
# Fig. 4
naming_conv <- tibble(
    feature = c('dotted', 'allupper', 'upcamel', 'other', 'alllower', 'lowcamel', 'snake'), 
    long_name = c("dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake")) 

g_naming <- comm_feat %>% select(alllower:comm_name) %>% 
    mutate(snake2 = snake) %>%
    gather("feature", "proportion", -comm_name, 
           -snake2) %>% left_join(naming_conv, by = 'feature') %>% 
    mutate(comm_name = fct_reorder(comm_name, snake2)) %>%
    mutate(long_name = fct_relevel(long_name, 
                                   "dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", "lowerCamel", "lower_snake")) %>%
    mutate(percentage = proportion * 100) %>%
    ggplot(aes(y = percentage, x = comm_name, fill = long_name)) + 
    geom_bar(stat="identity") + 
    labs(x = "", y = "%") + 
    theme(legend.title = element_blank()) +
    coord_flip() + scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
    poster_theme 
ggsave("visualization_community/naming_among_community2.png", plot = g_naming, width = 9, height = 6, units = "in", bg = "transparent")



#############
# poster: network graph
selected_comm_id <- c(27, 447, 79)
V(cran_graph)$comm <- membership(comm)
pkgs_to_extract <- names(membership(comm)[membership(comm) %in% selected_comm_id])
cran_sub_graph <- induced_subgraph(cran_graph, which(V(cran_graph)$name %in% pkgs_to_extract))

set.seed(168979208)
plot(cran_sub_graph, vertex.color = V(cran_sub_graph)$comm, vertex.size = page_rank(cran_sub_graph)$vector * 200, 
     edge.arrow.size = 0, vertex.label = ifelse(V(cran_sub_graph)$name %in% c("randomForest", "sna", "tm", "rJava"), V(cran_sub_graph)$name, ""), 
     vertex.label.cex = 3, edge.color = "gray80", vertex.frame.color = "gray80", vertex.label.color = "#000000")

tibble(pkg_name = V(cran_sub_graph)$name, var = page.rank(cran_sub_graph)$vector * 100, comm = V(cran_sub_graph)$comm) %>% group_by(comm) %>% top_n(n = 1, wt = var)


#############
# poster: variation within communities
### Network supplement figures
cran_wc <- membership(comm)
cran_event <- evcent(cran_graph, direct = TRUE)$vector
node_info <- tibble(pkg_name = names(cran_wc), cran_wc, cran_event)

comm_lab <- tibble(community_ids = comm_size$comm_id,cols = comm_size$top)

top_pkg <- node_info %>% filter(cran_wc %in% comm_lab$community_ids) %>% 
    left_join(comm_lab, c('cran_wc' = 'community_ids')) %>% group_by(cols) %>% 
    summarise(n = n()) %>% left_join((node_info %>% filter(cran_wc %in% comm_lab$community_ids) %>% 
                                          left_join(comm_lab, c('cran_wc' = 'community_ids')) %>% 
                                          group_by(cols) %>% filter(rank(desc(cran_event), 
                                                                         ties.method = "first") <= 3) 
                                      %>% arrange(cran_wc, desc(cran_event)) %>% 
                                          summarise(top_pkg = paste(pkg_name, collapse = ", "))), by = 'cols') %>% 
    arrange(desc(n)) %>% rename("comm_name" = cols) %>% select(comm_name, top_pkg) 

network_supple_data <- comm_feat %>% left_join(top_pkg, by = "comm_name") %>% 
    # filter(comm_id %in% c(18, 25, 36, 35)) %>%
    select(alllower:top_pkg) %>% gather("feature", "proportion", -comm_name, -n_mem, -top_pkg) %>% 
    left_join(naming_conv, by = 'feature') %>% 
    mutate(long_name = fct_relevel(long_name, 
                                   "dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", 
                                   "lowerCamel", "lower_snake"), percentage = proportion * 100) 
wrapper <- function(x, ...)  {
    paste(strwrap(x, ...), collapse = "\n")
}

RColorBrewer::brewer.pal(7, 'Dark2')[c(1,5,3,7)]

plot_target <- function(target_community_abbr = "Java", network_supple_data) {
    target_community <- get_comm_name(target_community_abbr)
    subtitle <- network_supple_data %>% filter(comm_name == target_community) %>% rowwise %>%
        mutate(subtitle = wrapper(sprintf("Top 3 / %s: %s", n_mem, top_pkg), width=60)) %>%
        head(1) %>% pull(subtitle) 
    g <- network_supple_data %>% filter(comm_name == target_community) %>%
        mutate(long_name = fct_relevel(long_name, 
                                       "dotted.func", "ALLUPPER", "UpperCamel", "other", "alllower", 
                                       "lowerCamel", "lower_snake")) %>%
        filter(feature %in% c('snake', 'lowcamel', 'upcamel', 'dotted')) %>% 
        ggplot(aes(y = percentage, x = long_name, fill = long_name)) + 
        geom_bar(stat="identity") + ylim(0, 100) +         
        labs(title = target_community, subtitle = subtitle, x = "", y = "") + 
        theme_hc() +
        theme(legend.position = "none", 
              panel.grid.major.y = element_blank(), 
              axis.text.x = element_blank(), 
              axis.ticks.x = element_blank()) +
        coord_flip() +
        geom_text(aes(label=sprintf("%s %s", round(percentage, 2), "%")), 
                  position = position_dodge(width=0.9), hjust = -0.25) + 
        scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')[c(1,3,6,7)])  + poster_theme##bruteforce fixing
    filename <- sprintf("visualization_community/New_naming_in_comm_%s.png", target_community_abbr)
    ggsave(filename, plot = g, width = 12, height = 8, units = "in", bg = "transparent")
    return(g)
}

get_comm_name <- function(comm_name_abbr){
    if (comm_name_abbr == "Java"){
        return("rJava, RWekajars, xlsxjars")
    } else if (comm_name_abbr == "Machine Learning"){
        return("rpart, nnet, e1071")
    } else if (comm_name_abbr == "Text Analysis"){
        return("ISOcodes, wordcloud, koRpus")
    } else {
        stop("no corresponding implementation")
    }
}

map(c("Java", "Machine Learning", "Text Analysis"), 
    plot_target, network_supple_data = network_supple_data)

