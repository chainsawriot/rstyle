require(tidyverse)
require(ggthemes)
require(igraph)
require(ggthemes)
cfg <- modules::use("config.R")

###################################################
### local variables
pkgs <- readRDS(cfg$PATH_PKGS_FUNCTIONS_W_SYNTAX_FEATURE)
cran_graph <- read_rds(cfg$PATH_CRAN_GRAPH)
comm <- read_rds(cfg$PATH_COMM)
comm_size <- read_rds(cfg$PATH_COMM_SIZE)
comm_feat <- readRDS(cfg$PATH_COMM_LARGEST_FEATURES)
poster_theme <- theme(plot.title = element_text(size = 24, face = "bold"), 
                      plot.subtitle =  element_text(size = 10), 
                      axis.text = element_text(size = 15), 
                      axis.title=element_text(size=14,face="bold"),
                      rect = element_rect(fill = "transparent")) 
naming_conv <- read.csv(cfg$PATH_NAMING_CONVENTION, stringsAsFactors = FALSE)
###################################################
### functions
cal_dist <- function(i, feat, avg) {
    target <- select(feat[i,], fx_assign:other)
    res <- bind_rows(target, avg) %>% as.matrix %>% dist 
    res[1]
}

plot_naming_among_comm <- function(comm_feat){
    data_naming <- comm_feat %>% select(comm_id, comm_name, alllower:other) %>% 
        mutate(rank_by_snake = snake) %>%
        gather("feature", "proportion", -comm_id, -comm_name, -rank_by_snake) %>%
        left_join(naming_conv, by = "feature") %>% 
        mutate(comm_name = fct_reorder(comm_name, rank_by_snake),
               long_name = fct_relevel(long_name, naming_conv$long_name)) %>%
        mutate(percentage = proportion * 100) 
    
    g_naming <- ggplot(data_naming, aes(y = percentage, x = comm_name, fill = long_name)) + 
        geom_bar(stat="identity") + 
        labs(x = "", y = "%") + 
        theme(legend.title = element_blank()) +
        coord_flip() + scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')) + 
        poster_theme 
    return(g_naming)
}

plot_subgraph_given_comm_ids <- function(selected_comm_ids){
    V(cran_graph)$comm <- membership(comm)
    pkgs_to_extract <- names(membership(comm)[membership(comm) %in% selected_comm_ids])
    cran_sub_graph <- induced_subgraph(cran_graph, which(V(cran_graph)$name %in% pkgs_to_extract))
    
    set.seed(168979208)
    plot(cran_sub_graph, 
         vertex.color = V(cran_sub_graph)$comm,
         vertex.size = page_rank(cran_sub_graph)$vector * 200, 
         edge.arrow.size = 0, 
         vertex.label = ifelse(V(cran_sub_graph)$name %in% c("randomForest", "tm", "rJava"), V(cran_sub_graph)$name, ""), 
         vertex.label.cex = 3, 
         edge.color = "gray80", 
         vertex.frame.color = "gray80", 
         vertex.label.color = "#000000")
    
    p <- recordPlot()
    return(p)
}

plot_comm_naming_distribution <- function(comm_id, selected_naming_features = c('snake', 'lowcamel', 'upcamel', 'dotted')) {
    data <- comm_feat %>%  filter(comm_id == !!comm_id) %>% 
        select(comm_id, comm_name, alllower:other) %>% 
        left_join(comm_size %>% select(comm_id, top, n_mem), by = "comm_id") %>% 
        gather("feature", "proportion", -comm_id, -comm_name, -n_mem, -top) %>% 
        left_join(naming_conv, by = "feature") %>% 
        mutate(percentage = proportion * 100) %>% 
        mutate(long_name = fct_relevel(long_name, naming_conv$long_name)) %>% 
        filter(feature %in% selected_naming_features)  
    
    title <- data %>% head(1) %>% pull(comm_name)
    subtitle <- data %>% head(1) %>% mutate(subtitle = sprintf("Top 3 / %s: %s", n_mem, top)) %>% pull(subtitle)
    
    g <- ggplot(data, aes(y = percentage, x = long_name, fill = long_name)) + 
        geom_bar(stat="identity") + ylim(0, 100) +         
        labs(title = title, subtitle = subtitle, x = "", y = "") + 
        theme_hc() +
        theme(legend.position = "none", 
              panel.grid.major.y = element_blank(), 
              axis.text.x = element_blank(), 
              axis.ticks.x = element_blank()) +
        coord_flip() +
        geom_text(aes(label=sprintf("%s %s", round(percentage, 2), "%")), 
                  position = position_dodge(width = 0.9), hjust = -0.25) + 
        scale_fill_manual(values = RColorBrewer::brewer.pal(7, 'Dark2')[c(1,3,6,7)])  + 
        poster_theme
    return(g)
}

get_comm_feat_with_distance <- function(comm_feat, selected_syntax_features){
    avg_comm_feat <- comm_feat %>% 
        summarise_at(vars(fx_assign:other), list(~weighted.mean(., n_mem))) 
    selected_comm_feat <- comm_feat %>%
        bind_rows(bind_cols(tibble(comm_id = 0, comm_name = "Average", n_mem = 0), avg_comm_feat)) %>%
        mutate(distance = c(map_dbl(1:nrow(comm_feat), cal_dist, feat = comm_feat, avg = avg_comm_feat), 0))%>%
        mutate(comm_name = factor(comm_name)) %>%
        mutate(comm_name = fct_reorder(comm_name, distance)) %>% 
        select(comm_id, comm_name, one_of(selected_syntax_features), distance, n_mem) %>% 
        arrange(desc(distance))
    return(selected_comm_feat)   
}

plot_syntax_features_among_comm <- function(comm_feat, selected_syntax_features){
    selected_comm_feat <- get_comm_feat_with_distance(comm_feat, selected_syntax_features)
    data_features <- selected_comm_feat %>% 
        gather("feature", "proportion", -comm_id, -comm_name, -distance, -n_mem) %>% 
        mutate(percentage = proportion * 100, 
               avg = (comm_name == "Average")) %>% 
        mutate(feature = factor(feature), 
               comm_name = factor(comm_name)) %>% 
        mutate(feature = fct_relevel(feature, selected_syntax_features),
               comm_name = fct_reorder(comm_name, distance)) 
    
    g_features <- ggplot(data_features, aes(y = percentage, x = comm_name, fill = avg)) + 
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
    return(g_features)
}

plot_syntax_distance_among_comm <- function(comm_feat, selected_syntax_features){
    selected_comm_feat <- get_comm_feat_with_distance(comm_feat, selected_syntax_features)
    dist_summ <- selected_comm_feat %>% 
        filter(comm_name != "Average") %>% 
        summarise(mu_dist = weighted.mean(distance, n_mem), 
                  sigma_dist = sqrt(sum((n_mem / sum(n_mem))* (distance - mu_dist)^2))) 
    mu_dist <- dist_summ %>% pull(mu_dist)
    sigma_dist <- dist_summ %>% pull(sigma_dist)
    
    data_dist <- selected_comm_feat %>% 
        mutate(comm_name = fct_reorder(comm_name, distance)) 
    
    g_dist <- ggplot(data_dist, aes(x = comm_name, y = distance)) + 
        geom_point() + 
        geom_segment(aes(x=comm_name, xend = comm_name, y = 0, yend = distance))  +
        labs(x = "", y = "distance") + 
        geom_hline(yintercept = mu_dist, color = "#E7298A", alpha = 0.4) + 
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = mu_dist + sigma_dist * 1, ymax = mu_dist + sigma_dist * 2, alpha = 0.2, fill = "#562457") + 
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = mu_dist + sigma_dist * 2, ymax = mu_dist + sigma_dist * 3, alpha = 0.4, fill = "#562457") +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = mu_dist + sigma_dist * 3, ymax = Inf, alpha = 0.7, fill = "#562457") +
        geom_text(aes(label = round(distance, 2)), position = position_dodge(width = 0.9), hjust = -0.40) + 
        poster_theme + 
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank()) +
        coord_flip() 
    return(g_dist)
}

###################################################
### main

# poster: Fig. 4 in paper
filename <- str_glue(cfg$FOLDER_COMM_OUPUT, "comm05_naming_among_community.png")
g_naming <- plot_naming_among_comm(comm_feat)
ggsave(filename, plot = g_naming, width = 9, height = 6, units = "in", bg = "transparent")


# poster: simplied Fig. 5
selected_syntax_features <- c("fx_opencurly", "fx_integer", "fx_infix", "fx_assign")
filename <- str_glue(cfg$FOLDER_COMM_OUPUT, "comm05_syntax_features_among_community.png")

g_features <- plot_syntax_features_among_comm(comm_feat, selected_syntax_features)
ggsave(filename, plot = g_features, width = 12, height = 6, units = "in", bg = "transparent")

# poster: distance-to-average  
selected_syntax_features <- c("fx_opencurly", "fx_integer", "fx_infix", "fx_assign")
filename <- str_glue(cfg$FOLDER_COMM_OUPUT, "comm05_feature_distance.png")

g_dist <- plot_syntax_distance_among_comm(comm_feat, selected_syntax_features)
ggsave(filename, plot = g_dist, width = 10, height = 6, units = "in", bg = "transparent")


# poster: network graph
selected_comm_ids <- c(9, 45, 49)
filename <- str_glue(cfg$FOLDER_COMM_OUPUT, "comm05_subgraph_selected_comm_", paste(selected_comm_ids, collapse = "_"), ".png")

png(filename, width = 1800, height = 900)
p <- plot_subgraph_given_comm_ids(selected_comm_ids)
dev.off()

# poster: variation within communities
selected_naming_features = c('snake', 'lowcamel', 'upcamel', 'dotted')

for (comm_id in selected_comm_ids){
    filename <- str_glue(cfg$FOLDER_COMM_OUPUT, "comm05_naming_in_comm_", comm_id, ".png")
    g_comm_naming_distribution <- plot_comm_naming_distribution(comm_id, selected_naming_features)
    ggsave(filename, plot = g_comm_naming_distribution, width = 12, height = 8, units = "in", bg = "transparent")
}
