require(igraph)
require(tidyverse)
cfg <- modules::use("config.R")

### local variables
seed <- 42

### functions
get_community <- function(cran_graph, seed){
    set.seed(seed)
    cran_wc <- walktrap.community(cran_graph, steps = 4)
    cran_event <- evcent(cran_graph, direct = TRUE)
    return(list(cran_wc = cran_wc, cran_event = cran_event))
}

view_community_influential_member <- function(comm, id_community){
    tibble(pkg = V(cran_graph)$name, comm_id = comm$cran_wc$membership, evcent = comm$cran_event$vector) %>% 
        filter(comm_id == id_community) %>% arrange(desc(evcent))
}

get_pkgs_from_largest_community <- function(seed, fraction, n_largest, cran_graph){
    comm <- get_community(cran_graph, seed)
    pkgs <- tibble(pkg = V(cran_graph)$name, comm_id = comm$cran_wc$membership, evcent = comm$cran_event$vector) %>% 
        arrange(desc(evcent)) %>%  
        group_by(comm_id) %>%
        summarize(n_mem = n(), top = list(head(pkg, round(n()*0.1)))) %>%
        arrange(desc(n_mem)) %>%
        head(n_largest) %>% pull(top)
    pkgs <- map(pkgs, sets::as.set)
    return(pkgs)
}

get_avg_jacc_corr <- function(pkgs_compare, pkgs_base){
    avg_jacc_corr <- map_dbl(1:n_largest, function(idx) sets::set_similarity(pkgs_base[[idx]], pkgs_compare[[idx]], method = "Jaccard")) %>% mean()
    return(avg_jacc_corr)
}

get_jacc_corr_for_largest_comm <- function(seed_base, n_sample, n_largest, fraction){
    # get the most important packages within each community from the base community with random seed 42
    pkgs_base <- get_pkgs_from_largest_community(seed_base, fraction, n_largest, cran_graph)
    
    # generate communities to be compared with different seeds
    pkgs_community <- map(seed_candidates, function(seed) get_pkgs_from_largest_community(seed, fraction, n_largest, cran_graph)) %>% set_names(seed_candidates) 
    
    # compute Jaccard correlation
    comm_corr <- map(pkgs_community, function(pkgs_compare) get_avg_jacc_corr(pkgs_compare, pkgs_base))
    
    # summary and visualization
    data <- tibble(seed_base = factor(seed_base), seed_compared = names(comm_corr), jaccard_corr = unlist(comm_corr))
    return(data)
}

#####################
# dependent data
cran_graph <- read_rds(cfg$PATH_CRAN_GRAPH)

#####################
### main
comm <- get_community(cran_graph, seed)
comm_size <- tibble(pkg = V(cran_graph)$name, comm_id = comm$cran_wc$membership, evcent = comm$cran_event$vector) %>% 
    arrange(desc(evcent)) %>%  
    group_by(comm_id) %>% 
    summarize(n_mem = n(), top = paste(head(pkg, 3), collapse = ', ')) %>% 
    arrange(desc(n_mem)) %>% 
    mutate(rank = row_number(), seed = seed) 
write_rds(comm$cran_wc, cfg$PATH_COMM)
write_rds(comm$cran_evcent, cfg$PATH_COMM_EVCENT)
write_rds(comm_size, cfg$PATH_COMM_SIZE)

#####################
# view: Table 1 
path_table1 <- paste(cfg$FOLDER_COMM_OUPUT, "comm03_top3_pkgs_of_communities.csv", sep = "")
comm_size %>% head(cfg$MAX_NUM_COMM_TO_ANALYZE) %>% write.csv(path_table1, row.names = FALSE)

# view: see each community
comm_id <- 3
view_community_influential_member(comm, comm_id)

#####################
# robustness checking on the influence of the choice of random seeds
seed_base <- seed
n_sample <- 100
fraction <- 0.1
n_largest <- cfg$MAX_NUM_COMM_TO_ANALYZE

data <- get_jacc_corr_for_largest_comm(seed_base, n_sample, n_largest, fraction)
tbl <- data %>% 
    group_by(seed_base) %>% 
    summarise(n = n(), 
              mean = mean(jaccard_corr), 
              sd = sd(jaccard_corr), 
              min = min(jaccard_corr), 
              q25 = quantile(jaccard_corr, c(0.25)), 
              q50 = quantile(jaccard_corr, c(0.5)), 
              q75 = quantile(jaccard_corr, c(0.75)), 
              max = max(jaccard_corr), 
              seed_compared = paste(seed_candidates, collapse = ", ")) %>% 
    mutate_if(is.numeric, round, 2 ) 
tbl
write.csv(tbl, paste(cfg$FOLDER_COMM_OUPUT, "comm03_influence_of_random_seeds.csv", sep = ""), row.names = FALSE)

g <- ggplot(data, aes(seed_base, jaccard_corr)) + geom_violin() + 
    ylim(0, 1) + 
    ggtitle('Similarity of identified communities from different seeds') + xlab('base seed') + ylab('Jaccard Correlation')
g
ggsave(paste(cfg$FOLDER_COMM_OUPUT, "comm03_jaccard_distribution_of_random_seeds.png", sep = ""), plot = g, width = 6, height = 6, units = "in", bg = "transparent")


