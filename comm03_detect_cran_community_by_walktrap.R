rm(list=ls())
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

# dependent data
cran_graph <- read_rds(cfg$PATH_CRAN_GRAPH)

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


# view: Table 1 
path_table1 <- 'visualization_community/table1_community_top3_pkgs.csv'
comm_size %>% head(cfg$MAX_NUM_COMM_TO_ANALYZE) %>% write.csv(path_table1, row.names = FALSE)

# view: see each community
comm_id <- 3
view_community_influential_member(comm, comm_id)

# TODO: compute correlation between random seeds
# TODO: compare the boundary of communites over time
