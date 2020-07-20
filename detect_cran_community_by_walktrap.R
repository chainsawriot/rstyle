require(igraph)
require(tidyverse)

get_community <- function(cran_graph, seed){
    set.seed(seed)
    cran_wc <- walktrap.community(cran_graph, steps = 4)
    cran_event <- evcent(cran_graph, direct = TRUE)
    return(list(cran_wc=cran_wc, cran_event=cran_event))
}

view_community_influential_member <- function(comm, id_community){
    tibble(pkg = V(cran_graph)$name, id_comm = comm$cran_wc$membership, evcent = comm$cran_event$vector) %>% 
        filter(id_comm == id_community) %>% arrange(desc(evcent))
}

# main
seed <- 42

cran_graph <- read_rds("cran_graph.RDS")
comm <- get_community(cran_graph, seed)
write_rds(cran_wc, "cran_community.RDS")



# View: Table 1 
overview <- tibble(pkg = V(cran_graph)$name, id_comm = comm$cran_wc$membership, evcent = comm$cran_event$vector) %>% 
    arrange(desc(evcent)) %>%  
    group_by(id_comm) %>% 
    summarize(n = n(), top3 = paste(head(pkg, 3), collapse = ', ')) %>% 
    arrange(desc(n)) %>% 
    mutate(rank = row_number(), seed = seed) %>% 
    head(20)
overview

# View: see each community
id_community <- 3
view_community_influential_member(comm, id_community)

# TODO: compute correlation between random seeds
# TODO: compare the boundary of communites over time
