require(igraph)
require(tidyverse)
setwd('docker_data/rstyle/')#todo: make it automatically and more flexible



get_dependency_snapshot <- function(type){
    pkg_dependency <- readRDS('pkg_dependency.RDS')
    if (type=="latest"){
        snapshot <- pkg_dependency %>% group_by(pkg_name) %>% filter(pub_year==max(pub_year)) %>% ungroup()        
    } else if (type=="cross-sectional") {
        # TODO: aggregate across time
    } else {
        # TODO: raise error   
    }
    return(snapshot)
}

get_neibor_graph <- function(pkg_source, type_dependency='latest'){
    dependency <- get_dependency_snapshot(type=type_dependency)#todo: how can I put it outside the function safely for better performance?
    df <- dependency %>% filter(pkg_name==pkg_source) %>% 
        select(pkg_name, field, pkgs) %>% unnest() %>% 
        transmute(source=pkg_name, dest=pkgs, weight=1, neibor_type=field, depth=1)
    return(df)
}



get_neibor_graph_given_depth <- function(pkg_source, max_depth, type_dependency='latest'){
    # neibor_type
    df <- get_neibor_graph(pkg_source, type_dependency)
    n_depth <- max(df$depth) + 1
    while (n_depth <= max_depth){
        # collect source packag
        pcks_source <-  df %>% filter(depth==n_depth-1) %>% pull(dest) %>% unique() %>% as.character()
        
        # get neibor of each source package
        pcks_has_detected <-  df %>% pull(source) %>% unique() %>% as.character()
        for (pkg_source in pcks_source){
            if (!pkg_source %in% pcks_has_detected){
                tryCatch({
                    sprintf('Depth: %s, Package: %s', n_depth, pkg_source) %>% print()
                    
                    # df_imports <- get_neibor_graph(pkg_source, neibor_type='Imports')
                    # df_suggests <- get_neibor_graph(pkg_source, neibor_type='Suggests')
                    # df_depth <- rbind(df_imports, df_suggests) %>% filter(!is.na(dest))
                    # df_depth['depth'] <- n_depth <- ≠
                    df_depth <- get_neibor_graph(pkg_source, type_dependency) 
                    df <- bind_rows(df, df_depth)
                }, error = function(err) {
                    sprintf('- %s can not found', pkg_source) %>% print()
                })
            }
        }
        # go to next depth
        n_depth <- n_depth + 1
    }
    return(df)
}


pkg_source <- 'ggplot2'
df <- get_neibor_graph_given_depth(pkg_source, max_depth = 10, type_dependency = 'latest') 
df <- df %>% filter(!is.na(dest), neibor_type=="imports")
graph <- graph.data.frame(df,directed = TRUE)
plot(graph)




# community walktrap
communities <- cluster_walktrap(graph)
communities
