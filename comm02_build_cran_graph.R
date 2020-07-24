rm(list=ls())
require(igraph)
require(tidyverse)
cfg <- modules::use("config.R")

### local variables
max_depth <- 1
type_dependency <- 'latest'

### functions
get_dependency_snapshot <- function(type){
    pkg_dependency <- readRDS(cfg$PATH_CRAN_DEPENDENCY)
    if (type=="latest"){
        snapshot <- pkg_dependency %>% 
            group_by(pkg_name, field) %>% filter(pub_year==max(pub_year)) %>% ungroup() %>% 
            select(pkg_name, field, pkgs) %>% 
            mutate(type_dependency=type)
    } else if (type=="cross-sectional") {
        snapshot <- pkg_dependency %>% 
            group_by(pkg_name, field) %>% summarize(pkgs=pkgs %>% unlist() %>% 
                                                 unique() %>% na.omit() %>% 
                                                 list()) %>% ungroup() %>% 
            mutate(type_dependency=type)
    } else {
        # TODO: raise error
	    print('only "latest" and "cross-sectional" are allowed as input of type')   
    }
    return(snapshot)
}

get_neibor_graph <- function(dependency, pkg_source){ # dependency as global variable?
    df <- dependency %>% filter(pkg_name==pkg_source) %>% 
        select(type_dependency, pkg_name, field, pkgs) %>% unnest()
    if (nrow(df) == 0){
        # some packages have no fields like "Imports" or "Suggests"
        # for example, stats (https://stat.ethz.ch/R-manual/R-devel/library/stats/html/stats-package.html)
        return(NA)
    }#TODO: better way to write it? the problem is at "pkgs"
    df_rename <- df %>% 
        transmute(source=pkg_name, dest=pkgs, weight=1, neibor_type=field, type_dependency=type_dependency) %>% 
        filter(!is.na(dest))
    return(df_rename)
}

get_neibor_graph_given_depth <- function(dependency, pkg_source, max_depth){
    n_depth <- 1
    pcks_source <- c(pkg_source)
    pcks_has_detected <- c()
    df <- NULL
    
    while (n_depth <= max_depth){
        if (n_depth > 1){
            print(df)
            pcks_source <- df %>% filter(depth==n_depth-1) %>% pull(dest) %>% unique() %>% as.character()
            pcks_has_detected <-  df %>% pull(source) %>% unique() %>% as.character()    
        }
        
        for (pkg_source in pcks_source){
            if (!pkg_source %in% pcks_has_detected){#todo: if using "break" then it breaks both for-loop and while-loop
                tryCatch({
                    sprintf('Depth: %s, Package: %s', n_depth, pkg_source) %>% print()
                    df_depth <- get_neibor_graph(dependency, pkg_source) %>% mutate(depth=n_depth)
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

get_neibor <- function(pkg_name, dependency) {
    get_neibor_graph_given_depth(dependency, pkg_name, 1)
}

### main
dependency <- get_dependency_snapshot(type=type_dependency)
dependency$pkg_name %>% map_dfr(get_neibor, dependency = dependency) -> dependency_edgelist

cran_graph <- graph.data.frame(dependency_edgelist, direct = TRUE)
write_rds(cran_graph, cfg$PATH_CRAN_GRAPH)





