#import libraries
library(desc) 
library(stringr) 
library(igraph)
library(dplyr)


parse_desc_to_pck_names <-  function(raw_desc_pkg){
  #TODO: warning for strsplit
  li <- strsplit(raw_desc_pkg, ",") 
  package_relate <- str_extract_all(li, "[a-z0-9]{2,}") 
  return(package_relate[[1]])
}

get_neibor_graph <- function(pkg_source, neibor_type='Imports'){
  desc <- description$new(package = pkg_source)
  raw_neibor_from_desc <- desc$get(neibor_type)
  pkg_neibors <- parse_desc_to_pck_names(raw_neibor_from_desc)
  df <- data.frame(
    source = pkg_source,
    dest = pkg_neibors,
    weight = 1,
    neibor_type=neibor_type
  )
  return(df)
}



MAX_DEPTH <- 10
pck_name <- "ggplot2"



df <- data.frame("source"=character(), "dest"=character(), "weight"=numeric(),"neibor_type"=character(), "depth"=numeric())
n_depth <- 1
while (n_depth <= MAX_DEPTH){

    # collect source packags 
    if (n_depth == 1){
      pcks_source <-  c(pck_name) 
    } else {
      pcks_source <-  df %>% filter(depth==n_depth-1) %>% pull(dest) %>% unique() %>% as.character()
    }
    print(pcks_source)
    
    # get neibor of each source package
    has_detected <-  df  %>% pull(source) %>% unique() %>% as.character()
    for (pck_source in pcks_source){
      if (pck_source %in% has_detected){
        break
      }
      sprintf('Depth: %s, Package: %s', n_depth, pck_source) %>% print()
      

      tryCatch({
        df_imports <- get_neibor_graph(pck_source, neibor_type='Imports')
        # df_suggests <- get_neibor_graph(pck_source, neibor_type='Suggests')
        # df_depth <- rbind(df_imports, df_suggests)
        df_depth <- df_imports#TODO: tmp
        df_depth['depth'] <- n_depth
        df_depth <- df_depth %>% filter(!is.na(dest))
        df <- rbind(df, df_depth)  
        
      }, error = function(err) {
        sprintf('- %s can not found', pck_source) %>% print()
      })
    }
    
    # go to next depth
    n_depth <- n_depth + 1
}

import_plot<-graph.data.frame(df,directed = TRUE)
plot(import_plot)


# community walktrap
communities <- cluster_walktrap(import_plot)
communities
