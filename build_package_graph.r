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

get_neibor_graph_given_depth <- function(pck_name, max_depth){
  df <- data.frame("source"=character(), 
                   "dest"=character(), 
                   "weight"=numeric(),
                   "neibor_type"=character(), 
                   "depth"=numeric())
  
  n_depth <- 1
  while (n_depth <= max_depth){
  
      # collect source packags 
      if (n_depth == 1){
        pcks_source <-  c(pck_name) 
      } else {
        pcks_source <-  df %>% filter(depth==n_depth-1) %>% pull(dest) %>% unique() %>% as.character()
      }
      print(pcks_source)
      
      # get neibor of each source package
      pcks_has_detected <-  df  %>% pull(source) %>% unique() %>% as.character()
      for (pck_source in pcks_source){
        if (!pck_source %in% pcks_has_detected){
          tryCatch({
            sprintf('Depth: %s, Package: %s', n_depth, pck_source) %>% print()
            
            df_imports <- get_neibor_graph(pck_source, neibor_type='Imports')
            df_suggests <- get_neibor_graph(pck_source, neibor_type='Suggests')
            df_depth <- rbind(df_imports, df_suggests) %>% filter(!is.na(dest))
            df_depth['depth'] <- n_depth
            df <- rbind(df, df_depth)  
            
            }, error = function(err) {
              sprintf('- %s can not found', pck_source) %>% print()
            })
        }
      }
      # go to next depth
      n_depth <- n_depth + 1
  }
  return(df)
}


df <- get_neibor_graph_given_depth("ggplot2", max_depth = 10)
import_plot<-graph.data.frame(df,directed = TRUE)
plot(import_plot)




# community walktrap
communities <- cluster_walktrap(import_plot)
communities
