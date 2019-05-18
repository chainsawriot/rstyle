require(RSQLite)
require(dplyr)
require(tidyverse)
setwd('docker_data/rstyle/')#todo: make it automatically and more flexible

execute_sql <- function(path_db, sql_string){
    con <- dbConnect(RSQLite::SQLite(), path_db)
    res <- dbSendQuery(con, sql_string) 
    data <- res %>% dbFetch()
    dbClearResult(res)
    dbDisconnect(con)
    return(data)    
}

extract_description <- function(path_db = 'code.db'){
    sql_string <- 'select pkg_name, pub_year, line_num, code from cran_code where filename="DESCRIPTION"'
    desc <- execute_sql(path_db, sql_string)
    desc <- desc %>% group_by(pkg_name, pub_year) %>% nest(.key = 'description')
    return(desc)    
}

parse_field <- function(df, field){
    # todo: dirty code, need refactor to simpler and more readible strcuture
    lines <- df$code
    idx_start <- str_which(lines, pattern = str_c('^', field))
    
    no_exist <- length(idx_start) == 0
    if (no_exist){
        return(NA)
    }
    
    is_last_line <- idx_start == length(lines)
    if (is_last_line){
        idx_end <- idx_start 
    } else {
        idx_end <- idx_start + str_which(lines[idx_start+1:length(lines)], pattern = '^[^ ]')[1] - 1
    }
    
    valid_lines  <- lines[idx_start:idx_end]
    field_content <- str_c(valid_lines, collapse = '') %>% str_replace_all(' ' , '')
    return(field_content)
}

parse_pkgname <-  function(content){
    # todo: dirty code when extract data from list
    if (is.na(content)){
        return(NA)
    }
    pkgs_versions <- str_split(content, ':') %>% map(2) %>% str_split(',')
    pkgs <- map_chr(pkgs_versions[[1]], function(pkg) str_extract(pkg, '[a-zA-Z0-9]{2,}'))
    return(pkgs)
}


desc <- extract_description(path_db = 'code.db')
#todo: warning
dependency <- desc %>% 
    mutate(imports=map_chr(description, parse_field, field='Imports'),
           suggests=map_chr(description, parse_field, field='Suggests')) %>% 
    gather(field, content, imports:suggests) %>% 
    mutate(pkgs=map(content, parse_pkgname))

saveRDS(dependency, "pkg_dependency.RDS")




