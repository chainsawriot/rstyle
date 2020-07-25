rm(list=ls())
require(RSQLite)
require(tidyverse)
cfg <- modules::use("config.R")

### functions
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
    # TODO: can use baugwo::read_dcf => does not export
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
    # TODO: dirty code when extract data from list
    # TODO: pkg names are not correctly parsed. Ex. tm (https://cran.r-project.org/web/packages/tm/index.html)
    if (is.na(content)){
        return(NA)
    }
    pkgs_versions <- str_split(content, ':') %>% map(2) %>% str_split(',')
    pkgs <- map_chr(pkgs_versions[[1]], function(pkg) str_replace_all(pkg, "\\(.+\\)", "") %>% str_extract('[a-zA-Z0-9]{2,}'))
    return(pkgs)
}

### main
desc <- extract_description(path_db = cfg$PATH_CODE_DB)
#TODO: warning
dependency <- desc %>% 
    filter(pub_year <= cfg$END_YEAR) %>% 
    mutate(imports=map_chr(description, parse_field, field='Imports'),
           suggests=map_chr(description, parse_field, field='Suggests')) %>% 
    gather(field, content, imports:suggests) %>% 
    mutate(pkgs=map(content, parse_pkgname))

saveRDS(dependency, cfg$PATH_CRAN_DEPENDENCY)




