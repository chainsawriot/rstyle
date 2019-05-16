require(RSQLite)
require(dplyr)
require(stringr)

parse_field_content <- function(field, codes){
    idx_start <- str_which(codes, pattern = str_c('^', field))
    idx_end <-   idx_start + str_which(codes[idx_start+1:length(codes)], pattern = '^[^ ]')[1] - 1
    valid_lines  <- codes[idx_start:idx_end]
    field_content <- str_c(valid_lines, collapse = '') %>% str_replace_all(' ' , '')
    return(field_content)
}

parse_pkgname_from_content <-  function(raw_content){
    pkgs_versions <- str_split(raw_content, ':') %>% map(2) %>% str_split(',')
    pkgs <- map_chr(pkgs_versions[[1]], function(pkg) str_extract(pkg, '[a-zA-Z0-9]{2,}'))
    return(pkgs)
}


con <- dbConnect(RSQLite::SQLite(), 'code.db')


sql_string = 'select pkg_name, pub_year, line_num, code from cran_code where filename="DESCRIPTION"'
codes <- dbSendQuery(con, sql_string) %>% dbFetch()
pkg_codes <- codes %>% group_by(pkg_name, pub_year) %>% nest()


# todo: map each df into fun: get_imports_suggests
df <- pkg_codes$data[[300]]
parse_field_content(field='Suggests', codes = df$code) %>% parse_pkgname_from_content()
parse_field_content(field='Imports', codes = df$code) %>% parse_pkgname_from_content()


dbDisconnect(con)



