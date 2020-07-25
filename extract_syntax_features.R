require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)

source('helpers.R')

str_trim_extra <- function(x) {
    if (length(x) == 1) {
        if (str_detect(x, "^c\\(")) {
            ## Can one be that crazy by putting this into NAMESPACE?
            x <- paste("res <- ", x)
            e <- new.env()
            zz <- textConnection(x)
            source(zz, local = e)
            close(zz)
            return(get('res', envir = e))
        }
    }   
    str_replace_all(str_trim(x), "^[\"`']|[\"`']$", "")
}
## Turing test for function definition
## NOT Sure if the Amsterdam style checker will introduce undesirable behaviours. But I guess no sane people will write crazy code like this.
## e.g. 'hello' = lapply(z, function(x) x + 1)
is_named_function_definition <- function(expression) {
    if (is.null(expression$parsed_content)) {
        return(FALSE)
    }
    expression$parsed_content %>% mutate(tid = row_number()) %>% filter(text == 'function') -> function_tokens
    if (nrow(function_tokens) == 0) {
        return(FALSE)
    }
    expression$parsed_content %>% mutate(tid = row_number()) %>% filter(text == 'function') %>% head(1) %>% pull(tid) -> function_tid
    expression$parsed_content %>% mutate(tid = row_number()) %>% filter(tid <= function_tid) -> function_parsed_content
    function_parsed_content %>% summarise(function_definition = ('SYMBOL' %in% token & "LEFT_ASSIGN" %in% token) | ("SYMBOL" %in% token & "EQ_ASSIGN" %in% token) | ("STR_CONST" %in% token & "LEFT_ASSIGN" %in% token)) %>% pull -> expr_is_function_definition
    ## Not annonymous function
    function_parsed_content %>% filter(token == "SYMBOL" | token == "STR_CONST") %>% nrow -> n_symbols
    named_function_definition <- n_symbols == 1
    return(expr_is_function_definition & named_function_definition)
}

extract_functions_from_source <- function(pkg_source) {
    z <- make_expression(pkg_source)
    if (is.null(z)) {
        return(NULL)
    } 
##    map_chr(z$expression, extract_function_name_from_expr) %>% Filter(Negate(is.na), .) %>% return
}

# src can either pkg_source or pkg_ns
make_expression <- function(src) {
    tmp_rcode_location <- tempfile()
    writeLines(pull(src), tmp_rcode_location)
    tryCatch({
        return(get_source_expressions(tmp_rcode_location))
    }, error = function(e) {
        warning("Parsing failed!")
        return(NULL)
    })
}

filter_good_expressions <- function(expressions, content_regex) {
    expressions_w_parsed_content <- expressions[map_lgl(expressions, ~ !is.null(.$parsed_content))]
    Filter(function(x) str_detect(x$content, content_regex), expressions_w_parsed_content)
}

extract_function_name_from_expr <- function(expression) {
    if (is_named_function_definition(expression)) {
        expression$parsed_content %>% mutate(tid = row_number()) %>%
            filter(text == 'function') %>% head(1) %>%
            pull(tid) -> function_tid
        expression$parsed_content %>% mutate(tid = row_number()) %>%
            filter(tid <= function_tid) %>% filter(token == "SYMBOL" | token == "STR_CONST") %>%
            select(text) %>% pull %>% str_trim_extra -> function_name
        return(function_name)
    } else {
        return(NA)
    }
}



## return true when any problem occur
## linter's booleanizer decorator
b <- function(linter) {
    z <- function(g) {
        res <- linter(g)
        return(length(res) != 0 & !any(is.null(unlist(res))))
    }
    return(z)
}


extract_features <- function(expr) {
    fx_name <- extract_function_name_from_expr(expr)
    fx_assign <- b(assignment_linter)(expr)
    fx_opencurly <- b(open_curly_linter())(expr)
    fx_infix <- b(infix_spaces_linter)(expr)
    fx_integer <- b(implicit_integer_linter)(expr)
    fx_singleq <- b(single_quotes_linter)(expr)
    fx_commas <- b(commas_linter)(expr)
    fx_semi <- b(semicolon_terminator_linter())(expr)
    fx_t_f <- b(T_and_F_symbol_linter)(expr)
    fx_closecurly <- b(closed_curly_linter())(expr)
    fx_tab <- b(no_tab_linter)(expr)
    res <- tibble(fx_name, fx_assign, fx_opencurly, fx_infix, fx_integer, fx_singleq, fx_commas, fx_semi, fx_t_f, fx_closecurly, fx_tab)
    return(res)
}

## target_pkg_name <- "rio"
## target_pub_year  <- 2017
## dbname <- "code.db"

## con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)
## cran_code <- tbl(con, "cran_code")
## pkg_source <- grab_source_ns(target_pkg_name, target_pub_year, dbname = dbname, source_only = TRUE)$pkg_source


##all_functions <- extract_functions_from_source(data_frame(src = amsterdam))

extract_pkg_fx_features <- function(target_pkg_name, target_pub_year, dbname = 'code.db', verbose = TRUE) {
    pkg_source <- grab_source_ns(target_pkg_name, target_pub_year, dbname = dbname, source_only = TRUE)$pkg_source
    z <- make_expression(pkg_source)
    fx_expr <- Filter(is_named_function_definition, z$expression)
    return(map_dfr(fx_expr, extract_features))
}

###pkg_functions <- readRDS('pkgs_functions.RDS')

pkg_functions <- readRDS('target_meta.RDS')
pkg_functions  %>% filter(year < 2020) %>% mutate(pub_year = year) %>% group_by(year) %>% dplyr::group_nest() -> all_pkgs_nest

extract_features_pkgsdata <- function(pkgsdata) {
    pkgsdata %>% mutate(function_feat  = map2(pkg_name, pub_year, safely(extract_pkg_fx_features), dbname = 'code.db')) -> res
    yr <- unique(pkgsdata$pub_year)
    saveRDS(res, paste0("syntax_feature_yr", yr, ".RDS"))
    return(yr)
}

extract_features_pkgsdata2 <- function(pkgsdata) {
    pkgsdata %>% mutate(function_feat  = future_map2(pkg_name, pub_year, safely(extract_pkg_fx_features), dbname = 'code.db', .progress = TRUE)) -> res
    yr <- unique(pkgsdata$pub_year)
    saveRDS(res, paste0("syntax_feature_yr", yr, ".RDS"))
    return(yr)
}


plan(multiprocess)
require(fs)

## all_pkgs_nest %>% dplyr::filter(!year %in% as.numeric(str_extract(dir_ls(regexp="syntax_feature_yr"), "[0-9]{4}")))


## %>% pull(data) %>% future_map(extract_features_pkgsdata, .progress = TRUE)

args <- commandArgs(trailingOnly=TRUE)

year_arg <- as.numeric(args[1])
print(year_arg)

all_pkgs_nest %>% filter(year == year_arg) %>% pull(data) -> data_to_extract

extract_features_pkgsdata2(data_to_extract[[1]])

## pkg_functions %>% rename(pub_year = 'year') %>% ungroup %>% sample_n(5) %>% mutate(function_feat  = future_map2(pkg_name, pub_year, safely(extract_pkg_fx_features), dbname = 'code.db', .progress = TRUE)) -> test


## pkg_functions %>% rename(pub_year = 'year') %>% ungroup %>% mutate(function_feat  = future_map2(pkg_name, pub_year, safely(extract_pkg_fx_features), dbname = 'code.db', .progress = TRUE)) -> pkg_functions

## saveRDS(pkg_functions, "pkgs_functions_with_syntax_feature.RDS")



# ##saveRDS(test, "lang_feat_test.RDS")

