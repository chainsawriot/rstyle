require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)

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


break_export_content <- function(code) {
    required_content <- str_extract(code, "^exportP?a?t?t?e?r?n?[^\\(]*\\([^\\)]+\\)")
    middle_content <- str_remove(str_remove(required_content, "^exportP?a?t?t?e?r?n?[^\\(]*\\("), "\\)")
    splited_middle_content <- str_split(middle_content, ",")
    splited_middle_content <- splited_middle_content[splited_middle_content != ""]
    return(map(splited_middle_content, str_trim_extra))
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

extract_functions_from_source <- function(pkg_source) {
    z <- make_expression(pkg_source)
    if (is.null(z)) {
        return(NULL)
    } 
    map_chr(z$expression, extract_function_name_from_expr) %>% Filter(Negate(is.na), .) %>% return
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

extract_functions_from_ns <- function(pkg_ns) {
    if (is.null(pkg_ns)) {
        return(list('functions' = NULL, 'patterns' = NULL))
    }
    z <- make_expression(pkg_ns)
    if (is.null(z)) {
        return(list('functions' = NULL, 'patterns' = NULL))
    }
    export_expression <- filter_good_expressions(z$expression, "^export *\\(")
    exportpattern_expression <- filter_good_expressions(z$expression, "^exportPattern *\\(")
    if (length(export_expression) > 0) {
        map(export_expression, extract_symbolconst) %>% unlist -> exported_functions
    } else {
        exported_functions <- c()
    }
    if (length(exportpattern_expression) > 0) {
        map(exportpattern_expression, extract_symbolconst) %>% unlist -> exported_patterns
    } else {
        exported_patterns <- ""
    }
    return(list('functions' = exported_functions, 'patterns' = exported_patterns))
}

extract_symbolconst <- function(expression) {
    if (is.null(expression$parsed_content)) {
        return(NULL)
    }
    expression$parsed_content %>% filter(token == "SYMBOL" | token == "STR_CONST") %>% select(text) %>% pull %>% str_trim_extra
}


grab_source_ns <- function(target_pkg_name, target_pub_year, ns_only = FALSE, source_only = FALSE, dbname = 'code.db') {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)
    cran_code <- tbl(con, "cran_code")
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename == "NAMESPACE") %>% select(code) %>% collect() -> pkg_ns
    if (nrow(pkg_ns) == 0) {
        pkg_ns <- NULL
    }
    if (ns_only) {
        DBI::dbDisconnect(con)
        return(list("pkg_ns" = pkg_ns))
    }
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename != "NAMESPACE" & filename != "DESCRIPTION") %>% select(code) %>% collect() -> pkg_source
    if (source_only) {
        DBI::dbDisconnect(con)
        return(list("pkg_source" = pkg_source))
    }
    DBI::dbDisconnect(con)
    return(list("pkg_source" = pkg_source, "pkg_ns" = pkg_ns))
}

extract_exported_functions_old <- function(target_pkg_name, target_pub_year, dbname = 'code.db', verbose = TRUE) {
    raw_data <- grab_source_ns(target_pkg_name, target_pub_year, dbname = dbname)
    pkg_source <- raw_data$pkg_source
    pkg_ns <- raw_data$pkg_ns
    all_functions <- extract_functions_from_source(pkg_source)
    if (is.null(pkg_ns)) {
        if (is.null(all_functions)) {
            return(NA)
        }
        return(all_functions)
    }
    res <- extract_functions_from_ns(pkg_ns)
    ns_export <- res$functions
    ns_exportpattern <- res$patterns
    export_pattern <- paste(ns_exportpattern, collapse = "|")
    if (is.null(all_functions) | length(all_functions) == 0) {
### Can only infer from NS
        if (verbose) {
            warning("Parsing R source failed. Infer from NAMESPACE only.")
        }
        return(ns_export)
    }
    if (export_pattern == "") {
        final_exported_functions <- intersect(all_functions, ns_export)
    } else {
        if (is.null(ns_export)) {
            final_exported_functions <- c(grep(export_pattern, all_functions, value = TRUE))
        } else {
            final_exported_functions <- c(intersect(all_functions, ns_export), grep(export_pattern, all_functions, value = TRUE))

        }
    }
    if (is.null(final_exported_functions)) {
        final_exported_functions <- NA
    }
    return(final_exported_functions)
}

extract_exported_functions <- function(target_pkg_name, target_pub_year, dbname = 'code.db', verbose = TRUE) {
    raw_data <- grab_source_ns(target_pkg_name, target_pub_year, dbname = dbname, ns_only = TRUE)
    ##pkg_source <- raw_data$pkg_source
    pkg_ns <- raw_data$pkg_ns
    res <- extract_functions_from_ns(pkg_ns)
    ns_export <- res$functions
    ns_exportpattern <- res$patterns
    export_pattern <- paste(ns_exportpattern, collapse = "|")
    ## Most typical case
    if (!is.null(pkg_ns) & length(ns_export) >= 1 & export_pattern == "") {
        return(ns_export)
    }
    pkg_source <- grab_source_ns(target_pkg_name, target_pub_year, dbname = dbname, source_only = TRUE)$pkg_source
    all_functions <- extract_functions_from_source(pkg_source)
    if (is.null(pkg_ns)) {
        if (is.null(all_functions)) {
            return(NA)
        }
        return(all_functions)
    }
    if (is.null(all_functions) | length(all_functions) == 0) {
### Can only infer from NS
        if (verbose) {
            warning("Parsing R source failed. Infer from NAMESPACE only.")
        }
        return(ns_export)
    }
    if (export_pattern == "") {
        final_exported_functions <- intersect(all_functions, ns_export)
    } else {
        if (is.null(ns_export)) {
            final_exported_functions <- c(grep(export_pattern, all_functions, value = TRUE))
        } else {
            final_exported_functions <- c(intersect(all_functions, ns_export), grep(export_pattern, all_functions, value = TRUE))

        }
    }
    if (is.null(final_exported_functions)) {
        final_exported_functions <- NA
    }
    return(final_exported_functions)
}

## r_src <- grab_source_ns('rio', 2017, dbname = 'code.db')$pkg_source
## r_exp <- make_expression(r_src)

## extract_functions_from_source <- function(pkg_source) {
##     z <- make_expression(pkg_source)
##     if (is.null(z)) {
##         return(NULL)
##     } 
##     map_chr(z$expression, extract_function_name_from_expr) %>% Filter(Negate(is.na), .) %>% return
## }
## extract_function_name_from_expr <- function(expression) {
##     if (is_named_function_definition(expression)) {
##         expression$parsed_content %>% mutate(tid = row_number()) %>%
##             filter(text == 'function') %>% head(1) %>%
##             pull(tid) -> function_tid
##         expression$parsed_content %>% mutate(tid = row_number()) %>%
##             filter(tid <= function_tid) %>% filter(token == "SYMBOL" | token == "STR_CONST") %>%
##             select(text) %>% pull %>% str_trim_extra -> function_name
##         return(function_name)
##     } else {
##         return(NA)
##     }
## }

