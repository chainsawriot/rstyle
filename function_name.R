require(dbplyr)
require(tidyverse)
require(lintr)
require(furrr)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "code.db")

cran_code <- tbl(con, "cran_code")

##cran_code %>% filter(filename == "NAMESPACE") %>% summarize(minyear = min(pub_year), maxyear = max(pub_year))

## NAMESPACE was introduced in R version 1.7.0 (Jan 2003)
## https://developer.r-project.org/170update.txt

## TASK: Find out packages which do not have a NAMESPACE
## If NAMESPACE -> parse NAMESPACE
## IF no NAMESPACE -> parse R files (Problem: some of them are possibly S3 objects)

cran_code %>% group_by(pkg_name, pub_year) %>% summarize(NS = sum(str_detect(filename, "NAMESPACE"))) %>% ungroup %>% mutate(has_ns = NS != 0) %>% collect -> pkg_has_namespace

### good news is: most of them have NAMESPACE

pkg_has_namespace %>% group_by(has_ns) %>% tally

## let's work with those with NAMESPACE first, shall we?

pkg_has_namespace %>% filter(has_ns == 1) %>% sample_n(20) -> test_cases

pkg_has_namespace %>% filter(has_ns == 0) %>% sample_n(10) -> test_cases_no_ns



str_trim_extra <- function(x) {
    str_replace_all(str_trim(x), "\"", "")
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
            select(text) %>% pull -> function_name
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

extract_functions_from_ns <- function(pkg_ns) {
    z <- make_expression(pkg_ns)
    if (is.null(z)) {
        return(list('functions' = NULL, 'patterns' = NULL))
    }
    Filter(function(x) str_detect(x$content, "^export *\\("), z$expression) -> export_expression
    Filter(function(x) str_detect(x$content, "^exportPattern *\\("), z$expression) -> exportpattern_expression
    if (length(export_expression) > 0) {
        map_dfr(export_expression, extract_symbolconst) %>% pull -> exported_functions
    } else {
        exported_functions <- c()
    }
    if (length(exportpattern_expression) > 0) {
        map_dfr(exportpattern_expression, extract_symbolconst) %>% pull %>% str_replace_all("\"", "") -> exported_patterns
    } else {
        exported_patterns <- ""
    }
    return(list('functions' = exported_functions, 'patterns' = exported_patterns))
}

extract_symbolconst <- function(expression) {
    if (is.null(expression$parsed_content)) {
        return(NULL)
    }
    expression$parsed_content %>% filter(token == "SYMBOL" | token == "STR_CONST") %>% select(text)
}


grab_source_ns <- function(target_pkg_name, target_pub_year, dbname = 'code.db') {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)
    cran_code <- tbl(con, "cran_code")
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename != "NAMESPACE" & filename != "DESCRIPTION") %>% select(code) %>% collect() -> pkg_source
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename == "NAMESPACE") %>% select(code) %>% collect() -> pkg_ns
    DBI::dbDisconnect(con)
    if (nrow(pkg_ns) == 0) {
        pkg_ns <- NULL
    }
    return(list("pkg_source" = pkg_source, "pkg_ns" = pkg_ns))
}

extract_exported_functions <- function(target_pkg_name, target_pub_year, dbname = 'code.db', verbose = TRUE) {
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

plan(multiprocess)
test_cases %>% mutate(functions = future_map2(pkg_name, pub_year, safely(extract_exported_functions), dbname = 'code.db', verbose = FALSE, .progress = TRUE)) -> res

test_cases_no_ns %>% mutate(functions = future_map2(pkg_name, pub_year, safely(extract_exported_functions), dbname = 'code.db', verbose = FALSE, .progress = TRUE)) -> res

## inlinedocs 2013
## uniah 2015
## coin 2009
## coin 2006


## Amsterdam style test case
# pheatmap 2012

## no ns
# ade4 2010

## Other test case
## Rpad 2006

## using exportPattern

## NOTE: some smart people put `` around function names
