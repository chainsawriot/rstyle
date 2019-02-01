require(dbplyr)
require(tidyverse)
require(lintr)

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

pkg_has_namespace %>% filter(has_ns == 1) %>% sample_n(50) -> test_cases



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
    function_parsed_content %>% summarise(function_definition = ('SYMBOL' %in% token & "LEFT_ASSIGN" %in% token)) %>% pull -> expr_is_function_definition
    ## Not annonymous function
    function_parsed_content %>% filter(token == "SYMBOL") %>% nrow -> n_symbols
    named_function_definition <- n_symbols == 1
    return(expr_is_function_definition & named_function_definition)
}

extract_function_name_from_expr <- function(expression) {
    if (is_named_function_definition(expression)) {
        expression$parsed_content %>% mutate(tid = row_number()) %>%
            filter(text == 'function') %>% head(1) %>%
            pull(tid) -> function_tid
        expression$parsed_content %>% mutate(tid = row_number()) %>%
            filter(tid <= function_tid) %>% filter(token == "SYMBOL") %>%
            select(text) %>% pull -> function_name
        return(function_name)
    } else {
        return(NA)
    }
}

extract_functions_from_source <- function(pkg_source) {
    tmp_rcode_location <- tempfile()
    writeLines(pull(pkg_source), tmp_rcode_location)
    z <- get_source_expressions(tmp_rcode_location)    
    map_chr(z$expression, extract_function_name_from_expr) %>% Filter(Negate(is.na), .)
}

extract_exported_functions <- function(target_pkg_name, target_pub_year, cran_code, verbose = TRUE) {
    if (verbose) {
        print(target_pkg_name)
    }
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename != "NAMESPACE" & filename != "DESCRIPTION") %>% select(code) %>% collect() -> pkg_source
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename == "NAMESPACE") %>% select(code) %>% collect() -> pkg_ns
    all_functions <- extract_functions_from_source(pkg_source)
    pkg_ns %>% filter(str_detect(code, "^export")) %>% rowwise %>% mutate(pattern = str_detect(code, "exportPattern"), content = break_export_content(code)) %>% ungroup -> res
    res %>% filter(!pattern) %>% select(content) %>% pull %>% unlist -> ns_export
    res %>% filter(pattern) %>% select(content) %>% pull %>% unlist -> ns_exportpattern
    export_pattern <- paste(ns_exportpattern, collapse = "|")
    if (export_pattern == "") {
        final_exported_functions <- intersect(all_functions, ns_export)
    } else {
        final_exported_functions <- c(intersect(all_functions, ns_export), grep(export_pattern, all_functions, value = TRUE))

    }
    if (is.null(final_exported_functions)) {
        return(NA)
    }
    return(final_exported_functions)
}

test_cases %>% mutate(functions = map2(pkg_name, pub_year, safely(extract_exported_functions), cran_code =  cran_code)) -> res

map(map(res$functions, "error"), is.null) %>% unlist %>% sum ## only one dead.

### CRAZY CASE: inlinedocs, 2013



## Possible strategy
## if eval ok, use ns and all_functions to infer
## if eval ok and no ns, return all_functions
## if eval not ok and ns, use ns only to infer
## if eval not ok and no ns, can't infer and return NA.
