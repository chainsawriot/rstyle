require(dbplyr)
require(tidyverse)

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




extract_exported_functions <- function(target_pkg_name, target_pub_year, cran_code, verbose = TRUE) {
    if (verbose) {
        print(target_pkg_name)
    }
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename != "NAMESPACE" & filename != "DESCRIPTION") %>% select(code) %>% collect() -> pkg_source
    cran_code %>% filter(pkg_name == target_pkg_name & pub_year == target_pub_year & filename == "NAMESPACE") %>% select(code) %>% collect() -> pkg_ns
    ## This part is super error-prone! Must spin this off!
    pull(pkg_source) %>% textConnection() -> zz
    e <- new.env()
    source(zz, local = e)
    close(zz)
    all_functions <- ls(envir = e)
    rm(e)
    if (length(all_functions) == 0) {
        return(NA)
    }
    ##
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

map(map(res$functions, "error"), is.null) %>% unlist %>% sum ## ~10% death


## Possible strategy
## if eval ok, use ns and all_functions to infer
## if eval ok and no ns, return all_functions
## if eval not ok and ns, use ns only to infer
## if eval not ok and no ns, can't infer and return NA.
