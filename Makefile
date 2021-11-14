rr = Rscript
cran:
	rsync -rtlzv --delete cran.r-project.org::CRAN ./cran
code.db data/target_meta.RDS: 
	$(rr) 0prep01_extract_metadata.R
	cat code.sql | sqlite3 code.db
	$(rr) 0prep02_dump.R
	$(rr) 0prep03_extract_desc.R
data/pkgs_functions_with_syntax_feature.RDS: code.db
	$(rr) 2syntax01_extract_features.R
	$(rr) 2syntax02_gen_pkgs_functions_with_syntax_feature.R
