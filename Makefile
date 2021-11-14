rr = Rscript
cran:
	rsync -rtlzv --delete cran.r-project.org::CRAN ./cran
code.db data/target_meta.RDS: 
	$(rr) prep01_extract_metadata.R
	cat code.sql | sqlite3 code.db
	$(rr) prep02_dump.R
	$(rr) prep03_extract_desc.R
data/pkgs_functions_with_syntax_feature.RDS: code.db
	$(rr) syntax01_extract_features.R
	$(rr) syntax02_gen_pkgs_functions_with_syntax_feature.R
