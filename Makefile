## probably you don't need to run this.

rr = Rscript
code.db target_meta.RDS: 
	$(rr) extract_metadata.R
	cat code.sql | sqlite3 code.db
	$(rr) dump.R
	$(rr) extract_desc.R
pkgs_functions_with_syntax_feature.RDS: code.db
	$(rr) extract_syntax_features.R
