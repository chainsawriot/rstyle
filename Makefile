## probably you don't need to run this.

rr = Rscript
target_meta: 
	$(rr) extract_metadata.R
code.db: target_meta.R
	cat code.sql | sqlite3 code.db
	$(rr) dump.R
