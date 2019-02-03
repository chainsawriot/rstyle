create table if not exists cran_code (
       line_id integer primary key autoincrement,
       pkg_name text,
       tarball text,
       pub_year integer,
       filename text,
       line_num integer,
       code text
);

create index cran_pkg_name on cran_code(pkg_name);
create index cran_year on cran_code(pub_year);
create index cran_filename on cran_code(filename);
