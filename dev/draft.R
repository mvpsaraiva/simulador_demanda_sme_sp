db_con <- DBI::dbConnect(RSQLite::SQLite(), "data/demanda_sme_v2.db")
con <- db_con

DBI::dbListTables(con)

