.PHONY: pgstart
pgstart:
	pg_ctl -D /usr/local/var/postgres start

.PHONY: pgstop
pgstop:
	pg_ctl -D /usr/local/var/postgres start

.PHONY: pginit
pginit:
	createdb graphdb
	psql -h localhost -U postgres -d graphdb -c "CREATE EXTENSION intarray;"
	psql -U postgres -d graphdb -c "CREATE TABLE nodes ( id serial PRIMARY KEY , label VARCHAR(50) NOT NULL, relations INTEGER []);"

.PHONY: dropdb
dropdb:
	dropdb graphdb

.PHONY: psql
psql:
	pslq graphdb
