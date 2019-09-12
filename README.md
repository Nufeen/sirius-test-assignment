# sirius-test-assignment

Task: https://gist.github.com/qnikst/b50b0cd43bcabad881f3b32371cc9e93

### DB

```
make pgstart

make pgstop

make pginit
```

**Note!**

In macOS `postgres` user is not created by default. Use something like `CREATE USER postgres WITH SUPERUSER PASSWORD '***'` in `psql`

**Note!**

SQL queries use `intarray` (postges > 9.1) extention: https://www.postgresql.org/docs/9.1/intarray.html

### Install/use

**Note!**

`pg_config` should be available for stack build

```
stack build

stack exec sirius-test-assignment-exe
```

or

```
stack ghci / main
```

### Test links

Neighbours: http://127.0.0.1:8081/node/{id}/neighbours

Link: http://127.0.0.1:8081/graph/link/{id}/{id}

Delete: http://127.0.0.1:8081/graph/node/{id}

Swagger: http://127.0.0.1:8081/swagger.json

```bash
# New node
curl -X PUT -d '{"label": "add node test"}' -H 'Content-type: application/json' http://127.0.0.1:8081/graph/node

# Rename node
curl -X PUT -d '{"label": "rename  node test"}' -H 'Content-type: application/json' http://127.0.0.1:8081/graph/node/<id>
```

## TODO

- app config
- proper error handling, http://hackage.haskell.org/package/postgresql-error-codes-1.0.1/docs/PostgreSQL-ErrorCodes.html
- hlint
- swagger docs
- postman file
- db init script for different platforms
- api tests


## Notes for myself (getting along with servant/hasql)

### docs

https://docs.servant.dev/en/stable/tutorial/Server.html

https://docs.servant.dev/en/stable/cookbook/generic/Generic.html

### tutorials

https://www.parsonsmatt.org/2016/06/24/take_over_an_api_with_servant.html

https://gust.com/tech/blog/type-level-apis

### examples

https://github.com/haskell-servant/example-servant-minimal/blob/master/src/App.hs

https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

### hasql example

https://github.com/jmackie/servant-example/blob/08924f8a310909f97ad7bd01d3b4326b0d2f33b9/src/Database.hs

### testing

http://hackage.haskell.org/package/servant-quickcheck-0.0.7.4/docs/Servant-QuickCheck.html

### interesting

https://github.com/haskell-servant/servant-swagger/issues/90

https://github.com/haskell-servant/servant/issues/1015


### graph notes

http://dev.stephendiehl.com/hask/#graphs


### Topics to explore

- Generics

- Data.Functor.Contravariant
