# WebSQL test code

This directory contains some simple static source for working with
WebSQL. This probably only works with a Chrome browser. Apparently,
WebSQL is compatible with SQLite. It appears that a cross-browser
version exists: https://github.com/sql-js/sql.js

The various idosyncracies of SQLite are here:
https://www.sqlite.org/lang.html

See also:
  https://james-priest.github.io/100-days-of-code-log-r2/CH16-Offline1-WebSQL.html
  https://jsfiddle.net/Trae/76srLbwr/
  https://www.w3.org/TR/webdatabase/

## How to run

From this directory:
```
swipl ../simple_server.pl --port=9998 --staticdir=.
```

Access by this URL:
http://localhost:9998/static/websql_test.html
