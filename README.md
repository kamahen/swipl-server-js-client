# Simple SWI-Prolog - Javascript client/server

A simple example of a SWI-Prolog server and a Javascript client.


## License
[Apache 2.0](LICENSE)

## Warning

This code is an overly simple example of client-server code, intended
as a tutorial or to extend the HTTP support
[documentation](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27))
in the SWI-Prolog manual.

It has only been tested with a recent "development" release of SWI-Prolog
(8.3.7) on Ubuntu 18.0.4 and with the Chrome browser.

## How to run

Install [SWI-Prolog](http://www.swi-prolog.org/Download.html) development version.

For Ubuntu, Debian, and similar (following the instructions at https://www.swi-prolog.org/build/PPA.html), use these steps:
  *  `sudo apt install software-properties-common`
  *  `sudo apt-add-repository ppa:swi-prolog/devel`
  *  `sudo apt update`
  *  `sudo apt install swi-prolog`

Start the server:
```
swipl simple_server.pl --port=9999 --staticdir=static
```
In a browser, start the client:
```
htp://localhost:9999
```

You might try this query:
```
get_time(TimeStamp), stamp_date_time(TimeStamp, DateTime, local), bagof(Key-Value, date_time_value(Key, DateTime, Value), DatePieces), dict_create(DateDict, date, DatePieces)
```
or this:
```
setof(K:V,current_prolog_flag(K,V), Flags), forall(member(K:V, Flags), format('~|~q:~t~40+~q~n', [K,V]))
```

## Code overview

There are three client components and one server component:
*  `simple_client.html`
*  `simple_client.css`
*  `simple_client.js`
*  `simple_server.pl`

If the server is accessed at the top level ("`/`"), the `http_handler`
for `root(.)` does a redirect to `static('simple_client.html')`, which
causes the browser to fetch that HTML (using the server). All other
static files are accessed by the same mechanism.

The `simple_client.html` file has this line in its `<head>`:
```
<script src="simple_client.js" defer="def"></script>
```
which contains a function `renderPage()`, which is invoked by:
```
<body onload="renderPage();">
```

The JavaScript code in the client communicates with the server using
the `fetchFromServer` function, which sends a stringified JSON data
structure containing the request and sets a callback for processing
the result (which is also a JSON-encoded data structure).

On the server, the `http_handler` for `root(json)` (`/json`) calls
`reply_with_json/1`, which deserializes the JSON request and calls
`json_response/2` to deal with the specific request.


## Miscellaneous

The `favico.ico` is taken from `http://swi-prolog



