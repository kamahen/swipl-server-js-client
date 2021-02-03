# Simple SWI-Prolog - Javascript client/server

A simple example of a SWI-Prolog server and a Javascript client.

See also: https://www.swi-prolog.org/howto/http/

## License
[Simplified BSD](LICENSE)

## Javascript/ECMAScript

“We are tied down to a language which makes up in obscurity what it lacks in style.”
<br/>— Tom Stoppard, _Rosencrantz and Guildenstern are Dead_

1995 — Brendan Eich reads up on every mistake ever made in designing a
programming language, invents a few more, and creates
LiveScript. Later, in an effort to cash in on the popularity of Java
the language is renamed JavaScript. Later still, in an effort to cash
in on the popularity of skin diseases the language is renamed
ECMAScript.
<br/>— [A Brief, Incomplete, and Mostly Wrong History of Programming Languages](http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html)

## Warning

This code is an overly simple example of client-server code, intended
as a tutorial or to extend the HTTP support
[documentation](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27))
in the SWI-Prolog manual and the tutorial at
[http://www.pathwayslms.com/swipltuts/html](http://www.pathwayslms.com/swipltuts/html/index.html).

The example code sends a query to the server, which executes it and
returns the result. Obviously, this is a dangerous thing to do without
proper sand-boxing, but it's useful for tutorial purposes. (In other
words, don't run this server anywhere that someone from the "outside"
can access it.)

The code has only been tested with a recent "development" release of
SWI-Prolog (8.3.7) on Ubuntu 18.0.4 and with the Chrome browser. The
server has been
[reported](https://swi-prolog.discourse.group/t/simple-prolog-server-with-javascript-client/2898/2)
to run under Windows 10 from the DOS prompt, with the client on
Microsoft Edge browser.

## How to run

Install [SWI-Prolog](http://www.swi-prolog.org/Download.html) development version.

For Ubuntu, Debian, and similar (following the instructions at https://www.swi-prolog.org/build/PPA.html), use these steps:

* `sudo apt install software-properties-common`
* `sudo apt-add-repository ppa:swi-prolog/devel`
* `sudo apt update`
* `sudo apt install swi-prolog`

Start the server:

    swipl simple_server.pl --port=9999 --staticdir=static

In a browser, start the client: [http://localhost:9999](http://localhost:9999).

You might try this query:

    get_time(TimeStamp), stamp_date_time(TimeStamp, DateTime, local), bagof(Key-Value, date_time_value(Key, DateTime, Value), DatePieces), dict_create(DateDict, date, DatePieces)

or this:

    setof(K:V,current_prolog_flag(K,V), Flags), forall(member(K:V, Flags), format('~|~q:~t~40+~q~n', [K,V]))

## Code overview

There are three client components and one server component:

* `static/simple_client.html`
* `static/simple_client.css`
* `static/simple_client.js`
* `simple_server.pl`

If the server is accessed at the top level ("`/`"), the `http_handler`
for `root(.)` does a redirect to `static('simple_client.html')`, which
causes the browser to fetch that HTML (using the server). All other
static files are accessed by the same mechanism.

*  The HTML path `static('simple_client.html')` is resolved
   using `http:location/2` facts - see the comments there.

*  `static_dir('simple_client.html')` is resolved using
   [file_search_path/2](https://www.swi-prolog.org/pldoc/man?predicate=file_search_path/2).
   During server start-up, `assert_server_locations/1` is called
   to asserta a file_search_path fact to redirect `static(Path)` to
   the directory specified by the option `--staticdir`).

The `simple_client.html` file has this line in its `<head>`:

    <script src="simple_client.js" defer="defer"></script>

which contains a function `renderPage()`, which is invoked by:


    <body onload="renderPage();">

(The `simple_client.js` code is fetched from the server, using the
`http_handler/3` for `static(.)`, which in turn uses
`http_reply_from_files/2` (in `library(http/http_files)` to do the
work.)

The JavaScript code in the client communicates with the server using
the `fetchFromServer` function, which sends a stringified JSON data
structure containing the request and sets a callback for processing
the result (which is also a JSON-encoded data structure).

On the server, the `http_handler` for `root(json)` (`/json`) calls
`reply_with_json/1`, which deserializes the JSON request and calls
`json_response/2` to deal with the specific request.


## Miscellaneous

The `favico.ico` is taken from [http://swi-prolog.org](http://swi-prolog.org).
