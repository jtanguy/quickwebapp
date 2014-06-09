quickwebapp
===========
A quick-and-dirty api generator, for any function 'Text -> Text'.
Inspired from the 'interact' function from 'Prelude'.

~~~{haskell}
interactWeb [ reverse ]
~~~

This creates a server listening on port 3000. A `GET` request on the server
returns a JSON file containing all the endpoints
(here, we would have `[ "convert" ]`).

One can specify the name of the endpoint and content type with `<?>` and
`<::>`:

~~~{haskell}
interactWeb [ reverse <?> "reverse" <::> "text/plain"
            , id <?> "echo" <::> "text/rtf"
            ]
~~~
