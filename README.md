quickwebapp
===========
A quick-and-dirty api generator, for any function `a -> Either String b`.

It is inspired from the `interact` function from the Prelude.

~~~{haskell}
interactWeb (reverse :: String -> String)
~~~

This creates a server listening on port 8080. You can change the port with the `PORT` env variable.

You can query it via a browser at <http://localhost:8080> or by using
curl/httpie

httpie
------

~~~{bash}
http :8080 input="<your input string>"
~~~

curl
----

~~~{bash}
curl localhost:8080 -d input="<your input string>"
~~~

