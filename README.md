quickwebapp
===========
A quick-and-dirty api generator, for any function `a -> b` which can be wrapped
inside a function `ByteString -> ByteString`.
It is inspired from the 'interact' function from 'Prelude'.

~~~{haskell}
interactWeb reverse
~~~

This creates a server listening on port 3000.

