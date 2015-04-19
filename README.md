# [Hutton][]

A Haskell program for [the button][] on Reddit.

Use `hutton` to automatically press the button whenever you want. In the mean
time, it will display information about the button. Install it with:

``` sh
$ cabal install hutton
```

Use it like this:

``` sh
$ hutton THRESHOLD USERNAME PASSWORD
```

For example:

``` sh
# Automatically press the button when the timer is at 10 seconds or less.
$ hutton 10 taylorfausak secret
Logging in...
Logged in.
    cookie = ...
    modhash = ...
Getting query parameters...
Got query parameters.
    h = ...
    e = ...
Connecting to WebSocket...
Connected to WebSocket.

60	787780	2015-04-19 18:10:24 UTC	b444ecb98ce9a2786df8fbe5eabd6ef960daaae9
59	787780	2015-04-19 18:10:25 UTC	76bbd3f085f3c7ec335bd0e5b7ad085d082ffcb5
58	787780	2015-04-19 18:10:26 UTC	75a5631e8a7d8c86964f746113e7a12dd4f9585e
# Columns:
#   1. Timer
#   2. Number of pressers
#   3. Timestamp
#   4. MAC
```

[hutton]: https://github.com/tfausak/hutton
[the button]: https://www.reddit.com/r/thebutton
