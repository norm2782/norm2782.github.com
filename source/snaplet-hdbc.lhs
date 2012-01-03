---
layout: page
title: "HDBC Snaplet Tutorial"
date: 2012-01-03 08:23
comments: true
sharing: true
footer: true
---

About the snaplet-hdbc library
---------------------------------

Many applications use a relational database for information storage. A popular
library, called HDBC, already exists to make this possible. The snaplet-hdbc
library provides two snaplets to make it easier to integrate HDBC into your web
application: `Snap.Snaplet.Auth.Backends.Hdbc` and `Snap.Snaplet.Hdbc`. The
former serves as a backend for Snap's own authentication snaplet, while the
latter provides an abstraction over the plain HDBC functions. This tutorial
assumes that you are familiar with writing your own snaplet. If you are not
familiar with snaplets yet, please consider studying the
[snaplet tutorial](http://snapframework.com/docs/tutorials/snaplets-tutorial)
first.

In this tutorial we will write a small application that uses these two snaplets
to interact with an SQLite database. The ideas presented here are implemented
in [one of my applications](https://github.com/norm2782/JCU).

Installing
----------

Installing the snaplet is easy: just do a `cabal install snaplet-hdbc` and
you're all set.


Literate Haskell
----------------

This tutorial is written in Literate Haskell, so we need to enable some
extensions and define our imports first.

```haskell

> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
>
> module Application where
>
> import            Control.Monad
> import            Control.Monad.State
> import            Data.ByteString.Char8 (ByteString)
> import qualified  Data.ByteString.Char8 as BS
> import            Data.Lens.Template
> import            Data.Map ((!))
> import            Data.Maybe
> import            Data.String
> import            Database.HDBC.Sqlite3
> import            Snap.Core
> import            Snap.Snaplet
> import            Snap.Snaplet.Auth
> import            Snap.Snaplet.Auth.Backends.Hdbc
> import            Snap.Snaplet.Hdbc
> import            Snap.Snaplet.Session
> import            Snap.Snaplet.Session.Backends.CookieSession

```

Snaplet state
-------------

Our goal is to be able to authenticate against a database, remember the fact
that we are logged in and then retrieve some information from the database.
Before we can do so, we need to define our snaplet's state and generate the
corresponding lenses:

```haskell

> data App
>   = App
>   { _authLens :: Snaplet (AuthManager App)
>   , _sessLens :: Snaplet SessionManager
>   , _dbLens   :: Snaplet (HdbcSnaplet Connection IO)
>   }
>
> makeLens ''App

```

The `authLens` allows us to do the actual authentication, while the `sessLens`
allows us to remember the result of the authentication attempt. As you might
have guessed, the `dbLens` allows us to interact with the database. The
`AuthManager`, `SessionManager` and `HdbcSnaplet` types are provided by the
corresponding snaplets, while the `Connection` type is provided by HDBC. In
this case, `Connection` represents a connection to our SQLite database.

After having defined the application state, we can start writing our
initialiser:

```haskell

> tutorialAppInit :: SnapletInit App App
> tutorialAppInit = makeSnaplet "snaplet-hdbc-tutorial"
>   "A tutorial snaplet showing the use of snaplet-hdbc" Nothing $ do
>     addRoutes  [ ("/some/:num",  someNumHandler) ]
>     _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager
>                     "config/site_key.txt" "_session" Nothing
>     let sqli = connectSqlite3 "resources/tutorial.db"
>     _dblens'   <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
>     _authlens' <- nestSnaplet "auth" authLens $ initHdbcAuthManager
>                     defAuthSettings sessLens sqli defAuthTable defQueries
>     return  $ App _authlens' _sesslens' _dblens'

```

Many things are happening in this initialiser. First we define a route and a
fallback handler, after which we initialise a session manager which stores the
session information in a cookie. We then use the `connectSqlite3` function from
HDBC to make a connection to the `tutorial.db` file locate in the `resources`
directory, which sits in our project directory. This gives us a value of type
`IO Connection` in `sqli`. We use this raw connection to initialise both the
HDBC snaplet and the HDBC authentication snaplet, which happens on the
following two lines.

Initialising the HDBC snaplet is simple enough. We just pass the SQLite
connection we obtained earlier  to the initialiser and use to usual
`nestSnaplet` function to nest the HDBC snaplet in our application snaplet. The
HDBC authentication snaplet on the other hand, has several parameters. Lets go
through them one by one:

  - `defAuthSettings` is exposed by the authentication snaplets and  offers a
    default value of type `AuthSettings`, which contains information about your
    sessions.
  - `sessLens` is the session lens that has been generated by Template Haskell
    using the `makeLens` function.
  - `sqli` is the raw database connection we have defined earlier in the
    initialiser.
  - `defAuthTable` is exposed by the HDBC authentication snaplet and it
    provides a default set of column names for the table against you will
    authenticate. It is of type `AuthTable`.
  - `defQueries` is again exposed by the HDBC authentication snaplet and
    contains a default set of functions which generate the SQL queries the
    snaplet uses for the authentication process. Its type is `Queries`.

By separating the column names from the actual queries, it becomes possible to
support varying use-cases. In the simplest case, the application uses the
default table layout and the default queries. When different column names are
desired, one will only need to provide a custom value of type `AuthTable` and
the queries will automatically use these new names. If completely custom
queries are desired, one can easily override them by providing a custom value
of type `Queries`. This allows for, e.g., using stored procedures for
authentication. This tutorial will not cover authentication (that's another
tutorial), nor overriding the default queries. Please see the snaplet's API
documentation and code to learn how to do that.

After all lenses are initialised, the `App` constructor is applied to all newly
created lenses and the initialiser's work is done.


Querying the database
---------------------

Now that the snaplet is initialised, we can start querying our database.
Amongst other functions, the HDBC snaplet offers the `query` and `query'`
functions:

```haskell

query  :: HasHdbc m c s => String -> [SqlValue] -> m [Row]
query' :: HasHdbc m c s => String -> [SqlValue] -> m Integer

```

These functions make it easy to execute basic queries. The first one is
intended to be used for `SELECT` queries. It takes a `String` with the query
and a list of `SqlValue`s. The `SqlValue`s will be inserted on the places where
there is have a `?` in the query. The result of the query is a list of `Row`s.
A row is nothing more than a type synonym for a `Map`:

```haskell

type Row = Map String SqlValue

```

The second one is intended to be used with any other type of query, usually
the `INSERT`, `UPDATE` and `DELETE` queries.

Now suppose we have some table containing messages, we can now define a
function to retrieve them based on the value of some integer:

```haskell

> data Message = Message String deriving Show
>
> getMessages :: HasHdbc m c s => ByteString -> m [Message]
> getMessages n = do
>   rows <- query "SELECT * FROM messages WHERE somenum = ?" [toSql n]
>   return $ map toMsg rows
>   where toMsg :: Row -> Message
>         toMsg rw = Message $ fromSql (rw ! "msgcol")

```

Normally you would expect to apply this function in your application's
handlers, but these query functions are not of type `Handler`, nor are they of
type `MonadSnap`! Instead, we expect our query to be executed in the context of
the `HasHdbc` typeclass. What is that all about?


The HasHdbc typeclass
---------------------

One of the goals when designing the HDBC snaplet was to separate the functions
from the `Handler` monad, or any Snap-related monad, so that the same queries
could also be run outside of a Snap context, for example in a command-line
tool. This goal is accomplished by the `HasHdbc` class. Whenever we are in some
monad and a corresponding connection for which this class is defined, we can
use the functions provided by the snaplet. In order to be able to use the
snaplet's functions in our application handlers, we need to make sure that our
application's handlers become an instance of `HasHdbc`. Before we can do that,
however, we need to understand the `HasHdbc` typeclass:

```haskell

class  (  IConnection c
       ,  ConnSrc s
       ,  MonadCatchIO m
       )
  =>   HasHdbc m c s | m -> c s where
  getHdbcState :: m (HdbcSnaplet c s)

```

Again there are many things happening on only three lines of code. First we see
that the typeclass is parameterised by three type variables, `m`, `c` and `s`.
The `m` is any monad for which there is a `MonadCatchIO` instance available.
As it so happens, `Handler` has an instance for this out of the box. The second
parameter needs to be some type for which we also have an instance of
`IConnection`, which is a typeclass provided by HDBC. The `Connection` type we
saw earlier is such a type. The `s` parameter needs to be something of type
`ConnSrc`; some context from which we can get a new connection. By default, the
snaplet provides `ConnSrc` instances for `IO` and `Pool` (from the
`resource-pool` package; it offers resource pooling capabilities). Since our
example state type defines `s` to be `IO`, we don't have to worry about this
parameter anymore. Finally, the functional dependency `m -> c s` says: "if we
know the type of `m`, we also know the corresponding types of `c` and `s`". Or:
"`m` uniquely determines `c` and `s`". This is very powerful, because we can
now do things with a connection, even if we only know in which monad we are.
The downside is that we cannot use two different adapter types with our
application handlers. The typeclass defines an expression `getHdbcState`, which
only requires you to yield the snaplet's state type in the context of monad `m`.

Getting back to our application, we want to define an instance of `HasHdbc` for
our application's handlers and we want the `getHdbcState` expression to give us
something of type `HdbcSnaplet Connection IO` in the context of these handlers.
A common type for an application handler would be `Handler App App`. With that
in mind, lets see what the `getHdbcState` type would look like if we were to
instantiate the typeclass:

```haskell

getHdbcState :: Handler App App (HdbcSnaplet Connection IO)

```

It looks like we're just defining a regular `Handler`, like we always do in our
Snap applications, but where do we get an `HdbcSnaplet Connection IO`? It turns
out that this is exactly the HDBC snaplet's state type, so all we need to do is
get the state from the snaplet using the `dbLens` and the state monad's `get`
function:

```haskell

instance HasHdbc (Handler App App) Connection IO where
  getHdbcState = with dbLens get

```

And we're done! Now we can interact with the database in our handlers, using
the functions provided by the HDBC snaplet.


Putting it all together
-----------------------

Now that we have all the basics, we can finally write a handler which interacts
with the database. Lets create the `someNumHandler` from the example
initialiser. It reads an integer from the URL and uses that integer to
parameterise a database query:

```haskell

> someNumHandler :: Handler App App ()
> someNumHandler = do
>   mnum <- getParam "num"
>   let n = fromMaybe "11" mnum
>   msgs <- getMessages n
>   writeBS . BS.pack $ show msgs

```

Since `Handler` now has a `HasHdbc` instance, we can happily execute our
queries from the context of our handlers.


Wrapping up
-----------

If you are already a bit more familiar with snaplets, you might have notices
that the `HasHdbc` instance for `Handler` is not as general as it could be.
Since our implementation of `getConnSrc` is only interested in the current
snaplet's state, we can leave the type of the first parameter to `Handler`
variable. This gives us the instance:

```haskell

> instance HasHdbc (Handler b App) Connection IO where
>   getHdbcState = with dbLens get

```

That's it! You can now write web applications, backed by an HDBC-supported
database of your choosing.


Using `resource-pool`
---------------------

The snaplet comes with support for the `resource-pool` package built-in. If you
wish to use it, change the type of the subsnaplet from

```haskell

  , _dbLens   :: Snaplet (HdbcSnaplet Connection IO)

```

to

```haskell

  , _dbLens   :: Snaplet (HdbcSnaplet Connection Pool)

```

and do not forget to import `Data.Pool`.

