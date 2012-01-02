---
layout: post
title: "Using digestive-functors with Heist"
date: 2012-01-01 21:23
comments: true
categories: [Haskell, Snap, digestive-functors, Heist]
---

In this post we will make digestive-functors and Heist play together
nicely. We will see how we can create and validate forms using vanilla
digestive-functors and render these, together with potential validation
errors, in a Heist template.

<!-- more -->

Currently this post is secretly just here to allow me to play with
Octopress.

This post assumes you are familiar with the snaplet infrastructure and
that you are more or less comfortable with defining routes and rendering
templates with Heist. If this is not the case, you might want to read
some of the tutorials on the Snap website first. This post also assumes
that you know how to work with the digestive-functors library.

Since this post is written as a Literate Haskell file, we first define
some imports and other boilerplate:

``` haskell

> {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
>
> module Site (app) where
>
> import            Control.Applicative
> import            Control.Monad.State
> import            Data.Lens.Template
> import            Data.Maybe
> import            Data.Text (Text)
> import qualified  Data.Text as T
> import            Snap.Core
> import            Snap.Snaplet
> import            Snap.Snaplet.Heist
> import            Text.Blaze
> import qualified  Text.Blaze.Html5 as H
> import qualified  Text.Blaze.Html5.Attributes as A
> import            Text.Blaze.Internal (HtmlM(..))
> import            Text.Blaze.Renderer.XmlHtml
> import            Text.Digestive
> import            Text.Digestive.Forms.Snap
> import            Text.Digestive.Blaze.Html5
> import qualified  Text.Email.Validate as E
> import            Text.Templating.Heist

```

Since we are using Snap 0.7 at the moment of writing, we start by defining
out snaplet state type, generating some lenses using Template Haskell and
defining a handy type synonym for our handlers. We also want to use Heist,
so we need to define a `HasHeist` instance for our `App` type as well.
And, since this is a snaplet, we need to define our snaplet initialiser.

``` haskell

> data App
>   =  App
>   {  _heist :: Snaplet (Heist App)
>   }
>
> makeLens ''App
>
> type AppHandler = Handler App App
>
> instance HasHeist App where
>   heistLens = subSnaplet heist
>
> app :: SnapletInit App App
> app = makeSnaplet "hdf"
>   "An example of digestive-functors and Heist playing nicely together."
>   Nothing $ do
>     h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
>     addRoutes [ ("/", formHandler) ]
>     return $ App h

```

Having written all the boilerplate, we can get started with defining our form.
In this case it's a simple login form with a plain textfield, a password field,
a remember me checkbox and a login button.

When the form has been validated, we want to store the form data in a custom
datatype:

``` haskell

> data LoginData
>   =  LoginData
>   {  emailAddress :: Text
>   ,  password :: Text
>   ,  rememberMe :: Bool }

```

Defining the form is straight-forward if you are used to working with
digestive-functors. The form is wrapped in divs for better styling options and
we attach validators to make sure that we get a valid email address and a long
enough password. The `isValid` function comes from the email-validate library.

``` haskell

> loginForm :: Form AppHandler SnapInput Html (FormHtml Html) LoginData
> loginForm = (\e p r _ -> LoginData e p r)
>   <$>  mapViewHtml H.div (
>        label  "Email address: "
>        ++>    inputText Nothing `validate` isEmail
>        <++    errors)
>   <*>  mapViewHtml H.div(
>        label  "Password: "
>        ++>    inputPassword False `validate` longPwd
>        <++    errors)
>   <*>  mapViewHtml H.div (
>        label  "Remember me?"
>        ++>    inputCheckBox True)
>   <*>  mapViewHtml H.div (
>        submit "Login")
>
> isEmail :: Validator AppHandler Html Text
> isEmail = check "Invalid email address" (E.isValid . T.unpack)
>
> longPwd :: Validator AppHandler Html Text
> longPwd  =  check "Password needs to be at least six characters long"
>          $  \xs -> T.length xs >= 6

```

Up to this point we have not seen anything new yet, so lets start with
something a bit more interesting. For most of my Snap apps I use the following
function to render a form:

``` haskell

> showForm :: AttributeValue -> AttributeValue -> FormHtml (HtmlM a) -> Html
> showForm act mth frm =
>   let  (formHtml', enctype) = renderFormHtml frm
>   in   H.form  ! A.enctype (H.toValue $ show enctype) ! A.method mth
>                ! A.action act $ formHtml' >> return ()

```

It takes an `AttributeValue` containing the target of the form, an
`AttributeValue` containing the HTTP request method and a form as produced by
the `eitherSnapForm` function we will see below, resulting in a rendered form
of type `Html`.

Now for the request handler, which is where most of the action will take place.
We want to make our lives easy, so we call in the help of the
digestive-functors-snap library, which provides the `eitherSnapForm` function.
This function can be applied to a digestive-functors form and a form name,
after which it will use the Snap API to parse the request. Before continueing,
lets have a look at some code:

``` haskell

> formHandler :: AppHandler ()
> formHandler = do
>   res <- eitherSnapForm loginForm "login-form"
>   case res of
>     Left form -> do
>       let nodes = renderHtmlNodes $ showForm "/" "post" form
>       heistLocal (bindSplice "formElm" (return nodes)) $ render "formTpl"
>     Right (LoginData e p r) -> writeBS "Success!"

```

The result of `eitherSnapForm` is an `Either` value. When the form has not been
submitted yet, or if a submitted form failed validation, the result will be a
`Left` constructor containing a form of type `FormHtml (HtmlM a)`. When a form
has been submitted and has succesfully passed validation, we will get a `Right`
value containing the constructor applied in our form (in this case the
`LoginData` constructor).

Rendering the form is done when we get a `Left` result. As it turns out, it is
almost trivially easy to render the form in Heist. To bind the form as a Heist
splice, we first need to render it to an `Html` value using our `showForm`
function. Since Heist cannot work with values of type `Html`, we have to
convert the `Html` to something Heist does understand. Luckily, the xmlhtml
library provides us with a function that does just that: `renderHtmlNodes ::
Html -> [Node]`. Heist loves a list of `Node`s, so all we need to do is
`return` it to the `Splice` context so we can bind it as a splice to our
template.

The final piece of the puzzle is the template in which the form needs to be
rendered. As you can see, rendering the form--including potential validation
error messages--is done by adding nothing but a single element to the template.

``` xml
<html>
  <head>
    <title>Heist and digestive-functors playing nice</title>
  </head>
  <body>
    <formElm />
  </body>
</html>
```

With this, we have seen how to use digestive-functors and Heist together in a
win-win scenario. On the one hand you mostly maintain your separation of
concerns by using Heist for most of your HTML output, while on the other hand
you can enjoy the great digestive-functors library as-is.
