Oak.Ajax is a module that leverages the Simple.Ajax module to provide an easy
way to add ajax requests to your Oak application. Becuase the next function in
an Oak app returns any Effect, it's pretty easy to implement your own ajax or
any other kind of side effects, but if you want the ease of making http
requests like an Elm app, then this module is for you!

Below you can see the source of an example Oak app that uses this module to
make an http request, then uses the response from that request to show some
text on the page.

This package does not depend on Oak because it's not actually Oak specific,
it's api is just handily shaped to work well within an Oak application.

```
module Main (main) where

import Prelude
  ( Unit
  , bind
  , mempty
  , unit
  , (>>>)
  )
import Oak.Html.Events (onClick)
import Data.Either (Either(..))
import Effect (Effect)
import Oak
  ( runApp
  , App
  , createApp
  )
import Oak.Html
  ( Html
  , div
  , text
  , button
  )
import Oak.Document
  ( appendChildNode
  , getElementById
  )
import Oak.Ajax
  ( get
  , AjaxError
  )


type Model = { message :: String }

type Response = { text :: String }

data Msg
  = Get
  | GetResult (Either AjaxError Response)


view :: Model -> Html Msg
view model =
  div []
    [ div [] [ button [ onClick Get ] [ text "get" ] ]
    , div [] [ text model.message ]
    ]


next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg mod h =
  case msg of

    (GetResult _) -> mempty

    Get -> get "/1.json" (GetResult >>> h)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Get                        -> model
    (GetResult (Left e))       -> model
    (GetResult (Right result)) -> model { message = result.text }


init :: Unit -> Model
init _ =
  { message: ""
  }

app :: App Model Msg Unit
app = createApp
  { init: init
  , view: view
  , update: update
  , next: next
  }

main :: Effect Unit
main = do
  rootNode <- runApp app Nothing
  container <- getElementById "app"
  appendChildNode container rootNode
```
