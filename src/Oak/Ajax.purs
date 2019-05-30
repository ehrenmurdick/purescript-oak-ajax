module Oak.Ajax
  ( module Simple.Ajax
  , post
  , get
  , delete
  , simpleRequest
  ) where

import Prelude (Unit)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Effect.Aff (Aff, Error, runAff_)
import Data.HTTP.Method (Method(..))
import Effect.Console (errorShow)
import Data.Either (Either(..))
import Simple.Ajax as SA
import Data.MediaType (MediaType(..))
import Affjax.RequestHeader (RequestHeader(..))
import Simple.Ajax (AjaxError)
import Affjax (URL)
import Effect (Effect)
import Data.Maybe


simpleRequest :: ∀ a b.
  WriteForeign a
  => ReadForeign b
  => Method
    -> URL
    -> Maybe a
    -> Aff (Either AjaxError b)
simpleRequest m =
  SA.simpleRequest
    (Left m)
    { headers:
      [ Accept (MediaType "application/json")
      , ContentType (MediaType "application/json")
      ]
    }

handler :: ∀ a.
  ReadForeign a =>
  (Either AjaxError a -> Effect Unit)
    -> Either Error (Either AjaxError a)
    -> Effect Unit
handler h eth =
  case eth of
    Left e  -> errorShow e
    Right v -> h v


emptyBody :: Maybe String
emptyBody = Nothing

get :: ∀ a.
  ReadForeign a =>
  URL
    -> (Either AjaxError a -> Effect Unit)
    -> Effect Unit
get url h =
  let getFun = simpleRequest GET in do
  runAff_ (handler h) (getFun url emptyBody)

delete :: ∀ a.
  ReadForeign a =>
  URL
    -> (Either AjaxError a -> Effect Unit)
    -> Effect Unit
delete url h =
  let getFun = simpleRequest DELETE in do
  runAff_ (handler h) (getFun url emptyBody)

post :: ∀ a b.
  WriteForeign a
    => ReadForeign b
    => Maybe a
    -> URL
    -> (Either AjaxError b -> Effect Unit)
    -> Effect Unit
post dat url h =
  let postFun = simpleRequest POST in do
  runAff_ (handler h) (postFun url dat)

