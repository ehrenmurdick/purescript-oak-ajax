module Oak.Ajax where

import Prelude (Unit)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Effect.Aff (Aff, Error, runAff_)
import Data.HTTP.Method (CustomMethod, Method(..))
import Effect.Console (errorShow)
import Data.Either (Either(..))
import Simple.Ajax as SA
import Data.MediaType (MediaType(..))
import Affjax.RequestHeader (RequestHeader(..))
import Simple.Ajax (AjaxError)
import Affjax (URL)
import Effect (Effect)
import Data.Maybe

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


-- delete :: ∀ a msg.
--   ReadForeign a =>
--   (Either AjaxError a -> msg)
--     -> URL
--     -> (msg -> Effect Unit)
--     -> Effect Unit
-- delete = mkNoRequestBodyFun SA.delete


-- simpleRequest ::
--   forall a b r rx t.
--   WriteForeign a =>
--   ReadForeign b =>
--   Row.Union r SimpleRequestRow rx =>
--   Row.Union r (RequestRow String) t =>
--   Row.Nub rx SimpleRequestRow =>
--   Row.Nub t (RequestRow String) =>
--   Either Method CustomMethod ->
--   { | r } ->
--   URL ->
--   Maybe a ->
--   Aff (Either AjaxError b)

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

