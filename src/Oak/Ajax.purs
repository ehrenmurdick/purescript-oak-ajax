module Oak.Ajax (get) where

import Prelude (Unit)
import Simple.JSON (class ReadForeign)
import Effect.Aff (Error, runAff_)
import Effect.Console (errorShow)
import Data.Either (Either(..))
import Simple.Ajax (get, AjaxError) as SA
import Affjax (URL)
import Effect (Effect)

handler :: ∀ a msg.
  ReadForeign a =>
  (Either SA.AjaxError a -> msg)
    -> (msg -> Effect Unit)
    -> Either Error (Either SA.AjaxError a)
    -> Effect Unit
handler ctor h eth =
  case eth of
    Left e  -> errorShow e
    Right v -> h (ctor v)

get :: ∀ a msg.
  ReadForeign a =>
  (Either SA.AjaxError a -> msg)
    -> URL
    -> (msg -> Effect Unit)
    -> Effect Unit
get ctor url h = do
  runAff_ (handler ctor h) (SA.get url)
