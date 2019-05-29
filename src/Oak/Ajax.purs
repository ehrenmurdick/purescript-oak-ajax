module Oak.Ajax
  ( get
  , delete
  , module Simple.Ajax
  ) where

import Prelude (Unit)
import Simple.JSON (class ReadForeign)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Console (errorShow)
import Data.Either (Either(..))
import Simple.Ajax as SA
import Simple.Ajax (AjaxError)
import Affjax (URL)
import Effect (Effect)

handler :: ∀ a msg.
  ReadForeign a =>
  (Either AjaxError a -> msg)
    -> (msg -> Effect Unit)
    -> Either Error (Either AjaxError a)
    -> Effect Unit
handler ctor h eth =
  case eth of
    Left e  -> errorShow e
    Right v -> h (ctor v)

mkNoRequestBodyFun :: ∀ a msg.
  ReadForeign a =>
     (URL -> Aff (Either AjaxError a))
      -> (Either AjaxError a -> msg)
      -> URL
      -> (msg -> Effect Unit)
      -> Effect Unit
mkNoRequestBodyFun f ctor url h = do
  runAff_ (handler ctor h) (f url)

get :: ∀ a msg.
  ReadForeign a =>
  (Either AjaxError a -> msg)
    -> URL
    -> (msg -> Effect Unit)
    -> Effect Unit
get = mkNoRequestBodyFun SA.get

delete :: ∀ a msg.
  ReadForeign a =>
  (Either AjaxError a -> msg)
    -> URL
    -> (msg -> Effect Unit)
    -> Effect Unit
delete = mkNoRequestBodyFun SA.delete
