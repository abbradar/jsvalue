module JSValue
       ( JSValue(..)
       , true
       , false
       , null
       , undefined
       , (===)
       , not
       , (&&)
       , (||)
       ) where

import Prelude hiding (null, undefined, not, (&&), (||))
import qualified Data.Bool as B
import Data.Maybe
import Control.Applicative
import Data.String (IsString(..))
import Text.Read (readMaybe)

data JSValue = JSNumber Double
             | JSString String
             | JSBool Bool
             | JSNull
             | JSUndefined

nan :: Double
nan = 0 / 0

true :: JSValue
true = JSBool True

false :: JSValue
false = JSBool False

null :: JSValue
null = JSNull

undefined :: JSValue
undefined = JSUndefined

toNumber :: JSValue -> Double
toNumber (JSNumber n) = n
toNumber (JSString str) = fromMaybe nan $ readMaybe str
toNumber (JSBool False) = 0
toNumber (JSBool True) = 1
toNumber JSNull = 0
toNumber JSUndefined = nan

toString :: JSValue -> String
toString (JSNumber n) = show n
toString (JSString s) = s
toString (JSBool True) = "true"
toString (JSBool False) = "false"
toString JSNull = "null"
toString JSUndefined = "undefined"

toBool :: JSValue -> Bool
toBool (JSNumber n) = n /= 0
toBool (JSString s) = s /= ""
toBool (JSBool b) = b
toBool JSNull = False
toBool JSUndefined = False

strictEq :: JSValue -> JSValue -> Maybe Bool
strictEq (JSNumber a) (JSNumber b) = Just $ a == b
strictEq (JSString a) (JSString b) = Just $ a == b
strictEq (JSBool a) (JSBool b) = Just $ a == b
strictEq JSNull JSNull = Just True
strictEq JSUndefined JSUndefined = Just True
strictEq _ _ = Nothing

(===) :: JSValue -> JSValue -> Bool
a === b = fromMaybe False $ strictEq a b

infix 4 ===

instance Eq JSValue where
  a == b = fromMaybe False $ strictEq a b <|> partEq a b <|> partEq b a
    where partEq JSNull JSUndefined = Just True
          partEq (JSNumber a) b@(JSString _) = Just $ a == toNumber b
          partEq a@(JSBool _) b = Just $ (JSNumber $ toNumber a) == b
          partEq _ _ = Nothing

instance IsString JSValue where
  fromString = JSString

instance Show JSValue where
  show = toString

instance Num JSValue where
  a + (JSString b) = JSString $ toString a ++ b
  (JSString a) + b = JSString $ a ++ toString b
  a + b = JSNumber $ toNumber a + toNumber b

  a - b = JSNumber $ toNumber a - toNumber b
  a * b = JSNumber $ toNumber a * toNumber b
  abs = JSNumber . abs . toNumber
  signum = JSNumber . signum . toNumber
  fromInteger = JSNumber . fromInteger
  negate = JSNumber . negate . toNumber

instance Fractional JSValue where
  a / b = JSNumber $ toNumber a / toNumber b
  fromRational = JSNumber . fromRational

not :: JSValue -> JSValue
not = JSBool . B.not . toBool

(&&) :: JSValue -> JSValue -> JSValue
a && b = JSBool $ toBool a B.&& toBool b

infixr 3 &&

(||) :: JSValue -> JSValue -> JSValue
a || b = JSBool $ toBool a B.|| toBool b

infixr 2 ||
